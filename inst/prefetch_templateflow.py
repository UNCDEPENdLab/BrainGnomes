#!/usr/bin/env python3
import argparse
import hashlib
import json
import os
import sys
import time
from dataclasses import asdict, dataclass, is_dataclass
from datetime import datetime, timezone
from itertools import product
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Set, Tuple, Union

try:
    from templateflow import api  # type: ignore
except ImportError as exc:  # pragma: no cover
    raise SystemExit(f"TemplateFlow is required to prefetch templates: {exc}") from exc

try:
    from niworkflows.utils.spaces import Reference, SpatialReferences  # type: ignore
except ImportError:  # pragma: no cover
    Reference = SpatialReferences = None  # type: ignore[misc]

SKIP_TOKENS = {
    "anat",
    "func",
    "T1w",
    "T2w",
    # surface-only spaces
    "fsnative",
    "fsaverage",
    "fsaverage5",
    "fsaverage6",
    "fsLR",
}

MRIQC_REQUIRED_PROBSEG = {
    "MNI152NLin2009cAsym": [
        {
            "suffix": "probseg",
            "label": "CSF",
            "resolution": 1,
            "extension": "nii.gz",
        }
    ]
}

QueryFingerprint = Tuple[str, Tuple[Tuple[str, Any], ...]]


@dataclass
class QuerySpec:
    template: str
    params: Dict[str, Any]
    label: str
    optional: bool = False
    allow_desc_retry: bool = False


DEBUG = False
DEFAULT_RESOLUTION = 1
DEFAULT_API_GET_MAX_RETRIES = 2
DEFAULT_API_GET_RETRY_DELAY_SECONDS = 1.0
DEFAULT_FALLBACK_DESC_BY_SUFFIX = {
    "T1w": None,
    "mask": "brain",
}


def dbg(message: str) -> None:
    if DEBUG:
        print(f"[DEBUG] {message}")


def parse_spaces(spaces: str) -> List[str]:
    """Split a whitespace-delimited string and drop duplicates while preserving order."""
    seen = set()
    ordered: List[str] = []
    for token in spaces.split():
        token = token.strip()
        if not token:
            continue
        if token in seen:
            continue
        seen.add(token)
        ordered.append(token)
    return ordered


def _coerce_int_if_possible(value: Any) -> Any:
    """Convert numeric-like strings to int while preserving non-numeric values."""
    if isinstance(value, str):
        value = value.strip()
        if not value:
            return None
    try:
        return int(value)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        return value


def _split_output_space_token(token: str) -> Tuple[str, Dict[str, Any], bool]:
    """Split a token into template name, embedded query kwargs, and surface flag."""
    parts = [part.strip() for part in token.split(":")]
    template = parts[0] if parts else token

    # Detect obvious surface spaces
    is_surface = template in {"fsnative", "fsaverage", "fsaverage5", "fsaverage6", "fsLR"}
    query: Dict[str, Any] = {}

    for qual in parts[1:]:
        if not qual:
            continue

        key, sep, raw_value = qual.partition("-")
        if not sep:
            continue
        key = key.strip().lower()
        raw_value = raw_value.strip()
        if not raw_value:
            continue

        if key == "res":
            resolution = _coerce_resolution(raw_value)
            if resolution is not None:
                query["resolution"] = resolution
            continue
        if key == "cohort":
            query["cohort"] = _coerce_int_if_possible(raw_value)
            continue
        if key == "atlas":
            query["atlas"] = raw_value
            continue
        if key in {"den", "density"}:
            query["density"] = raw_value
            is_surface = True
            continue
        if key in {"hemi", "hemisphere"}:
            query["hemi"] = raw_value
            continue
        if key == "label":
            query["label"] = raw_value
            continue
        if key == "desc":
            query["desc"] = raw_value
            continue
        if key == "suffix":
            query["suffix"] = raw_value
            continue
        if key == "space":
            query["space"] = raw_value
            continue
        if key in {"extension", "ext"}:
            query["extension"] = raw_value
            continue

    return template, query, is_surface


def _token_to_references(token: str) -> List["Reference"]:
    """Convert an output-spaces token into niworkflows Reference objects."""
    if Reference is None:  # pragma: no cover
        raise RuntimeError("niworkflows Reference class is unavailable.")

    head = token.split(":", 1)[0]
    if head in SKIP_TOKENS:
        dbg(f"Skipping non-template space '{token}'.")
        return []

    try:
        refs = Reference.from_string(token)
    except Exception as exc:
        dbg(f"Reference.from_string failed for '{token}': {exc}")
        raise

    filtered = []
    for ref in refs:
        if ref.space in SKIP_TOKENS:
            dbg(f"Dropping non-template reference {ref}.")
            continue
        if getattr(ref, "dim", 3) != 3:
            dbg(f"Dropping non-volumetric reference {ref}.")
            continue
        filtered.append(ref)
    return filtered


def parse_output_space_token(token: str) -> Tuple[str, Optional[int], bool]:
    """Parse an fMRIPrep --output-spaces token into (template, resolution, is_surface).

    Examples
    - "MNI152NLin2009cAsym:res-2" -> ("MNI152NLin2009cAsym", 2, False)
    - "MNI152NLin6Asym" -> ("MNI152NLin6Asym", None, False)
    - "fsaverage:den-10k" -> ("fsaverage", None, True)
    """
    template, embedded_query, is_surface = _split_output_space_token(token)
    resolution: Optional[int] = None
    parsed_resolution = embedded_query.get("resolution")
    if isinstance(parsed_resolution, int):
        resolution = parsed_resolution

    return template, resolution, is_surface


def resolution_preferences(tokens: List[str], override_res: Optional[int]) -> Dict[str, List[int]]:
    """Resolve per-template resolution preferences from tokens or an explicit override."""
    prefs: Dict[str, Set[int]] = {}
    for token in tokens:
        if token in SKIP_TOKENS:
            continue
        template, resolution, is_surface = parse_output_space_token(token)
        if is_surface or template in SKIP_TOKENS:
            continue
        prefs.setdefault(template, set())
        if resolution is not None:
            prefs[template].add(resolution)

    if override_res is not None:
        return {template: [override_res] for template in prefs}

    for template, res_set in prefs.items():
        if not res_set:
            res_set.add(DEFAULT_RESOLUTION)
    return {template: sorted(res_set) for template, res_set in prefs.items()}


def _paths_exist(result: Union[str, os.PathLike, Sequence[Union[str, os.PathLike]]]) -> bool:
    """Return True if the TemplateFlow api.get result resolves to at least one existing file."""
    if isinstance(result, (str, os.PathLike)):
        return Path(result).exists()
    try:
        seq = list(result)  # type: ignore[arg-type]
    except TypeError:
        return False
    if not seq:
        return False
    return any(Path(p).exists() for p in seq)


def _existing_paths(result: Union[str, os.PathLike, Sequence[Union[str, os.PathLike]]]) -> List[str]:
    """Return existing file paths resolved by TemplateFlow api.get."""
    if isinstance(result, (str, os.PathLike)):
        paths = [result]
    else:
        try:
            paths = list(result)  # type: ignore[arg-type]
        except TypeError:
            return []

    existing: List[str] = []
    seen: Set[str] = set()
    for path_obj in paths:
        path_str = os.fspath(path_obj)
        if not path_str:
            continue
        abs_path = os.path.abspath(path_str)
        if abs_path in seen or not Path(abs_path).exists():
            continue
        seen.add(abs_path)
        existing.append(abs_path)
    return existing


def _space_label(space_obj: Any) -> str:
    """Best-effort string label for a SpatialReferences space."""
    if isinstance(space_obj, str):
        return space_obj
    for attr in ("fullname", "template", "id", "name", "label", "value"):
        value = getattr(space_obj, attr, None)
        if isinstance(value, str) and value:
            return value
    return str(space_obj)


def _spec_to_dict(spec: Any) -> Dict[str, Any]:
    """Normalize a SpatialReferences spec-like object into a dict."""
    if spec is None:
        return {}
    if isinstance(spec, dict):
        data = dict(spec)
    elif is_dataclass(spec):
        data = asdict(spec)
    elif hasattr(spec, "_asdict"):
        data = spec._asdict()
    elif hasattr(spec, "items"):
        try:
            data = dict(spec.items())
        except Exception:
            data = {}
    elif hasattr(spec, "__dict__"):
        data = {k: v for k, v in vars(spec).items() if not k.startswith("_")}
    elif isinstance(spec, (list, tuple, set)):
        try:
            data = dict(spec)
        except Exception:
            data = {}
    else:  # pragma: no cover - defensive
        data = {}
    return {k: v for k, v in data.items() if v is not None}


def _coerce_resolution(value: Any) -> Optional[int]:
    """Convert a SpatialReferences resolution specification into TemplateFlow-compatible form."""
    if value in (None, "", "native"):
        return None
    if isinstance(value, (list, tuple)) and value:
        value = value[0]
    if isinstance(value, str):
        if value.startswith("res-"):
            value = value[4:]
        if not value:
            return None
    try:
        return int(value)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        return value  # allow TemplateFlow to interpret non-integer inputs


def _unique_preserving(seq: List[Any]) -> List[Any]:
    """Return list with duplicates removed while preserving order."""
    seen: Set[Any] = set()
    result: List[Any] = []
    for item in seq:
        marker = (item if isinstance(item, (str, int, float, bytes, tuple, frozenset, type(None))) else id(item))
        if marker in seen:
            continue
        seen.add(marker)
        result.append(item)
    return result


def _option_values(value: Any, allow_none: bool = False, transform=None) -> List[Any]:
    """Normalize a value (scalar or iterable) to a list, optionally keeping None."""
    if value is None:
        items: List[Any] = []
    elif isinstance(value, (list, tuple, set)):
        items = list(value)
    else:
        items = [value]

    processed: List[Any] = []
    for item in items:
        if isinstance(item, str):
            item = item.strip()
        if transform is not None:
            item = transform(item)
        if item in ("", None):
            if allow_none:
                processed.append(None)
            continue
        processed.append(item)

    if allow_none and not processed:
        processed = [None]

    return _unique_preserving(processed)


def _pop_any(data: Dict[str, Any], *keys: str) -> Any:
    """Pop the first matching key from data."""
    for key in keys:
        if key in data:
            return data.pop(key)
    return None


def _normalize_query_dict(
    spec: Dict[str, Any],
    override_res: Optional[int],
    override_suffix: Optional[str],
    override_desc: Optional[str],
    preferred_resolutions: Optional[Sequence[int]] = None,
) -> List[Tuple[Dict[str, Any], bool]]:
    """Map SpatialReferences spec fields to TemplateFlow api.get keyword arguments."""
    spec_data = dict(spec)
    queries: List[Tuple[Dict[str, Any], bool]] = []

    suffix_source = override_suffix if override_suffix else _pop_any(spec_data, "suffixes", "suffix")
    suffix_values = _option_values(suffix_source, allow_none=False)
    fallback_suffix = False
    if not suffix_values:
        dbg(f"No suffix values found in spec {spec_data}; falling back to default T1w/mask.")
        suffix_values = ["T1w", "mask"]
        fallback_suffix = True

    desc_source = override_desc if override_desc else _pop_any(spec_data, "descs", "desc")
    desc_values = _option_values(desc_source, allow_none=True)
    use_suffix_default_desc = bool(
        fallback_suffix and (not desc_values or desc_values == [None])
    )

    if override_res is not None:
        res_source = override_res
    elif preferred_resolutions is not None:
        res_source = list(preferred_resolutions)
    else:
        res_source = _pop_any(spec_data, "resolution", "res", "resolutions")
    res_values = _option_values(res_source, allow_none=True, transform=_coerce_resolution)

    atlas_values = _option_values(_pop_any(spec_data, "atlases", "atlas"), allow_none=True)
    cohort_values = _option_values(_pop_any(spec_data, "cohorts", "cohort"), allow_none=True)
    density_values = _option_values(_pop_any(spec_data, "densities", "density", "den"), allow_none=True)
    hemi_values = _option_values(_pop_any(spec_data, "hemispheres", "hemisphere", "hemis", "hemi"), allow_none=True)
    label_values = _option_values(_pop_any(spec_data, "labels", "label"), allow_none=True)
    space_values = _option_values(_pop_any(spec_data, "spaces", "space"), allow_none=True)
    extension_values = _option_values(_pop_any(spec_data, "extensions", "extension"), allow_none=True)

    shared_value_grid = [
        ("resolution", res_values, True),
        ("atlas", atlas_values, True),
        ("cohort", cohort_values, True),
        ("density", density_values, True),
        ("hemi", hemi_values, True),
        ("label", label_values, True),
        ("space", space_values, True),
        ("extension", extension_values, True),
    ]

    for suffix in suffix_values:
        if use_suffix_default_desc:
            suffix_default_desc = DEFAULT_FALLBACK_DESC_BY_SUFFIX.get(suffix)
            suffix_desc_values = [suffix_default_desc]
        else:
            suffix_default_desc = None
            suffix_desc_values = desc_values

        value_grid = [
            ("suffix", [suffix], False),
            ("desc", suffix_desc_values, True),
            *shared_value_grid,
        ]

        for combo in product(*(values or [None] for _, values, _ in value_grid)):
            query: Dict[str, Any] = {}
            for (key, _, allow_none), value in zip(value_grid, combo):
                if value in (None, ""):
                    continue
                query[key] = value
            if not query.get("suffix"):
                continue
            used_default_desc = bool(
                use_suffix_default_desc
                and suffix_default_desc not in (None, "")
                and query.get("desc") == suffix_default_desc
            )
            queries.append((query, used_default_desc))

    return queries


def _freeze_query(template: str, params: Dict[str, Any]) -> Tuple[str, Tuple[Tuple[str, Any], ...]]:
    """Create a hashable fingerprint for deduplicating TemplateFlow queries."""
    def _freeze_value(value: Any) -> Any:
        if isinstance(value, dict):
            return tuple(sorted((k, _freeze_value(v)) for k, v in value.items()))
        if isinstance(value, (list, tuple, set)):
            return tuple(_freeze_value(v) for v in value)
        return value

    items = tuple(sorted((key, _freeze_value(val)) for key, val in params.items()))
    return template, items


def _is_transient_fetch_exception(exc: Exception) -> bool:
    """Best-effort classification for transient TemplateFlow fetch failures."""
    msg = str(exc).lower()
    cls = exc.__class__.__name__.lower()
    transient_markers = (
        "timeout",
        "timed out",
        "temporar",
        "connection",
        "dns",
        "name resolution",
        "max retries exceeded",
        "503",
        "502",
        "504",
        "429",
        "unavailable",
        "reset",
        "refused",
        "ssl",
        "rate limit",
    )
    return any(marker in msg or marker in cls for marker in transient_markers)


def _api_get_with_retry(
    template: str,
    params: Dict[str, Any],
    retry_transient: bool,
    max_retries: int = DEFAULT_API_GET_MAX_RETRIES,
    base_delay_seconds: float = DEFAULT_API_GET_RETRY_DELAY_SECONDS,
) -> Tuple[Optional[Any], Optional[Exception]]:
    """Execute TemplateFlow api.get with optional transient retry/backoff."""
    attempts = max_retries + 1 if retry_transient else 1
    for attempt in range(attempts):
        try:
            return api.get(template=template, **params), None
        except Exception as exc:  # pragma: no cover
            should_retry = (
                retry_transient
                and attempt < attempts - 1
                and _is_transient_fetch_exception(exc)
            )
            if not should_retry:
                return None, exc

            delay = base_delay_seconds * (2 ** attempt)
            print(
                f"Transient TemplateFlow fetch error for template={template} "
                f"(attempt {attempt + 1}/{attempts}): {exc}. Retrying in {delay:.1f}s ...",
                file=sys.stderr,
            )
            time.sleep(delay)
    return None, RuntimeError("Unreachable retry state in _api_get_with_retry")


def _format_query_detail(template: str, params: Dict[str, Any]) -> str:
    """Generate a human-readable string describing the TemplateFlow query."""
    parts = [f"template={template}"]
    for key in sorted(params):
        parts.append(f"{key}={params[key]}")
    return ", ".join(parts)


def _format_label_with_query(label: str, query: Dict[str, Any]) -> str:
    """Append key details to a label for logging."""
    extras: List[str] = []
    for key in ("suffix", "desc", "resolution", "cohort", "atlas", "density", "hemi", "label"):
        value = query.get(key)
        if value in (None, ""):
            continue
        extras.append(f"{key}={value}")
    if extras:
        return f"{label} ({', '.join(extras)})"
    return label


def _utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _query_spec_payload(query: QuerySpec) -> Dict[str, Any]:
    return {
        "template": query.template,
        "params": dict(query.params),
        "optional": bool(query.optional),
        "label": query.label,
    }


def _query_signature_payload(queries: List[QuerySpec]) -> List[Dict[str, Any]]:
    payload = [
        {
            "template": query.template,
            "params": dict(query.params),
            "optional": bool(query.optional),
        }
        for query in queries
    ]
    return sorted(
        payload,
        key=lambda item: (
            item["template"],
            json.dumps(item["params"], sort_keys=True, separators=(",", ":")),
            item["optional"],
        ),
    )


def _compute_query_signature(queries: List[QuerySpec]) -> str:
    payload = json.dumps(
        _query_signature_payload(queries),
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def _write_json(path: Optional[str], payload: Dict[str, Any]) -> None:
    if not path:
        return
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    tmp = target.with_name(f"{target.name}.tmp.{os.getpid()}")
    tmp.write_text(json.dumps(payload, sort_keys=True, separators=(",", ":")), encoding="utf-8")
    tmp.replace(target)


def _write_prefetch_state(
    state_file: Optional[str],
    *,
    status: str,
    templateflow_home: str,
    spaces: List[str],
    scheduler_job_id: Optional[str],
    query_signature: Optional[str],
    query_count: int,
) -> None:
    if not state_file:
        return

    target = Path(state_file)
    target.parent.mkdir(parents=True, exist_ok=True)
    tmp = target.with_name(f"{target.name}.tmp.{os.getpid()}")
    lines = [
        f"status: {status}",
        f"templateflow_home: {templateflow_home}",
        f"spaces: {' '.join(spaces)}",
        f"updated_at_utc: {_utc_now()}",
    ]
    if scheduler_job_id:
        lines.append(f"scheduler_job_id: {scheduler_job_id}")
    if query_signature:
        lines.append(f"query_signature: {query_signature}")
    lines.append(f"query_count: {query_count}")
    tmp.write_text("\n".join(lines) + "\n", encoding="utf-8")
    tmp.replace(target)


def _build_manifest_payload(
    templateflow_home: str,
    query_signature: str,
    resolved_paths: List[str],
) -> Dict[str, Any]:
    root = os.path.abspath(templateflow_home)
    entries: List[Dict[str, Any]] = []
    total_size = 0

    for path_str in sorted(set(resolved_paths)):
        path = Path(path_str)
        if not path.exists():
            continue
        stat = path.stat()
        rel_path = Path(os.path.relpath(str(path), root)).as_posix()
        entries.append(
            {
                "path": rel_path,
                "size": stat.st_size,
                "mtime": stat.st_mtime,
            }
        )
        total_size += stat.st_size

    return {
        "output_dir": root,
        "captured_at": _utc_now(),
        "query_signature": query_signature,
        "file_count": len(entries),
        "total_size_bytes": total_size,
        "files": entries,
    }


def _extract_entry(entry: Any) -> Tuple[str, List[Any], str]:
    """Turn a SpatialReferences entry into (template, specs, display_label)."""
    space_obj: Any = None
    specs: Any = None

    if isinstance(entry, tuple) and len(entry) == 2:
        space_obj, specs = entry
    else:
        space_obj = getattr(entry, "space", getattr(entry, "template", None))
        if space_obj is None and hasattr(entry, "name"):
            space_obj = entry
        specs = getattr(entry, "specs", getattr(entry, "spec", None))
        if specs is None and hasattr(entry, "references"):
            specs = entry.references
    if space_obj is None:
        space_obj = entry

    if specs is None:
        spec_list: List[Any] = [{}]
    elif isinstance(specs, (list, tuple, set)):
        spec_list = list(specs) or [{}]
    else:
        spec_list = [specs]

    template_label = _space_label(space_obj)
    template_name, embedded_spec, _ = _split_output_space_token(template_label)
    display_label = template_label
    if embedded_spec:
        merged_specs: List[Any] = []
        for spec in spec_list:
            spec_dict = _spec_to_dict(spec)
            # SpatialReferences-derived spec values take precedence over embedded label fields.
            merged_spec = dict(embedded_spec)
            merged_spec.update(spec_dict)
            merged_specs.append(merged_spec)
        spec_list = merged_specs
    return template_name, spec_list, display_label


def _instantiate_spatial_references(tokens: List[str]):
    """Instantiate SpatialReferences after canonicalizing tokens via Reference parser."""
    if SpatialReferences is None or Reference is None:  # pragma: no cover
        raise RuntimeError("niworkflows is not available in this environment.")

    references: List[Reference] = []
    for token in tokens:
        references.extend(_token_to_references(token))

    if not references:
        raise RuntimeError("No volumetric TemplateFlow references could be derived from tokens.")

    dbg(f"Constructing SpatialReferences with {len(references)} Reference objects.")
    return SpatialReferences(spaces=references)


def _iter_spatial_entries(spatial_refs: Any) -> List[Any]:
    """Return the list of template/spec entries from a SpatialReferences instance."""
    call_patterns = [
        ("get_std_spaces", {"dim": (3,)}),
        ("get_std_spaces", {}),
        ("get_spaces", {"nonstandard": False, "dim": (3,)}),
        ("get_spaces", {"standard": True, "dim": (3,)}),
        ("get_spaces", {}),
    ]
    for name, kwargs in call_patterns:
        method = getattr(spatial_refs, name, None)
        if not callable(method):
            continue
        try:
            result_iter = method(**kwargs)
        except TypeError:
            continue
        entries = list(result_iter)
        if entries:
            return entries

    references = getattr(spatial_refs, "references", None) or getattr(spatial_refs, "_references", None)
    if references:
        return list(references)

    raise RuntimeError("SpatialReferences did not return any standard spaces.")


def build_queries_from_spatial_refs(
    tokens: List[str],
    override_res: Optional[int],
    override_suffix: Optional[str],
    override_desc: Optional[str],
) -> List[QuerySpec]:
    """Mirror fMRIPrep's TemplateFlow usage via niworkflows SpatialReferences."""
    if not tokens:
        return []

    dbg(f"Attempting SpatialReferences parsing for tokens: {tokens}")
    spatial_refs = _instantiate_spatial_references(tokens)
    entries = _iter_spatial_entries(spatial_refs)
    preferred_resolutions = resolution_preferences(tokens, override_res)
    dbg(f"SpatialReferences returned {len(entries)} entry/entries.")

    queries: List[QuerySpec] = []
    seen: Set[QueryFingerprint] = set()
    for entry in entries:
        template_name, specs, label = _extract_entry(entry)
        if not template_name:
            continue
        dbg(f"Processing template '{template_name}' (label='{label}') with {len(specs)} spec(s).")
        template_resolutions = preferred_resolutions.get(template_name)
        for spec in specs:
            spec_dict = _spec_to_dict(spec)
            dbg(f"Spec dict: {spec_dict}")
            query_variants = _normalize_query_dict(
                spec_dict,
                override_res,
                override_suffix,
                override_desc,
                preferred_resolutions=template_resolutions,
            )
            if not query_variants:
                dbg("Spec produced zero query variants.")
                continue
            for query, used_default_desc in query_variants:
                fingerprint = _freeze_query(template_name, query)
                if fingerprint in seen:
                    continue
                seen.add(fingerprint)
                detail_label = _format_label_with_query(label, query)
                dbg(f"Resolved query: template={template_name}, params={query}, label={detail_label}")
                queries.append(
                    QuerySpec(
                        template=template_name,
                        params=dict(query),
                        label=detail_label,
                        allow_desc_retry=bool(used_default_desc and query.get("desc") == "brain"),
                    )
                )
    dbg(f"Total SpatialReferences-derived queries: {len(queries)}")
    return queries


def build_queries_legacy(
    tokens: List[str],
    override_res: Optional[int],
    override_suffix: Optional[str],
    override_desc: Optional[str],
) -> List[QuerySpec]:
    """Fallback to the original parsing logic when SpatialReferences is unavailable."""
    dbg(f"Using legacy parser for tokens: {tokens}")
    queries: List[QuerySpec] = []
    for token in tokens:
        if token in SKIP_TOKENS:
            continue
        template, token_query, is_surface = _split_output_space_token(token)
        if is_surface:
            continue

        token_query = dict(token_query)
        token_resolution = token_query.pop("resolution", None)
        token_suffix = token_query.pop("suffix", None)
        token_desc = token_query.pop("desc", None)

        res_arg = override_res if override_res is not None else token_resolution
        if res_arg is None:
            res_arg = DEFAULT_RESOLUTION

        if override_suffix:
            suffix_values = [override_suffix]
        elif token_suffix not in (None, ""):
            suffix_values = [token_suffix]
        else:
            suffix_values = ["T1w", "mask"]

        for suffix in suffix_values:
            if override_desc:
                desc_values = [override_desc]
            elif token_desc not in (None, ""):
                desc_values = [token_desc]
            elif token_suffix in (None, "") and not override_suffix:
                desc_values = [DEFAULT_FALLBACK_DESC_BY_SUFFIX.get(suffix)]
            else:
                desc_values = [None]

            for desc in desc_values:
                query: Dict[str, Any] = dict(token_query)
                query["suffix"] = suffix
                if res_arg is not None:
                    query["resolution"] = res_arg
                if desc not in (None, ""):
                    query["desc"] = desc
                query_copy = dict(query)  # ensure stored query is immutable for later iterations
                allow_desc_retry = (
                    desc == DEFAULT_FALLBACK_DESC_BY_SUFFIX.get(suffix)
                    and desc == "brain"
                    and not override_desc
                    and token_desc in (None, "")
                    and token_suffix in (None, "")
                    and not override_suffix
                )
                detail_label = _format_label_with_query(token, query_copy)
                queries.append(
                    QuerySpec(
                        template=template,
                        params=query_copy,
                        label=detail_label,
                        allow_desc_retry=bool(allow_desc_retry),
                    )
                )
                dbg(f"Legacy query: template={template}, params={query_copy}, label={detail_label}")
    dbg(f"Total legacy queries: {len(queries)}")
    return queries


def _dedupe_query_specs(queries: List[QuerySpec]) -> List[QuerySpec]:
    """Deduplicate queries by template+params while preserving order."""
    deduped: List[QuerySpec] = []
    seen: Set[QueryFingerprint] = set()
    for query in queries:
        fingerprint = _freeze_query(query.template, query.params)
        if fingerprint in seen:
            continue
        seen.add(fingerprint)
        deduped.append(query)
    return deduped


def _resolve_queries_with_token_fallback(
    tokens: List[str],
    override_res: Optional[int],
    override_suffix: Optional[str],
    override_desc: Optional[str],
) -> List[QuerySpec]:
    """Resolve queries token-by-token, falling back to legacy only for tokens that fail."""
    if not tokens:
        return []

    if SpatialReferences is None:
        print(
            "niworkflows is unavailable in this environment; falling back to legacy TemplateFlow parsing.",
            file=sys.stderr,
        )
        return build_queries_legacy(tokens, override_res, override_suffix, override_desc)

    queries: List[QuerySpec] = []
    for token in tokens:
        try:
            token_queries = build_queries_from_spatial_refs([token], override_res, override_suffix, override_desc)
        except Exception as exc:
            print(
                f"SpatialReferences parsing failed for token '{token}' ({exc}); "
                "falling back to legacy TemplateFlow parsing for this token.",
                file=sys.stderr,
            )
            dbg(f"SpatialReferences parsing error for token '{token}': {exc}")
            token_queries = build_queries_legacy([token], override_res, override_suffix, override_desc)
        queries.extend(token_queries)

    return _dedupe_query_specs(queries)


def _template_names_from_tokens(tokens: List[str]) -> List[str]:
    """Extract unique template names from output-space tokens."""
    templates: List[str] = []
    seen: Set[str] = set()
    for token in tokens:
        if token in SKIP_TOKENS:
            continue
        template, _, is_surface = parse_output_space_token(token)
        if is_surface or template in SKIP_TOKENS:
            continue
        if template in seen:
            continue
        seen.add(template)
        templates.append(template)
    return templates


def add_default_t2w_queries(
    queries: List[QuerySpec],
    tokens: List[str],
) -> List[QuerySpec]:
    """Ensure T2w res-01 templates are prefetched for each template space."""
    templates = _template_names_from_tokens(tokens)
    if not templates:
        return queries

    cohorts_by_template: Dict[str, List[Any]] = {}
    for query in queries:
        cohort_value = query.params.get("cohort")
        if cohort_value in (None, ""):
            continue
        if query.template not in cohorts_by_template:
            cohorts_by_template[query.template] = []
        if cohort_value not in cohorts_by_template[query.template]:
            cohorts_by_template[query.template].append(cohort_value)

    existing: Set[QueryFingerprint] = {
        _freeze_query(query.template, query.params) for query in queries
    }
    augmented = list(queries)
    for template in templates:
        cohort_values = cohorts_by_template.get(template, [None])
        for cohort_value in cohort_values:
            params = {"suffix": "T2w", "resolution": 1}
            if cohort_value not in (None, ""):
                params["cohort"] = cohort_value
            fingerprint = _freeze_query(template, params)
            if fingerprint in existing:
                continue
            existing.add(fingerprint)
            if cohort_value in (None, ""):
                label = f"{template} (T2w res-01)"
            else:
                label = f"{template} (T2w res-01 cohort-{cohort_value})"
            augmented.append(QuerySpec(template=template, params=params, label=label, optional=True))
    return augmented


def add_required_probseg_queries(
    queries: List[QuerySpec],
    tokens: List[str],
) -> List[QuerySpec]:
    """Ensure known MRIQC-required tissue probability maps are prefetched."""
    templates = _template_names_from_tokens(tokens)
    if not templates:
        return queries

    existing: Set[QueryFingerprint] = {
        _freeze_query(query.template, query.params) for query in queries
    }
    augmented = list(queries)
    for template in templates:
        required = MRIQC_REQUIRED_PROBSEG.get(template, [])
        for params in required:
            query = dict(params)
            fingerprint = _freeze_query(template, query)
            if fingerprint in existing:
                continue
            existing.add(fingerprint)
            label = (
                f"{template} (MRIQC required: "
                f"suffix={query.get('suffix')}, label={query.get('label')}, resolution={query.get('resolution')})"
            )
            augmented.append(QuerySpec(template=template, params=query, label=label))
    return augmented


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Prefetch TemplateFlow resources needed for the configured fMRIPrep output spaces."
    )
    parser.add_argument(
        "--output-spaces",
        required=True,
        help="Whitespace-delimited list of fMRIPrep --output-spaces values.",
    )
    parser.add_argument(
        "--res",
        type=int,
        default=None,
        help=(
            "Resolution to fetch (overrides any res-* in tokens). "
            "If omitted, uses token-provided res when present; otherwise defaults to res-01."
        ),
    )
    parser.add_argument(
        "--suffix",
        default=None,
        help=(
            "Suffix to request from TemplateFlow (e.g., 'T1w', 'mask'). "
            "If omitted, no suffix is passed to TemplateFlow."
        ),
    )
    parser.add_argument(
        "--desc",
        default=None,
        help=(
            "Description qualifier to request from TemplateFlow (e.g., 'brain'). "
            "If omitted, no desc is passed to TemplateFlow."
        ),
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Emit verbose debugging information about TemplateFlow query resolution.",
    )
    parser.add_argument(
        "--plan-only",
        action="store_true",
        help="Resolve TemplateFlow queries and write summary metadata without fetching files.",
    )
    parser.add_argument(
        "--summary-json",
        default=None,
        help="Path to a JSON file describing the resolved query plan and fetch outcome.",
    )
    parser.add_argument(
        "--manifest-json",
        default=None,
        help="Path to a JSON file containing the resolved-file manifest written on success.",
    )
    parser.add_argument(
        "--state-file",
        default=None,
        help="Path to the BrainGnomes prefetch state DCF file to update.",
    )
    parser.add_argument(
        "--scheduler-job-id",
        default=None,
        help="Scheduler job id to record in the prefetch state file.",
    )
    args = parser.parse_args()

    global DEBUG
    DEBUG = bool(args.debug or os.environ.get("TEMPLATEFLOW_PREFETCH_DEBUG"))

    spaces = parse_spaces(args.output_spaces)
    if not spaces:
        print("No TemplateFlow output spaces supplied; nothing to prefetch.")
        return 0
    dbg(f"Parsed output spaces tokens: {spaces}")

    # Build TemplateFlow queries via niworkflows when available; fallback is token-scoped.
    queries = _resolve_queries_with_token_fallback(spaces, args.res, args.suffix, args.desc)

    dbg(f"Resolved {len(queries)} total TemplateFlow queries.")
    if not queries:
        print("No TemplateFlow queries were resolved; nothing to prefetch.")
        return 0
    queries = add_default_t2w_queries(queries, spaces)
    queries = add_required_probseg_queries(queries, spaces)
    query_signature = _compute_query_signature(queries)

    # Determine where TemplateFlow will place fetched files
    tf_home = os.environ.get("TEMPLATEFLOW_HOME")
    if not tf_home:
        tf_home = str(Path.home() / ".cache" / "templateflow")
    print(f"TemplateFlow cache directory: {tf_home}")

    summary_payload: Dict[str, Any] = {
        "status": "PLANNED",
        "captured_at": _utc_now(),
        "templateflow_home": os.path.abspath(tf_home),
        "output_spaces": spaces,
        "query_signature": query_signature,
        "query_count": len(queries),
        "queries": [_query_spec_payload(query) for query in queries],
        "resolved_resources": [],
        "resolved_files": [],
        "failures": [],
        "optional_failures": [],
    }
    _write_json(args.summary_json, summary_payload)

    if args.plan_only:
        print(
            "Resolved TemplateFlow prefetch plan for spaces: "
            + " ".join(spaces)
        )
        return 0

    failures: List[str] = []
    soft_failures: List[str] = []
    fetched: List[str] = []
    resolved_resources: List[Dict[str, Any]] = []
    resolved_files: List[str] = []
    for query_spec in queries:
        template = query_spec.template
        params = query_spec.params
        label = query_spec.label
        detail = _format_query_detail(template, params)
        is_optional = query_spec.optional
        print(f"Fetching TemplateFlow resource for '{label}' ({detail}) ...")

        result, fetch_exc = _api_get_with_retry(
            template=template,
            params=params,
            retry_transient=not is_optional,
        )
        if fetch_exc is not None:
            if is_optional:
                print(f"Optional TemplateFlow resource failed to fetch '{label}': {fetch_exc}", file=sys.stderr)
                soft_failures.append(label)
            else:
                print(f"Failed to fetch '{label}': {fetch_exc}", file=sys.stderr)
                failures.append(label)
            continue

        if not _paths_exist(result):
            allow_desc_retry = (
                query_spec.allow_desc_retry
                and params.get("desc") == "brain"
            )
            if allow_desc_retry:
                retry_params = dict(params)
                retry_params.pop("desc", None)
                retry_detail = _format_query_detail(template, retry_params)
                print(
                    f"No files resolved for space '{label}' ({detail}); retrying without desc=brain ({retry_detail}).",
                    file=sys.stderr,
                )
                retry_result, retry_exc = _api_get_with_retry(
                    template=template,
                    params=retry_params,
                    retry_transient=not is_optional,
                )
                if retry_exc is not None:
                    if is_optional:
                        print(
                            f"Optional retry without desc failed for '{label}': {retry_exc}",
                            file=sys.stderr,
                        )
                        soft_failures.append(label)
                    else:
                        print(
                            f"Retry without desc failed for '{label}': {retry_exc}",
                            file=sys.stderr,
                        )
                        failures.append(label)
                    continue
                if retry_result is not None and _paths_exist(retry_result):
                    retry_paths = _existing_paths(retry_result)
                    resolved_resources.append(
                        {
                            "label": label,
                            "template": template,
                            "params": dict(retry_params),
                            "paths": retry_paths,
                        }
                    )
                    resolved_files.extend(retry_paths)
                    print(
                        f"Resolved space '{label}' after retrying without desc=brain.",
                        file=sys.stderr,
                    )
                    fetched.append(label)
                    continue

            if is_optional:
                print(
                    f"Optional TemplateFlow resource unavailable for space '{label}' ({detail}).",
                    file=sys.stderr,
                )
                soft_failures.append(label)
            else:
                print(
                    f"No files resolved for space '{label}' ({detail}).",
                    file=sys.stderr,
                )
                failures.append(label)
            continue
        result_paths = _existing_paths(result)
        resolved_resources.append(
            {
                "label": label,
                "template": template,
                "params": dict(params),
                "paths": result_paths,
            }
        )
        resolved_files.extend(result_paths)
        fetched.append(label)

    if soft_failures:
        print(
            "TemplateFlow prefetch completed with optional resources unavailable for: "
            + ", ".join(soft_failures),
            file=sys.stderr,
        )

    if failures:
        summary_payload.update(
            {
                "status": "FAILED",
                "captured_at": _utc_now(),
                "resolved_resources": resolved_resources,
                "resolved_files": sorted(set(resolved_files)),
                "failures": failures,
                "optional_failures": soft_failures,
            }
        )
        _write_json(args.summary_json, summary_payload)
        _write_prefetch_state(
            args.state_file,
            status="FAILED",
            templateflow_home=os.path.abspath(tf_home),
            spaces=spaces,
            scheduler_job_id=args.scheduler_job_id,
            query_signature=query_signature,
            query_count=len(queries),
        )
        print(
            "TemplateFlow prefetch completed with failures for: " + ", ".join(failures),
            file=sys.stderr,
        )
        return 1

    if fetched:
        manifest_payload = _build_manifest_payload(tf_home, query_signature, resolved_files)
        _write_json(args.manifest_json, manifest_payload)
        summary_payload.update(
            {
                "status": "COMPLETED",
                "captured_at": _utc_now(),
                "resolved_resources": resolved_resources,
                "resolved_files": sorted(set(resolved_files)),
                "failures": failures,
                "optional_failures": soft_failures,
                "manifest_file_count": manifest_payload["file_count"],
            }
        )
        _write_json(args.summary_json, summary_payload)
        _write_prefetch_state(
            args.state_file,
            status="COMPLETED",
            templateflow_home=os.path.abspath(tf_home),
            spaces=spaces,
            scheduler_job_id=args.scheduler_job_id,
            query_signature=query_signature,
            query_count=len(queries),
        )
        print(
            "Successfully prefetched TemplateFlow resources for: "
            + ", ".join(fetched)
            + f"\nFiles stored under: {tf_home}"
        )
    else:
        summary_payload.update(
            {
                "status": "COMPLETED",
                "captured_at": _utc_now(),
                "resolved_resources": resolved_resources,
                "resolved_files": sorted(set(resolved_files)),
                "failures": failures,
                "optional_failures": soft_failures,
                "manifest_file_count": 0,
            }
        )
        _write_json(args.summary_json, summary_payload)
        _write_json(args.manifest_json, _build_manifest_payload(tf_home, query_signature, resolved_files))
        _write_prefetch_state(
            args.state_file,
            status="COMPLETED",
            templateflow_home=os.path.abspath(tf_home),
            spaces=spaces,
            scheduler_job_id=args.scheduler_job_id,
            query_signature=query_signature,
            query_count=len(queries),
        )
        print("No TemplateFlow resources were prefetched.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
