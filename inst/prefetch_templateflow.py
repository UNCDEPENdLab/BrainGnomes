#!/usr/bin/env python3
import argparse
import os
import sys
import re
from pathlib import Path
from typing import List, Optional, Tuple, Sequence, Union

try:
    from templateflow import api  # type: ignore
except ImportError as exc:  # pragma: no cover
    raise SystemExit(f"TemplateFlow is required to prefetch templates: {exc}") from exc

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


def parse_output_space_token(token: str) -> Tuple[str, Optional[int], bool]:
    """Parse an fMRIPrep --output-spaces token into (template, resolution, is_surface).

    Examples
    - "MNI152NLin2009cAsym:res-2" -> ("MNI152NLin2009cAsym", 2, False)
    - "MNI152NLin6Asym" -> ("MNI152NLin6Asym", None, False)
    - "fsaverage:den-10k" -> ("fsaverage", None, True)
    """
    parts = token.split(":")
    template = parts[0]

    # Detect obvious surface spaces
    is_surface = template in {"fsnative", "fsaverage", "fsaverage5", "fsaverage6", "fsLR"}

    resolution: Optional[int] = None
    if len(parts) > 1:
        for qual in parts[1:]:
            qual = qual.strip()
            if not qual:
                continue
            # resolution may be "res-2" or "res-01"; ignore "res-native"
            m = re.match(r"^res-(\d+)$", qual)
            if m:
                try:
                    resolution = int(m.group(1))
                except Exception:
                    resolution = None
            # presence of a surface density qualifier strongly implies a surface space
            if qual.startswith("den-"):
                is_surface = True

    return template, resolution, is_surface


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
            "If omitted, uses token-provided res when present; otherwise no resolution is passed."
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
    args = parser.parse_args()

    spaces = parse_spaces(args.output_spaces)
    if not spaces:
        print("No TemplateFlow output spaces supplied; nothing to prefetch.")
        return 0

    # Determine where TemplateFlow will place fetched files
    tf_home = os.environ.get("TEMPLATEFLOW_HOME")
    if not tf_home:
        tf_home = str(Path.home() / ".cache" / "templateflow")
    print(f"TemplateFlow cache directory: {tf_home}")

    failures: List[str] = []
    fetched: List[str] = []
    for space in spaces:
        if space in SKIP_TOKENS:
            print(f"Skipping non-template space '{space}'.")
            continue

        template, resolution, is_surface = parse_output_space_token(space)

        if is_surface:
            print(f"Skipping surface-only space '{space}'.")
            continue

        # Resolution precedence: CLI --res > token res-* > no resolution passed
        res_arg = args.res if args.res is not None else (resolution if resolution is not None else None)

        try:
            # Build query for TemplateFlow, passing suffix/desc only when provided
            query = {
                "template": template,
            }
            if res_arg is not None:
                query["resolution"] = res_arg
            if args.suffix:
                query["suffix"] = args.suffix
            if args.desc:
                query["desc"] = args.desc

            parts = [f"template={template}"]
            if res_arg is not None:
                parts.append(f"resolution={res_arg}")
            if args.suffix:
                parts.append(f"suffix={args.suffix}")
            if args.desc:
                parts.append(f"desc={args.desc}")
            detail = ", ".join(parts)
            print(f"Fetching TemplateFlow resource for '{space}' ({detail}) ...")

            result = api.get(**query)
            if not _paths_exist(result):
                print(
                    f"No files resolved for space '{space}' ({detail}).",
                    file=sys.stderr,
                )
                failures.append(space)
                continue
            fetched.append(space)
        except Exception as exc:  # pragma: no cover
            print(f"Failed to fetch '{space}': {exc}", file=sys.stderr)
            failures.append(space)

    if failures:
        print(
            "TemplateFlow prefetch completed with failures for: " + ", ".join(failures),
            file=sys.stderr,
        )
        return 1

    if fetched:
        print(
            "Successfully prefetched TemplateFlow resources for: "
            + ", ".join(fetched)
            + f"\nFiles stored under: {tf_home}"
        )
    else:
        print("No TemplateFlow resources were prefetched.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
