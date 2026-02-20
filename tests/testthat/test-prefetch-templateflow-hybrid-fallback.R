test_that("prefetch uses token-scoped legacy fallback when one token fails spatial parsing", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch hybrid fallback test")

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  expect_true(nzchar(script_path))

  py_code <- paste(
    "import json",
    "import types",
    "import importlib.util",
    "import pathlib",
    "import sys",
    "",
    "templateflow = types.ModuleType('templateflow')",
    "templateflow.api = types.SimpleNamespace(get=lambda **kwargs: [])",
    "sys.modules['templateflow'] = templateflow",
    "",
    "niworkflows = types.ModuleType('niworkflows')",
    "niworkflows_utils = types.ModuleType('niworkflows.utils')",
    "niworkflows_spaces = types.ModuleType('niworkflows.utils.spaces')",
    "class DummyReference:",
    "    def __init__(self, space, dim=3):",
    "        self.space = space",
    "        self.dim = dim",
    "    @staticmethod",
    "    def from_string(token):",
    "        if token.startswith('BAD'):",
    "            raise RuntimeError('synthetic spatial parse failure')",
    "        return [DummyReference(token.split(':', 1)[0], dim=3)]",
    "class DummySpatialReferences:",
    "    def __init__(self, spaces=None):",
    "        self.references = spaces or []",
    "    def get_std_spaces(self, **kwargs):",
    "        out = []",
    "        for ref in self.references:",
    "            if ref.space == 'GOOD':",
    "                out.append(('GOOD', [{'suffix': 'probseg', 'label': 'CSF'}]))",
    "            else:",
    "                out.append((ref.space, [{}]))",
    "        return out",
    "niworkflows_spaces.Reference = DummyReference",
    "niworkflows_spaces.SpatialReferences = DummySpatialReferences",
    "sys.modules['niworkflows'] = niworkflows",
    "sys.modules['niworkflows.utils'] = niworkflows_utils",
    "sys.modules['niworkflows.utils.spaces'] = niworkflows_spaces",
    "",
    "script = pathlib.Path(sys.argv[1])",
    "spec = importlib.util.spec_from_file_location('prefetch_templateflow', script)",
    "mod = importlib.util.module_from_spec(spec)",
    "spec.loader.exec_module(mod)",
    "",
    "queries = mod._resolve_queries_with_token_fallback(['GOOD:res-2', 'BAD:cohort-2'], None, None, None)",
    "serialized = [",
    "    {",
    "        'template': q.template,",
    "        'params': q.params,",
    "        'label': q.label",
    "    }",
    "    for q in queries",
    "]",
    "print('QUERIES|' + json.dumps(serialized, sort_keys=True))",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_hybrid_fallback_", fileext = ".py")
  writeLines(py_code, py_file)
  on.exit(unlink(py_file), add = TRUE)

  out <- suppressWarnings(
    system2(
      py,
      c(py_file, script_path),
      stdout = TRUE,
      stderr = TRUE
    )
  )

  status <- attr(out, "status")
  expect_true(is.null(status) || identical(status, 0L), info = paste(out, collapse = "\n"))

  query_line <- out[grepl("^QUERIES\\|", out)]
  expect_length(query_line, 1L)
  query_json <- sub("^QUERIES\\|", "", query_line)
  queries <- jsonlite::fromJSON(query_json, simplifyDataFrame = FALSE)

  good_probseg <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "GOOD") &&
      identical(q[["params"]][["suffix"]], "probseg") &&
      identical(q[["params"]][["label"]], "CSF"),
    logical(1)
  ))
  bad_t1w <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "BAD") &&
      identical(q[["params"]][["suffix"]], "T1w") &&
      identical(q[["params"]][["cohort"]], 2L),
    logical(1)
  ))
  good_legacy_t1w <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "GOOD") &&
      identical(q[["params"]][["suffix"]], "T1w"),
    logical(1)
  ))

  expect_true(good_probseg)
  expect_true(bad_t1w)
  expect_false(good_legacy_t1w)
  expect_true(any(grepl("falling back to legacy TemplateFlow parsing for this token", out, fixed = TRUE)))
})
