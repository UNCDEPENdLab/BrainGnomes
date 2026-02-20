test_that("default T2w augmentation inherits cohort from parsed queries", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch t2w cohort test")

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
    "        return [DummyReference(token.split(':', 1)[0], dim=3)]",
    "class DummySpatialReferences:",
    "    def __init__(self, spaces=None):",
    "        self.references = spaces or []",
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
    "queries = [mod.QuerySpec(template='MNIPediatricAsym', params={'suffix': 'T1w', 'desc': 'brain', 'resolution': 1, 'cohort': 2}, label='MNIPediatricAsym:cohort-2')]",
    "augmented = mod.add_default_t2w_queries(queries, ['MNIPediatricAsym:cohort-2'])",
    "t2w_params = [query.params for query in augmented if query.template == 'MNIPediatricAsym' and query.params.get('suffix') == 'T2w']",
    "print('T2W|' + json.dumps(t2w_params, sort_keys=True))",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_t2w_cohort_", fileext = ".py")
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

  t2w_line <- out[grepl("^T2W\\|", out)]
  expect_length(t2w_line, 1L)

  t2w_json <- sub("^T2W\\|", "", t2w_line)
  t2w_params <- jsonlite::fromJSON(t2w_json, simplifyDataFrame = FALSE)
  expect_length(t2w_params, 1L)
  expect_identical(t2w_params[[1]][["suffix"]], "T2w")
  expect_identical(t2w_params[[1]][["resolution"]], 1L)
  expect_identical(t2w_params[[1]][["cohort"]], 2L)
})
