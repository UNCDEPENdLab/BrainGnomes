test_that("prefetch parser splits cohort-qualified template tokens", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch parser regression test")

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  expect_true(nzchar(script_path))
  py_code <- paste(
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
    "template, query, is_surface = mod._split_output_space_token('MNIPediatricAsym:cohort-2:res-1')",
    "print(f'SPLIT|template={template}|cohort={query.get(\"cohort\")}|resolution={query.get(\"resolution\")}|surface={is_surface}')",
    "",
    "entry_template, entry_specs, entry_label = mod._extract_entry(('MNIPediatricAsym:cohort-2', {}))",
    "cohort_val = entry_specs[0].get('cohort') if entry_specs and isinstance(entry_specs[0], dict) else None",
    "print(f'ENTRY|template={entry_template}|cohort={cohort_val}|label={entry_label}')",
    "",
    "legacy = mod.build_queries_legacy(['MNIPediatricAsym:cohort-2'], None, None, None)",
    "legacy_query = legacy[0]",
    "legacy_template = legacy_query.template",
    "legacy_params = legacy_query.params",
    "legacy_label = legacy_query.label",
    "print(f'LEGACY|template={legacy_template}|cohort={legacy_params.get(\"cohort\")}|suffix={legacy_params.get(\"suffix\")}|resolution={legacy_params.get(\"resolution\")}|label={legacy_label}')",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_parser_", fileext = ".py")
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

  split_line <- out[grepl("^SPLIT\\|", out)]
  entry_line <- out[grepl("^ENTRY\\|", out)]
  legacy_line <- out[grepl("^LEGACY\\|", out)]

  expect_length(split_line, 1L)
  expect_length(entry_line, 1L)
  expect_length(legacy_line, 1L)

  expect_true(grepl("template=MNIPediatricAsym", split_line, fixed = TRUE))
  expect_true(grepl("cohort=2", split_line, fixed = TRUE))
  expect_true(grepl("resolution=1", split_line, fixed = TRUE))

  expect_true(grepl("template=MNIPediatricAsym", entry_line, fixed = TRUE))
  expect_true(grepl("cohort=2", entry_line, fixed = TRUE))
  expect_true(grepl("label=MNIPediatricAsym:cohort-2", entry_line, fixed = TRUE))

  expect_true(grepl("template=MNIPediatricAsym", legacy_line, fixed = TRUE))
  expect_true(grepl("cohort=2", legacy_line, fixed = TRUE))
  expect_true(grepl("suffix=T1w", legacy_line, fixed = TRUE))
  expect_true(grepl("resolution=1", legacy_line, fixed = TRUE))
  expect_true(grepl("label=MNIPediatricAsym:cohort-2 (suffix=T1w", legacy_line, fixed = TRUE))
})
