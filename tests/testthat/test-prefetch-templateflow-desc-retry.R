test_that("prefetch retries default desc=brain queries without desc on miss", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch desc retry test")

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  expect_true(nzchar(script_path))

  py_code <- paste(
    "import json",
    "import pathlib",
    "import tempfile",
    "import types",
    "import importlib.util",
    "import sys",
    "",
    "calls = []",
    "tmpdir = pathlib.Path(tempfile.mkdtemp(prefix='bg_prefetch_retry_'))",
    "ok_file = tmpdir / 'resolved.nii.gz'",
    "ok_file.write_text('ok')",
    "",
    "def fake_get(template, **kwargs):",
    "    call = {'template': template}",
    "    call.update(kwargs)",
    "    calls.append(call)",
    "    if kwargs.get('suffix') == 'T1w' and kwargs.get('desc') == 'brain':",
    "        return []",
    "    return [str(ok_file)]",
    "",
    "templateflow = types.ModuleType('templateflow')",
    "templateflow.api = types.SimpleNamespace(get=fake_get)",
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
    "    def get_std_spaces(self, **kwargs):",
    "        return [('MNIPediatricAsym', [{}])]",
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
    "sys.argv = [str(script), '--output-spaces', 'MNIPediatricAsym']",
    "rc = mod.main()",
    "print(f'RC|{rc}')",
    "print('CALLS|' + json.dumps(calls, sort_keys=True))",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_desc_retry_", fileext = ".py")
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

  rc_line <- out[grepl("^RC\\|", out)]
  calls_line <- out[grepl("^CALLS\\|", out)]
  expect_length(rc_line, 1L)
  expect_length(calls_line, 1L)
  expect_true(grepl("RC\\|0$", rc_line))

  calls_json <- sub("^CALLS\\|", "", calls_line)
  calls <- jsonlite::fromJSON(calls_json, simplifyDataFrame = FALSE)
  expect_true(length(calls) >= 4L)

  saw_t1w_with_desc <- any(vapply(
    calls,
    function(x) identical(x[["suffix"]], "T1w") && identical(x[["desc"]], "brain"),
    logical(1)
  ))
  saw_t1w_without_desc <- any(vapply(
    calls,
    function(x) identical(x[["suffix"]], "T1w") && is.null(x[["desc"]]),
    logical(1)
  ))

  expect_true(saw_t1w_with_desc)
  expect_true(saw_t1w_without_desc)
})
