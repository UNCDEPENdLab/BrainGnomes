test_that("prefetch retries transient api.get exceptions for required queries", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch transient retry test")

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
    "attempts = {}",
    "tmpdir = pathlib.Path(tempfile.mkdtemp(prefix='bg_prefetch_transient_'))",
    "ok_file = tmpdir / 'resolved.nii.gz'",
    "ok_file.write_text('ok')",
    "",
    "def fake_get(template, **kwargs):",
    "    key_dict = {'template': template}",
    "    key_dict.update(kwargs)",
    "    key = json.dumps(key_dict, sort_keys=True)",
    "    attempts[key] = attempts.get(key, 0) + 1",
    "    if kwargs.get('suffix') == 'T1w' and kwargs.get('desc') == 'brain' and attempts[key] < 3:",
    "        raise RuntimeError('Connection timed out')",
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
    "mod.DEFAULT_API_GET_RETRY_DELAY_SECONDS = 0.0",
    "sys.argv = [str(script), '--output-spaces', 'MNIPediatricAsym']",
    "rc = mod.main()",
    "print(f'RC|{rc}')",
    "print('ATTEMPTS|' + json.dumps(attempts, sort_keys=True))",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_transient_retry_", fileext = ".py")
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
  attempts_line <- out[grepl("^ATTEMPTS\\|", out)]
  expect_length(rc_line, 1L)
  expect_length(attempts_line, 1L)
  expect_true(grepl("RC\\|0$", rc_line))

  attempts_json <- sub("^ATTEMPTS\\|", "", attempts_line)
  attempts <- jsonlite::fromJSON(attempts_json, simplifyDataFrame = FALSE)
  t1w_attempts <- attempts[grepl("\"suffix\": \"T1w\"", names(attempts), fixed = TRUE)]
  expect_true(length(t1w_attempts) >= 1L)
  expect_true(any(unlist(t1w_attempts) >= 3L))
  expect_true(any(grepl("Transient TemplateFlow fetch error", out, fixed = TRUE)))
})
