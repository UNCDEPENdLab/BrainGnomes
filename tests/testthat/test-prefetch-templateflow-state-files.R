test_that("prefetch writes query-specific state and resolved-file manifest on success", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch state test")

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  expect_true(nzchar(script_path))

  root <- tempfile("prefetch_state_success_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  py_code <- paste(
    "import importlib.util",
    "import os",
    "import pathlib",
    "import sys",
    "import types",
    "",
    "root = pathlib.Path(sys.argv[2])",
    "os.environ['TEMPLATEFLOW_HOME'] = str(root)",
    "resolved = root / 'tpl-MNI152NLin2009cAsym' / 'tpl-MNI152NLin2009cAsym_res-01_T1w.nii.gz'",
    "resolved.parent.mkdir(parents=True, exist_ok=True)",
    "resolved.write_text('ok')",
    "",
    "def fake_get(template=None, **kwargs):",
    "    return [str(resolved)]",
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
    "        return [('MNI152NLin2009cAsym', [{}])]",
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
    "state_file = root / '.braingnomes_prefetch_state.dcf'",
    "summary_file = root / 'summary.json'",
    "manifest_file = root / 'manifest.json'",
    "sys.argv = [",
    "    str(script),",
    "    '--output-spaces', 'MNI152NLin2009cAsym',",
    "    '--summary-json', str(summary_file),",
    "    '--manifest-json', str(manifest_file),",
    "    '--state-file', str(state_file),",
    "    '--scheduler-job-id', 'job-123'",
    "]",
    "raise SystemExit(mod.main())",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_state_success_", fileext = ".py")
  writeLines(py_code, py_file)
  on.exit(unlink(py_file), add = TRUE)

  status <- suppressWarnings(system2(py, c(py_file, script_path, root)))
  expect_identical(status, 0L)

  state_lines <- readLines(file.path(root, ".braingnomes_prefetch_state.dcf"))
  expect_true(any(grepl("^status: COMPLETED$", state_lines)))
  expect_true(any(grepl("^scheduler_job_id: job-123$", state_lines)))
  expect_true(any(grepl("^query_signature: [0-9a-f]{64}$", state_lines)))

  manifest <- jsonlite::fromJSON(file.path(root, "manifest.json"), simplifyVector = FALSE)
  expect_equal(manifest$file_count, 1)
  expect_equal(manifest$files[[1]]$path, "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-01_T1w.nii.gz")
  expect_true(grepl("^[0-9a-f]{64}$", manifest$query_signature))
})

test_that("prefetch overwrites stale success state with FAILED state on fetch failure", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch state test")

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  expect_true(nzchar(script_path))

  root <- tempfile("prefetch_state_fail_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  state_file <- file.path(root, ".braingnomes_prefetch_state.dcf")
  writeLines(c(
    "status: COMPLETED",
    paste0("templateflow_home: ", normalizePath(root, winslash = "/", mustWork = FALSE)),
    "spaces: MNI152NLin2009cAsym",
    "scheduler_job_id: old-job",
    "query_signature: oldsig"
  ), state_file)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  py_code <- paste(
    "import importlib.util",
    "import os",
    "import pathlib",
    "import sys",
    "import types",
    "",
    "root = pathlib.Path(sys.argv[2])",
    "os.environ['TEMPLATEFLOW_HOME'] = str(root)",
    "",
    "def fake_get(template=None, **kwargs):",
    "    return []",
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
    "        return [('MNI152NLin2009cAsym', [{}])]",
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
    "summary_file = root / 'summary.json'",
    "manifest_file = root / 'manifest.json'",
    "state_file = root / '.braingnomes_prefetch_state.dcf'",
    "sys.argv = [",
    "    str(script),",
    "    '--output-spaces', 'MNI152NLin2009cAsym',",
    "    '--summary-json', str(summary_file),",
    "    '--manifest-json', str(manifest_file),",
    "    '--state-file', str(state_file),",
    "    '--scheduler-job-id', 'job-999'",
    "]",
    "raise SystemExit(mod.main())",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_state_fail_", fileext = ".py")
  writeLines(py_code, py_file)
  on.exit(unlink(py_file), add = TRUE)

  out <- suppressWarnings(system2(py, c(py_file, script_path, root), stdout = TRUE, stderr = TRUE))
  expect_identical(attr(out, "status"), 1L)

  state_lines <- readLines(state_file)
  expect_true(any(grepl("^status: FAILED$", state_lines)))
  expect_true(any(grepl("^scheduler_job_id: job-999$", state_lines)))
  expect_false(any(grepl("^scheduler_job_id: old-job$", state_lines)))
  expect_true(any(grepl("^query_signature: [0-9a-f]{64}$", state_lines)))
})

test_that("prefetch query signatures are stable for reordered equivalent query sets", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch query signature test")

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  expect_true(nzchar(script_path))

  py_code <- paste(
    "import importlib.util",
    "import json",
    "import pathlib",
    "import sys",
    "import types",
    "",
    "templateflow = types.ModuleType('templateflow')",
    "templateflow.api = types.SimpleNamespace(get=lambda *args, **kwargs: [])",
    "sys.modules['templateflow'] = templateflow",
    "",
    "niworkflows = types.ModuleType('niworkflows')",
    "niworkflows_utils = types.ModuleType('niworkflows.utils')",
    "niworkflows_spaces = types.ModuleType('niworkflows.utils.spaces')",
    "niworkflows_spaces.Reference = object",
    "niworkflows_spaces.SpatialReferences = object",
    "sys.modules['niworkflows'] = niworkflows",
    "sys.modules['niworkflows.utils'] = niworkflows_utils",
    "sys.modules['niworkflows.utils.spaces'] = niworkflows_spaces",
    "",
    "script = pathlib.Path(sys.argv[1])",
    "spec = importlib.util.spec_from_file_location('prefetch_templateflow', script)",
    "mod = importlib.util.module_from_spec(spec)",
    "spec.loader.exec_module(mod)",
    "",
    "queries_a = [",
    "    mod.QuerySpec('MNI152NLin2009cAsym', {'suffix': 'T1w', 'resolution': 1}, 'q1'),",
    "    mod.QuerySpec('MNI152NLin2009cAsym', {'suffix': 'mask', 'resolution': 1, 'desc': 'brain'}, 'q2'),",
    "    mod.QuerySpec('MNI152NLin2009cAsym', {'suffix': 'probseg', 'label': 'CSF', 'resolution': 1}, 'q3'),",
    "]",
    "queries_b = [queries_a[2], queries_a[0], queries_a[1]]",
    "payload = {",
    "    'sig_a': mod._compute_query_signature(queries_a),",
    "    'sig_b': mod._compute_query_signature(queries_b),",
    "    'payload_a': mod._query_signature_payload(queries_a),",
    "    'payload_b': mod._query_signature_payload(queries_b),",
    "}",
    "print(json.dumps(payload, sort_keys=True))",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_signature_stable_", fileext = ".py")
  writeLines(py_code, py_file)
  on.exit(unlink(py_file), add = TRUE)

  out <- suppressWarnings(system2(py, c(py_file, script_path), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && !identical(status, 0L)) {
    fail(paste(out, collapse = "\n"))
  }

  payload <- jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE)
  expect_identical(payload$sig_a, payload$sig_b)
  expect_equal(payload$payload_a, payload$payload_b)
})
