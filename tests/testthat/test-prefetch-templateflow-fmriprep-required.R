test_that("prefetch plan includes default TemplateFlow resources required by offline fMRIPrep", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch required-template test")

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
    "queries = mod._resolve_queries_with_token_fallback(['MNI152NLin2009cAsym'], None, None, None)",
    "queries = mod.add_default_t2w_queries(queries, ['MNI152NLin2009cAsym'])",
    "queries = mod.add_required_probseg_queries(queries, ['MNI152NLin2009cAsym'])",
    "queries = mod.add_required_fmriprep_queries(queries)",
    "serialized = [{'template': q.template, 'params': q.params, 'label': q.label, 'optional': q.optional, 'fallback_params': q.fallback_params} for q in queries]",
    "print('QUERIES|' + json.dumps(serialized, sort_keys=True))",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_fmriprep_required_", fileext = ".py")
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
  queries <- jsonlite::fromJSON(sub("^QUERIES\\|", "", query_line), simplifyDataFrame = FALSE)

  saw_oasis_t1w <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "OASIS30ANTs") &&
      identical(q[["params"]][["suffix"]], "T1w") &&
      identical(q[["params"]][["resolution"]], 1L),
    logical(1)
  ))

  saw_oasis_brain_probseg <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "OASIS30ANTs") &&
      identical(q[["params"]][["label"]], "brain") &&
      identical(q[["params"]][["suffix"]], "probseg") &&
      identical(q[["params"]][["resolution"]], 1L),
    logical(1)
  ))
  saw_oasis_brain_mask_fallback <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "OASIS30ANTs") &&
      length(q[["fallback_params"]]) == 1L &&
      identical(q[["fallback_params"]][[1]][["desc"]], "brain") &&
      identical(q[["fallback_params"]][[1]][["suffix"]], "mask") &&
      identical(q[["fallback_params"]][[1]][["resolution"]], 1L),
    logical(1)
  ))
  saw_oasis_regmask <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "OASIS30ANTs") &&
      identical(q[["params"]][["desc"]], "BrainCerebellumExtraction") &&
      identical(q[["params"]][["suffix"]], "mask") &&
      identical(q[["optional"]], TRUE),
    logical(1)
  ))
  saw_oasis_wm <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "OASIS30ANTs") &&
      identical(q[["params"]][["label"]], "WM") &&
      identical(q[["params"]][["suffix"]], "probseg") &&
      identical(q[["optional"]], TRUE),
    logical(1)
  ))
  saw_oasis_bs <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "OASIS30ANTs") &&
      identical(q[["params"]][["label"]], "BS") &&
      identical(q[["params"]][["suffix"]], "probseg") &&
      identical(q[["optional"]], TRUE),
    logical(1)
  ))
  saw_mni_boldref <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["desc"]], "fMRIPrep") &&
      identical(q[["params"]][["suffix"]], "boldref") &&
      identical(q[["params"]][["resolution"]], 2L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_mni_brain_mask_res2 <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["desc"]], "brain") &&
      identical(q[["params"]][["suffix"]], "mask") &&
      identical(q[["params"]][["resolution"]], 2L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_mni_brain_probseg <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["label"]], "brain") &&
      identical(q[["params"]][["suffix"]], "probseg") &&
      identical(q[["params"]][["resolution"]], 1L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_mni_carpet_dseg <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["desc"]], "carpet") &&
      identical(q[["params"]][["suffix"]], "dseg") &&
      identical(q[["params"]][["resolution"]], 1L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_mni6_t1w <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin6Asym") &&
      identical(q[["params"]][["suffix"]], "T1w") &&
      identical(q[["params"]][["resolution"]], 1L),
    logical(1)
  ))
  saw_fslr_sphere <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "fsLR") &&
      identical(q[["params"]][["density"]], "32k") &&
      identical(q[["params"]][["suffix"]], "sphere"),
    logical(1)
  ))

  expect_true(saw_mni_boldref)
  expect_true(saw_mni_brain_mask_res2)
  expect_true(saw_mni_brain_probseg)
  expect_true(saw_mni_carpet_dseg)
  expect_false(saw_mni6_t1w)
  expect_false(saw_fslr_sphere)
  expect_true(saw_oasis_t1w)
  expect_true(saw_oasis_brain_probseg)
  expect_true(saw_oasis_brain_mask_fallback)
  expect_true(saw_oasis_regmask)
  expect_true(saw_oasis_wm)
  expect_true(saw_oasis_bs)
})

test_that("prefetch plan includes additional TemplateFlow resources for fMRIPrep CIFTI output", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch required-template test")

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
    "queries = mod.add_required_fmriprep_queries([], include_cifti_defaults=True)",
    "serialized = [{'template': q.template, 'params': q.params, 'label': q.label, 'optional': q.optional} for q in queries]",
    "print('QUERIES|' + json.dumps(serialized, sort_keys=True))",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_fmriprep_cifti_required_", fileext = ".py")
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
  queries <- jsonlite::fromJSON(sub("^QUERIES\\|", "", query_line), simplifyDataFrame = FALSE)

  saw_mni6_t1w <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin6Asym") &&
      identical(q[["params"]][["suffix"]], "T1w") &&
      identical(q[["params"]][["resolution"]], 1L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_mni6_brain_mask <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin6Asym") &&
      identical(q[["params"]][["desc"]], "brain") &&
      identical(q[["params"]][["suffix"]], "mask") &&
      identical(q[["params"]][["resolution"]], 1L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_fslr_sphere <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "fsLR") &&
      identical(q[["params"]][["density"]], "32k") &&
      is.null(q[["params"]][["space"]]) &&
      identical(q[["params"]][["suffix"]], "sphere") &&
      identical(q[["params"]][["extension"]], ".surf.gii") &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))

  expect_true(saw_mni6_t1w)
  expect_true(saw_mni6_brain_mask)
  expect_true(saw_fslr_sphere)
})

test_that("OASIS brain-mask requirement falls back from probseg to desc-brain mask", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for prefetch required-template fallback test")

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  expect_true(nzchar(script_path))

  py_code <- paste(
    "import json",
    "import os",
    "import types",
    "import importlib.util",
    "import pathlib",
    "import sys",
    "",
    "root = pathlib.Path(sys.argv[2])",
    "os.environ['TEMPLATEFLOW_HOME'] = str(root)",
    "fallback = root / 'tpl-OASIS30ANTs' / 'tpl-OASIS30ANTs_res-01_desc-brain_mask.nii.gz'",
    "fallback.parent.mkdir(parents=True, exist_ok=True)",
    "fallback.write_text('ok')",
    "",
    "def fake_get(template=None, **kwargs):",
    "    if template == 'OASIS30ANTs' and kwargs.get('label') == 'brain' and kwargs.get('suffix') == 'probseg':",
    "        return []",
    "    if template == 'OASIS30ANTs' and kwargs.get('desc') == 'brain' and kwargs.get('suffix') == 'mask':",
    "        return [str(fallback)]",
    "    return [str(fallback)]",
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
    "queries = mod.add_required_fmriprep_queries([])",
    "brain_query = next(q for q in queries if q.template == 'OASIS30ANTs' and q.params.get('label') == 'brain')",
    "paths, resolved_params, error = mod._resolve_query_result(",
    "    template=brain_query.template,",
    "    params=brain_query.params,",
    "    label=brain_query.label,",
    "    is_optional=brain_query.optional,",
    "    allow_desc_retry=brain_query.allow_desc_retry,",
    "    fallback_params=brain_query.fallback_params,",
    ")",
    "print(json.dumps({'paths': paths, 'resolved_params': resolved_params, 'error': error}, sort_keys=True))",
    sep = "\n"
  )

  root <- tempfile("prefetch_oasis_fallback_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  py_file <- tempfile("prefetch_oasis_fallback_", fileext = ".py")
  writeLines(py_code, py_file)
  on.exit(unlink(py_file), add = TRUE)

  out <- suppressWarnings(
    system2(
      py,
      c(py_file, script_path, root),
      stdout = TRUE,
      stderr = TRUE
    )
  )

  status <- attr(out, "status")
  expect_true(is.null(status) || identical(status, 0L), info = paste(out, collapse = "\n"))

  payload <- jsonlite::fromJSON(tail(out, 1), simplifyVector = FALSE)
  expect_null(payload$error)
  expect_identical(payload$resolved_params$desc, "brain")
  expect_identical(payload$resolved_params$suffix, "mask")
  expect_identical(payload$resolved_params$resolution, 1L)
  expect_length(payload$paths, 1L)
})
