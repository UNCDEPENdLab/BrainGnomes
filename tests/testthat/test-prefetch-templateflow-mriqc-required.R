test_that("prefetch plan includes TemplateFlow resources required by MRIQC", {
  py <- Sys.which("python")
  skip_if(py == "", "python is required for MRIQC prefetch required-template test")

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
    "serialized = [{'template': q.template, 'params': q.params, 'label': q.label, 'optional': q.optional} for q in queries]",
    "print('QUERIES|' + json.dumps(serialized, sort_keys=True))",
    sep = "\n"
  )

  py_file <- tempfile("prefetch_mriqc_required_", fileext = ".py")
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

  saw_mni_t1w_res2 <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["suffix"]], "T1w") &&
      identical(q[["params"]][["resolution"]], 2L) &&
      identical(q[["optional"]], FALSE),
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
  saw_mni_carpet_dseg <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["desc"]], "carpet") &&
      identical(q[["params"]][["suffix"]], "dseg") &&
      identical(q[["params"]][["resolution"]], 1L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_mni_csf_probseg <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["label"]], "CSF") &&
      identical(q[["params"]][["suffix"]], "probseg") &&
      identical(q[["params"]][["resolution"]], 1L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_mni_gm_probseg <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["label"]], "GM") &&
      identical(q[["params"]][["suffix"]], "probseg") &&
      identical(q[["params"]][["resolution"]], 1L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))
  saw_mni_wm_probseg <- any(vapply(
    queries,
    function(q) identical(q[["template"]], "MNI152NLin2009cAsym") &&
      identical(q[["params"]][["label"]], "WM") &&
      identical(q[["params"]][["suffix"]], "probseg") &&
      identical(q[["params"]][["resolution"]], 1L) &&
      identical(q[["optional"]], FALSE),
    logical(1)
  ))

  expect_true(saw_mni_t1w_res2)
  expect_true(saw_mni_boldref)
  expect_true(saw_mni_brain_mask_res2)
  expect_true(saw_mni_carpet_dseg)
  expect_true(saw_mni_csf_probseg)
  expect_true(saw_mni_gm_probseg)
  expect_true(saw_mni_wm_probseg)
})
