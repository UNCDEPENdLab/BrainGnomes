test_that("prefetch integration downloads default fMRIPrep TemplateFlow resources via srun", {
  skip_if(
    !identical(tolower(Sys.getenv("BG_RUN_SRUN_TEMPLATEFLOW_INTEGRATION", "false")), "true"),
    "Set BG_RUN_SRUN_TEMPLATEFLOW_INTEGRATION=true to run the HPC-backed TemplateFlow integration test."
  )
  skip_if(Sys.which("srun") == "", "srun is required for the HPC-backed TemplateFlow integration test")
  skip_on_os("windows")

  container_path <- "/proj/hng/software/containers/fmriprep-25.0.0.simg"
  skip_if(!file.exists(container_path), paste("Missing fMRIPrep container:", container_path))

  resolve_pkg_file <- function(inst_rel, fallback_rel) {
    inst_path <- system.file(inst_rel, package = "BrainGnomes")
    if (nzchar(inst_path) && file.exists(inst_path)) {
      return(normalizePath(inst_path, mustWork = TRUE))
    }
    fallback <- do.call(testthat::test_path, as.list(c("..", "..", "inst", fallback_rel)))
    normalizePath(fallback, mustWork = TRUE)
  }

  script_path <- resolve_pkg_file(
    inst_rel = "prefetch_templateflow.py",
    fallback_rel = "prefetch_templateflow.py"
  )

  root_base <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  root <- tempfile("prefetch_srun_integration_", tmpdir = root_base)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  templateflow_home <- file.path(root, "templateflow")
  artifacts_dir <- file.path(root, "artifacts")
  dir.create(templateflow_home, recursive = TRUE, showWarnings = FALSE)
  dir.create(artifacts_dir, recursive = TRUE, showWarnings = FALSE)

  summary_file <- file.path(artifacts_dir, "summary.json")
  manifest_file <- file.path(artifacts_dir, "manifest.json")
  state_file <- file.path(artifacts_dir, "prefetch_state.dcf")
  stdout_log <- file.path(artifacts_dir, "srun_stdout.log")
  stderr_log <- file.path(artifacts_dir, "srun_stderr.log")
  shell_script_file <- file.path(artifacts_dir, "run_prefetch_integration.sh")

  script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
  artifacts_dir_norm <- normalizePath(artifacts_dir, winslash = "/", mustWork = TRUE)
  templateflow_home_norm <- normalizePath(templateflow_home, winslash = "/", mustWork = TRUE)
  container_path_norm <- normalizePath(container_path, winslash = "/", mustWork = TRUE)
  script_path_norm <- normalizePath(script_path, winslash = "/", mustWork = TRUE)

  shell_lines <- c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "runtime=''",
    "for candidate in /usr/bin/singularity /usr/bin/apptainer; do",
    "  if [ -x \"$candidate\" ]; then",
    "    runtime=\"$candidate\"",
    "    break",
    "  fi",
    "done",
    "if [ -z \"$runtime\" ]; then",
    "  runtime=$(command -v singularity || command -v apptainer || true)",
    "fi",
    "echo \"PATH=$PATH\"",
    "echo \"runtime=${runtime:-<missing>}\"",
    "if [ -z \"$runtime\" ]; then",
    "  echo 'No singularity/apptainer runtime found inside srun session' >&2",
    "  exit 1",
    "fi",
    sprintf("rm -rf %s", shQuote(templateflow_home_norm)),
    sprintf("mkdir -p %s %s", shQuote(templateflow_home_norm), shQuote(artifacts_dir_norm)),
    sprintf("export TEMPLATEFLOW_HOME=%s", shQuote(templateflow_home_norm)),
    sprintf("export APPTAINERENV_TEMPLATEFLOW_HOME=%s", shQuote(templateflow_home_norm)),
    "exec \"$runtime\" exec --cleanenv --containall \\",
    sprintf("  -B %s:%s \\", shQuote(script_dir), shQuote(script_dir)),
    sprintf("  -B %s:%s \\", shQuote(artifacts_dir_norm), shQuote(artifacts_dir_norm)),
    sprintf("  -B %s:%s \\", shQuote(templateflow_home_norm), shQuote(templateflow_home_norm)),
    sprintf("  %s python %s \\", shQuote(container_path_norm), shQuote(script_path_norm)),
    "  --output-spaces MNI152NLin2009cAsym \\",
    sprintf("  --summary-json %s \\", shQuote(summary_file)),
    sprintf("  --manifest-json %s \\", shQuote(manifest_file)),
    sprintf("  --state-file %s", shQuote(state_file))
  )
  writeLines(shell_lines, shell_script_file)
  Sys.chmod(shell_script_file, mode = "0755")

  status <- suppressWarnings(system2(
    "srun",
    args = c(
      "-t", "8:00:00",
      "--mem=32gb",
      "-p", "interact",
      "-N", "1",
      "-n", "1",
      "/bin/bash",
      shell_script_file
    ),
    stdout = stdout_log,
    stderr = stderr_log
  ))

  failure_context <- paste(
    c(
      paste0("exit_status: ", status),
      if (file.exists(stdout_log)) c("stdout (tail):", tail(readLines(stdout_log, warn = FALSE), 50)) else "stdout missing",
      if (file.exists(stderr_log)) c("stderr (tail):", tail(readLines(stderr_log, warn = FALSE), 50)) else "stderr missing"
    ),
    collapse = "\n"
  )

  expect_identical(status, 0L, info = failure_context)
  expect_true(file.exists(summary_file), info = failure_context)
  expect_true(file.exists(manifest_file), info = failure_context)
  expect_true(file.exists(state_file), info = failure_context)
  if (!identical(status, 0L) || !file.exists(summary_file) || !file.exists(manifest_file) || !file.exists(state_file)) {
    return(invisible())
  }

  summary <- jsonlite::fromJSON(summary_file, simplifyVector = FALSE)
  manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)

  expect_identical(summary$status, "COMPLETED")
  expect_identical(manifest$output_dir, templateflow_home_norm)

  manifest_paths <- vapply(manifest$files, `[[`, character(1), "path")
  expected_required <- c(
    "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-01_T1w.nii.gz",
    "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-02_desc-fMRIPrep_boldref.nii.gz",
    "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-02_desc-brain_mask.nii.gz",
    "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-01_label-brain_probseg.nii.gz",
    "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-01_desc-carpet_dseg.nii.gz",
    "tpl-OASIS30ANTs/tpl-OASIS30ANTs_res-01_T1w.nii.gz"
  )
  expect_true(all(expected_required %in% manifest_paths), info = failure_context)

  oasis_brain_variants <- c(
    "tpl-OASIS30ANTs/tpl-OASIS30ANTs_res-01_label-brain_probseg.nii.gz",
    "tpl-OASIS30ANTs/tpl-OASIS30ANTs_res-01_desc-brain_mask.nii.gz"
  )
  expect_true(any(oasis_brain_variants %in% manifest_paths), info = failure_context)

  for (rel_path in unique(c(expected_required, manifest_paths[manifest_paths %in% oasis_brain_variants]))) {
    expect_true(file.exists(file.path(templateflow_home_norm, rel_path)), info = failure_context)
  }
})
