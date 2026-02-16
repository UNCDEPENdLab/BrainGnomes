test_that("aroma sbatch reconciles non-zero exit with success token", {
  skip_if(Sys.which("bash") == "", "bash is required for shell-script integration test")
  skip_on_os("windows")

  resolve_pkg_file <- function(inst_rel, fallback_rel) {
    inst_path <- system.file(inst_rel, package = "BrainGnomes")
    if (nzchar(inst_path) && file.exists(inst_path)) {
      return(normalizePath(inst_path, mustWork = TRUE))
    }
    fallback <- do.call(testthat::test_path, as.list(c("..", "..", "inst", fallback_rel)))
    normalizePath(fallback, mustWork = TRUE)
  }

  script_path <- resolve_pkg_file(
    inst_rel = file.path("hpc_scripts", "aroma_subject.sbatch"),
    fallback_rel = c("hpc_scripts", "aroma_subject.sbatch")
  )

  pkg_dir_installed <- system.file(package = "BrainGnomes")
  if (nzchar(pkg_dir_installed) && file.exists(file.path(pkg_dir_installed, "shell_functions"))) {
    pkg_dir <- normalizePath(pkg_dir_installed, mustWork = TRUE)
  } else {
    pkg_dir <- normalizePath(testthat::test_path("..", "..", "inst"), mustWork = TRUE)
  }

  root <- tempfile("aroma_reconcile_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  bids_dir <- file.path(root, "bids")
  mrproc_dir <- file.path(root, "fmriprep")
  scratch_dir <- file.path(root, "scratch")
  bin_dir <- file.path(root, "bin")
  out_dir <- file.path(mrproc_dir, "sub-01", "func")
  dir.create(log_dir, recursive = TRUE)
  dir.create(file.path(bids_dir, "sub-01"), recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)
  dir.create(scratch_dir, recursive = TRUE)
  dir.create(bin_dir, recursive = TRUE)

  bold_file <- file.path(
    out_dir,
    "sub-01_task-rest_space-MNI152NLin6Asym_res-2_desc-preproc_bold.nii.gz"
  )
  aroma_container <- file.path(root, "aroma.sif")
  upd_job_status_path <- file.path(root, "upd_job_status.R")
  sqlite_db <- file.path(root, "tracking.sqlite")
  complete_file <- file.path(log_dir, ".aroma_sub-01_complete")
  fail_file <- sub("_complete$", "_fail", complete_file)
  stdout_log_env <- file.path(log_dir, "aroma_sub-01_%j.out")
  stderr_log_env <- file.path(log_dir, "aroma_sub-01_%j.err")
  stdout_log <- file.path(log_dir, "aroma_sub-01_654.out")
  stderr_log <- file.path(log_dir, "aroma_sub-01_654.err")
  status_trace <- file.path(root, "status_trace.log")

  file.create(bold_file)
  file.create(aroma_container)
  file.create(sqlite_db)
  writeLines("stale fail marker", fail_file)

  writeLines(
    c(
      "args <- commandArgs(trailingOnly = TRUE)",
      "status <- NA_character_",
      "for (i in seq_along(args)) {",
      "  if (identical(args[[i]], '--status') && i < length(args)) status <- args[[i + 1L]]",
      "}",
      "trace_file <- Sys.getenv('BG_TEST_STATUS_TRACE', unset = '')",
      "if (!is.na(status) && nzchar(trace_file)) {",
      "  cat(status, file = trace_file, sep = '\\n', append = TRUE)",
      "}"
    ),
    upd_job_status_path
  )

  singularity_path <- file.path(bin_dir, "singularity")
  writeLines(
    c(
      "#!/usr/bin/env bash",
      "echo \"fMRIPost-AROMA finished successfully!\"",
      "exit 42"
    ),
    singularity_path
  )
  Sys.chmod(singularity_path, mode = "0755")

  env <- c(
    paste0("pkg_dir=", pkg_dir),
    paste0("PATH=", bin_dir, ":", Sys.getenv("PATH")),
    paste0("R_HOME=", R.home()),
    paste0("BG_TEST_STATUS_TRACE=", status_trace),
    paste0("aroma_container=", aroma_container),
    paste0("loc_mrproc_root=", mrproc_dir),
    paste0("loc_scratch=", scratch_dir),
    paste0("loc_bids_root=", bids_dir),
    "sub_id=01",
    paste0("complete_file=", complete_file),
    paste0("stdout_log=", stdout_log_env),
    paste0("stderr_log=", stderr_log_env),
    paste0("log_file=", file.path(log_dir, "commands.log")),
    paste0("upd_job_status_path=", upd_job_status_path),
    paste0("sqlite_db=", sqlite_db),
    "debug_pipeline=0",
    "aroma_cleanup=0",
    "SLURM_JOB_ID=654",
    "SLURM_JOB_NAME=aroma_sub-01",
    "SLURM_NTASKS=4",
    "SLURM_MEM_PER_NODE=8192",
    "SLURM_JOB_CPUS_PER_NODE=4",
    "cli_options="
  )

  exit_status <- system2(
    "bash",
    args = script_path,
    env = env,
    stdout = stdout_log,
    stderr = stderr_log
  )

  expect_equal(exit_status, 0)
  expect_true(file.exists(stdout_log))
  expect_true(file.exists(complete_file))
  expect_false(file.exists(fail_file))
  expect_true(file.exists(status_trace))
  expect_equal(readLines(status_trace), c("STARTED", "COMPLETED"))
})
