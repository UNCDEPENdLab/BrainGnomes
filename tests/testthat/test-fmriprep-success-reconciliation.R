test_that("fmriprep sbatch reconciles non-zero exit with success token", {
  skip_if(Sys.which("bash") == "", "bash is required for shell-script integration test")

  resolve_pkg_file <- function(inst_rel, fallback_rel) {
    inst_path <- system.file(inst_rel, package = "BrainGnomes")
    if (nzchar(inst_path) && file.exists(inst_path)) {
      return(normalizePath(inst_path, mustWork = TRUE))
    }
    fallback <- do.call(testthat::test_path, as.list(c("..", "..", "inst", fallback_rel)))
    normalizePath(fallback, mustWork = TRUE)
  }

  script_path <- resolve_pkg_file(
    inst_rel = file.path("hpc_scripts", "fmriprep_subject.sbatch"),
    fallback_rel = c("hpc_scripts", "fmriprep_subject.sbatch")
  )

  pkg_dir_installed <- system.file(package = "BrainGnomes")
  if (nzchar(pkg_dir_installed) && file.exists(file.path(pkg_dir_installed, "shell_functions"))) {
    pkg_dir <- normalizePath(pkg_dir_installed, mustWork = TRUE)
  } else {
    pkg_dir <- normalizePath(testthat::test_path("..", "..", "inst"), mustWork = TRUE)
  }

  root <- tempfile("fmriprep_reconcile_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  bids_dir <- file.path(root, "bids")
  mrproc_dir <- file.path(root, "fmriprep")
  scratch_dir <- file.path(root, "scratch")
  templateflow_dir <- file.path(root, "templateflow")
  bin_dir <- file.path(root, "bin")
  r_home <- file.path(root, "R_HOME")
  r_bin <- file.path(r_home, "bin")
  dir.create(log_dir, recursive = TRUE)
  dir.create(file.path(bids_dir, "sub-01"), recursive = TRUE)
  dir.create(mrproc_dir, recursive = TRUE)
  dir.create(scratch_dir, recursive = TRUE)
  dir.create(templateflow_dir, recursive = TRUE)
  dir.create(bin_dir, recursive = TRUE)
  dir.create(r_bin, recursive = TRUE)

  fmriprep_container <- file.path(root, "fmriprep.sif")
  fs_license_file <- file.path(root, "fs_license.txt")
  upd_job_status_path <- file.path(root, "upd_job_status.R")
  sqlite_db <- file.path(root, "tracking.sqlite")
  complete_file <- file.path(log_dir, ".fmriprep_sub-01_complete")
  fail_file <- sub("_complete$", "_fail", complete_file)
  stdout_log_env <- file.path(log_dir, "fmriprep_sub-01_%j.out")
  stderr_log_env <- file.path(log_dir, "fmriprep_sub-01_%j.err")
  stdout_log <- file.path(log_dir, "fmriprep_sub-01_321.out")
  stderr_log <- file.path(log_dir, "fmriprep_sub-01_321.err")
  status_trace <- file.path(root, "status_trace.log")

  file.create(fmriprep_container)
  file.create(fs_license_file)
  file.create(upd_job_status_path)
  file.create(sqlite_db)
  writeLines("stale fail marker", fail_file)

  singularity_path <- file.path(bin_dir, "singularity")
  writeLines(
    c(
      "#!/usr/bin/env bash",
      "echo \"fMRIPrep finished successfully!\"",
      "exit 42"
    ),
    singularity_path
  )
  Sys.chmod(singularity_path, mode = "0755")

  rscript_path <- file.path(r_bin, "Rscript")
  writeLines(
    c(
      "#!/usr/bin/env bash",
      "status=\"\"",
      "for ((i=1; i<=$#; i++)); do",
      "  if [[ \"${!i}\" == \"--status\" ]]; then",
      "    next=$((i+1))",
      "    status=\"${!next}\"",
      "    break",
      "  fi",
      "done",
      "if [[ -n \"$BG_TEST_STATUS_TRACE\" && -n \"$status\" ]]; then",
      "  echo \"$status\" >> \"$BG_TEST_STATUS_TRACE\"",
      "fi",
      "exit 0"
    ),
    rscript_path
  )
  Sys.chmod(rscript_path, mode = "0755")

  env <- c(
    paste0("pkg_dir=", pkg_dir),
    paste0("PATH=", bin_dir, ":", Sys.getenv("PATH")),
    paste0("R_HOME=", r_home),
    paste0("BG_TEST_STATUS_TRACE=", status_trace),
    paste0("fmriprep_container=", fmriprep_container),
    paste0("loc_mrproc_root=", mrproc_dir),
    paste0("loc_scratch=", scratch_dir),
    paste0("loc_bids_root=", bids_dir),
    paste0("templateflow_home=", templateflow_dir),
    "sub_id=01",
    paste0("complete_file=", complete_file),
    paste0("stdout_log=", stdout_log_env),
    paste0("stderr_log=", stderr_log_env),
    paste0("log_file=", file.path(log_dir, "commands.log")),
    paste0("fs_license_file=", fs_license_file),
    paste0("upd_job_status_path=", upd_job_status_path),
    paste0("sqlite_db=", sqlite_db),
    "debug_pipeline=0",
    "SLURM_JOB_ID=321",
    "SLURM_JOB_NAME=fmriprep_sub-01",
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
