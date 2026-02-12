test_that("run_bids_validation sets upd_job_status_path for scheduler script", {
  root <- tempfile("run_bids_validation_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  bids_validator <- file.path(root, "bids-validator")
  file.create(bids_validator)
  Sys.chmod(bids_validator, mode = "0755")

  scfg <- list(
    metadata = list(
      log_directory = root,
      bids_directory = root
    ),
    compute_environment = list(
      bids_validator = bids_validator,
      scheduler = "slurm"
    ),
    bids_validation = list(
      outfile = "bids_validator_output.html",
      memgb = 4,
      nhours = 0.1,
      ncores = 1,
      sched_args = NULL
    ),
    log_level = "INFO",
    debug = FALSE
  )
  class(scfg) <- "bg_project_cfg"

  captured_env <- NULL
  local_mocked_bindings(
    get_job_script = function(...) "/fake/bids_validation_subject.sbatch",
    get_job_sched_args = function(...) "--time=00:10:00",
    submit_bids_validation = function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL,
                                      outfile = NULL, env_variables = NULL, sched_script = NULL,
                                      sched_args = NULL, parent_ids = NULL, lg = NULL,
                                      tracking_sqlite_db = NULL, tracking_args = NULL) {
      captured_env <<- env_variables
      "12345"
    },
    .package = "BrainGnomes"
  )

  job_id <- run_bids_validation(scfg)
  expect_equal(job_id, "12345")
  expect_true("upd_job_status_path" %in% names(captured_env))
  expect_true(nzchar(captured_env[["upd_job_status_path"]]))
  expect_match(basename(captured_env[["upd_job_status_path"]]), "^upd_job_status\\.R$")
})

test_that("submit_bids_validation handles failed submission without logging error", {
  root <- tempfile("submit_bids_validation_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    compute_environment = list(
      bids_validator = file.path(root, "bids-validator"),
      scheduler = "slurm"
    ),
    bids_validation = list(outfile = "bids_validator_output.html")
  )
  class(scfg) <- "bg_project_cfg"

  local_mocked_bindings(
    cluster_job_submit = function(...) NULL,
    .package = "BrainGnomes"
  )

  expect_warning(
    job_id <- submit_bids_validation(
      scfg = scfg,
      sub_dir = root,
      sub_id = "project",
      outfile = "bids_validator_output.html",
      env_variables = c(),
      sched_script = "/fake/script.sbatch",
      sched_args = "--time=00:10:00",
      lg = lgr::get_logger("test_submit_bids_validation")
    ),
    regexp = "Failed to schedule bids_validation job"
  )
  expect_null(job_id)
})
