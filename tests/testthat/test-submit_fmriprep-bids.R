test_that("submit_fmriprep binds external BIDS directory", {
  proj_dir <- tempfile("proj_")
  dir.create(proj_dir)
  bids_dir <- tempfile("bids_")
  dir.create(file.path(bids_dir, "sub-01"), recursive = TRUE)
  fmriprep_dir <- file.path(proj_dir, "fmriprep")
  dir.create(fmriprep_dir)
  scratch_dir <- file.path(proj_dir, "scratch")
  dir.create(scratch_dir)
  templateflow <- file.path(proj_dir, "templateflow")
  dir.create(templateflow)
  log_dir <- file.path(proj_dir, "logs")
  dir.create(log_dir)
  fs_license <- file.path(proj_dir, "fs_license.txt")
  file.create(fs_license)
  container_file <- file.path(proj_dir, "fmriprep.sif")
  file.create(container_file)
  sched_script <- file.path(proj_dir, "fmriprep_subject.sbatch")
  file.create(sched_script)

  scfg <- list(
    metadata = list(
      project_directory = proj_dir,
      bids_directory = bids_dir,
      fmriprep_directory = fmriprep_dir,
      scratch_directory = scratch_dir,
      templateflow_home = templateflow,
      log_directory = log_dir
    ),
    compute_environment = list(
      fmriprep_container = container_file,
      scheduler = "sh"
    ),
    fmriprep = list(
      enable = TRUE,
      memgb = 4,
      nhours = 1,
      ncores = 1,
      cli_options = "",
      sched_args = "",
      output_spaces = "MNI152NLin2009cAsym",
      fs_license_file = fs_license
    )
  )
  class(scfg) <- "bg_project_cfg"

  captured <- NULL
  ns <- asNamespace("BrainGnomes")
  orig <- get("cluster_job_submit", envir = ns)
  unlockBinding("cluster_job_submit", ns)
  assign("cluster_job_submit", function(sched_script, scheduler, sched_args, env_variables, wait_jobs, echo) {
    captured <<- env_variables
    structure("1", cmd = "cmd")
  }, envir = ns)
  lockBinding("cluster_job_submit", ns)
  on.exit({
    unlockBinding("cluster_job_submit", ns)
    assign("cluster_job_submit", orig, envir = ns)
    lockBinding("cluster_job_submit", ns)
  })

  submit_fmriprep(scfg, sub_dir = file.path(bids_dir, "sub-01"), sub_id = "01",
                  env_variables = NULL, sched_script = sched_script, sched_args = NULL,
                  parent_ids = NULL, lg = lgr::get_logger("test"))

  expect_equal(captured[["loc_bids_root"]], bids_dir)
  expect_equal(captured[["loc_mrproc_root"]], fmriprep_dir)
})
