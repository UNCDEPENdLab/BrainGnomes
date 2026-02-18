test_that("run_project expands steps = 'all' to enabled step flags", {
  root <- tempfile("run_project_all_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dicom_dir <- file.path(root, "dicom")
  bids_dir <- file.path(root, "bids")
  fmriprep_dir <- file.path(root, "fmriprep")
  postproc_dir <- file.path(root, "postproc")
  mriqc_dir <- file.path(root, "mriqc")
  log_dir <- file.path(root, "logs")
  scratch_dir <- file.path(root, "scratch")
  flywheel_sync_dir <- file.path(root, "flywheel_sync")
  dirs <- c(dicom_dir, bids_dir, fmriprep_dir, postproc_dir, mriqc_dir, log_dir, scratch_dir, flywheel_sync_dir)
  vapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE, FUN.VALUE = logical(1))

  fw_cli <- file.path(root, "fw")
  file.create(fw_cli)

  scfg <- list(
    metadata = list(
      project_name = "test_project",
      project_directory = root,
      dicom_directory = dicom_dir,
      bids_directory = bids_dir,
      fmriprep_directory = fmriprep_dir,
      postproc_directory = postproc_dir,
      mriqc_directory = mriqc_dir,
      log_directory = log_dir,
      scratch_directory = scratch_dir,
      flywheel_sync_directory = flywheel_sync_dir
    ),
    flywheel_sync = list(enable = TRUE, source_url = "fw://group/project"),
    bids_conversion = list(enable = TRUE, sub_regex = ".*", ses_regex = ".*"),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = TRUE),
    postprocess = list(enable = TRUE, stream1 = list()),
    extract_rois = list(enable = FALSE, stream1 = list()),
    compute_environment = list(
      flywheel = fw_cli,
      scheduler = "slurm"
    )
  )
  class(scfg) <- "bg_project_cfg"

  submitted <- FALSE
  local_mocked_bindings(
    setup_project_directories = function(scfg, check_cache = NULL) scfg,
    validate_exists = function(...) TRUE,
    submit_flywheel_sync = function(...) "flywheel123",
    submit_fsaverage_setup = function(...) "fsaverage123",
    submit_prefetch_templates = function(...) "prefetch123",
    get_job_sched_args = function(...) "--time=00:30:00",
    get_job_script = function(...) file.path(root, "submit_subjects.sbatch"),
    cluster_job_submit = function(...) {
      submitted <<- TRUE
      jid <- "controller123"
      attr(jid, "cmd") <- "sbatch submit_subjects.sbatch"
      jid
    },
    .package = "BrainGnomes"
  )

  run_project(scfg, steps = "all", debug = TRUE)

  expect_true(submitted)
  snapshot_file <- file.path(log_dir, "run_project_snapshot.rds")
  expect_true(file.exists(snapshot_file))

  snapshot <- readRDS(snapshot_file)
  expected_steps <- c("flywheel_sync", "bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess", "extract_rois")
  expected_enabled <- c("flywheel_sync", "bids_conversion", "fmriprep", "aroma", "postprocess")
  expected_disabled <- setdiff(expected_steps, expected_enabled)
  expect_identical(names(snapshot$steps), expected_steps)
  expect_true(all(snapshot$steps[expected_enabled]))
  expect_true(all(!snapshot$steps[expected_disabled]))
  expect_false("all" %in% names(snapshot$steps))
})
