test_that("is_external_path identifies directories", {
  proj <- tempfile("proj_")
  dir.create(proj, showWarnings = FALSE)
  inside <- file.path(proj, "data_fmriprep")
  outside <- tempfile("fmriprep_")
  expect_false(is_external_path(inside, proj))
  expect_true(is_external_path(outside, proj))
})

test_that("process_subject warns on missing complete file and validates inputs for internal fmriprep", {
  root <- tempfile("proj_int_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  proj_dir <- file.path(root, "proj_int"); dir.create(proj_dir)
  log_dir <- file.path(proj_dir, "logs"); dir.create(log_dir)
  bids_dir <- file.path(proj_dir, "bids"); dir.create(bids_dir)
  fmriprep_dir <- file.path(proj_dir, "data_fmriprep"); dir.create(file.path(fmriprep_dir, "sub-01"), recursive = TRUE)
  scfg <- list(
    metadata = list(project_directory = proj_dir, log_directory = log_dir,
                    bids_directory = bids_dir, fmriprep_directory = fmriprep_dir),
    postprocess = list(enable = TRUE, default = list(input_regex = "desc:preproc suffix:bold")),
    fmriprep = list(enable = FALSE),
    compute_environment = list(scheduler = "slurm"),
    force = FALSE
  )
  class(scfg) <- "bg_project_cfg"
  sub_cfg <- data.frame(sub_id = "01", ses_id = NA_character_, dicom_sub_dir = NA_character_,
                        dicom_ses_dir = NA_character_, bids_sub_dir = file.path(bids_dir, "sub-01"),
                        bids_ses_dir = NA_character_, stringsAsFactors = FALSE)
  if (!dir.exists(sub_cfg$bids_sub_dir[1L])) dir.create(sub_cfg$bids_sub_dir[1L])
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE, aroma = FALSE,
             postprocess = TRUE, extract_rois = FALSE)
  warns <- character(0)
  withCallingHandlers(
    process_subject(scfg, sub_cfg, steps),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("Missing .*_complete", warns)))
  expect_true(any(grepl("no fmriprep NIfTI inputs matched expected patterns", warns)))
})

test_that("process_subject proceeds when inputs exist but fmriprep complete file is missing", {
  root <- tempfile("proj_int_inputs_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  proj_dir <- file.path(root, "proj_int"); dir.create(proj_dir)
  log_dir <- file.path(proj_dir, "logs"); dir.create(log_dir)
  bids_dir <- file.path(proj_dir, "bids"); dir.create(bids_dir)
  fmriprep_dir <- file.path(proj_dir, "data_fmriprep")
  fmriprep_sub_dir <- file.path(fmriprep_dir, "sub-01")
  dir.create(fmriprep_sub_dir, recursive = TRUE)
  input_file <- file.path(fmriprep_sub_dir, "sub-01_task-rest_desc-preproc_bold.nii.gz")
  writeBin(raw(0), input_file)

  scfg <- list(
    metadata = list(project_directory = proj_dir, log_directory = log_dir,
                    bids_directory = bids_dir, fmriprep_directory = fmriprep_dir),
    postprocess = list(enable = TRUE, default = list(input_regex = "desc:preproc suffix:bold",
                                                     ncores = 1, nhours = 1, memgb = 1)),
    fmriprep = list(enable = FALSE),
    compute_environment = list(scheduler = "slurm"),
    force = FALSE
  )
  class(scfg) <- "bg_project_cfg"
  sub_cfg <- data.frame(sub_id = "01", ses_id = NA_character_, dicom_sub_dir = NA_character_,
                        dicom_ses_dir = NA_character_, bids_sub_dir = file.path(bids_dir, "sub-01"),
                        bids_ses_dir = NA_character_, stringsAsFactors = FALSE)
  if (!dir.exists(sub_cfg$bids_sub_dir[1L])) dir.create(sub_cfg$bids_sub_dir[1L], recursive = TRUE)
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE, aroma = FALSE,
             postprocess = TRUE, extract_rois = FALSE)

  submit_calls <- 0
  local_mocked_bindings(
    get_job_script = function(...) "mock_script",
    get_job_sched_args = function(...) "--mock",
    submit_postprocess = function(...) {
      submit_calls <<- submit_calls + 1
      "job-1"
    },
    .package = "BrainGnomes"
  )

  warns <- character(0)
  withCallingHandlers(
    process_subject(scfg, sub_cfg, steps),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Missing .*_complete", warns)))
  expect_true(any(grepl("Proceeding with postprocessing", warns)))
  expect_gt(submit_calls, 0)
})

test_that("process_subject validates fmriprep inputs for external fmriprep", {
  root <- tempfile("proj_ext_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  proj_dir <- file.path(root, "proj_ext"); dir.create(proj_dir)
  log_dir <- file.path(proj_dir, "logs"); dir.create(log_dir)
  bids_dir <- file.path(proj_dir, "bids"); dir.create(bids_dir)
  fmriprep_dir <- file.path(root, "external_fmriprep"); dir.create(file.path(fmriprep_dir, "sub-01"), recursive = TRUE)
  scfg <- list(
    metadata = list(project_directory = proj_dir, log_directory = log_dir,
                    bids_directory = bids_dir, fmriprep_directory = fmriprep_dir),
    postprocess = list(enable = TRUE, default = list(input_regex = "desc:preproc suffix:bold")),
    fmriprep = list(enable = FALSE),
    compute_environment = list(scheduler = "slurm"),
    force = FALSE
  )
  class(scfg) <- "bg_project_cfg"
  sub_cfg <- data.frame(sub_id = "01", ses_id = NA_character_, dicom_sub_dir = NA_character_,
                        dicom_ses_dir = NA_character_, bids_sub_dir = file.path(bids_dir, "sub-01"),
                        bids_ses_dir = NA_character_, stringsAsFactors = FALSE)
  if (!dir.exists(sub_cfg$bids_sub_dir[1L])) dir.create(sub_cfg$bids_sub_dir[1L], recursive = TRUE)
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE, aroma = FALSE,
             postprocess = TRUE, extract_rois = FALSE)
  warns <- character(0)
  withCallingHandlers(
    process_subject(scfg, sub_cfg, steps),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("no fmriprep NIfTI inputs matched expected patterns", warns)))
})

test_that("process_subject skips postprocess submission when preflight permission checks fail", {
  root <- tempfile("proj_preflight_fail_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  proj_dir <- file.path(root, "proj"); dir.create(proj_dir)
  log_dir <- file.path(proj_dir, "logs"); dir.create(log_dir)
  bids_dir <- file.path(proj_dir, "bids"); dir.create(bids_dir)
  fmriprep_dir <- file.path(proj_dir, "data_fmriprep")
  fmriprep_sub_dir <- file.path(fmriprep_dir, "sub-01")
  dir.create(fmriprep_sub_dir, recursive = TRUE)
  input_file <- file.path(fmriprep_sub_dir, "sub-01_task-rest_desc-preproc_bold.nii.gz")
  writeBin(raw(0), input_file)

  scfg <- list(
    metadata = list(
      project_directory = proj_dir,
      log_directory = log_dir,
      bids_directory = bids_dir,
      fmriprep_directory = fmriprep_dir,
      postproc_directory = file.path(proj_dir, "data_postproc"),
      rois_directory = file.path(proj_dir, "data_rois"),
      mriqc_directory = file.path(proj_dir, "data_mriqc"),
      scratch_directory = file.path(proj_dir, "scratch"),
      templateflow_home = file.path(proj_dir, "templateflow"),
      sqlite_db = file.path(proj_dir, "track.sqlite")
    ),
    postprocess = list(enable = TRUE, default = list(input_regex = "desc:preproc suffix:bold",
                                                     ncores = 1, nhours = 1, memgb = 1)),
    fmriprep = list(enable = FALSE),
    compute_environment = list(scheduler = "slurm"),
    force = FALSE,
    log_level = "INFO",
    debug = FALSE
  )
  class(scfg) <- "bg_project_cfg"

  dirs_to_create <- c(
    scfg$metadata$postproc_directory,
    scfg$metadata$rois_directory,
    scfg$metadata$mriqc_directory,
    scfg$metadata$scratch_directory,
    scfg$metadata$templateflow_home
  )
  for (dd in dirs_to_create) dir.create(dd, recursive = TRUE, showWarnings = FALSE)

  sub_cfg <- data.frame(sub_id = "01", ses_id = NA_character_, dicom_sub_dir = NA_character_,
                        dicom_ses_dir = NA_character_, bids_sub_dir = file.path(bids_dir, "sub-01"),
                        bids_ses_dir = NA_character_, stringsAsFactors = FALSE)
  if (!dir.exists(sub_cfg$bids_sub_dir[1L])) dir.create(sub_cfg$bids_sub_dir[1L], recursive = TRUE)
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE, aroma = FALSE,
             postprocess = TRUE, extract_rois = FALSE)

  submit_calls <- 0
  local_mocked_bindings(
    get_job_script = function(...) "mock_script",
    get_job_sched_args = function(...) "--mock",
    collect_submit_permission_issues = function(...) "stdout log directory is not writable: /mock/path",
    submit_postprocess = function(...) {
      submit_calls <<- submit_calls + 1
      "job-1"
    },
    .package = "BrainGnomes"
  )

  warns <- character(0)
  withCallingHandlers(
    process_subject(scfg, sub_cfg, steps),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(submit_calls, 0)
  expect_true(any(grepl("preflight permission checks failed", warns)))
})
