test_that("run_project dry_run avoids scheduling and passes dry_run to submit_subjects", {
  root <- tempfile("run_project_dry_run_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- c("dicom", "bids", "fmriprep", "postproc", "mriqc", "logs", "scratch")
  dirs <- file.path(root, dirs)
  vapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE, FUN.VALUE = logical(1))

  scfg <- list(
    metadata = list(
      project_name = "dry_run_project",
      project_directory = root,
      dicom_directory = file.path(root, "dicom"),
      bids_directory = file.path(root, "bids"),
      fmriprep_directory = file.path(root, "fmriprep"),
      postproc_directory = file.path(root, "postproc"),
      mriqc_directory = file.path(root, "mriqc"),
      log_directory = file.path(root, "logs"),
      scratch_directory = file.path(root, "scratch")
    ),
    flywheel_sync = list(enable = FALSE),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE),
    extract_rois = list(enable = FALSE),
    compute_environment = list(
      scheduler = "slurm",
      fmriprep_container = file.path(root, "fmriprep.sif")
    )
  )
  class(scfg) <- "bg_project_cfg"

  submit_called <- FALSE
  submit_dry_run <- NULL
  flywheel_called <- FALSE

  local_mocked_bindings(
    setup_project_directories = function(scfg, check_cache = NULL) scfg,
    validate_exists = function(...) TRUE,
    submit_flywheel_sync = function(...) {
      flywheel_called <<- TRUE
      "flywheel123"
    },
    submit_subjects = function(scfg, steps, subject_filter = NULL, postprocess_streams = NULL,
                               extract_streams = NULL, parent_ids = NULL, sequence_id = NULL,
                               permission_check_cache = NULL, dry_run = FALSE) {
      submit_called <<- TRUE
      submit_dry_run <<- dry_run
      invisible(TRUE)
    },
    .package = "BrainGnomes"
  )

  run_project(scfg, steps = "fmriprep", dry_run = TRUE)

  expect_true(submit_called)
  expect_true(isTRUE(submit_dry_run))
  expect_false(flywheel_called)
})

test_that("run_project interactive mode prompts for dry run and honors selection", {
  root <- tempfile("run_project_prompt_dry_run_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- c("dicom", "bids", "fmriprep", "postproc", "mriqc", "logs", "scratch")
  dirs <- file.path(root, dirs)
  vapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE, FUN.VALUE = logical(1))

  scfg <- list(
    metadata = list(
      project_name = "dry_run_prompt_project",
      project_directory = root,
      dicom_directory = file.path(root, "dicom"),
      bids_directory = file.path(root, "bids"),
      fmriprep_directory = file.path(root, "fmriprep"),
      postproc_directory = file.path(root, "postproc"),
      mriqc_directory = file.path(root, "mriqc"),
      log_directory = file.path(root, "logs"),
      scratch_directory = file.path(root, "scratch")
    ),
    flywheel_sync = list(enable = FALSE),
    bids_conversion = list(enable = TRUE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE),
    extract_rois = list(enable = FALSE),
    compute_environment = list(
      scheduler = "slurm",
      heudiconv_container = file.path(root, "heudiconv.sif")
    )
  )
  class(scfg) <- "bg_project_cfg"

  prompt_calls <- character()
  submit_dry_run <- NULL

  local_mocked_bindings(
    setup_project_directories = function(scfg, check_cache = NULL) scfg,
    validate_exists = function(...) TRUE,
    submit_subjects = function(scfg, steps, subject_filter = NULL, postprocess_streams = NULL,
                               extract_streams = NULL, parent_ids = NULL, sequence_id = NULL,
                               permission_check_cache = NULL, dry_run = FALSE) {
      submit_dry_run <<- dry_run
      invisible(TRUE)
    },
    prompt_input = function(instruct = NULL, type = "character", ...) {
      prompt_calls <<- c(prompt_calls, instruct)
      if (grepl("Enter subject IDs", instruct, fixed = TRUE)) return(NA_character_)
      if (grepl("Run BIDS conversion?", instruct, fixed = TRUE)) return(TRUE)
      if (grepl("Run pipeline in debug mode?", instruct, fixed = TRUE)) return(FALSE)
      if (grepl("Force (re-run)", instruct, fixed = TRUE)) return(FALSE)
      if (grepl("Run as dry run?", instruct, fixed = TRUE)) return(TRUE)
      if (grepl("Select log level", instruct, fixed = TRUE)) return("INFO")
      FALSE
    },
    .package = "BrainGnomes"
  )

  run_project(scfg, steps = NULL)

  expect_true(any(grepl("Run as dry run\\?", prompt_calls)))
  expect_true(isTRUE(submit_dry_run))
})
