test_that("run_project dry_run avoids scheduling and passes dry_run to submit_subjects", {
  root <- tempfile("run_project_dry_run_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- c("dicom", "bids", "fmriprep", "postproc", "mriqc", "logs", "scratch", "templateflow")
  dirs <- file.path(root, dirs)
  vapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE, FUN.VALUE = logical(1))
  dir.create(file.path(root, "bids", "sub-01"))
  file.create(file.path(root, c("fmriprep.sif", "license.txt")))

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
      scratch_directory = file.path(root, "scratch"),
      templateflow_home = file.path(root, "templateflow")
    ),
    flywheel_sync = list(enable = FALSE),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE, memgb = 8, nhours = 1, ncores = 2,
      fs_license_file = file.path(root, "license.txt")),
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
  submitted_subjects <- NULL
  flywheel_called <- FALSE

  local_mocked_bindings(
    setup_project_directories = function(scfg, check_cache = NULL) scfg,
    validate_exists = function(...) TRUE,
    submit_flywheel_sync = function(...) {
      flywheel_called <<- TRUE
      "flywheel123"
    },
    submit_subjects = function(scfg, steps, subject_filter = NULL,
                               resolved_subjects = NULL, postprocess_streams = NULL,
                               extract_streams = NULL, parent_ids = NULL, sequence_id = NULL,
                               permission_check_cache = NULL, dry_run = FALSE) {
      submit_called <<- TRUE
      submit_dry_run <<- dry_run
      submitted_subjects <<- resolved_subjects
      invisible(TRUE)
    },
    .package = "BrainGnomes"
  )

  run_project(scfg, steps = "fmriprep", dry_run = TRUE)

  expect_true(submit_called)
  expect_true(isTRUE(submit_dry_run))
  expect_identical(submitted_subjects$sub_id, "01")
  expect_false(flywheel_called)
})

test_that("run_project interactive mode prompts for dry run and honors selection", {
  root <- tempfile("run_project_prompt_dry_run_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- c("dicom", "bids", "fmriprep", "postproc", "mriqc", "logs", "scratch")
  dirs <- file.path(root, dirs)
  vapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE, FUN.VALUE = logical(1))
  dir.create(file.path(root, "dicom", "sub-01"))
  file.create(file.path(root, c("heudiconv.sif", "heuristic.py")))

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
    bids_conversion = list(
      enable = TRUE,
      sub_regex = "^sub-.+",
      sub_id_match = "sub-(.*)",
      ses_regex = NA_character_,
      ses_id_match = NA_character_,
      heuristic_file = file.path(root, "heuristic.py"), memgb = 4, nhours = 1,
      ncores = 1, overwrite = FALSE, clear_cache = FALSE
    ),
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
    submit_subjects = function(scfg, steps, subject_filter = NULL,
                               resolved_subjects = NULL, postprocess_streams = NULL,
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

test_that("run_project dry_run prints resolved stream settings", {
  root <- tempfile("run_project_stream_plan_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(root, "bids", "sub-01"), recursive = TRUE)

  scfg <- list(
    metadata = list(
      project_name = "stream_plan_project",
      project_directory = root,
      dicom_directory = file.path(root, "dicom"),
      bids_directory = file.path(root, "bids"),
      fmriprep_directory = file.path(root, "fmriprep"),
      postproc_directory = file.path(root, "postproc"),
      rois_directory = file.path(root, "rois"),
      mriqc_directory = file.path(root, "mriqc"),
      log_directory = file.path(root, "logs"),
      scratch_directory = file.path(root, "scratch")
    ),
    flywheel_sync = list(enable = FALSE),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(
      enable = TRUE,
      clean = list(
        input_regex = "space:MNI152NLin2009cAsym desc:preproc suffix:bold",
        bids_desc = "clean",
        apply_mask = list(enable = TRUE, mask_file = "template"),
        spatial_smooth = list(enable = TRUE),
        intensity_normalize = list(enable = FALSE),
        apply_aroma = list(enable = FALSE),
        scrubbing = list(enable = FALSE),
        temporal_filter = list(enable = TRUE),
        confound_regression = list(enable = FALSE),
        overwrite = FALSE
      )
    ),
    extract_rois = list(
      enable = TRUE,
      networks = list(
        input_streams = "clean",
        atlases = c("/atlases/network.nii.gz", "/atlases/subcortex.nii.gz"),
        mask_file = "/masks/group-mask.nii.gz",
        roi_reduce = "median",
        correlation = list(method = c("pearson", "cor.shrink")),
        min_vox_per_roi = "80%",
        save_ts = FALSE,
        save_diagnostics = TRUE,
        rtoz = TRUE,
        overwrite = FALSE
      )
    ),
    compute_environment = list(
      scheduler = "slurm",
      fsl_container = file.path(root, "fsl.sif")
    )
  )
  class(scfg) <- "bg_project_cfg"

  local_mocked_bindings(
    setup_project_directories = function(scfg, check_cache = NULL) scfg,
    # This test isolates display of stream settings; selected-stage validation
    # is exercised with real fixtures in test-selected-stage-validation.R.
    validate_project_config = function(input, ...) list(valid = TRUE,
      issues = empty_issue_df(), messages = character(), config = input),
    submit_subjects = function(...) invisible(TRUE),
    .package = "BrainGnomes"
  )

  output <- capture.output(run_project(
    scfg,
    steps = c("postprocess", "extract_rois"),
    dry_run = TRUE
  ))
  output <- paste(output, collapse = "\n")

  expect_match(output, "Resolved postprocess plan:", fixed = TRUE)
  expect_match(output, "processing order: apply_mask, spatial_smooth, temporal_filter", fixed = TRUE)
  expect_match(output, "Resolved extraction plan:", fixed = TRUE)
  expect_match(output, "clean [space:MNI152NLin2009cAsym desc:preproc suffix:bold -> desc:clean]", fixed = TRUE)
  expect_match(output, "atlases: /atlases/network.nii.gz, /atlases/subcortex.nii.gz", fixed = TRUE)
  expect_match(output, "correlation methods: pearson, cor.shrink", fixed = TRUE)
  expect_match(output, "minimum voxels per ROI: 80%", fixed = TRUE)
  expect_match(output, "save time series: false", fixed = TRUE)
  expect_match(output, "save ROI diagnostics: true", fixed = TRUE)
  expect_match(output, paste0("output root: ", file.path(root, "rois")), fixed = TRUE)
})
