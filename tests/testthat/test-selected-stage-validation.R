# Build a valid project for end-to-end validation checks without real software
# or scheduler jobs. The caller's test scope owns the temporary project.
make_selected_validation_project <- function() {
  root <- tempfile("selected-validation-")
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  cfg <- setup_project(project_name = "validation", project_directory = root, interactive = FALSE)
  dir.create(file.path(cfg$metadata$bids_directory, "sub-01"))
  resources <- file.path(root, c("container.sif", "license.txt", "atlas.nii.gz"))
  file.create(resources)
  cfg$fmriprep <- list(enable = TRUE, fs_license_file = resources[2],
    memgb = 8, nhours = 1, ncores = 2, output_spaces = "MNI152NLin2009cAsym")
  cfg$mriqc <- list(enable = TRUE, memgb = 8, nhours = 1, ncores = 2)
  cfg$compute_environment$fmriprep_container <- resources[1]
  cfg$compute_environment$mriqc_container <- resources[1]
  cfg$compute_environment$fsl_container <- resources[1]
  pp <- list(input_regex = "desc:preproc suffix:bold", bids_desc = "clean",
    memgb = 4, nhours = 1, ncores = 1, tr = 2, keep_intermediates = FALSE,
    overwrite = FALSE, validate_postproc_steps = TRUE, stop_on_failed_validation = TRUE,
    force_processing_order = FALSE)
  for (step in c("temporal_filter", "spatial_smooth", "intensity_normalize",
      "confound_calculate", "scrubbing", "confound_regression", "apply_mask", "apply_aroma")) {
    pp[[step]] <- list(enable = FALSE)
  }
  cfg$postprocess <- list(enable = TRUE, clean = pp, other = pp)
  ex <- list(input_streams = "clean", atlases = resources[3], roi_reduce = "mean",
    rtoz = FALSE, min_vox_per_roi = "5", memgb = 4, nhours = 1, ncores = 1)
  cfg$extract_rois <- list(enable = TRUE, roi = ex, other = ex)
  cfg
}

test_that("plans, previews and execution reject the same selected fMRIPrep errors", {
  cfg <- make_selected_validation_project()
  cfg$metadata$log_directory <- file.path(cfg$metadata$project_directory, "future", "logs")
  cfg$metadata$scratch_directory <- file.path(cfg$metadata$project_directory, "future", "scratch")
  original <- cfg
  local_mocked_bindings(
    setup_project_directories = function(...) stop("Unexpected directory writes"),
    record_run_provenance = function(...) stop("Unexpected provenance writes"),
    cluster_job_submit = function(...) stop("Unexpected scheduler submission"),
    .package = "BrainGnomes"
  )
  for (issue in c("license", "cores", "both")) {
    bad <- cfg
    if (issue != "cores") bad$fmriprep$fs_license_file <- file.path(cfg$metadata$project_directory, "missing-license.txt")
    if (issue != "license") bad$fmriprep$ncores <- -1
    report <- validate_project_config(bad, quiet = TRUE, steps = "fmriprep")
    expected <- c(if (issue != "cores") "fmriprep/fs_license_file", if (issue != "license") "fmriprep/ncores")
    expect_false(report$valid)
    expect_setequal(report$issues$field, expected)
    for (field in expected) {
      expect_error(plan_project(bad, "fmriprep", quiet = TRUE), field, fixed = TRUE)
      expect_error(run_project(bad, "fmriprep", dry_run = TRUE), field, fixed = TRUE)
      expect_error(run_project(bad, "fmriprep", dry_run = FALSE), field, fixed = TRUE)
    }
    exploratory <- plan_project(bad, "fmriprep", allow_invalid = TRUE, quiet = TRUE)
    expect_false(exploratory$validation$valid)
    expect_error(submit_project_plan(exploratory), expected[1], fixed = TRUE)
  }
  expect_identical(cfg, original)
  expect_false(dir.exists(dirname(cfg$metadata$log_directory)))
  expect_false(file.exists(cfg$metadata$sqlite_db))
})

test_that("unselected stages and streams do not block valid selected work", {
  cfg <- make_selected_validation_project()
  cfg$fmriprep$ncores <- -1
  cfg$fmriprep$fs_license_file <- NULL
  cfg$postprocess$other$ncores <- -1
  cfg$extract_rois$other$ncores <- -1
  cfg$bids_validation <- list(enable = TRUE) # independently scheduled, not part of run_project
  expect_false(validate_project_config(cfg, quiet = TRUE)$valid)
  for (stage in c("mriqc", "postprocess", "extract_rois")) {
    args <- list(steps = stage, postprocess_streams = "clean", extract_streams = "roi")
    report <- do.call(validate_project_config, c(list(input = cfg, quiet = TRUE), args))
    expect_true(report$valid, info = paste(report$issues$field, collapse = ", "))
    plan <- do.call(plan_project, c(list(input = cfg, quiet = TRUE), args))
    preview <- do.call(run_project, c(list(scfg = cfg, dry_run = TRUE), args))
    expect_true(plan$validation$valid)
    expect_identical(preview$validation, plan$validation)
    expect_identical(preview$preview, plan$preview)
  }
  expect_false(validate_project_config(cfg, quiet = TRUE, steps = "postprocess")$valid)
  expect_false(validate_project_config(cfg, quiet = TRUE, steps = "extract_rois")$valid)
})

test_that("valid plans are revalidated when a required file disappears", {
  cfg <- make_selected_validation_project()
  plan <- plan_project(cfg, "fmriprep", quiet = TRUE)
  file <- file.path(cfg$metadata$project_directory, "plan.yaml")
  write_project_plan(plan, file)
  unlink(cfg$fmriprep$fs_license_file)
  local_mocked_bindings(
    setup_project_directories = function(...) stop("Unexpected directory writes"),
    record_run_provenance = function(...) stop("Unexpected provenance writes"),
    cluster_job_submit = function(...) stop("Unexpected scheduler submission"),
    .package = "BrainGnomes"
  )
  expect_error(submit_project_plan(plan), "fmriprep/fs_license_file", fixed = TRUE)
  expect_error(submit_project_plan(file), "fmriprep/fs_license_file", fixed = TRUE)
})

test_that("live submission permits valid work when an unrelated stage is invalid", {
  cfg <- make_selected_validation_project()
  cfg$mriqc$ncores <- -1
  seen_steps <- NULL
  local_mocked_bindings(
    setup_project_directories = function(scfg, ...) scfg,
    record_run_provenance = function(...) NULL,
    submit_fsaverage_setup = function(...) "setup1",
    submit_prefetch_templates = function(...) "setup2",
    submit_subjects = function(scfg, steps, ...) { seen_steps <<- names(steps)[steps]; NULL },
    new_project_run = function(...) "submitted",
    cluster_job_submit = function(...) stop("Unexpected real scheduler submission"),
    .package = "BrainGnomes"
  )
  expect_identical(run_project(cfg, "fmriprep"), "submitted")
  expect_identical(seen_steps, "fmriprep")
})

test_that("guided runs validate their final selections before any writes", {
  cfg <- make_selected_validation_project()
  cfg$fmriprep$ncores <- -1
  local_mocked_bindings(
    prompt_input = function(instruct, ...) identical(instruct, "Run fmriprep?"),
    setup_project_directories = function(...) stop("Unexpected directory writes"),
    .package = "BrainGnomes"
  )
  for (preview in c(FALSE, TRUE)) {
    expect_error(run_project(cfg, subject_filter = NULL, postprocess_streams = NULL,
      extract_streams = NULL, debug = FALSE, force = FALSE, dry_run = preview, log_level = "INFO"),
      "fmriprep/ncores", fixed = TRUE)
  }
})

test_that("future output and synchronized input directories stay read-only", {
  cfg <- make_selected_validation_project()
  root <- cfg$metadata$project_directory
  cfg$metadata$log_directory <- file.path(root, "future", "logs")
  cfg$metadata$scratch_directory <- file.path(root, "future", "scratch")
  cfg$metadata$fmriprep_directory <- file.path(root, "future", "fmriprep")
  cfg$metadata$templateflow_home <- file.path(root, "future", "templates")
  expect_true(validate_project_config(cfg, quiet = TRUE, steps = "fmriprep")$valid)
  expect_true(plan_project(cfg, "fmriprep", quiet = TRUE)$validation$valid)
  expect_true(run_project(cfg, "fmriprep", dry_run = TRUE)$validation$valid)
  cfg$metadata$bids_directory <- file.path(root, "future", "bids")
  expect_false(validate_project_config(cfg, quiet = TRUE, steps = "fmriprep")$valid)
  cfg$flywheel_sync <- list(enable = TRUE, source_url = "fw://example/project", save_audit_logs = FALSE,
    memgb = 4, nhours = 1, ncores = 1)
  cfg$compute_environment$flywheel <- cfg$compute_environment$fmriprep_container
  cfg$metadata$flywheel_sync_directory <- cfg$metadata$dicom_directory <- file.path(root, "future", "dicoms")
  cfg$metadata$flywheel_temp_directory <- file.path(root, "future", "flywheel-tmp")
  cfg$bids_conversion <- list(enable = TRUE, sub_regex = "^sub-", sub_id_match = "sub-(.*)",
    ses_regex = NA_character_, ses_id_match = NA_character_, overwrite = FALSE, clear_cache = FALSE,
    heuristic_file = cfg$compute_environment$fmriprep_container, memgb = 4, nhours = 1, ncores = 1)
  cfg$compute_environment$heudiconv_container <- cfg$compute_environment$fmriprep_container
  stages <- c("flywheel_sync", "bids_conversion", "fmriprep")
  report <- validate_project_config(cfg, quiet = TRUE, steps = stages)
  expect_true(report$valid, info = paste(report$issues$field, collapse = ", "))
  plan <- plan_project(cfg, stages, quiet = TRUE)
  preview <- run_project(cfg, stages, dry_run = TRUE)
  expect_true(plan$scope_deferred)
  expect_identical(preview$validation, plan$validation)
  expect_identical(preview$preview, plan$preview)
  expect_false(dir.exists(file.path(root, "future")))
})
