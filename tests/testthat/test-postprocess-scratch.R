test_that("postprocess_subject stages outputs in scratch workspace", {
  bold_dir <- file.path(tempdir(), "pp-bold", "sub-TEST", "func")
  dir.create(bold_dir, recursive = TRUE, showWarnings = FALSE)
  bold_file <- file.path(bold_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz")
  con <- gzfile(bold_file, "wb")
  writeChar("fake-bold", con, eos = NULL)
  close(con)

  mask_file <- file.path(tempdir(), "pp-mask.nii.gz")
  writeLines("mask", mask_file)

  output_dir <- file.path(tempdir(), "pp-out", "sub-TEST")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  scratch_dir <- file.path(tempdir(), "pp-scratch")
  dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
  workspace_dir <- file.path(
    scratch_dir, "demo_project", "sub-TEST",
    gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(bold_file)))
  )

  cfg <- list(
    bids_desc = "postproc",
    keep_intermediates = FALSE,
    overwrite = TRUE,
    tr = 0.8,
    output_dir = output_dir,
    scratch_directory = scratch_dir,
    project_name = "demo_project",
    fsl_img = NULL,
    force_processing_order = FALSE,
    apply_mask = list(enable = TRUE, prefix = "m", mask_file = mask_file),
    spatial_smooth = list(enable = FALSE, prefix = "s", fwhm_mm = 4),
    apply_aroma = list(enable = FALSE, prefix = "a", nonaggressive = TRUE),
    temporal_filter = list(enable = FALSE, prefix = "f", method = "fslmaths"),
    intensity_normalize = list(enable = FALSE, prefix = "n", global_median = 10000),
    confound_regression = list(enable = FALSE, prefix = "r"),
    confound_calculate = list(enable = FALSE, columns = NULL, noproc_columns = NULL, demean = FALSE),
    scrubbing = list(enable = FALSE, expression = NULL, interpolate = FALSE, interpolate_prefix = "i", apply = FALSE, prefix = "x", add_to_confounds = FALSE),
    motion_filter = list(enable = FALSE)
  )

  fake_copy_step <- function(in_file, out_file, ...) {
    file.copy(in_file, out_file, overwrite = TRUE)
    out_file
  }

  recorded_paths <- new.env()

  with_mocked_bindings({
    final_file <- postprocess_subject(bold_file, cfg)
  }, automask = function(in_file, outfile, ...) {
    file.copy(in_file, outfile, overwrite = TRUE)
    outfile
  }, apply_mask = function(cur_file, mask_file, out_file, ...) {
    recorded_paths$apply_mask <- out_file
    fake_copy_step(cur_file, out_file)
  }, postprocess_confounds = function(...) NULL)

  expected_final <- file.path(output_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-postproc_bold.nii.gz")
  expect_true(file.exists(expected_final))
  expect_identical(final_file, expected_final)
  expect_identical(recorded_paths$apply_mask, expected_final)
  intermediate_file <- file.path(output_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-mPostproc_bold.nii.gz")
  expect_false(file.exists(intermediate_file))
  staged_exists <- dir.exists(workspace_dir)
  expect_false(staged_exists)
})

test_that("postprocess_subject moves intermediates when requested", {
  bold_dir <- file.path(tempdir(), "pp-bold2", "sub-TEST", "func")
  dir.create(bold_dir, recursive = TRUE, showWarnings = FALSE)
  bold_file <- file.path(bold_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz")
  con <- gzfile(bold_file, "wb")
  writeChar("fake-bold", con, eos = NULL)
  close(con)

  mask_file <- file.path(tempdir(), "pp-mask2.nii.gz")
  writeLines("mask", mask_file)

  output_dir <- file.path(tempdir(), "pp-out2", "sub-TEST")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  scratch_dir <- file.path(tempdir(), "pp-scratch2")
  dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
  workspace_dir <- file.path(
    scratch_dir, "demo_project2", "sub-TEST",
    gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(bold_file)))
  )

  cfg <- list(
    bids_desc = "postproc",
    keep_intermediates = TRUE,
    overwrite = TRUE,
    tr = 0.8,
    output_dir = output_dir,
    scratch_directory = scratch_dir,
    project_name = "demo_project2",
    fsl_img = NULL,
    force_processing_order = FALSE,
    apply_mask = list(enable = TRUE, prefix = "m", mask_file = mask_file),
    spatial_smooth = list(enable = FALSE, prefix = "s", fwhm_mm = 4),
    apply_aroma = list(enable = FALSE, prefix = "a", nonaggressive = TRUE),
    temporal_filter = list(enable = FALSE, prefix = "f", method = "fslmaths"),
    intensity_normalize = list(enable = TRUE, prefix = "n", global_median = 10000),
    confound_regression = list(enable = FALSE, prefix = "r"),
    confound_calculate = list(enable = FALSE, columns = NULL, noproc_columns = NULL, demean = FALSE),
    scrubbing = list(enable = FALSE, expression = NULL, interpolate = FALSE, interpolate_prefix = "i", apply = FALSE, prefix = "x", add_to_confounds = FALSE),
    motion_filter = list(enable = FALSE)
  )

  fake_copy_step <- function(in_file, out_file, ...) {
    file.copy(in_file, out_file, overwrite = TRUE)
    out_file
  }

  recorded_paths <- new.env()

  with_mocked_bindings({
    final_file <- postprocess_subject(bold_file, cfg)
  }, automask = function(in_file, outfile, ...) {
    file.copy(in_file, outfile, overwrite = TRUE)
    outfile
  }, apply_mask = function(cur_file, mask_file, out_file, ...) {
    recorded_paths$apply_mask <- out_file
    fake_copy_step(cur_file, out_file)
  }, intensity_normalize = function(cur_file, out_file, ...) {
    recorded_paths$intensity_normalize <- out_file
    fake_copy_step(cur_file, out_file)
  }, postprocess_confounds = function(...) NULL)

  expected_final <- file.path(output_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-postproc_bold.nii.gz")
  expect_true(file.exists(expected_final))
  expect_identical(final_file, expected_final)
  expect_identical(recorded_paths$intensity_normalize, expected_final)
  expected_workspace_mask <- file.path(workspace_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-mPostproc_bold.nii.gz")
  expect_identical(recorded_paths$apply_mask, expected_workspace_mask)
  intermediate_file <- file.path(output_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-mPostproc_bold.nii.gz")
  expect_true(file.exists(intermediate_file))
  staged_exists <- dir.exists(workspace_dir)
  expect_false(staged_exists)
})
