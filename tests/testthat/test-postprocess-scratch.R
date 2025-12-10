norm_path <- function(path, mustWork = FALSE) normalizePath(path, winslash = "/", mustWork = mustWork)

test_that("postprocess_subject stages outputs in scratch workspace", {
  tmp_dir <- norm_path(tempfile("pp-scratch-"), mustWork = FALSE)
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(tmp_dir, "logs", "sub-TEST")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  old_log <- Sys.getenv("log_file")
  on.exit(Sys.setenv(log_file = old_log), add = TRUE)
  Sys.setenv(log_file = file.path(log_dir, "postprocess.log"))

  bold_dir <- file.path(tmp_dir, "pp-bold", "sub-TEST", "func")
  dir.create(bold_dir, recursive = TRUE, showWarnings = FALSE)
  bold_file <- file.path(bold_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz")
  con <- gzfile(bold_file, "wb")
  writeChar("fake-bold", con, eos = NULL)
  close(con)

  mask_file <- file.path(tmp_dir, "pp-mask.nii.gz")
  writeLines("mask", mask_file)

  output_dir <- file.path(tmp_dir, "pp-out", "sub-TEST")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  scratch_dir <- file.path(tmp_dir, "pp-scratch")
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

  final_file <- norm_path(final_file, mustWork = TRUE)
  expected_final <- norm_path(file.path(output_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-postproc_bold.nii.gz"), mustWork = TRUE)
  expect_true(file.exists(expected_final))
  expect_identical(final_file, expected_final)
  expect_identical(norm_path(recorded_paths$apply_mask, mustWork = TRUE), expected_final)
  intermediate_file <- file.path(output_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-mPostproc_bold.nii.gz")
  expect_false(file.exists(intermediate_file))
  staged_exists <- dir.exists(workspace_dir)
  expect_false(staged_exists)
})

test_that("postprocess_subject moves intermediates when requested", {
  tmp_dir <- norm_path(tempfile("pp-scratch-"), mustWork = FALSE)
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(tmp_dir, "logs", "sub-TEST")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  old_log <- Sys.getenv("log_file")
  on.exit(Sys.setenv(log_file = old_log), add = TRUE)
  Sys.setenv(log_file = file.path(log_dir, "postprocess.log"))

  bold_dir <- file.path(tmp_dir, "pp-bold2", "sub-TEST", "func")
  dir.create(bold_dir, recursive = TRUE, showWarnings = FALSE)
  bold_file <- file.path(bold_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz")
  con <- gzfile(bold_file, "wb")
  writeChar("fake-bold", con, eos = NULL)
  close(con)

  mask_file <- file.path(tmp_dir, "pp-mask2.nii.gz")
  writeLines("mask", mask_file)

  output_dir <- file.path(tmp_dir, "pp-out2", "sub-TEST")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  scratch_dir <- file.path(tmp_dir, "pp-scratch2")
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

  final_file <- norm_path(final_file, mustWork = TRUE)
  expected_final <- norm_path(file.path(output_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-postproc_bold.nii.gz"), mustWork = TRUE)
  expect_true(file.exists(expected_final))
  expect_identical(final_file, expected_final)
  expect_identical(norm_path(recorded_paths$intensity_normalize, mustWork = TRUE), expected_final)
  expected_workspace_mask <- norm_path(file.path(workspace_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-mPostproc_bold.nii.gz"))
  expect_identical(norm_path(recorded_paths$apply_mask), expected_workspace_mask)
  intermediate_file <- file.path(output_dir, "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-mPostproc_bold.nii.gz")
  expect_true(file.exists(intermediate_file))
  staged_exists <- dir.exists(workspace_dir)
  expect_false(staged_exists)
})
