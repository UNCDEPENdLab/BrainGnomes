# Mock an external operator with a known, volume-independent transformation.
# The unit tests exercise sampling, whole-image comparisons and provenance;
# actual FSL/AFNI smoothing is checked separately on compute-node replays.
test_that("spatial replay detects wrong kernels, corruption, and unchanged outputs", {
  skip_if_not_installed("RNifti")
  files <- vapply(seq_len(4), function(i) tempfile(fileext = ".nii"), character(1))
  on.exit(unlink(files), add = TRUE)
  dimensions <- c(3, 3, 3, 101)
  values <- array(seq_len(prod(dimensions)) / 100, dimensions)
  image <- RNifti::asNifti(values)
  RNifti::pixdim(image) <- c(2, 2, 4, 1)
  RNifti::writeNifti(image, files[1], datatype = "float")
  RNifti::writeNifti(RNifti::asNifti(values * 0.5, reference = image), files[2], datatype = "float")
  RNifti::writeNifti(RNifti::asNifti(array(1, dimensions[1:3]), reference = image), files[3])
  work_dirs <- character()
  sampled_indices <- NULL
  local_mocked_bindings(pp_run_spatial_replay_operator = function(
      pre_file, sampled_file, out_file, mask_file, kernel_fwhm, ...) {
    work_dirs <<- c(work_dirs, dirname(out_file))
    sampled <- RNifti::readNifti(sampled_file)
    sampled_indices <<- as.numeric(sampled[1, 1, 1, ])
    result <- RNifti::asNifti(array(as.numeric(sampled) * kernel_fwhm / 10,
                                   dim(sampled)), reference = sampled)
    RNifti::writeNifti(result, out_file, datatype = "float")
    list(backend = "test_operator", susan_brightness_threshold = NULL)
  })
  actual <- pp_replay_spatial_smooth(files[1], files[2], files[3], 5, "gaussian", FALSE)
  expect_true(actual)
  details <- attr(actual, "details")
  expect_identical(details$validation_method, "operator_replay")
  expect_equal(details$volumes_compared, 96L)
  expect_equal(details$total_volumes, 101L)
  expect_equal(details$volume_indices, pp_distributed_volume_indices(101L, 96L))
  expect_equal(sampled_indices,
               as.numeric(values[1, 1, 1, details$volume_indices]), tolerance = 1e-5)
  expect_true(details$operation_changed_data)
  expect_equal(details$n_mismatched, 0)
  expect_silent(assert_provenance_metadata(details))
  wrong <- pp_replay_spatial_smooth(files[1], files[2], files[3], 6, "gaussian", FALSE)
  expect_false(wrong)
  expect_gt(attr(wrong, "details")$n_mismatched, 0)
  unchanged <- pp_replay_spatial_smooth(files[1], files[1], files[3], 5, "gaussian", FALSE)
  expect_false(unchanged)
  identity <- pp_replay_spatial_smooth(files[1], files[1], files[3], 10, "gaussian", FALSE)
  expect_false(identity)
  expect_false(attr(identity, "details")$operation_changed_data)
  corrupted <- values * 0.5
  corrupted[1, 1, 1, 101] <- corrupted[1, 1, 1, 101] + 1
  RNifti::writeNifti(RNifti::asNifti(corrupted, reference = image), files[4], datatype = "float")
  expect_false(pp_replay_spatial_smooth(files[1], files[4], files[3], 5, "gaussian", FALSE))
  expect_false(any(dir.exists(work_dirs)))
  explicit <- validate_spatial_smooth(files[1], files[2], files[3], 5,
                                      smoother = "gaussian", used_mask = FALSE,
                                      validation_mode = "replay")
  expect_true(explicit)
  expect_identical(attr(explicit, "details")$input_mask, "none")
  expect_equal(attr(explicit, "details")$voxel_spacing_mm, c(2, 2, 4))
  local_mocked_bindings(pp_run_spatial_replay_operator = function(...) stop("backend unavailable"))
  failed <- validate_spatial_smooth(files[1], files[2], files[3], 5,
                                    validation_mode = "replay")
  expect_false(failed)
  expect_match(attr(failed, "message"), "backend unavailable")
  expect_silent(assert_provenance_metadata(attr(failed, "details")))
  expect_false(any(dir.exists(work_dirs)))
})

test_that("validated replay models select exact masks and require their sampling rule", {
  model <- pp_operator_replay_model(0, 2L, 4L, "FWHM_cross_validation_failed")
  expect_error(pp_operator_replay_model(2e-5, 2L, 4L, "FWHM_cross_validation_failed"),
               "max_replay_relative_error")
  expect_error(pp_operator_replay_model(0, 1L, 4L, "FWHM_cross_validation_failed"),
               "n_positive_replays")
  expect_error(pp_operator_replay_model(0, 2L, 3L, "FWHM_cross_validation_failed"),
               "n_negative_controls")
  contexts <- c("susan_none", "susan_fmriprep", "susan_template",
                "gaussian_mask", "gaussian_nomask", "gaussian_mask_fmriprep",
                "gaussian_nomask_fmriprep", "gaussian_mask_template",
                "gaussian_nomask_template")
  models <- setNames(rep(list(model), length(contexts)), contexts)
  calibration <- pp_attach_continuous_calibration(pp_calibration_coeffs, models)
  local_mocked_bindings(pp_calibration_coeffs = calibration)
  for (smoother in c("susan", "gaussian")) for (mask in c("none", "fmriprep", "template")) {
    selected <- pp_select_calibration(smoother, TRUE, mask, rep(2.1, 3), 4.2)
    expect_identical(selected$type, "operator_replay")
    expect_identical(selected$calibrated_input_mask, mask)
    expect_null(selected$coeffs)
    expect_null(selected$tolerance_mm)
    expect_true(selected$operator_replay_selected)
    expect_false(selected$surface_model_selected)
    expect_identical(pp_calibration_support(selected, 4.2, rep(2.1, 3)), "interpolated")
    expect_identical(pp_calibration_support(selected, 8, rep(2.1, 3)), "EXTRAPOLATED")
    expect_silent(assert_provenance_metadata(selected))
  }
  skip_if_not_installed("RNifti")
  files <- vapply(seq_len(3), function(i) tempfile(fileext = ".nii"), character(1))
  on.exit(unlink(files), add = TRUE)
  image <- RNifti::asNifti(array(1, c(3, 3, 3, 100)))
  RNifti::pixdim(image) <- c(2.1, 2.1, 2.1, 1)
  RNifti::writeNifti(image, files[1])
  RNifti::writeNifti(image, files[2])
  RNifti::writeNifti(RNifti::asNifti(array(1, c(3, 3, 3)), reference = image), files[3])
  captured <- NULL
  local_mocked_bindings(pp_replay_spatial_smooth = function(...) {
    captured <<- list(...)
    out <- TRUE
    attr(out, "details") <- list(validation_method = "operator_replay")
    out
  })
  result <- validate_spatial_smooth(files[1], files[2], files[3], 4.2,
                                    input_mask = "fmriprep", fsl_img = "test-fsl.simg")
  expect_true(result)
  expect_identical(captured[[7]], "test-fsl.simg")
  expect_true(attr(result, "details")$operator_replay_required)
  expect_identical(attr(result, "details")$calibration_support, "interpolated")
  expect_silent(assert_provenance_metadata(attr(result, "details")))
  expect_error(validate_spatial_smooth(files[1], files[2], files[3], 4.2,
                                      max_volumes = 95L), "requires max_volumes=96")
  refused <- validate_spatial_smooth(files[1], files[2], files[3], 4.2,
                                     validation_mode = "calibration")
  expect_false(refused)
  expect_false(attr(refused, "details")$calibration_accepted)
})
