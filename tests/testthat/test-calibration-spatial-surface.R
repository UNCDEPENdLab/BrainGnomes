# Construct a known isotropic Gaussian response for numerical surface tests.
# Returns compact model parameters; measured calibration models are tested
# separately against the published held-out evidence.
gaussian_surface_fixture <- function() {
  list(
    type = "quadrature_axis_surface", surface_family = "power",
    resolution_degree = 0L, baseline_feature = FALSE,
    anisotropy_feature = FALSE,
    coeffs = list(level = 0, shape = qlogis((1 - 0.3) / 2.7)),
    voxel_axis_range_mm = c(1.8, 4), kernel_ratio_range = c(1.5, 3),
    tolerance_voxels = 0.25, model_version = "test-surface", max_volumes = 96L
  )
}

test_that("directional quadrature preserves the known Gaussian response", {
  model <- gaussian_surface_fixture()
  pre <- c(3, 4, 5)
  spacing <- c(2, 3, 4)
  prediction <- pp_calibration_surface_prediction(model, 6, pre, spacing)
  expect_equal(prediction$expected_axes_mm, sqrt(pre^2 + 6^2))
  expect_equal(prediction$effective_kernel_axes_mm, rep(6, 3))
  expect_equal(prediction$axis_gain, rep(1, 3))
  expect_equal(prediction$expected_geom_mm, prod(sqrt(pre^2 + 6^2))^(1 / 3))
  expect_true(prediction$equivalent_geometric_gain > 0)
  expect_error(pp_calibration_surface_prediction(model, 6, NULL, spacing),
               "pre_axis_fwhm")
  expect_error(pp_calibration_surface_prediction(model, 6, c(3, NA, 5), spacing),
               "pre_axis_fwhm")
  expect_silent(assert_provenance_metadata(prediction))
})

test_that("smooth anisotropic features and predictions respect axis permutations", {
  model <- gaussian_surface_fixture()
  model$resolution_degree <- 2L
  model$baseline_feature <- TRUE
  model$anisotropy_feature <- TRUE
  model$coeffs <- list(level = c(-0.2, 0.1, -0.05, 0.04, 0.15, -0.2),
                       shape = c(-1, 0.05, 0.02, -0.1, 0.2, 0.1))
  pre <- c(2.6, 3.9, 4.5)
  spacing <- c(1.8, 2.4, 4)
  prediction <- pp_calibration_surface_prediction(model, 5, pre, spacing)
  for (permutation in list(c(1, 3, 2), c(2, 1, 3), c(2, 3, 1),
                          c(3, 1, 2), c(3, 2, 1))) {
    permuted <- pp_calibration_surface_prediction(
      model, 5, pre[permutation], spacing[permutation]
    )
    expect_equal(permuted$expected_axes_mm, prediction$expected_axes_mm[permutation])
    expect_equal(permuted$expected_geom_mm, prediction$expected_geom_mm)
  }
  left <- pp_calibration_surface_prediction(model, 5, pre, spacing - 1e-7)
  right <- pp_calibration_surface_prediction(model, 5, pre, spacing + 1e-7)
  expect_lt(max(abs(left$expected_axes_mm - right$expected_axes_mm)), 1e-5)
})

test_that("coupled baseline features preserve permutations and share cross-axis information", {
  spacing <- c(1.8, 2.4, 4)
  pre <- c(2.6, 3.9, 4.5)
  features <- pp_calibration_surface_features(spacing, pre, 2L, TRUE, TRUE, TRUE)
  expect_equal(ncol(features), 8L)
  normalized <- log(pre / spacing)
  expect_equal(features[, "baseline_geom_log"], rep(mean(normalized), 3))
  expect_equal(features[, "baseline_anisotropy_variance"],
               rep(mean((normalized - mean(normalized))^2), 3))
  expect_error(pp_calibration_surface_features(spacing, pre, coupled_baseline = TRUE),
               "require directional baseline")
  model <- pp_calibration_surface_model(
    "power", 2L, TRUE, TRUE,
    list(level = c(-0.2, 0.1, -0.05, 0.04, 0.15, -0.2, 0.1, 0.2),
         shape = c(-1, 0.05, 0.02, -0.1, 0.2, 0.1, -0.1, 0.1)),
    0.25, 0.2, 0.21, coupled_baseline_feature = TRUE
  )
  original <- pp_calibration_surface_prediction(model, 5, pre, spacing)
  for (permutation in list(c(1, 3, 2), c(2, 1, 3), c(2, 3, 1),
                          c(3, 1, 2), c(3, 2, 1))) {
    permuted <- pp_calibration_surface_prediction(
      model, 5, pre[permutation], spacing[permutation]
    )
    expect_equal(permuted$expected_axes_mm, original$expected_axes_mm[permutation])
    expect_equal(permuted$expected_geom_mm, original$expected_geom_mm)
  }
  changed <- pre
  changed[3] <- changed[3] * 1.2
  shared <- pp_calibration_surface_features(spacing, changed, 2L, TRUE, TRUE, TRUE)
  expect_equal(shared[1, "baseline_log"], features[1, "baseline_log"])
  expect_false(isTRUE(all.equal(shared[1, "baseline_geom_log"],
                                features[1, "baseline_geom_log"])))
  expect_silent(assert_provenance_metadata(model))
})

test_that("baseline curvature and resolution interactions retain directional symmetry", {
  spacing <- c(1.8, 2.4, 4)
  pre <- c(2.6, 3.9, 4.5)
  features <- pp_calibration_surface_features(
    spacing, pre, 2L, TRUE, quadratic_baseline = TRUE
  )
  expect_equal(ncol(features), 6L)
  expect_equal(features[, "baseline_log_sq"], log(pre / spacing)^2)
  expect_equal(features[, "resolution_baseline_log"],
               log(spacing / 2.5) * log(pre / spacing))
  expect_equal(ncol(pp_calibration_surface_features(
    spacing, pre, 0L, TRUE, quadratic_baseline = TRUE
  )), 3L)
  expect_error(pp_calibration_surface_features(spacing, pre, quadratic_baseline = TRUE),
               "require directional baseline")
  model <- pp_calibration_surface_model(
    "power", 2L, TRUE, FALSE,
    list(level = c(-0.2, 0.1, -0.05, 0.04, 0.1, -0.2),
         shape = c(-1, 0.05, 0.02, -0.1, 0.2, 0.1)),
    0.25, 0.2, 0.21, quadratic_baseline_feature = TRUE
  )
  original <- pp_calibration_surface_prediction(model, 5, pre, spacing)
  permutation <- c(3, 1, 2)
  permuted <- pp_calibration_surface_prediction(model, 5, pre[permutation], spacing[permutation])
  expect_equal(permuted$expected_axes_mm, original$expected_axes_mm[permutation])
  expect_equal(permuted$expected_geom_mm, original$expected_geom_mm)
  expect_silent(assert_provenance_metadata(model))
})

test_that("surface support uses each voxel axis and geometric mean kernel ratios", {
  model <- gaussian_surface_fixture()
  for (spacing in list(rep(1.8, 3), rep(2.1, 3), rep(4, 3),
                       c(1.8, 1.8, 4), c(1.8, 4, 4), c(2, 2, 4))) {
    reference <- prod(spacing)^(1 / 3)
    expect_true(pp_calibration_surface_domain(model, spacing, 1.5 * reference))
    expect_true(pp_calibration_surface_domain(model, spacing, 3 * reference))
    expect_false(pp_calibration_surface_domain(model, spacing, 1.49 * reference))
    expect_false(pp_calibration_surface_domain(model, spacing, 3.01 * reference))
  }
  expect_false(pp_calibration_surface_domain(model, c(1.79, 3, 3), 6))
  expect_false(pp_calibration_surface_domain(model, c(4.01, 2, 2), 6))
  expect_false(pp_calibration_surface_domain(model, NULL, 6))
  expect_false(pp_calibration_surface_domain(model, rep(2.1, 3), NULL))
  expect_true(pp_calibration_surface_domain(model, rep(1.8 - 1e-8, 3), 2.7))
})

test_that("positive surface families increase smoothly across undersmoothing ratios", {
  minimum_axis_ratio <- 1.5 * (1.8 / 4)^(2 / 3)
  expect_gt(minimum_axis_ratio, 0.75)
  model <- gaussian_surface_fixture()
  spacing <- c(4, 1.8, 1.8)
  pre <- c(2.3, 2.8, 3)
  kernels <- seq(1.5, 3, length.out = 101) * prod(spacing)^(1 / 3)
  for (family in c("power", "affine")) {
    model$surface_family <- family
    predictions <- vapply(kernels, function(kernel) {
      pp_calibration_surface_prediction(model, kernel, pre, spacing)$expected_geom_mm
    }, numeric(1))
    expect_true(all(is.finite(predictions)))
    expect_true(all(diff(predictions) > 0))
  }
  model$surface_family <- "affine"
  model$coeffs <- list(level = log(0.25), shape = log(0.9))
  prediction <- pp_calibration_surface_prediction(model, 5, pre, spacing)
  effective <- spacing * (0.25 + 0.9 * (5 / spacing - 0.75))
  expect_equal(prediction$expected_axes_mm, sqrt(pre^2 + effective^2))
})

test_that("continuous selection preserves preparation and excludes unrelated support", {
  surface <- gaussian_surface_fixture()
  base <- list(type = "quadrature_ratio_linear", model_version = "legacy",
               coeffs = c(1, 0), preprocess = TRUE, polydeg = 3L, unif = TRUE,
               external_support = list(list(voxel_range_mm = c(1, 5))),
               continuous_models = list(fmriprep = surface),
               grid_models = list(list(voxel_spacing_mm = rep(2, 3))))
  selected <- pp_select_calibration_surface(base, "fmriprep", c(2, 2, 4), 5)
  expect_identical(selected$type, "quadrature_axis_surface")
  expect_true(selected$preprocess)
  expect_true(selected$unif)
  expect_identical(selected$polydeg, 3L)
  expect_identical(selected$base_model_version, "legacy")
  expect_identical(selected$calibrated_input_mask, "fmriprep")
  expect_false(selected$input_mask_extrapolated)
  expect_null(selected$grid_models)
  expect_null(selected$external_support)
  expect_equal(selected$tolerance_axis_mm, c(0.5, 0.5, 1))
  expect_equal(selected$tolerance_mm, 0.25 * 16^(1 / 3))
  expect_null(pp_select_calibration_surface(base, "custom", c(2, 2, 4), 5))
  expect_null(pp_select_calibration_surface(base, "fmriprep", c(2, 2, 4), 8))
  expect_null(pp_select_calibration_surface(base, "fmriprep", NULL, 5))
  expect_silent(assert_provenance_metadata(selected))
})

test_that("surface construction requires independently validated bounded errors", {
  fixture <- gaussian_surface_fixture()
  construct <- function(tolerance = 0.25, cv = 0.2, validation = 0.21,
                         coeffs = fixture$coeffs) {
    pp_calibration_surface_model("power", 0L, FALSE, FALSE, coeffs,
                                 tolerance, cv, validation)
  }
  model <- construct()
  expect_identical(model$type, "quadrature_axis_surface")
  expect_equal(model$voxel_axis_range_mm, c(1.8, 4))
  expect_equal(model$kernel_ratio_range, c(1.5, 3))
  expect_identical(model$max_volumes, 96L)
  expect_error(construct(tolerance = 0.51), "tolerance_voxels")
  expect_error(construct(cv = 0.26), "max_cv_error_voxels")
  expect_error(construct(validation = 0.26), "max_validation_error_voxels")
  expect_error(construct(coeffs = list(level = NA_real_, shape = 0)), "level")
  expect_error(construct(coeffs = list(level = 0, slope = 0)), "level and shape")
})

test_that("all production contexts attach to exact masks and constrain every axis", {
  contexts <- c("susan_none", "susan_fmriprep", "susan_template",
             "gaussian_mask", "gaussian_nomask",
             "gaussian_mask_fmriprep", "gaussian_nomask_fmriprep",
             "gaussian_mask_template", "gaussian_nomask_template")
  models <- setNames(lapply(contexts, function(context) gaussian_surface_fixture()), contexts)
  calibration <- pp_attach_continuous_calibration(pp_calibration_coeffs, models)
  expect_length(calibration$gaussian$classic$mask$continuous_models, 3L)
  expect_length(calibration$gaussian$classic$nomask$continuous_models, 3L)
  expect_identical(calibration$susan$classic$nomask,
                   pp_calibration_coeffs$susan$classic$nomask)
  expect_equal(calibration$susan$classic$mask$template$voxel_axis_range_mm, c(1.8, 4))
  expect_error(pp_attach_continuous_calibration(pp_calibration_coeffs, models[-1]),
               "length 9")
  local_mocked_bindings(pp_calibration_coeffs = calibration)
  outside <- pp_select_calibration("gaussian", TRUE, "none", c(4.1, 2, 2), 6)
  expect_identical(outside$type, "quadrature_ratio_linear")
  expect_identical(pp_calibration_support(outside, 6, c(4.1, 2, 2)), "EXTRAPOLATED")
  old_grid <- pp_select_calibration("gaussian", TRUE, "none", rep(2, 3), 8)
  expect_identical(old_grid$type, "quadrature_ratio_linear")
  expect_identical(pp_calibration_support(old_grid, 8, rep(2, 3)), "interpolated")
})

test_that("surface selection handles Gaussian input masks before legacy extrapolation", {
  calibration <- pp_calibration_coeffs
  calibration$gaussian$classic$mask$continuous_models <-
    list(none = gaussian_surface_fixture(), fmriprep = gaussian_surface_fixture(),
         template = gaussian_surface_fixture())
  local_mocked_bindings(pp_calibration_coeffs = calibration)
  for (mask in c("none", "fmriprep", "template")) {
    expect_silent(model <- pp_select_calibration("gaussian", TRUE, mask,
                                                rep(2.1, 3), 4.2))
    expect_identical(model$calibrated_input_mask, mask)
    expect_true(model$surface_model_selected)
    expect_identical(pp_calibration_support(model, 4.2, rep(2.1, 3)), "interpolated")
    expect_identical(pp_calibration_support(model, 4.2, c(1.7, 2.1, 2.1)),
                     "EXTRAPOLATED")
    expect_identical(pp_calibration_support(model, 7, rep(2.1, 3)), "EXTRAPOLATED")
  }
  expect_warning(custom <- pp_select_calibration("gaussian", TRUE, "custom",
                                                 rep(2.1, 3), 4.2), "extrapolation")
  expect_true(custom$input_mask_extrapolated)
  expect_false(isTRUE(custom$surface_model_selected))
  legacy <- pp_select_calibration("gaussian", TRUE, "none", rep(2, 3))
  expect_identical(legacy$type, "quadrature_ratio_linear")
})

test_that("axis validation rejects cancellation, wrong kernels, and unchanged data", {
  skip_if_not_installed("RNifti")
  calibration <- pp_calibration_coeffs
  calibration$gaussian$classic$mask$continuous_models <-
    list(fmriprep = gaussian_surface_fixture())
  spacing <- c(2, 2, 4)
  pre_axes <- c(3, 3.5, 4)
  expected <- sqrt(pre_axes^2 + 5^2)
  post_axes <- expected
  files <- vapply(seq_len(3), function(i) tempfile(fileext = ".nii"), character(1))
  on.exit(unlink(files), add = TRUE)
  image <- RNifti::asNifti(array(1, c(2, 2, 2, 100)))
  RNifti::pixdim(image) <- c(spacing, 1)
  RNifti::writeNifti(image, files[1])
  RNifti::writeNifti(image, files[2])
  RNifti::writeNifti(RNifti::asNifti(array(1, c(2, 2, 2)), reference = image), files[3])
  local_mocked_bindings(
    pp_calibration_coeffs = calibration,
    pp_estimate_classic_smoothness_file = function(path, ...) {
      axes <- if (identical(path, files[1])) pre_axes else post_axes
      list(geom = exp(mean(log(axes))), geom_axes = axes,
           volumes_used = 96L, total_volumes = 100L,
           volume_indices = pp_distributed_volume_indices(100L, 96L),
           volume_sampling = "distributed")
    }
  )
  validate <- function(...) validate_spatial_smooth(
    files[1], files[2], files[3], fwhm_mm = 5, smoother = "gaussian",
    input_mask = "fmriprep", validation_mode = "calibration", ...
  )
  result <- validate()
  expect_true(result)
  details <- attr(result, "details")
  expect_true(details$calibration_surface_model_selected)
  expect_equal(details$axis_abs_error_mm, rep(0, 3), tolerance = 1e-8)
  expect_equal(details$axis_tolerance_mm, c(0.5, 0.5, 1))
  expect_silent(assert_provenance_metadata(details))
  expect_error(validate(max_volumes = 95L), "requires max_volumes=96")
  expect_error(validate(unif = FALSE), "incompatible override")
  post_axes <- expected * c(0.6, 1 / 0.6, 1)
  cancelled <- validate()
  expect_false(cancelled)
  expect_equal(attr(cancelled, "details")$post_fwhm_mm,
               attr(cancelled, "details")$post_expected_mm)
  expect_false(attr(cancelled, "details")$within_tolerance)
  post_axes <- sqrt(pre_axes^2 + 8^2)
  expect_false(validate())
  post_axes <- pre_axes
  unchanged <- validate(tolerance_mm = 10)
  expect_false(unchanged)
  expect_false(attr(unchanged, "details")$smoothness_increased)
  post_axes <- expected + c(0.6, 0.6, 0.6)
  expect_false(validate())
  override <- validate(tolerance_mm = 0.7)
  expect_true(override)
  expect_equal(attr(override, "details")$axis_tolerance_mm, rep(0.7, 3))
  expect_equal(pp_predict_calibration(gaussian_surface_fixture(), 5,
                                      voxel_mm = spacing, pre_axis_fwhm = pre_axes),
               prod(expected)^(1 / 3) - prod(pre_axes)^(1 / 3))
  local_mocked_bindings(pp_estimate_classic_smoothness_file = function(path, ...) {
    list(geom = 4, geom_axes = c(NA_real_, 4, 4), volumes_used = 96L,
         total_volumes = 100L, volume_indices = seq_len(96),
         volume_sampling = "distributed")
  })
  invalid_axis <- validate()
  expect_false(invalid_axis)
  expect_match(attr(invalid_axis, "message"), "every spatial axis")
  expect_silent(assert_provenance_metadata(attr(invalid_axis, "details")))
})

test_that("spatial calibration rejects a changed grid before measuring smoothness", {
  skip_if_not_installed("RNifti")
  files <- vapply(seq_len(3), function(i) tempfile(fileext = ".nii"), character(1))
  on.exit(unlink(files), add = TRUE)
  image <- RNifti::asNifti(array(1, c(2, 2, 2, 100)))
  RNifti::pixdim(image) <- c(2.1, 2.1, 2.1, 1)
  RNifti::writeNifti(image, files[1])
  RNifti::writeNifti(RNifti::asNifti(array(1, c(2, 2, 2)), reference = image), files[3])
  RNifti::pixdim(image) <- c(2.2, 2.1, 2.1, 1)
  RNifti::writeNifti(image, files[2])
  local_mocked_bindings(pp_estimate_classic_smoothness_file = function(...) {
    stop("Changed geometry must be rejected before BOLD voxel reads.")
  })
  result <- validate_spatial_smooth(files[1], files[2], files[3], fwhm_mm = 4.2)
  expect_false(result)
  expect_match(attr(result, "message"), "spatial NIfTI grid mismatch")
  expect_silent(assert_provenance_metadata(attr(result, "details")))
})

test_that("auto mode replays supported FWHM failures without widening their limits", {
  calibration <- pp_calibration_coeffs
  calibration$gaussian$classic$mask$continuous_models <- list(none = gaussian_surface_fixture())
  files <- vapply(seq_len(3), function(i) tempfile(fileext = ".nii"), character(1))
  on.exit(unlink(files), add = TRUE)
  image <- RNifti::asNifti(array(1, c(3, 3, 3, 100)))
  RNifti::pixdim(image) <- c(2.1, 2.1, 2.1, 1)
  RNifti::writeNifti(image, files[1])
  RNifti::writeNifti(image, files[2])
  RNifti::writeNifti(RNifti::asNifti(array(1, c(3, 3, 3)), reference = image), files[3])
  calls <- 0L
  oracle_passes <- TRUE
  local_mocked_bindings(
    pp_calibration_coeffs = calibration,
    pp_estimate_classic_smoothness_file = function(path, ...) {
      axes <- if (identical(path, files[1])) rep(3, 3) else c(10, 5, 5)
      list(geom = exp(mean(log(axes))), geom_axes = axes, volumes_used = 96L,
           total_volumes = 100L, volume_indices = seq_len(96), volume_sampling = "distributed")
    },
    pp_replay_spatial_smooth = function(...) {
      calls <<- calls + 1L
      result <- oracle_passes
      attr(result, "message") <- if (oracle_passes) "operator PASS" else "operator FAIL"
      attr(result, "details") <- list(validation_method = "operator_replay", max_relative_error = 0)
      result
    }
  )
  actual <- validate_spatial_smooth(files[1], files[2], files[3], 4.2, smoother = "gaussian")
  expect_true(actual)
  expect_equal(calls, 1L)
  details <- attr(actual, "details")
  expect_identical(details$validation_method, "operator_replay")
  expect_false(details$fwhm_calibration_passed)
  expect_false(details$within_tolerance)
  expect_equal(details$tolerance_mm, 0.25 * 2.1, tolerance = 1e-6)
  expect_identical(details$operator_replay_trigger, "FWHM_error_or_effect_check_failed")
  expect_silent(assert_provenance_metadata(details))
  strict <- validate_spatial_smooth(files[1], files[2], files[3], 4.2,
                                    smoother = "gaussian", validation_mode = "calibration")
  expect_false(strict)
  expect_equal(calls, 1L)
  oracle_passes <- FALSE
  expect_false(validate_spatial_smooth(files[1], files[2], files[3], 4.2, smoother = "gaussian"))
  expect_equal(calls, 2L)
  outside <- validate_spatial_smooth(files[1], files[2], files[3], 8, smoother = "gaussian")
  expect_false(outside)
  expect_true(attr(outside, "details")$calibration_extrapolated)
  expect_equal(calls, 2L)
  oracle_passes <- TRUE
  local_mocked_bindings(pp_calibration_surface_prediction = function(...) stop("surface prediction unavailable"))
  fallback <- validate_spatial_smooth(files[1], files[2], files[3], 4.2, smoother = "gaussian")
  expect_true(fallback)
  expect_identical(attr(fallback, "details")$operator_replay_trigger, "FWHM_prediction_failed")
  expect_match(attr(fallback, "details")$prediction_failure, "surface prediction unavailable")
  expect_silent(assert_provenance_metadata(attr(fallback, "details")))
  expect_error(validate_spatial_smooth(files[1], files[2], files[3], 4.2,
                                      smoother = "gaussian", validation_mode = "calibration"),
               "surface prediction unavailable")
})

test_that("published continuous checks distinguish accepted calibration from replay", {
  evidence <- read.csv(system.file("extdata", "spatial_smooth_calibration_continuous.csv",
                                   package = "BrainGnomes"), stringsAsFactors = FALSE)
  expect_equal(nrow(evidence), 9L)
  expect_true(all(evidence$tolerance_voxels <= 0.5))
  expect_true(all(evidence$n_calibration == 360L))
  expect_true(all(evidence$n_validation == 1120L))
  expect_true(all(evidence$n_positive_replays == 2L))
  expect_true(all(evidence$n_negative_controls == 4L))
  expect_true(all(evidence$n_replay_control_failures == 0L))
  expect_true(all(evidence$max_replay_relative_error <= 1e-5))
  for (i in seq_len(nrow(evidence))) {
    row <- evidence[i, ]
    accepted <- row$coefficient_validation_passed
    if (accepted) {
      expect_equal(row$n_fwhm_reference_failures, 0L)
      expect_lte(row$max_cv_error_voxels, row$tolerance_voxels)
      expect_lte(row$max_validation_error_voxels, row$tolerance_voxels)
      expect_identical(row$validation_method, "calibration")
    } else {
      expect_identical(row$validation_method, "operator_replay")
      expect_true(!row$cv_accepted || row$n_fwhm_reference_failures > 0L)
    }
    smoother <- if (startsWith(row$context, "susan")) "susan" else "gaussian"
    used_mask <- !startsWith(row$context, "gaussian_nomask")
    input_mask <- if (endsWith(row$context, "_fmriprep")) "fmriprep" else {
      if (endsWith(row$context, "_template")) "template" else "none"
    }
    for (spacing in list(rep(1.8, 3), rep(2.1, 3), rep(2.8, 3), rep(4, 3),
                         c(2, 2, 4), c(4, 1.8, 1.8), c(1.8, 4, 4))) {
      reference <- prod(spacing)^(1 / 3)
      model <- pp_select_calibration(smoother, used_mask, input_mask,
                                     spacing, 2.25 * reference)
      expect_identical(model$type, if (accepted) "quadrature_axis_surface" else "operator_replay")
      expect_identical(model$calibrated_input_mask, input_mask)
      expect_identical(pp_calibration_support(model, 2.25 * reference, spacing), "interpolated")
      expect_silent(assert_provenance_metadata(model))
      if (accepted) {
        expect_equal(model$tolerance_axis_mm, row$tolerance_voxels * spacing)
        expect_equal(model$tolerance_mm, row$tolerance_voxels * reference)
        expect_equal(model$coeffs$level,
                     as.numeric(strsplit(row$level_coefficients, ";", fixed = TRUE)[[1]]))
        expect_equal(model$coeffs$shape,
                     as.numeric(strsplit(row$shape_coefficients, ";", fixed = TRUE)[[1]]))
        expect_identical(model$surface_family, row$surface_family)
        expect_identical(model$coupled_baseline_feature, row$coupled_baseline_feature)
        expect_identical(model$quadratic_baseline_feature, row$quadratic_baseline_feature)
        expect_equal(model$max_validation_error_voxels, row$max_validation_error_voxels)
        lower <- pp_calibration_surface_prediction(model, 1.5 * reference,
                                                   1.5 * spacing, spacing)
        upper <- pp_calibration_surface_prediction(model, 3 * reference,
                                                   1.5 * spacing, spacing)
        expect_true(all(upper$expected_axes_mm > lower$expected_axes_mm))
      } else {
        expect_null(model$coeffs)
        expect_equal(model$max_replay_relative_error, row$max_replay_relative_error)
        expect_equal(model$n_positive_replays, 2L)
        expect_equal(model$n_negative_controls, 4L)
      }
    }
  }
  for (smoother in c("susan", "gaussian")) {
    legacy <- pp_select_calibration(smoother, TRUE, "none", rep(2, 3), 8)
    expect_identical(legacy$type, "quadrature_ratio_linear")
    expect_identical(pp_calibration_support(legacy, 8, rep(2, 3)), "interpolated")
  }
})
