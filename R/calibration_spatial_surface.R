#' Construct an independently validated continuous spatial calibration
#'
#' @param surface_family Positive effective-kernel family, `power` or `affine`.
#' @param resolution_degree Resolution polynomial degree, zero through two.
#' @param baseline_feature Include normalized baseline FWHM in the features.
#' @param anisotropy_feature Include directional and symmetric anisotropy.
#' @param coeffs Named list of level and shape coefficient vectors.
#' @param tolerance_voxels Cross-validation limit in voxel units, at most 0.5.
#' @param max_cv_error_voxels Largest internal cross-validation normalized error.
#' @param max_validation_error_voxels Largest independent normalized error.
#' @param coupled_baseline_feature Include permutation-symmetric summaries of
#'   baseline smoothness across all three axes, rather than only the predicted axis.
#' @param quadratic_baseline_feature Include baseline curvature and its
#'   interaction with resolution; requires directional baseline features.
#' @return Compact surface metadata with validated parameters and support bounds.
#'   Axis predictions use the base model's exact estimator preparation.
#' @noRd
pp_calibration_surface_model <- function(surface_family, resolution_degree,
                                         baseline_feature, anisotropy_feature,
                                         coeffs, tolerance_voxels,
                                         max_cv_error_voxels,
                                         max_validation_error_voxels,
                                         coupled_baseline_feature = FALSE,
                                         quadratic_baseline_feature = FALSE) {
  features <- pp_calibration_surface_features(
    rep(2.5, 3L), rep(3, 3L), resolution_degree,
    baseline_feature, anisotropy_feature, coupled_baseline_feature,
    quadratic_baseline_feature
  )
  checkmate::assert_choice(surface_family, c("power", "affine"))
  checkmate::assert_list(coeffs, len = 2L, names = "unique")
  if (!setequal(names(coeffs), c("level", "shape"))) {
    stop("Surface coefficients must contain level and shape vectors.", call. = FALSE)
  }
  checkmate::assert_numeric(coeffs$level, len = ncol(features), finite = TRUE,
                            any.missing = FALSE)
  checkmate::assert_numeric(coeffs$shape, len = ncol(features), finite = TRUE,
                            any.missing = FALSE)
  checkmate::assert_number(tolerance_voxels, lower = 0, upper = 0.5, finite = TRUE)
  checkmate::assert_number(max_cv_error_voxels, lower = 0,
                           upper = tolerance_voxels, finite = TRUE)
  checkmate::assert_number(max_validation_error_voxels, lower = 0,
                           upper = tolerance_voxels, finite = TRUE)
  list(
    model_version = "smoothness-calibration-v6-continuous-axis-distributed96-r1p5-3",
    support_version = "v6-continuous-four-cohort-2026-10-05",
    type = "quadrature_axis_surface", surface_family = surface_family,
    resolution_degree = resolution_degree, baseline_feature = baseline_feature,
    anisotropy_feature = anisotropy_feature, coeffs = coeffs,
    coupled_baseline_feature = coupled_baseline_feature,
    quadratic_baseline_feature = quadratic_baseline_feature,
    tolerance_voxels = tolerance_voxels,
    voxel_axis_range_mm = c(1.8, 4), voxel_range_mm = c(1.8, 4),
    kernel_ratio_range = c(1.5, 3), kernel_range_mm = c(2.7, 12),
    max_volumes = 96L, volume_sampling = "distributed_full_run",
    smoothing_context = "full_run",
    source = "continuous_axis_four_cohort_2026-10-05",
    n_calibration = 360L, n_validation = 1120L,
    n_calibration_subjects = 6L, n_validation_subjects = 16L,
    max_cv_error_voxels = max_cv_error_voxels,
    max_validation_error_voxels = max_validation_error_voxels
  )
}

#' Build smooth, permutation-symmetric spatial-calibration features
#'
#' @param voxel_mm Three directional voxel spacings in millimeters.
#' @param pre_axis_fwhm Three directional baseline FWHM estimates in millimeters.
#' @param degree Resolution polynomial degree, from zero through two.
#' @param baseline Include normalized baseline smoothness as a predictor.
#' @param anisotropy Include axis-relative spacing and symmetric anisotropy.
#' @param coupled_baseline Include geometric baseline and baseline anisotropy;
#'   requires `baseline = TRUE` and retains symmetry under axis permutations.
#' @param quadratic_baseline Include squared log baseline and its interaction
#'   with log resolution when `degree >= 1`; requires `baseline = TRUE`.
#' @return A three-row numerical design matrix, used internally and never
#'   included in provenance records.
#' @noRd
pp_calibration_surface_features <- function(voxel_mm, pre_axis_fwhm,
                                            degree = 0L, baseline = FALSE,
                                            anisotropy = FALSE,
                                            coupled_baseline = FALSE,
                                            quadratic_baseline = FALSE) {
  checkmate::assert_numeric(voxel_mm, len = 3L, lower = 1e-6, finite = TRUE,
                            any.missing = FALSE)
  checkmate::assert_numeric(pre_axis_fwhm, len = 3L, lower = 1e-6, finite = TRUE,
                            any.missing = FALSE)
  checkmate::assert_int(degree, lower = 0L, upper = 2L)
  checkmate::assert_flag(baseline)
  checkmate::assert_flag(anisotropy)
  checkmate::assert_flag(coupled_baseline)
  checkmate::assert_flag(quadratic_baseline)
  if (coupled_baseline && !baseline) {
    stop("Coupled baseline features require directional baseline features.", call. = FALSE)
  }
  if (quadratic_baseline && !baseline) {
    stop("Quadratic baseline features require directional baseline features.", call. = FALSE)
  }
  features <- matrix(1, nrow = 3L, ncol = 1L,
                     dimnames = list(NULL, "intercept"))
  resolution_log <- log(voxel_mm / 2.5)
  if (degree >= 1L) {
    features <- cbind(features, resolution_log = resolution_log)
  }
  if (degree >= 2L) {
    features <- cbind(features, resolution_log_sq = resolution_log^2)
  }
  if (baseline) {
    features <- cbind(features, baseline_log = log(pre_axis_fwhm / voxel_mm))
  }
  if (anisotropy) {
    relative_spacing <- log(voxel_mm) - mean(log(voxel_mm))
    features <- cbind(features, axis_anisotropy_log = relative_spacing,
                      anisotropy_variance = rep(mean(relative_spacing^2), 3L))
  }
  if (coupled_baseline) {
    # A three-dimensional filter changes each directional gradient through
    # smoothing along the other axes too. These global summaries allow that
    # coupling without introducing orientation-dependent coefficients.
    normalized_baseline <- log(pre_axis_fwhm / voxel_mm)
    baseline_center <- mean(normalized_baseline)
    features <- cbind(features,
                      baseline_geom_log = rep(baseline_center, 3L),
                      baseline_anisotropy_variance = rep(
                        mean((normalized_baseline - baseline_center)^2), 3L
                      ))
  }
  if (quadratic_baseline) {
    baseline_log <- log(pre_axis_fwhm / voxel_mm)
    features <- cbind(features, baseline_log_sq = baseline_log^2)
    if (degree >= 1L) {
      # Baseline smoothness and spatial sampling jointly affect the bias of
      # a first-difference estimator; a full quadratic captures that interaction.
      features <- cbind(features, resolution_baseline_log = resolution_log * baseline_log)
    }
  }
  features
}

#' Predict directional smoothness from a positive continuous calibration surface
#'
#' @param model A `quadrature_axis_surface` calibration model.
#' @param kernel_fwhm Requested scalar kernel FWHM in millimeters.
#' @param pre_axis_fwhm Three directional baseline FWHM estimates.
#' @param voxel_mm Three directional voxel spacings in millimeters.
#' @return Compact directional and geometric predictions, effective kernels,
#'   and gains. Only scalar summaries and three-element vectors are returned.
#' @noRd
pp_calibration_surface_prediction <- function(model, kernel_fwhm,
                                              pre_axis_fwhm, voxel_mm) {
  checkmate::assert_number(kernel_fwhm, lower = 1e-6, finite = TRUE)
  checkmate::assert_choice(model$surface_family, c("power", "affine"))
  features <- pp_calibration_surface_features(
    voxel_mm, pre_axis_fwhm, model$resolution_degree,
    model$baseline_feature, model$anisotropy_feature,
    isTRUE(model$coupled_baseline_feature),
    isTRUE(model$quadratic_baseline_feature)
  )
  checkmate::assert_numeric(model$coeffs$level, len = ncol(features), finite = TRUE,
                            any.missing = FALSE)
  checkmate::assert_numeric(model$coeffs$shape, len = ncol(features), finite = TRUE,
                            any.missing = FALSE)
  level <- as.numeric(features %*% model$coeffs$level)
  shape <- as.numeric(features %*% model$coeffs$shape)
  axis_ratio <- kernel_fwhm / voxel_mm
  if (identical(model$surface_family, "power")) {
    # Bounded, positive powers preserve monotonicity and keep the optimizer's
    # candidate predictions finite over the declared physical domain.
    power <- 0.3 + 2.7 * stats::plogis(shape)
    effective <- exp(log(voxel_mm) + level + power * log(axis_ratio))
  } else {
    # The minimum possible axis ratio in the calibrated cube exceeds 0.75.
    # Positive level and slope therefore imply positive, increasing kernels.
    effective <- voxel_mm * (exp(level) + exp(shape) * (axis_ratio - 0.75))
  }
  if (any(!is.finite(effective) | effective <= 0)) {
    stop("Calibration surface predicts a nonpositive or nonfinite kernel.",
         call. = FALSE)
  }
  # Stable quadrature avoids squaring large, otherwise finite FWHM values.
  larger <- pmax(pre_axis_fwhm, effective)
  smaller <- pmin(pre_axis_fwhm, effective)
  expected_axes <- larger * sqrt(1 + (smaller / larger)^2)
  if (any(!is.finite(expected_axes))) {
    stop("Calibration surface predicts nonfinite directional FWHM.", call. = FALSE)
  }
  expected_geom <- exp(mean(log(expected_axes)))
  pre_geom <- exp(mean(log(pre_axis_fwhm)))
  equivalent_kernel <- expected_geom * sqrt(max(0, 1 - (pre_geom / expected_geom)^2))
  list(
    expected_axes_mm = as.numeric(expected_axes),
    expected_geom_mm = expected_geom,
    effective_kernel_axes_mm = as.numeric(effective),
    axis_gain = as.numeric(effective / kernel_fwhm),
    equivalent_geometric_gain = equivalent_kernel / kernel_fwhm
  )
}

#' Check the voxel cube and relative-kernel domain of a continuous model
#'
#' @param model Surface metadata with directional spacing and kernel-ratio bounds.
#' @param voxel_mm Three directional spacings, or `NULL` for legacy selection.
#' @param kernel_fwhm Requested kernel, or `NULL` for legacy selection.
#' @return A logical scalar; every directional spacing must satisfy the bounds.
#' @noRd
pp_calibration_surface_domain <- function(model, voxel_mm, kernel_fwhm) {
  if (!checkmate::test_numeric(voxel_mm, len = 3L, lower = 1e-6, finite = TRUE,
                               any.missing = FALSE) ||
      !checkmate::test_number(kernel_fwhm, lower = 1e-6, finite = TRUE)) {
    return(FALSE)
  }
  spacing_bounds <- model$voxel_axis_range_mm
  ratio_bounds <- model$kernel_ratio_range
  if (!checkmate::test_numeric(spacing_bounds, len = 2L, finite = TRUE,
                                lower = 1e-6, any.missing = FALSE) ||
      !checkmate::test_numeric(ratio_bounds, len = 2L, finite = TRUE,
                                lower = 1e-6, any.missing = FALSE) ||
      spacing_bounds[1L] > spacing_bounds[2L] ||
      ratio_bounds[1L] > ratio_bounds[2L]) return(FALSE)
  ratio <- kernel_fwhm / exp(mean(log(voxel_mm)))
  all(voxel_mm >= spacing_bounds[1L] - 1e-6 &
      voxel_mm <= spacing_bounds[2L] + 1e-6) &&
    ratio >= ratio_bounds[1L] - 1e-6 && ratio <= ratio_bounds[2L] + 1e-6
}

#' Resolve a mask-specific continuous spatial check when its domain matches
#'
#' @param model Legacy mask-specific base model containing `continuous_models`.
#' @param input_mask Exact mask applied to the BOLD before smoothing.
#' @param voxel_mm Three directional voxel spacings.
#' @param kernel_fwhm Requested scalar kernel FWHM.
#' @return The selected response or operator check, or `NULL`. Responses inherit
#'   the base model's estimator preparation.
#'   Legacy external support cannot extend a continuous model's measured domain.
#' @noRd
pp_select_calibration_surface <- function(model, input_mask, voxel_mm,
                                          kernel_fwhm) {
  surface <- model$continuous_models[[input_mask]]
  if (is.null(surface) ||
      !pp_calibration_surface_domain(surface, voxel_mm, kernel_fwhm)) return(NULL)
  selected <- utils::modifyList(model, surface)
  selected$continuous_models <- NULL
  selected$grid_models <- NULL
  selected$external_support <- NULL
  selected$voxel_spacing_mm <- NULL
  selected$base_model_version <- model$model_version
  selected$grid_model_selected <- TRUE
  selected$surface_model_selected <- identical(surface$type, "quadrature_axis_surface")
  selected$operator_replay_selected <- identical(surface$type, "operator_replay")
  selected$input_mask <- input_mask
  selected$calibrated_input_mask <- input_mask
  selected$input_mask_extrapolated <- FALSE
  if (identical(surface$type, "quadrature_axis_surface")) {
    selected$tolerance_mm <- surface$tolerance_voxels * exp(mean(log(voxel_mm)))
    selected$tolerance_axis_mm <- surface$tolerance_voxels * voxel_mm
  } else {
    selected$coeffs <- NULL
    selected$tolerance_mm <- NULL
    selected$tolerance_axis_mm <- NULL
  }
  selected
}

#' Attach validated continuous checks to their exact production contexts
#'
#' @param calibration Legacy spatial calibration tree.
#' @param models Named catalog of nine validated responses or operator checks.
#' @return Calibration tree with continuous checks added under the existing
#'   smoother/threshold-mask/input-mask branches; legacy fits remain available.
#' @noRd
pp_attach_continuous_calibration <- function(calibration, models) {
  expected <- c("susan_none", "susan_fmriprep", "susan_template",
                "gaussian_mask", "gaussian_nomask",
                "gaussian_mask_fmriprep", "gaussian_nomask_fmriprep",
                "gaussian_mask_template", "gaussian_nomask_template")
  checkmate::assert_list(models, len = length(expected), names = "unique")
  if (!setequal(names(models), expected) ||
      !all(vapply(models, function(model) {
        isTRUE(model$type %in% c("quadrature_axis_surface", "operator_replay"))
      }, logical(1)))) {
    stop("Continuous calibration requires all nine exact production contexts.",
         call. = FALSE)
  }
  for (input_mask in c("none", "fmriprep", "template")) {
    susan <- models[[paste0("susan_", input_mask)]]
    calibration$susan$classic$mask[[input_mask]]$continuous_models <-
      stats::setNames(list(susan), input_mask)
    calibration$susan$classic$mask[[input_mask]]$voxel_axis_range_mm <-
      susan$voxel_axis_range_mm
    for (method in c("mask", "nomask")) {
      context <- paste0("gaussian_", method,
                         if (input_mask == "none") "" else paste0("_", input_mask))
      calibration$gaussian$classic[[method]]$continuous_models[[input_mask]] <-
        models[[context]]
      calibration$gaussian$classic[[method]]$voxel_axis_range_mm <-
        models[[context]]$voxel_axis_range_mm
    }
  }
  calibration
}
