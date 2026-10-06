#' Continuous spatial checks with validated responses or operator replay
#'
#' The coefficient references use six calibration subjects and sixteen
#' independent subjects. Only responses meeting the half-voxel CV and held-out
#' limits are accepted; other conditions use full-parameter operator replay.
#' All nine operators passed two independent full-run positive cases and four
#' wrong-kernel/unchanged controls at the requested domain boundaries.
#' See aggregate FWHM reference errors, replay evidence and the protocol in
#' `inst/extdata`. Rejected reference coefficients are never used for acceptance.
#' @return Named catalog of nine production spatial-validation contexts.
#' @keywords internal
#' @noRd
pp_continuous_calibration_models <- function() {
  list(
    susan_none = pp_operator_replay_model(
      max_replay_relative_error = 0,
      n_positive_replays = 2L, n_negative_controls = 4L,
      replay_reason = "FWHM_independent_validation_failed"
    ),
    gaussian_mask = pp_operator_replay_model(
      max_replay_relative_error = 0,
      n_positive_replays = 2L, n_negative_controls = 4L,
      replay_reason = "FWHM_independent_validation_failed"
    ),
    gaussian_nomask = pp_operator_replay_model(
      max_replay_relative_error = 0,
      n_positive_replays = 2L, n_negative_controls = 4L,
      replay_reason = "FWHM_cross_validation_failed"
    ),
    susan_fmriprep = pp_operator_replay_model(
      max_replay_relative_error = 0,
      n_positive_replays = 2L, n_negative_controls = 4L,
      replay_reason = "FWHM_cross_validation_failed"
    ),
    gaussian_mask_fmriprep = pp_operator_replay_model(
      max_replay_relative_error = 0,
      n_positive_replays = 2L, n_negative_controls = 4L,
      replay_reason = "FWHM_independent_validation_failed"
    ),
    gaussian_nomask_fmriprep = pp_operator_replay_model(
      max_replay_relative_error = 0,
      n_positive_replays = 2L, n_negative_controls = 4L,
      replay_reason = "FWHM_cross_validation_failed"
    ),
    susan_template = pp_calibration_surface_model(
      "affine", 0L, FALSE, TRUE,
      coeffs = list(
        level = c(-0.39334841371416002, -0.96634946392733456, -1.9118720366941764),
        shape = c(0.033048148811180705, 0.54184717203926969, 0.58530118613014126)
      ),
      tolerance_voxels = 0.45000000000000001,
      max_cv_error_voxels = 0.41051226196044699,
      max_validation_error_voxels = 0.40508722415701798,
      coupled_baseline_feature = FALSE,
      quadratic_baseline_feature = FALSE
    ),
    gaussian_mask_template = pp_operator_replay_model(
      max_replay_relative_error = 0,
      n_positive_replays = 2L, n_negative_controls = 4L,
      replay_reason = "FWHM_independent_validation_failed"
    ),
    gaussian_nomask_template = pp_operator_replay_model(
      max_replay_relative_error = 0,
      n_positive_replays = 2L, n_negative_controls = 4L,
      replay_reason = "FWHM_cross_validation_failed"
    )
  )
}
