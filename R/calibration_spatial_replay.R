#' Replay a spatial smoother on distributed volumes with full-run parameters
#'
#' @param pre_file Input 4D NIfTI path.
#' @param post_file Observed 4D NIfTI path, with the same spatial and temporal grid.
#' @param mask_file Mask used for SUSAN brightness or masked Gaussian smoothing.
#' @param kernel_fwhm Requested scalar kernel FWHM in millimeters.
#' @param smoother `susan` or `gaussian`.
#' @param used_mask Whether the operation used the supplied mask.
#' @param fsl_img Optional FSL container image for SUSAN execution.
#' @param max_volumes Distributed sample cap; defaults to 96 volumes.
#' @param tolerance Hybrid numerical comparison tolerance, independent of FWHM.
#' @return Logical result with compact comparison and operation metadata only.
#'   All working voxel data stay in temporary NIfTI files and numerical buffers.
#' @noRd
pp_replay_spatial_smooth <- function(pre_file, post_file, mask_file, kernel_fwhm,
                                     smoother, used_mask = TRUE, fsl_img = NULL,
                                     max_volumes = 96L, tolerance = 1e-5) {
  checkmate::assert_choice(smoother, c("susan", "gaussian"))
  checkmate::assert_flag(used_mask)
  checkmate::assert_number(kernel_fwhm, lower = 1e-6, finite = TRUE)
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
  grid <- pp_compare_nifti_grid(pre_file, post_file, "pre", "post")
  dimensions <- pp_nifti_dims4(pre_file)
  post_dimensions <- pp_nifti_dims4(post_file)
  if (!grid$passed || length(dimensions) != 4L ||
      !identical(as.integer(dimensions), as.integer(post_dimensions))) {
    out <- FALSE
    attr(out, "message") <- "Spatial smoothing replay requires matching spatial and temporal grids."
    attr(out, "details") <- list(validation_method = "operator_replay", spatial_grid = grid)
    return(out)
  }
  volumes <- pp_distributed_volume_indices(dimensions[4L], max_volumes)
  scratch <- tempfile("spatial-replay-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  pre_staged <- pp_stage_nifti_for_chunks(pre_file, scratch)
  post_staged <- pp_stage_nifti_for_chunks(post_file, scratch)
  sampled <- file.path(scratch, "sampled.nii")
  expected_file <- file.path(scratch, "expected.nii.gz")
  image <- RNifti::readNifti(pre_staged, volumes = volumes)
  # Temporal subsetting makes inherited AFNI extensions stale. Reconstruct
  # from the small header, retaining spatial geometry without those extensions.
  clean_image <- RNifti::asNifti(array(as.numeric(image), dim(image)),
                                 reference = RNifti::niftiHeader(image))
  RNifti::writeNifti(clean_image, sampled, datatype = "float", version = 1L)
  rm(image, clean_image)
  invisible(gc(FALSE))
  operation <- pp_run_spatial_replay_operator(
    pre_staged, sampled, expected_file, mask_file, kernel_fwhm,
    smoother, used_mask, fsl_img
  )
  backend <- operation$backend
  brightness <- operation$susan_brightness_threshold
  expected_grid <- pp_compare_nifti_grid(sampled, expected_file, "sampled", "expected")
  if (!expected_grid$passed) stop(expected_grid$message, call. = FALSE)
  expected_staged <- pp_stage_nifti_for_chunks(expected_file, scratch)
  aggregate <- list(max_absolute_error = 0, max_relative_error = 0,
                    n_mismatched = 0, n_nonfinite_observed = 0,
                    finite_pattern_mismatches = 0)
  changed <- FALSE
  for (start in seq.int(1L, length(volumes), by = 8L)) {
    indices <- seq.int(start, min(start + 7L, length(volumes)))
    expected <- pp_read_volume_matrix(expected_staged, indices, dimensions[1:3])
    observed <- pp_read_volume_matrix(post_staged, volumes[indices], dimensions[1:3])
    original <- pp_read_volume_matrix(pre_staged, volumes[indices], dimensions[1:3])
    comparison <- pp_compare_numeric(observed, expected, tolerance)
    changed <- changed || !pp_compare_numeric(expected, original, tolerance)$passed
    for (name in c("max_absolute_error", "max_relative_error")) {
      aggregate[[name]] <- max(aggregate[[name]], comparison[[name]])
    }
    for (name in c("n_mismatched", "n_nonfinite_observed", "finite_pattern_mismatches")) {
      aggregate[[name]] <- aggregate[[name]] + comparison[[name]]
    }
    rm(expected, observed, original)
  }
  passed <- aggregate$n_mismatched == 0 && aggregate$n_nonfinite_observed == 0 && changed
  details <- c(list(validation_method = "operator_replay", backend = backend,
                    requested_kernel_mm = kernel_fwhm, used_mask = used_mask,
                    tolerance = tolerance, volumes_compared = length(volumes),
                    total_volumes = dimensions[4L], volume_indices = as.integer(volumes),
                    volume_sampling = "distributed_full_run", operation_changed_data = changed,
                    susan_brightness_threshold = brightness, spatial_grid = grid), aggregate)
  assert_provenance_metadata(details)
  out <- passed
  attr(out, "details") <- details
  attr(out, "message") <- sprintf(
    "Spatial smoothing replay (%s): %d mismatched values, maximum relative error %.6g (tol %.6g), changed data %s across %d volumes: %s.",
    backend, aggregate$n_mismatched, aggregate$max_relative_error, tolerance,
    changed, length(volumes), if (passed) "PASS" else "FAIL"
  )
  out
}

#' Execute the production spatial operator using a full-run reference
#'
#' @param pre_file Full input NIfTI used for SUSAN brightness, mean and extents.
#' @param sampled_file Temporal subset of that input to smooth.
#' @param out_file Working output NIfTI path.
#' @param mask_file Threshold or Gaussian smoothing mask.
#' @param kernel_fwhm Requested scalar kernel in millimeters.
#' @param smoother `susan` or `gaussian`.
#' @param used_mask Whether to use the supplied mask.
#' @param fsl_img Optional FSL container image.
#' @return Backend identifier and scalar brightness threshold; image data are
#'   written only to working NIfTI files, never returned as metadata.
#' @noRd
pp_run_spatial_replay_operator <- function(pre_file, sampled_file, out_file,
                                           mask_file, kernel_fwhm, smoother,
                                           used_mask, fsl_img = NULL) {
  brightness <- NULL
  backend <- if (smoother == "susan") "fsl_susan" else {
    if (used_mask) "afni_3dBlurInMask" else "afni_3dmerge"
  }
  # Execute FSL with the same container and path binds as the production step.
  # Inputs are paths and scalars; return no image objects or command payloads.
  run_fsl <- function(arguments) {
    run_fsl_command(paste(arguments, collapse = " "), fsl_img = fsl_img,
                    bind_paths = unique(dirname(c(pre_file, mask_file, sampled_file,
                                                   out_file))), echo = FALSE)
    invisible(NULL)
  }
  if (smoother == "susan") {
    mean_file <- file.path(dirname(out_file), "full_mean.nii.gz")
    extents <- file.path(dirname(out_file), "full_extents.nii.gz")
    quantiles <- image_quantile(pre_file, if (used_mask) mask_file else NULL,
                                quantiles = c(0.02, 0.5))
    brightness <- 0.75 * diff(quantiles)
    checkmate::assert_number(brightness, lower = 1e-6, finite = TRUE)
    run_fsl(c("fslmaths", shQuote(pre_file), "-Tmean", shQuote(mean_file)))
    run_fsl(c("fslmaths", shQuote(pre_file), "-Tmin -bin", shQuote(extents), "-odt char"))
    run_fsl(c("susan", shQuote(sampled_file), format(brightness, digits = 17),
              format(kernel_fwhm / sqrt(8 * log(2)), digits = 17), "3 1 1",
              shQuote(mean_file), format(brightness, digits = 17), shQuote(out_file)))
    run_fsl(c("fslmaths", shQuote(out_file), "-mul", shQuote(extents),
              shQuote(out_file), "-odt float"))
  } else {
    program <- if (used_mask) "3dBlurInMask" else "3dmerge"
    if (!nzchar(Sys.which(program))) stop("Spatial smoothing replay requires ", program, ".", call. = FALSE)
    arguments <- if (used_mask) {
      c("-overwrite", "-input", sampled_file, "-mask", mask_file,
        "-FWHM", format(kernel_fwhm, digits = 17), "-prefix", out_file)
    } else {
      c("-overwrite", "-doall", "-1blur_fwhm", format(kernel_fwhm, digits = 17),
        "-prefix", out_file, sampled_file)
    }
    command_output <- system2(program, vapply(arguments, shQuote, character(1)),
                              stdout = TRUE, stderr = TRUE)
    status <- attr(command_output, "status")
    if ((!is.null(status) && status != 0L) || any(grepl("^\\*\\* ERROR:", command_output))) {
      stop(program, " failed during spatial smoothing replay.", call. = FALSE)
    }
  }
  list(backend = backend, susan_brightness_threshold = brightness)
}

#' Construct a continuous-domain operator check when FWHM prediction is inadequate
#'
#' @param max_replay_relative_error Largest error in independent full-run replays.
#' @param n_positive_replays Number of successful full-run operator comparisons.
#' @param n_negative_controls Number of correctly rejected wrong-kernel/unchanged controls.
#' @param replay_reason Why an empirical FWHM response is not used for acceptance.
#' @return Compact operator-check metadata. No failed FWHM coefficients are
#'   represented as accepted calibration parameters.
#' @noRd
pp_operator_replay_model <- function(max_replay_relative_error, n_positive_replays,
                                      n_negative_controls, replay_reason) {
  checkmate::assert_number(max_replay_relative_error, lower = 0, upper = 1e-5,
                           finite = TRUE)
  checkmate::assert_int(n_positive_replays, lower = 2L)
  checkmate::assert_int(n_negative_controls, lower = 4L)
  checkmate::assert_choice(replay_reason, c("FWHM_cross_validation_failed",
                                           "FWHM_independent_validation_failed"))
  list(type = "operator_replay",
       model_version = "spatial-validation-v6-operator-replay-distributed96-r1p5-3",
       support_version = "v6-full-run-replays-2026-10-05",
       voxel_axis_range_mm = c(1.8, 4), voxel_range_mm = c(1.8, 4),
       kernel_ratio_range = c(1.5, 3), kernel_range_mm = c(2.7, 12),
       max_volumes = 96L, volume_sampling = "distributed_full_run",
       smoothing_context = "full_run", replay_tolerance = 1e-5,
       max_replay_relative_error = max_replay_relative_error,
       n_positive_replays = n_positive_replays,
       n_negative_controls = n_negative_controls, replay_reason = replay_reason)
}
