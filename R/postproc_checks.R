### Postprocessing validation functions

#' Read NIfTI dimensions without loading image data
#' @keywords internal
#' @noRd
pp_nifti_dims4 <- function(path) {
  checkmate::assert_file_exists(path)
  header <- RNifti::niftiHeader(path)
  dims <- as.integer(header$dim[2:5])
  dims[!is.finite(dims) | dims < 1L] <- 1L
  dims
}

#' Compare the spatial grids encoded by two NIfTI headers
#'
#' Postprocessing operations in this file are not resampling operations. They
#' must therefore preserve the spatial matrix, voxel sizes, spatial units, and
#' both qform- and sform-preferred transforms. Comparing only array dimensions
#' can miss an output whose voxel values are correct but whose world-space
#' mapping has been damaged.
#'
#' @keywords internal
#' @noRd
pp_compare_nifti_grid <- function(reference_path, candidate_path,
                                   reference_label = "reference",
                                   candidate_label = "candidate",
                                   tolerance = 1e-5) {
  checkmate::assert_file_exists(reference_path)
  checkmate::assert_file_exists(candidate_path)
  checkmate::assert_string(reference_label, min.chars = 1L)
  checkmate::assert_string(candidate_label, min.chars = 1L)
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)

  reference_header <- RNifti::niftiHeader(reference_path)
  candidate_header <- RNifti::niftiHeader(candidate_path)
  reference_dims <- as.integer(reference_header$dim[2:4])
  candidate_dims <- as.integer(candidate_header$dim[2:4])
  reference_pixdim <- as.numeric(reference_header$pixdim[2:4])
  candidate_pixdim <- as.numeric(candidate_header$pixdim[2:4])
  reference_codes <- c(
    qform = as.integer(reference_header$qform_code),
    sform = as.integer(reference_header$sform_code)
  )
  candidate_codes <- c(
    qform = as.integer(candidate_header$qform_code),
    sform = as.integer(candidate_header$sform_code)
  )
  # The low three bits encode spatial units; temporal units are deliberately
  # excluded because this helper's contract is spatial-grid invariance.
  reference_units <- bitwAnd(as.integer(reference_header$xyzt_units), 7L)
  candidate_units <- bitwAnd(as.integer(candidate_header$xyzt_units), 7L)
  reference_qform <- unclass(RNifti::xform(reference_header, TRUE))
  candidate_qform <- unclass(RNifti::xform(candidate_header, TRUE))
  reference_sform <- unclass(RNifti::xform(reference_header, FALSE))
  candidate_sform <- unclass(RNifti::xform(candidate_header, FALSE))

  max_difference <- function(x, y) {
    if (!identical(dim(x), dim(y)) || any(!is.finite(x)) || any(!is.finite(y))) {
      return(Inf)
    }
    max(abs(as.numeric(x) - as.numeric(y)))
  }
  pixdim_difference <- max_difference(
    matrix(reference_pixdim, nrow = 1L),
    matrix(candidate_pixdim, nrow = 1L)
  )
  qform_difference <- max_difference(reference_qform, candidate_qform)
  sform_difference <- max_difference(reference_sform, candidate_sform)
  dimensions_match <- identical(reference_dims, candidate_dims)
  codes_match <- identical(reference_codes, candidate_codes)
  units_match <- identical(reference_units, candidate_units)
  passed <- dimensions_match && codes_match && units_match &&
    pixdim_difference <= tolerance && qform_difference <= tolerance &&
    sform_difference <= tolerance

  reasons <- character()
  if (!dimensions_match) reasons <- c(reasons, "spatial dimensions")
  if (pixdim_difference > tolerance) reasons <- c(reasons, "voxel sizes")
  if (!units_match) reasons <- c(reasons, "spatial units")
  if (!codes_match) reasons <- c(reasons, "qform/sform codes")
  if (qform_difference > tolerance) reasons <- c(reasons, "qform transform")
  if (sform_difference > tolerance) reasons <- c(reasons, "sform transform")
  message <- if (passed) {
    sprintf(
      "%s and %s have the same spatial NIfTI grid.",
      reference_label, candidate_label
    )
  } else {
    sprintf(
      paste0(
        "%s/%s spatial NIfTI grid mismatch (%s): dims [%s] vs [%s], ",
        "pixdim [%s] vs [%s], qform/sform codes [%s] vs [%s], ",
        "maximum qform/sform differences %.6g/%.6g (tol %.6g)."
      ),
      reference_label, candidate_label, paste(reasons, collapse = ", "),
      paste(reference_dims, collapse = "x"),
      paste(candidate_dims, collapse = "x"),
      paste(signif(reference_pixdim, 7L), collapse = "x"),
      paste(signif(candidate_pixdim, 7L), collapse = "x"),
      paste(reference_codes, collapse = "/"),
      paste(candidate_codes, collapse = "/"),
      qform_difference, sform_difference, tolerance
    )
  }

  list(
    passed = passed, message = message,
    reference_path = reference_path, candidate_path = candidate_path,
    reference_dims = reference_dims, candidate_dims = candidate_dims,
    reference_pixdim = reference_pixdim,
    candidate_pixdim = candidate_pixdim,
    reference_codes = reference_codes, candidate_codes = candidate_codes,
    reference_spatial_units = reference_units,
    candidate_spatial_units = candidate_units,
    max_pixdim_difference = pixdim_difference,
    max_qform_difference = qform_difference,
    max_sform_difference = sform_difference,
    tolerance = tolerance, mismatch_reasons = reasons
  )
}

#' Return a standard failed validation result for a spatial-grid mismatch
#' @keywords internal
#' @noRd
pp_grid_failure <- function(grid_check) {
  stopifnot(is.list(grid_check), identical(grid_check$passed, FALSE))
  out <- FALSE
  attr(out, "message") <- grid_check$message
  attr(out, "details") <- list(spatial_grid = grid_check)
  out
}

#' Read selected NIfTI volumes as a voxels-by-time matrix
#' @keywords internal
#' @noRd
pp_read_volume_matrix <- function(path, volumes, spatial_dims) {
  checkmate::assert_integerish(volumes, lower = 1L, any.missing = FALSE)
  image <- RNifti::readNifti(path, volumes = as.integer(volumes))
  matrix(as.numeric(image), nrow = prod(spatial_dims), ncol = length(volumes))
}

#' Stage a compressed NIfTI for repeated volume reads using bounded I/O buffers
#'
#' @param path Existing NIfTI path. Uncompressed files are returned unchanged.
#' @param directory Writable scratch directory for the temporary `.nii` file.
#' @return Path suitable for seeking to successive volume chunks. The caller
#'   must remove a returned temporary copy when it differs from `path`.
#' @noRd
pp_stage_nifti_for_chunks <- function(path, directory) {
  if (!grepl("\\.nii\\.gz$", path, ignore.case = TRUE)) return(path)
  checkmate::assert_directory_exists(directory, access = "w")
  staged <- tempfile("intensity-validation-", tmpdir = directory, fileext = ".nii")
  input <- gzfile(path, "rb")
  on.exit(close(input), add = TRUE)
  output <- file(staged, "wb")
  on.exit(close(output), add = TRUE)
  completed <- FALSE
  on.exit(if (!completed) unlink(staged), add = TRUE)
  repeat {
    buffer <- readBin(input, what = "raw", n = 4L * 1024L * 1024L)
    if (!length(buffer)) break
    writeBin(buffer, output)
  }
  completed <- TRUE
  staged
}

#' Select deterministic timepoints distributed over a complete run
#'
#' Finite caps retain regularly spaced, ordered timepoints and include the
#' first and last volume whenever at least two volumes are selected. Runs no
#' longer than the cap retain every volume. `NULL`, `NA`, and `Inf` request all
#' volumes.
#'
#' @keywords internal
#' @noRd
pp_distributed_volume_indices <- function(nt, max_volumes) {
  if (!checkmate::test_count(nt, positive = TRUE)) {
    stop("nt must be a positive integer.", call. = FALSE)
  }
  if (is.null(max_volumes) ||
      (length(max_volumes) == 1L &&
       (is.na(max_volumes) || is.infinite(max_volumes)))) {
    return(seq_len(nt))
  }
  checkmate::assert_count(max_volumes, positive = TRUE)
  n_use <- min(nt, as.integer(max_volumes))
  if (n_use == nt) return(seq_len(nt))

  indices <- as.integer(round(seq.int(1, nt, length.out = n_use)))
  stopifnot(length(indices) == n_use, !anyDuplicated(indices))
  indices
}

#' Compare an observed numeric transform with its expected values
#' @keywords internal
#' @noRd
pp_compare_numeric <- function(observed, expected, tolerance = 1e-5,
                                require_finite = TRUE,
                                absolute_tolerance = NULL) {
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
  checkmate::assert_number(
    absolute_tolerance, lower = 0, finite = TRUE, null.ok = TRUE
  )
  if (length(observed) != length(expected)) {
    stop("Observed and expected values have different lengths.", call. = FALSE)
  }
  observed <- as.numeric(observed)
  expected <- as.numeric(expected)
  observed_finite <- is.finite(observed)
  expected_finite <- is.finite(expected)
  finite_pattern_mismatches <- sum(observed_finite != expected_finite)
  nonfinite_observed <- sum(!observed_finite)
  jointly_finite <- observed_finite & expected_finite
  if (any(jointly_finite)) {
    absolute_error <- abs(observed[jointly_finite] - expected[jointly_finite])
    relative_error <- absolute_error / pmax(1, abs(expected[jointly_finite]))
    max_absolute_error <- max(absolute_error)
    max_relative_error <- max(relative_error)
    if (is.null(absolute_tolerance)) {
      # Preserve the historical hybrid tolerance for validators that have not
      # explicitly selected separate absolute and relative tolerances.
      allowed_error <- tolerance * pmax(1, abs(expected[jointly_finite]))
    } else {
      allowed_error <- absolute_tolerance +
        tolerance * abs(expected[jointly_finite])
    }
    numeric_mismatches <- sum(absolute_error > allowed_error)
  } else {
    max_absolute_error <- if (length(observed)) Inf else 0
    max_relative_error <- if (length(observed)) Inf else 0
    numeric_mismatches <- 0L
  }
  n_mismatched <- finite_pattern_mismatches + numeric_mismatches
  passed <- n_mismatched == 0L &&
    (!isTRUE(require_finite) || nonfinite_observed == 0L)
  list(
    passed = passed,
    max_absolute_error = max_absolute_error,
    max_relative_error = max_relative_error,
    n_mismatched = n_mismatched,
    n_nonfinite_observed = nonfinite_observed,
    finite_pattern_mismatches = finite_pattern_mismatches
  )
}

#' Compare two complete NIfTI images without loading every volume at once
#' @keywords internal
#' @noRd
pp_compare_nifti_identity <- function(reference_path, candidate_path,
                                       tolerance = 1e-5,
                                       chunk_size = 100L) {
  checkmate::assert_file_exists(reference_path)
  checkmate::assert_file_exists(candidate_path)
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
  checkmate::assert_count(chunk_size, positive = TRUE)

  grid <- pp_compare_nifti_grid(
    reference_path, candidate_path,
    reference_label = "pre", candidate_label = "post",
    tolerance = tolerance
  )
  reference_dims <- pp_nifti_dims4(reference_path)
  candidate_dims <- pp_nifti_dims4(candidate_path)
  if (!isTRUE(grid$passed) || !identical(reference_dims, candidate_dims)) {
    return(list(
      passed = FALSE,
      message = if (!isTRUE(grid$passed)) grid$message else sprintf(
        "Pre/post dimensions differ: [%s] vs [%s].",
        paste(reference_dims, collapse = "x"),
        paste(candidate_dims, collapse = "x")
      ),
      spatial_grid = grid,
      reference_dims = reference_dims, candidate_dims = candidate_dims,
      max_absolute_error = Inf, max_relative_error = Inf,
      n_mismatched = Inf, n_nonfinite_observed = Inf,
      finite_pattern_mismatches = Inf, tolerance = tolerance
    ))
  }

  aggregate <- list(
    max_absolute_error = 0, max_relative_error = 0,
    n_mismatched = 0L, n_nonfinite_observed = 0L,
    finite_pattern_mismatches = 0L
  )
  volume_groups <- split(
    seq_len(reference_dims[4]),
    ceiling(seq_len(reference_dims[4]) / as.integer(chunk_size))
  )
  for (volumes in volume_groups) {
    comparison <- pp_compare_numeric(
      pp_read_volume_matrix(candidate_path, volumes, reference_dims[1:3]),
      pp_read_volume_matrix(reference_path, volumes, reference_dims[1:3]),
      tolerance = tolerance, require_finite = TRUE
    )
    aggregate$max_absolute_error <- max(
      aggregate$max_absolute_error, comparison$max_absolute_error
    )
    aggregate$max_relative_error <- max(
      aggregate$max_relative_error, comparison$max_relative_error
    )
    for (name in c(
      "n_mismatched", "n_nonfinite_observed", "finite_pattern_mismatches"
    )) {
      aggregate[[name]] <- aggregate[[name]] + comparison[[name]]
    }
  }
  passed <- aggregate$n_mismatched == 0L &&
    aggregate$n_nonfinite_observed == 0L
  c(
    list(
      passed = passed,
      message = sprintf(
        paste0(
          "Unchanged-image replay: %d mismatched values, maximum relative ",
          "error %.6g (tol %.6g) across %d volumes."
        ),
        aggregate$n_mismatched, aggregate$max_relative_error, tolerance,
        reference_dims[4]
      ),
      spatial_grid = grid,
      reference_dims = reference_dims, candidate_dims = candidate_dims
    ),
    aggregate,
    list(tolerance = tolerance, volumes_compared = reference_dims[4])
  )
}

#' Validate a binary censor vector against a BOLD time dimension
#' @keywords internal
#' @noRd
pp_validate_censor <- function(censor, n_timepoints) {
  numeric_censor <- suppressWarnings(as.numeric(censor))
  if (length(numeric_censor) != n_timepoints) {
    return(list(
      valid = FALSE, censor = numeric_censor,
      message = sprintf(
        "Censor length (%d) does not match pre image T (%d).",
        length(numeric_censor), n_timepoints
      )
    ))
  }
  if (any(!is.finite(numeric_censor)) ||
      any(!numeric_censor %in% c(0, 1))) {
    return(list(
      valid = FALSE, censor = numeric_censor,
      message = "Censor vector must contain only binary 0/1 values."
    ))
  }
  list(valid = TRUE, censor = as.integer(numeric_censor), message = NULL)
}

#' Validate that a brain mask was correctly applied to 4D fMRI data
#'
#' Replays the masking operation in volume chunks and checks that the post-mask
#' image equals the pre-mask image inside the mask and is exactly zero outside
#' it. Voxels inside the mask that remain zero across the compared volumes are
#' reported.
#'
#' @param pre_file Path to the 4D fMRI data before masking.
#' @param post_file Path to the 4D fMRI data after masking.
#' @param mask_file Path to the binary mask NIfTI file (1s = brain, 0s = non-brain).
#' @param tolerance Relative numerical tolerance allowed inside the mask.
#' @param max_volumes Maximum number of timepoints compared. The default uses
#'   32 timepoints deterministically distributed over the complete run,
#'   including the first and last. Runs with 32 or fewer timepoints use every
#'   volume. Use `Inf` for exhaustive replay.
#' @param chunk_size Number of volumes compared at a time.
#' @param absolute_tolerance Absolute numerical tolerance allowed inside the
#'   mask. The default accommodates float32 rounding when FSL converts scaled
#'   integer NIfTI data.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes:
#'   - `message`: Character string describing the validation result.
#'   - `external_violations`: Integer count of voxels outside mask with non-zero signal.
#'   - `internal_zeros`: Integer count of voxels inside mask that are zero in all
#'     compared volumes.
#'
#' @details
#' For sampled in-mask values, validation requires
#' `abs(post - expected) <= absolute_tolerance + tolerance * abs(expected)`.
#' This prevents an all-zero or otherwise altered output from passing while
#' allowing small float32 conversion differences. Sampled values outside the
#' mask must still be finite and exactly zero. Spatial-grid and mask-validity
#' checks remain exhaustive.
#'
#' @keywords internal
#' @importFrom RNifti readNifti
#' @importFrom matrixStats rowAnys
validate_apply_mask <- function(pre_file, post_file, mask_file,
                                tolerance = 1e-5,
                                max_volumes = 32L,
                                chunk_size = 100L,
                                absolute_tolerance = 1e-4) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(mask_file)
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
  checkmate::assert_number(absolute_tolerance, lower = 0, finite = TRUE)
  checkmate::assert_count(chunk_size, positive = TRUE)

  pre_post_grid <- pp_compare_nifti_grid(
    pre_file, post_file, "pre", "post", tolerance = tolerance
  )
  if (!isTRUE(pre_post_grid$passed)) return(pp_grid_failure(pre_post_grid))
  pre_mask_grid <- pp_compare_nifti_grid(
    pre_file, mask_file, "pre", "mask", tolerance = tolerance
  )
  if (!isTRUE(pre_mask_grid$passed)) return(pp_grid_failure(pre_mask_grid))

  pre_dims <- pp_nifti_dims4(pre_file)
  post_dims <- pp_nifti_dims4(post_file)
  mask <- RNifti::readNifti(mask_file)
  if (!identical(pre_dims, post_dims) ||
      !identical(pre_dims[1:3], as.integer(dim(mask)))) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      "Pre/post/mask dimensions mismatch: pre=[%s], post=[%s], mask=[%s].",
      paste(pre_dims, collapse = "x"), paste(post_dims, collapse = "x"),
      paste(dim(mask), collapse = "x")
    )
    attr(out, "details") <- list(
      pre_dim = pre_dims, post_dim = post_dims, mask_dim = dim(mask)
    )
    return(out)
  }
  if (any(!is.finite(mask)) || any(mask < 0)) {
    out <- FALSE
    attr(out, "message") <- "Mask contains nonfinite or negative values."
    attr(out, "details") <- list()
    return(out)
  }

  mask_vec <- as.vector(mask) > 0
  inside_any_nonzero <- rep(FALSE, length(mask_vec))
  outside_any_invalid <- rep(FALSE, length(mask_vec))
  aggregate <- list(
    max_absolute_error = 0, max_relative_error = 0,
    n_mismatched = 0L, n_nonfinite_observed = 0L,
    finite_pattern_mismatches = 0L
  )
  volume_indices <- pp_distributed_volume_indices(
    pre_dims[4], max_volumes = max_volumes
  )
  volume_sampling <- if (length(volume_indices) == pre_dims[4]) {
    "all"
  } else {
    "distributed"
  }
  volume_groups <- split(
    volume_indices,
    ceiling(seq_along(volume_indices) / as.integer(chunk_size))
  )
  for (volumes in volume_groups) {
    pre_matrix <- pp_read_volume_matrix(pre_file, volumes, pre_dims[1:3])
    post_matrix <- pp_read_volume_matrix(post_file, volumes, pre_dims[1:3])
    expected <- pre_matrix
    expected[!mask_vec, ] <- 0
    comparison <- pp_compare_numeric(
      post_matrix, expected,
      tolerance = tolerance,
      require_finite = TRUE,
      absolute_tolerance = absolute_tolerance
    )
    aggregate$max_absolute_error <- max(
      aggregate$max_absolute_error, comparison$max_absolute_error
    )
    aggregate$max_relative_error <- max(
      aggregate$max_relative_error, comparison$max_relative_error
    )
    aggregate$n_mismatched <- aggregate$n_mismatched + comparison$n_mismatched
    aggregate$n_nonfinite_observed <- aggregate$n_nonfinite_observed +
      comparison$n_nonfinite_observed
    aggregate$finite_pattern_mismatches <-
      aggregate$finite_pattern_mismatches + comparison$finite_pattern_mismatches
    inside_any_nonzero <- inside_any_nonzero |
      matrixStats::rowAnys(is.finite(post_matrix) & post_matrix != 0)
    outside_any_invalid <- outside_any_invalid |
      matrixStats::rowAnys(!is.finite(post_matrix) | post_matrix != 0)
  }

  external_violations <- sum(!mask_vec & outside_any_invalid)
  internal_zeros <- sum(mask_vec & !inside_any_nonzero)
  passed <- aggregate$n_mismatched == 0L &&
    aggregate$n_nonfinite_observed == 0L && external_violations == 0L
  replay_label <- if (identical(volume_sampling, "all")) {
    "Exact mask replay"
  } else {
    "Distributed mask replay"
  }
  includes_first_volume <- identical(volume_indices[1], 1L)
  includes_last_volume <- identical(
    volume_indices[length(volume_indices)], pre_dims[4]
  )
  endpoint_note <- if (includes_first_volume && includes_last_volume) {
    ", including first and last"
  } else if (includes_first_volume) {
    ", including first"
  } else if (includes_last_volume) {
    ", including last"
  } else {
    ""
  }
  msg <- sprintf(
    paste0(
      "%s (%d/%d volumes%s): ",
      "%d mismatched values, max absolute error %.6g, ",
      "max scaled error %.6g (atol %.6g, rtol %.6g); ",
      "%d outside-mask voxels nonzero/nonfinite; ",
      "%d in-mask voxels zero for all compared volumes."
    ),
    replay_label, length(volume_indices), pre_dims[4], endpoint_note,
    aggregate$n_mismatched, aggregate$max_absolute_error,
    aggregate$max_relative_error, absolute_tolerance, tolerance,
    external_violations, internal_zeros
  )

  result <- passed
  attr(result, "message") <- msg
  attr(result, "external_violations") <- external_violations
  attr(result, "internal_zeros") <- internal_zeros
  attr(result, "details") <- c(
    aggregate,
    list(
      tolerance = tolerance,
      absolute_tolerance = absolute_tolerance,
      volumes_compared = length(volume_indices),
      total_volumes = pre_dims[4],
      max_volumes = max_volumes,
      volume_indices = volume_indices,
      volume_sampling = volume_sampling,
      includes_first_volume = includes_first_volume,
      includes_last_volume = includes_last_volume,
      chunk_size = as.integer(chunk_size)
    )
  )

  return(result)
}

# --- multitaper helpers ---

#' Compute multitaper power spectral density estimate for a single time series
#' @importFrom signal sgolayfilt
#' @keywords internal
#' @noRd
pp_power_multitaper <- function(
    y, dt,
    nw = 3, k = NULL,
    pad_factor = 0.5, detrend = TRUE,
    centre = "Slepian", adaptive = TRUE, jackknife = FALSE,
    smooth_psd = TRUE
) {
  stopifnot(is.numeric(y), length(dt) == 1, is.finite(dt), dt > 0)
  # detrend <- match.arg(detrend)
  checkmate::assert_number(pad_factor, lower = 0, upper=100, null.ok = TRUE)
  if (is.null(pad_factor)) pad_factor <- 0.5 # 50% padding
  
  if (!requireNamespace("multitaper", quietly = TRUE))
    stop("Package 'multitaper' is required.")
  
  y <- as.numeric(y)
  if (any(!is.finite(y))) stop("Cannot process inputs with NA/Inf values.")
  if (length(y) < 5) stop("y is less than 5 timepoints")

  # optional linear detrend -- useful for removing drift prior to PSD
  if (isTRUE(detrend)) {
    t <- seq_along(y)
    y <- stats::residuals(stats::lm(y ~ t))
  }

  # guard against constant series
  if (stats::var(y) <= 2*.Machine$double.eps) {
    return(data.frame(f = numeric(), power = numeric()))
  }
  
  if (is.null(k)) k <- max(1L, floor(2 * nw - 1))
  
  n_fft <- 2^ceiling(log2((1+pad_factor) * length(y)))
  
  out <- multitaper::spec.mtm(
    y, k = k, nw = nw, nFFT = n_fft,
    centre = centre, adaptive = adaptive, jackknife = jackknife,
    plot = FALSE, returnZeroFreq = TRUE, deltat=dt
  )
  
  f_hz <- out$freq # in Hz because delta passed to spec.mtm
  p_db <- 10 * log10(out$spec) # convert to dB
  
  # Spectra tend to be much easier to look at when smoothed. Use a peak-preserving smoother
  # with a width of 1/20th of the series
  if (smooth_psd && length(p_db) >= 5L) {
    # sgolayfilt() requires an odd window strictly larger than p. The raw
    # 1/20-width rule produces n=3 for common shortened fMRI fixtures.
    window_length <- 2L * floor((length(p_db) / 20) / 2) + 1L
    window_length <- max(5L, window_length)
    max_window <- length(p_db) - as.integer(length(p_db) %% 2L == 0L)
    window_length <- min(window_length, max_window)
    p_db <- signal::sgolayfilt(p_db, p = 3, n = window_length)
    #p_db <- stats::filter(p_db, rep(1/5, 5), sides = 2)  # 5-point moving average
  }
  
  return(data.frame(f = f_hz, power = p_db))
  
}


pp_mtm_bandpower <- function(
    y, dt,
    bands,                       # data.frame with low/high[/label], or named list of length-2 numerics
    nw = 4, k = NULL,
    detrend = c("none","linear"),
    centre = TRUE,
    adaptive = TRUE, jackknife = FALSE,
    pad_factor = 2,              # padding for nicer grids; doesn't change variance
    exclude_dc = TRUE,           # drop f=0
    total_band = NULL,           # e.g., c(0.01, 0.5); power for "relative_power"
    na_rm = TRUE
) {
  stopifnot(is.numeric(y), length(dt) == 1, is.finite(dt), dt > 0)
  detrend <- match.arg(detrend)
  
  if (!requireNamespace("multitaper", quietly = TRUE))
    stop("Package 'multitaper' is required (install.packages('multitaper')).")
  
  # --- parse bands ---
  if (is.data.frame(bands)) {
    stopifnot(all(c("low","high") %in% names(bands)))
    labels <- if ("label" %in% names(bands)) bands$label else NULL
    bands_df <- data.frame(
      low  = as.numeric(bands$low),
      high = as.numeric(bands$high),
      label = if (is.null(labels)) sprintf("band_%02d", seq_len(nrow(bands))) else as.character(labels),
      stringsAsFactors = FALSE
    )
  } else if (is.list(bands) && length(bands) > 0) {
    nm <- names(bands)
    if (is.null(nm) || any(nm == "")) nm <- paste0("band_", seq_along(bands))
    bands_df <- do.call(rbind, lapply(seq_along(bands), function(i) {
      b <- as.numeric(bands[[i]])
      if (length(b) != 2) stop("Each band in the list must be length-2: c(low, high).")
      data.frame(low = min(b), high = max(b), label = nm[i], stringsAsFactors = FALSE)
    }))
  } else {
    stop("`bands` must be a data.frame with columns low/high[/label] or a named list of length-2 numerics.")
  }
  
  # --- prep series ---
  y <- as.numeric(y)
  if (na_rm) y <- y[is.finite(y)]
  if (!length(y)) stop("All values are NA/Inf after filtering.")
  if (centre) y <- y - mean(y)
  if (detrend == "linear" && length(y) > 2) {
    t <- seq_along(y)
    y <- stats::residuals(stats::lm(y ~ t))
  }
  if (stats::var(y) <= .Machine$double.eps) {
    stop("Time series is (near) constant after preprocessing; cannot estimate PSD.")
  }
  
  # --- multitaper PSD ---
  if (is.null(k)) k <- max(1L, floor(2*nw - 1))
  N <- length(y)
  n_fft <- as.integer(ceiling(N * pad_factor))
  # if you prefer power-of-two FFTs, uncomment:
  # n_fft <- 2^ceiling(log2(n_fft))
  
  mt <- multitaper::spec.mtm(
    y,
    k = k, nw = nw,
    nFFT = n_fft,
    deltat = dt,                 # frequency in Hz
    centreWithSlepians = TRUE,   # Slepian centering
    adaptive = adaptive,
    jackknife = jackknife,
    plot = FALSE,
    returnZeroFreq = TRUE
  )
  
  f <- mt$freq             # Hz, from 0 .. Nyquist
  S <- mt$spec             # linear spectral density
  
  # Exclude DC bin (recommended for fMRI)
  if (exclude_dc) {
    keep <- f > 0
    f <- f[keep]; S <- S[keep]
  }
  
  nyq <- 1/(2*dt)
  
  # Interpolator for integration (linear)
  # S_fun <- stats::approxfun(f, S, rule = 2)  # constant extrapolation outside range
  # 
  # # Helper: integrate safely over [a,b], clipped to [min(f), max(f)]
  # integrate_band <- function(a, b) {
  #   lo <- max(min(f), a); hi <- min(max(f), b)
  #   if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(NA_real_)
  #   res <- try(stats::integrate(S_fun, lower = lo, upper = hi), silent = TRUE)
  #   if (inherits(res, "try-error")) return(NA_real_)
  #   as.numeric(res$value)
  # }
  
  integrate_band <- function(a, b) {
    # clip to PSD span
    a <- max(a, min(f)); b <- min(b, max(f))
    if (!is.finite(a) || !is.finite(b) || b <= a) return(0)  # return 0 power if no overlap
    
    # find indices whose bins intersect [a,b]
    idx <- which(f >= a & f <= b)
    if (length(idx) < 2L) {
      # interpolate endpoints and do a tiny trapezoid
      Sa <- stats::approx(f, S, xout = a, rule = 2)$y
      Sb <- stats::approx(f, S, xout = b, rule = 2)$y
      return(0.5 * (Sa + Sb) * (b - a))
    } else {
      # include band edges explicitly
      ff <- c(a, f[idx], b)
      SS <- c(stats::approx(f, S, xout = a, rule = 2)$y, S[idx],
              stats::approx(f, S, xout = b, rule = 2)$y)
      # trapezoidal area
      sum(0.5 * (SS[-1] + SS[-length(SS)]) * diff(ff))
    }
  }
  
  # Compute band powers
  bands_df$low  <- pmax(0, bands_df$low)
  bands_df$high <- pmin(nyq, bands_df$high)
  bands_df$power_linear <- vapply(
    seq_len(nrow(bands_df)),
    function(i) integrate_band(bands_df$low[i], bands_df$high[i]),
    numeric(1)
  )
  bands_df$power_db <- ifelse(is.finite(bands_df$power_linear) & bands_df$power_linear > 0,
                              10*log10(bands_df$power_linear),
                              NA_real_)
  
  # Relative power vs total band (default: total over the union of provided bands;
  # you can pass `total_band = c(0.01, nyq)` to normalize to a fixed range)
  if (is.null(total_band)) {
    tot_lo <- min(bands_df$low, na.rm = TRUE)
    tot_hi <- max(bands_df$high, na.rm = TRUE)
  } else {
    stopifnot(is.numeric(total_band), length(total_band) == 2)
    tot_lo <- max(0, min(total_band)); tot_hi <- min(nyq, max(total_band))
  }
  total_power <- integrate_band(tot_lo, tot_hi)
  bands_df$relative_power <- if (is.finite(total_power) && total_power > 0) {
    bands_df$power_linear / total_power
  } else {
    NA_real_
  }
  return(bands_df[, c("label", "low", "high", "power_linear", "power_db", "relative_power")])
}


#' @keywords internal
#' @noRd
pp_is_valid_series <- function(x, tol = 2 * .Machine$double.eps) {
  return(all(is.finite(x)) && stats::var(x) > tol)
}

#' @keywords internal
#' @noRd
pp_select_nonconstant_voxels <- function(
    mask_idx,
    get_pre_ts,
    n_voxels,
    spatial_dims,
    var_tol = 2 * .Machine$double.eps) {

  checkmate::assert_integerish(mask_idx, lower = 1L, any.missing = FALSE)
  checkmate::assert_integerish(
    spatial_dims, len = 3L, lower = 1L, any.missing = FALSE
  )
  candidate_positions <- seq_along(mask_idx)

  if (is.null(n_voxels) || is.na(n_voxels)) {
    valid_mask <- vapply(
      candidate_positions,
      function(pos) pp_is_valid_series(get_pre_ts(pos), tol = var_tol),
      logical(1)
    )
    valid_positions <- candidate_positions[valid_mask]
    if (!length(valid_positions)) {
      stop("No finite, non-constant pre-step voxels are available.", call. = FALSE)
    }
    return(list(indices = mask_idx[valid_positions], positions = valid_positions))
  }

  checkmate::assert_count(n_voxels, positive = TRUE)
  n_want <- min(as.integer(n_voxels), length(mask_idx))
  # Most supplied masks are brain masks, so a five-fold deterministic spatial
  # pool normally finds enough eligible series in one pass. If it does not,
  # expand the same low-discrepancy ordering rather than drawing random
  # replacements. Post-step values never affect selection.
  pool_size <- min(length(mask_idx), max(n_want, 5L * n_want))
  repeat {
    pool <- pp_select_spatial_replay_voxels(
      mask_idx, spatial_dims = spatial_dims, n_voxels = pool_size
    )
    pool_positions <- match(pool$indices, mask_idx)
    valid <- vapply(
      pool_positions,
      function(pos) pp_is_valid_series(get_pre_ts(pos), tol = var_tol),
      logical(1)
    )
    valid_positions <- pool_positions[valid]
    if (length(valid_positions) >= n_want) {
      valid_positions <- valid_positions[seq_len(n_want)]
      selected_indices <- mask_idx[valid_positions]
      selected_normalized <- pool$normalized_coords[
        match(selected_indices, pool$indices), , drop = FALSE
      ]
      return(list(
        indices = selected_indices, positions = valid_positions,
        normalized_coords = selected_normalized
      ))
    }
    if (pool_size == length(mask_idx)) break
    pool_size <- min(length(mask_idx), 2L * pool_size)
  }
  stop(
    "Fewer than ", n_want,
    " finite, non-constant pre-step voxels are available.", call. = FALSE
  )
}

#' Select deterministic, resolution-independent spatial replay voxels
#'
#' Eligible coordinates are stratified in normalized 3D image space so that
#' matrices with different spatial dimensions and voxel resolutions are sampled
#' across comparable relative locations. Eligibility must be determined solely
#' from pre-step data before calling this helper.
#'
#' @keywords internal
#' @noRd
pp_select_spatial_replay_voxels <- function(candidate_idx, spatial_dims,
                                             n_voxels) {
  checkmate::assert_integerish(candidate_idx, lower = 1L, any.missing = FALSE)
  checkmate::assert_integerish(spatial_dims, len = 3L, lower = 1L,
                               any.missing = FALSE)
  checkmate::assert_count(n_voxels, positive = TRUE)
  if (!length(candidate_idx)) {
    return(list(indices = integer(),
                normalized_coords = matrix(numeric(), ncol = 3L)))
  }
  if (any(candidate_idx > prod(spatial_dims))) {
    stop("Replay candidate index exceeds the spatial matrix size.", call. = FALSE)
  }

  candidate_idx <- sort(unique(as.integer(candidate_idx)))
  coords_matrix <- arrayInd(candidate_idx, .dim = spatial_dims)
  n_want <- min(as.integer(n_voxels), length(candidate_idx))
  normalized <- sweep(
    coords_matrix - 0.5, 2L, as.numeric(spatial_dims), FUN = "/"
  )
  # A fixed low-discrepancy sequence supplies targets in relative image space.
  # Nearest eligible voxels therefore occupy comparable anatomical fractions
  # even when studies use different matrix dimensions or voxel resolutions.
  radical_inverse <- function(index, base) {
    value <- 0
    fraction <- 1 / base
    while (index > 0L) {
      value <- value + fraction * (index %% base)
      index <- index %/% base
      fraction <- fraction / base
    }
    value
  }
  halton <- cbind(
    vapply(seq_len(n_want), radical_inverse, numeric(1), base = 2L),
    vapply(seq_len(n_want), radical_inverse, numeric(1), base = 3L),
    vapply(seq_len(n_want), radical_inverse, numeric(1), base = 5L)
  )
  lower <- apply(normalized, 2L, min)
  upper <- apply(normalized, 2L, max)
  targets <- sweep(halton, 2L, upper - lower, FUN = "*")
  targets <- sweep(targets, 2L, lower, FUN = "+")

  selected <- integer(n_want)
  used <- rep(FALSE, length(candidate_idx))
  for (target_i in seq_len(n_want)) {
    distance <- rowSums(
      (normalized - matrix(
        targets[target_i, ], nrow = nrow(normalized), ncol = 3L,
        byrow = TRUE
      ))^2
    )
    distance[used] <- Inf
    chosen <- which.min(distance)
    selected[[target_i]] <- chosen
    used[[chosen]] <- TRUE
  }

  if (!length(selected)) {
    return(list(indices = integer(),
                normalized_coords = matrix(numeric(), ncol = 3L)))
  }
  list(
    indices = candidate_idx[selected],
    normalized_coords = normalized[selected, , drop = FALSE]
  )
}

#' @keywords internal
#' @noRd
pp_make_ts_extractor <- function(img, coords_matrix) {
  function(position) {
    coord <- coords_matrix[position, ]
    return(img[coord[1], coord[2], coord[3], , drop = TRUE])
  }
}

#' @keywords internal
#' @noRd
pp_average_multitaper_spectra <- function(spec_list) {
  if (!length(spec_list)) stop("No spectra supplied for averaging.", call. = FALSE)
  dt <- data.table::rbindlist(spec_list, idcol = "voxel")
  data.table::setnames(dt, c("f", "power"), c("freq", "power_db"))
  as.data.frame(dt[, .(power_db = mean(power_db, na.rm = TRUE)), by = freq])
}

#' @keywords internal
#' @noRd
pp_average_bandpower <- function(bp_list) {
  if (!length(bp_list)) stop("No bandpower estimates supplied for averaging.", call. = FALSE)
  dt <- data.table::rbindlist(bp_list, idcol = "voxel")
  out <- dt[, .(power_linear = mean(power_linear, na.rm = TRUE),
                relative_power = mean(relative_power, na.rm = TRUE)),
            by = .(label, low, high)]
  out[, power_db := ifelse(is.finite(power_linear) & power_linear > 0,
                           10 * log10(power_linear), NA_real_)]
  as.data.frame(out)
}

#' Validate temporal filtering (multitaper pre vs post)
#'
#' Band power outside / inside the passband; needs `multitaper` and `signal`.
#'
#' @param pre_file Path to 4D BOLD before `temporal_filter`.
#' @param post_file Path to 4D BOLD after `temporal_filter`.
#' @param tr TR in seconds (`cfg$tr`).
#' @param band_low_hz Lower passband edge (Hz); `NA` if open.
#' @param band_high_hz Upper passband edge (Hz); `NA` if open.
#' @param mask_file Optional 3D mask; if unset, sample the whole volume.
#' @param n_voxels How many voxels to use.
#' @param passband_loss_fail_db Max allowed passband loss (dB) before fail (default 3).
#' @param minimum_voxel_fraction Minimum fraction of sampled voxels that must
#'   show stopband power reduction and remain within the passband-loss limit.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details` (numeric summaries and flags).
#'
#' @keywords internal
#' @importFrom RNifti readNifti
validate_temporal_filter <- function(
    pre_file,
    post_file,
    tr,
    band_low_hz = NA_real_,
    band_high_hz = NA_real_,
    mask_file = NULL,
    n_voxels = 30L,
    passband_loss_fail_db = 3,
    minimum_voxel_fraction = 0.8
) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_number(tr, lower = 1e-4, upper = 100)
  checkmate::assert_int(n_voxels, lower = 1L)
  checkmate::assert_number(passband_loss_fail_db, lower = 0, finite = TRUE)
  checkmate::assert_number(
    minimum_voxel_fraction, lower = 0.5, upper = 1, finite = TRUE
  )

  pre_post_grid <- pp_compare_nifti_grid(
    pre_file, post_file, "pre", "post"
  )
  if (!isTRUE(pre_post_grid$passed)) return(pp_grid_failure(pre_post_grid))
  has_mask <- checkmate::test_string(mask_file, min.chars = 1L)
  if (!is.null(mask_file) && !has_mask) {
    out <- FALSE
    attr(out, "message") <- "mask_file must be NULL or a nonempty path."
    attr(out, "details") <- list(mask_file = mask_file)
    return(out)
  }
  if (has_mask) {
    checkmate::assert_file_exists(mask_file)
    pre_mask_grid <- pp_compare_nifti_grid(
      pre_file, mask_file, "pre", "mask"
    )
    if (!isTRUE(pre_mask_grid$passed)) return(pp_grid_failure(pre_mask_grid))
  }

  if (!requireNamespace("multitaper", quietly = TRUE) || !requireNamespace("signal", quietly = TRUE)) {
    out <- FALSE
    attr(out, "message") <- "multitaper and signal packages are required for temporal_filter validation."
    attr(out, "details") <- list(missing_packages = TRUE)
    return(out)
  }

  band_low <- if (!is.null(band_low_hz) && is.finite(band_low_hz) && band_low_hz > 0) {
    band_low_hz
  } else {
    NA_real_
  }
  band_high <- if (!is.null(band_high_hz) && is.finite(band_high_hz) && band_high_hz > 0) {
    band_high_hz
  } else {
    NA_real_
  }

  dt <- tr
  nyquist <- 1 / (2 * dt)
  invalid_cutoff <-
    (!is.na(band_low) && band_low >= nyquist) ||
    (!is.na(band_high) && band_high >= nyquist) ||
    (!is.na(band_low) && !is.na(band_high) && band_low >= band_high)
  if (invalid_cutoff || (is.na(band_low) && is.na(band_high))) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      paste0(
        "Temporal-filter validation requires at least one effective cutoff ",
        "strictly between 0 and Nyquist (%.6g Hz), with high-pass below ",
        "low-pass; received high-pass=%s and low-pass=%s."
      ),
      nyquist, format(band_low), format(band_high)
    )
    attr(out, "details") <- list(
      nyquist_hz = nyquist, band_low_hz = band_low,
      band_high_hz = band_high, invalid_band_specification = TRUE
    )
    return(out)
  }

  pre_img <- RNifti::readNifti(pre_file)
  pre_img_dims <- dim(pre_img)
  if (length(pre_img_dims) == 3L) {
    pre_img_dims <- c(pre_img_dims, 1L)
    dim(pre_img) <- pre_img_dims
  }
  pre_n_t <- pre_img_dims[4]
  pre_n_vox <- prod(pre_img_dims[1:3])

  mask_dims <- NULL
  if (has_mask) {
    mask <- RNifti::readNifti(mask_file)
    mask_dims <- dim(mask)
    if (!all(mask_dims == pre_img_dims[1:3])) {
      out <- FALSE
      attr(out, "message") <- "Mask dimensions do not match pre image."
      attr(out, "details") <- list()
      return(out)
    }
    mask_logical <- (mask != 0) & is.finite(mask)
    n_mask_vox <- sum(mask_logical)
    if (n_mask_vox == 0L) {
      out <- FALSE
      attr(out, "message") <- "Mask contains no voxels."
      attr(out, "details") <- list()
      return(out)
    }
    mask_idx <- which(mask_logical)
  } else {
    mask_idx <- seq_len(pre_n_vox)
    mask_dims <- pre_img_dims[1:3]
  }

  mask_coords <- arrayInd(mask_idx, pre_img_dims[1:3], useNames = FALSE)

  post_img <- RNifti::readNifti(post_file)
  post_img_dims <- dim(post_img)
  if (length(post_img_dims) == 3L) {
    post_img_dims <- c(post_img_dims, 1L)
    dim(post_img) <- post_img_dims
  }
  post_n_t <- post_img_dims[4]
  post_n_vox <- prod(post_img_dims[1:3])

  if (has_mask && !all(mask_dims == post_img_dims[1:3])) {
    out <- FALSE
    attr(out, "message") <- "Mask dimensions do not match post image."
    attr(out, "details") <- list()
    return(out)
  }
  if (pre_n_t != post_n_t) {
    out <- FALSE
    attr(out, "message") <- "Pre and post series must have the same number of timepoints."
    attr(out, "details") <- list(pre_n_t = pre_n_t, post_n_t = post_n_t)
    return(out)
  }
  if (pre_n_vox != post_n_vox) {
    out <- FALSE
    attr(out, "message") <- "Pre and post images must have the same spatial dimensions."
    attr(out, "details") <- list()
    return(out)
  }

  var_tol <- if (!has_mask) 1e-3 else 2 * .Machine$double.eps
  default_sample_size <- min(as.integer(n_voxels), length(mask_idx))
  voxels_to_sample <- if (length(mask_idx) <= default_sample_size) length(mask_idx) else default_sample_size

  get_pre_ts <- pp_make_ts_extractor(pre_img, mask_coords)
  get_post_ts <- pp_make_ts_extractor(post_img, mask_coords)

  selection <- tryCatch(
    pp_select_nonconstant_voxels(
      mask_idx = mask_idx,
      get_pre_ts = get_pre_ts,
      n_voxels = voxels_to_sample,
      spatial_dims = pre_img_dims[1:3],
      var_tol = var_tol
    ),
    error = function(e) e
  )
  if (inherits(selection, "error")) {
    out <- FALSE
    attr(out, "message") <- conditionMessage(selection)
    attr(out, "details") <- list(error = conditionMessage(selection))
    return(out)
  }

  selected_positions <- selection$positions
  post_valid <- vapply(
    selected_positions,
    function(pos) pp_is_valid_series(get_post_ts(pos), tol = var_tol),
    logical(1)
  )
  if (!all(post_valid)) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      paste0(
        "%d of %d deterministic pre-selected voxels became nonfinite or ",
        "constant after temporal filtering."
      ),
      sum(!post_valid), length(post_valid)
    )
    attr(out, "details") <- list(
      n_voxels_used = length(selected_positions),
      n_invalid_post_series = sum(!post_valid),
      sampled_indices = selection$indices,
      normalized_coords = selection$normalized_coords
    )
    return(out)
  }

  outside_bands <- list()
  avg_reduction <- NA_real_
  median_reduction <- NA_real_
  fraction_reduced <- NA_real_
  outside_summaries_finite <- FALSE
  fail_outside <- FALSE

  if (!is.na(band_low) && band_low > 0) {
    outside_bands$below <- c(0, max(0, band_low))
  }
  if (!is.na(band_high) && band_high < nyquist) {
    outside_bands$above <- c(min(band_high, nyquist), nyquist)
  }

  bandpower_error <- NULL
  outside_result <- tryCatch({
    pre_bp_list <- lapply(selected_positions, function(pos) {
      pp_mtm_bandpower(
        get_pre_ts(pos), dt = dt, bands = outside_bands,
        detrend = "linear", exclude_dc = TRUE,
        total_band = c(0, nyquist)
      )
    })
    post_bp_list <- lapply(selected_positions, function(pos) {
      pp_mtm_bandpower(
        get_post_ts(pos), dt = dt, bands = outside_bands,
        detrend = "linear", exclude_dc = TRUE,
        total_band = c(0, nyquist)
      )
    })
    reductions <- mapply(
      function(pre_bp, post_bp) {
        pre_power <- sum(pre_bp$power_linear)
        post_power <- sum(post_bp$power_linear)
        if (!is.finite(pre_power) || !is.finite(post_power) ||
            pre_power <= 0 || post_power <= 0) return(NA_real_)
        10 * log10(pre_power / post_power)
      },
      pre_bp_list, post_bp_list
    )
    list(reductions = reductions)
  }, error = function(e) {
    bandpower_error <<- conditionMessage(e)
    NULL
  })
  if (!is.null(outside_result)) {
    reductions <- outside_result$reductions
    outside_summaries_finite <- length(reductions) == length(selected_positions) &&
      all(is.finite(reductions))
    if (outside_summaries_finite) {
      avg_reduction <- mean(reductions)
      median_reduction <- stats::median(reductions)
      fraction_reduced <- mean(reductions > 0)
    }
  }
  fail_outside <- !outside_summaries_finite ||
    !is.finite(fraction_reduced) ||
    fraction_reduced < minimum_voxel_fraction

  passband_low <- if (!is.na(band_low)) max(0, band_low) else 0
  passband_high <- if (!is.na(band_high)) min(nyquist, band_high) else nyquist
  has_passband_bounds <- (!is.na(band_low) || !is.na(band_high)) && passband_high > passband_low

  avg_change_db <- NA_real_
  median_change_db <- NA_real_
  fraction_passband_preserved <- NA_real_
  passband_summaries_finite <- FALSE
  fail_passband <- FALSE

  if (has_passband_bounds) {
    passband <- list(passband = c(passband_low, passband_high))
    passband_result <- tryCatch({
      pre_pass_list <- lapply(selected_positions, function(pos) {
        pp_mtm_bandpower(
          get_pre_ts(pos), dt = dt, bands = passband,
          detrend = "linear", exclude_dc = TRUE,
          total_band = c(0, nyquist)
        )
      })
      post_pass_list <- lapply(selected_positions, function(pos) {
        pp_mtm_bandpower(
          get_post_ts(pos), dt = dt, bands = passband,
          detrend = "linear", exclude_dc = TRUE,
          total_band = c(0, nyquist)
        )
      })
      changes <- mapply(
        function(pre_bp, post_bp) {
          pre_power <- sum(pre_bp$power_linear)
          post_power <- sum(post_bp$power_linear)
          if (!is.finite(pre_power) || !is.finite(post_power) ||
              pre_power <= 0 || post_power <= 0) return(NA_real_)
          10 * log10(post_power / pre_power)
        },
        pre_pass_list, post_pass_list
      )
      list(changes = changes)
    }, error = function(e) {
      bandpower_error <<- conditionMessage(e)
      NULL
    })
    if (!is.null(passband_result)) {
      power_changes <- passband_result$changes
      passband_summaries_finite <-
        length(power_changes) == length(selected_positions) &&
        all(is.finite(power_changes))
      if (passband_summaries_finite) {
        avg_change_db <- mean(power_changes)
        median_change_db <- stats::median(power_changes)
        fraction_passband_preserved <- mean(
          power_changes >= -passband_loss_fail_db
        )
      }
    }
    fail_passband <- !passband_summaries_finite ||
      !is.finite(fraction_passband_preserved) ||
      fraction_passband_preserved < minimum_voxel_fraction
  } else {
    fail_passband <- TRUE
  }

  passed <- !fail_outside && !fail_passband

  msg_parts <- character()
  if (is.finite(avg_reduction)) {
    msg_parts <- c(msg_parts, sprintf(
      paste0(
        "outside-band reduction mean/median %.4g/%.4g dB; ",
        "fraction reduced %.3f"
      ),
      avg_reduction, median_reduction, fraction_reduced
    ))
  }
  if (!is.na(avg_change_db) && is.finite(avg_change_db)) {
    msg_parts <- c(msg_parts, sprintf(
      paste0(
        "passband change mean/median %.4g/%.4g dB; fraction within %.3g dB ",
        "loss %.3f"
      ),
      avg_change_db, median_change_db, passband_loss_fail_db,
      fraction_passband_preserved
    ))
  }
  if (!length(msg_parts)) {
    msg_parts <- "multitaper bandpower summaries were unavailable."
  }

  msg <- paste(msg_parts, collapse = "; ")
  if (fail_outside) {
    msg <- paste0(
      msg, "; FAIL: fewer than ", minimum_voxel_fraction * 100,
      "% of sampled voxels showed finite stopband power reduction."
    )
  }
  if (fail_passband) {
    msg <- paste0(
      msg,
      "; FAIL: fewer than ", minimum_voxel_fraction * 100,
      "% of sampled voxels stayed within the passband-loss threshold of ",
      passband_loss_fail_db, " dB."
    )
  }
  if (!is.null(bandpower_error)) {
    msg <- paste0(msg, "; spectral estimation error: ", bandpower_error)
  }

  details <- list(
    nyquist_hz = nyquist,
    n_voxels_used = length(selected_positions),
    sampled_indices = selection$indices,
    normalized_coords = selection$normalized_coords,
    avg_reduction_outside_db = avg_reduction,
    median_reduction_outside_db = median_reduction,
    fraction_voxels_outside_reduced = fraction_reduced,
    avg_passband_change_db = avg_change_db,
    median_passband_change_db = median_change_db,
    fraction_voxels_passband_preserved = fraction_passband_preserved,
    minimum_voxel_fraction = minimum_voxel_fraction,
    passband_loss_fail_db = passband_loss_fail_db,
    outside_summaries_finite = outside_summaries_finite,
    passband_summaries_finite = passband_summaries_finite,
    spectral_error = bandpower_error,
    band_low_hz = band_low,
    band_high_hz = band_high,
    fail_outside = fail_outside,
    fail_passband = fail_passband
  )

  result <- passed
  attr(result, "message") <- msg
  attr(result, "details") <- details
  return(result)
}

# --- smoothness helpers (from R/temp/smoothness_helpers.R) --------------------------------

#' @keywords internal
#' @noRd

var_sample <- function(x) {
  if (length(x) < 2) return(NA_real_)
  v <- stats::var(x)
  return(if (!is.finite(v) || v <= 0) NA_real_ else as.numeric(v))
}

compute_fwhm_1dif <- function(vol3d, mask3d, vox_mm) {
  stopifnot(length(dim(vol3d)) == 3L, all(dim(vol3d) == dim(mask3d)))
  nx <- dim(vol3d)[1]; ny <- dim(vol3d)[2]; nz <- dim(vol3d)[3]
  total_mask <- mask3d & is.finite(vol3d)
  nmask <- sum(total_mask)
  if (is.na(nmask) || nmask < 9) return(rep(-1, 3))
  vdat <- var_sample(as.numeric(vol3d[total_mask]))
  if (is.na(vdat) || vdat <= 0) return(rep(-1, 3))
  fx <- fy <- fz <- -1
  if (nx > 1) {
    pairs <- mask3d[1:(nx-1), , , drop = FALSE] & mask3d[2:nx, , , drop = FALSE]
    if (any(pairs)) {
      diffs <- vol3d[2:nx, , , drop = FALSE] - vol3d[1:(nx-1), , , drop = FALSE]
      vxx <- var_sample(as.numeric(diffs[pairs]))
      if (is.finite(vxx)) {
        arg <- 1 - 0.5 * (vxx / vdat)
        if (is.finite(arg) && arg > 0 && arg < 1) {
          fx <- 2.35482 * sqrt(-1 / (4 * log(arg))) * vox_mm[1]
        }
      }
    }
  }
  if (ny > 1) {
    pairs <- mask3d[ , 1:(ny-1), , drop = FALSE] & mask3d[ , 2:ny, , drop = FALSE]
    if (any(pairs)) {
      diffs <- vol3d[ , 2:ny, , drop = FALSE] - vol3d[ , 1:(ny-1), , drop = FALSE]
      vyy <- var_sample(as.numeric(diffs[pairs]))
      if (is.finite(vyy)) {
        arg <- 1 - 0.5 * (vyy / vdat)
        if (is.finite(arg) && arg > 0 && arg < 1) {
          fy <- 2.35482 * sqrt(-1 / (4 * log(arg))) * vox_mm[2]
        }
      }
    }
  }
  if (nz > 1) {
    pairs <- mask3d[ , , 1:(nz-1), drop = FALSE] & mask3d[ , , 2:nz, drop = FALSE]
    if (any(pairs)) {
      diffs <- vol3d[ , , 2:nz, drop = FALSE] - vol3d[ , , 1:(nz-1), drop = FALSE]
      vzz <- var_sample(as.numeric(diffs[pairs]))
      if (is.finite(vzz)) {
        arg <- 1 - 0.5 * (vzz / vdat)
        if (is.finite(arg) && arg > 0 && arg < 1) {
          fz <- 2.35482 * sqrt(-1 / (4 * log(arg))) * vox_mm[3]
        }
      }
    }
  }
  return(c(fx, fy, fz))
}

geom_mean_safe <- function(x) {
  x <- x[is.finite(x) & x > 0]
  return(if (!length(x)) NA_real_ else exp(mean(log(x))))
}

estimate_classic_fwhm <- function(arr4d, mask3d, vox_mm, agg = "geom") {
  dims <- dim(arr4d)
  nt <- dims[4]
  per_axis <- matrix(NA_real_, nrow = nt, ncol = 3)
  for (t in seq_len(nt)) {
    per_axis[t, ] <- compute_fwhm_1dif(arr4d[,,,t], mask3d, vox_mm)
  }
  geom_axes <- apply(per_axis, 2, geom_mean_safe)
  overall <- geom_mean_safe(geom_axes)
  return(list(per_axis = per_axis, geom_axes = geom_axes, geom = overall))
}

#' Estimate classic FWHM directly from the rows inside a spatial mask
#'
#' This is algebraically identical to reconstructing a full 3D volume and
#' calling `compute_fwhm_1dif()` at every timepoint. Precomputing the row pairs
#' that are adjacent along each spatial axis avoids repeatedly allocating three
#' full-volume difference arrays, which is material even for the distributed
#' 96-volume production validation sample.
#'
#' @keywords internal
#' @noRd
pp_estimate_classic_masked_matrix <- function(masked_matrix, mask3d, vox_mm) {
  checkmate::assert_matrix(masked_matrix, mode = "numeric")
  checkmate::assert_numeric(vox_mm, len = 3L, lower = 1e-6, finite = TRUE)
  mask3d <- (mask3d != 0) & is.finite(mask3d)
  spatial_dims <- dim(mask3d)
  if (length(spatial_dims) != 3L) {
    stop("Smoothness mask must be three-dimensional.", call. = FALSE)
  }
  mask_vec <- as.vector(mask3d)
  if (nrow(masked_matrix) != sum(mask_vec)) {
    stop("Masked matrix row count does not match the smoothness mask.", call. = FALSE)
  }

  full_index <- array(seq_len(prod(spatial_dims)), dim = spatial_dims)
  mask_row <- integer(length(mask_vec))
  mask_row[which(mask_vec)] <- seq_len(sum(mask_vec))
  pair_rows <- lapply(seq_len(3L), function(axis) {
    if (spatial_dims[[axis]] <= 1L) {
      return(matrix(integer(), nrow = 0L, ncol = 2L,
                    dimnames = list(NULL, c("lower", "upper"))))
    }
    lower_subscripts <- lapply(spatial_dims, seq_len)
    upper_subscripts <- lower_subscripts
    lower_subscripts[[axis]] <- seq_len(spatial_dims[[axis]] - 1L)
    upper_subscripts[[axis]] <- seq.int(2L, spatial_dims[[axis]])
    lower <- do.call(
      `[`, c(list(full_index), lower_subscripts, list(drop = FALSE))
    )
    upper <- do.call(
      `[`, c(list(full_index), upper_subscripts, list(drop = FALSE))
    )
    keep <- mask_vec[as.vector(lower)] & mask_vec[as.vector(upper)]
    cbind(
      lower = mask_row[as.vector(lower)[keep]],
      upper = mask_row[as.vector(upper)[keep]]
    )
  })
  rm(full_index, mask_row)

  per_axis <- matrix(-1, nrow = ncol(masked_matrix), ncol = 3L)
  for (time_i in seq_len(ncol(masked_matrix))) {
    values <- masked_matrix[, time_i]
    finite_values <- is.finite(values)
    if (sum(finite_values) < 9L) next
    vdat <- var_sample(values[finite_values])
    if (!is.finite(vdat) || vdat <= 0) next
    for (axis in seq_len(3L)) {
      pairs <- pair_rows[[axis]]
      if (!nrow(pairs)) next
      differences <- values[pairs[, "upper"]] - values[pairs[, "lower"]]
      difference_variance <- var_sample(differences)
      if (!is.finite(difference_variance)) next
      argument <- 1 - 0.5 * (difference_variance / vdat)
      if (is.finite(argument) && argument > 0 && argument < 1) {
        per_axis[time_i, axis] <-
          2.35482 * sqrt(-1 / (4 * log(argument))) * vox_mm[[axis]]
      }
    }
  }
  geom_axes <- apply(per_axis, 2L, geom_mean_safe)
  list(
    per_axis = per_axis,
    geom_axes = geom_axes,
    geom = geom_mean_safe(geom_axes)
  )
}

#' Build an orthonormal polynomial trend matrix for smoothness validation
#' @keywords internal
#' @noRd
pp_build_trend_matrix <- function(nt, degree = 3L, demean = TRUE) {
  degree <- max(0L, as.integer(degree))
  cols <- list()
  tvec <- seq_len(nt)
  if (isTRUE(demean)) cols[[length(cols) + 1L]] <- rep(1, nt)
  if (degree >= 1L && nt >= 2L) {
    degree <- min(degree, nt - 1L)
    rng <- range(tvec)
    scaled <- if (diff(rng) > 0) {
      2 * (tvec - mean(tvec)) / diff(rng)
    } else {
      rep(0, nt)
    }
    poly_mat <- stats::poly(scaled, degree = degree, raw = FALSE, simple = FALSE)
    for (p in seq_len(degree)) cols[[length(cols) + 1L]] <- poly_mat[, p]
  }
  if (!length(cols)) return(NULL)
  do.call(cbind, cols)
}

#' Apply voxelwise polynomial detrending for smoothness validation
#' @keywords internal
#' @noRd
pp_detrend_voxels <- function(mat, degree = 3L, demean = TRUE) {
  nt <- ncol(mat)
  if (!length(mat) || nt == 0L) return(mat)
  X <- pp_build_trend_matrix(nt, degree = degree, demean = demean)
  if (is.null(X)) return(mat)
  coeff <- qr.solve(X, t(mat))
  fitted <- t(X %*% coeff)
  mat - fitted
}

#' Normalize voxel time series by temporal MAD for smoothness validation
#' @keywords internal
#' @noRd
pp_mad_scale_matrix <- function(mat) {
  mad_vals <- matrixStats::rowMads(mat, constant = 1.4826, na.rm = TRUE)
  mad_vals[!is.finite(mad_vals) | mad_vals <= 1e-6] <- 1
  mat / mad_vals
}

#' Select distributed timepoints for smoothness validation
#' @keywords internal
#' @noRd
pp_smoothness_volume_indices <- function(nt, max_volumes = 96L) {
  # Smoothness is a spatial property. Spread the retained timepoints over the
  # complete run so a short validation sample is not determined by one
  # contiguous acquisition segment.
  pp_distributed_volume_indices(nt, max_volumes = max_volumes)
}

#' Match the classic smoothness preprocessing used by 3dSmoothnessChange.R
#' @keywords internal
#' @noRd
pp_prepare_classic_smoothness <- function(arr4d, mask3d, polydeg = 3L,
                                           demean = TRUE, unif = TRUE) {
  dims <- dim(arr4d)
  n_vox <- prod(dims[1:3])
  nt <- dims[4]
  mask_vec <- as.vector(mask3d)
  mat <- array(arr4d, dim = c(n_vox, nt))
  if (any(mask_vec)) {
    mat_mask <- mat[mask_vec, , drop = FALSE]
    mat_mask <- pp_detrend_voxels(mat_mask, degree = polydeg, demean = demean)
    if (isTRUE(unif)) mat_mask <- pp_mad_scale_matrix(mat_mask)
    mat[mask_vec, ] <- mat_mask
  }
  array(mat, dim = dims)
}

#' Estimate classic FWHM from one file without retaining a full prepared 4D copy
#' @keywords internal
#' @noRd
pp_estimate_classic_smoothness_file <- function(path, mask3d,
                                                 max_volumes = 96L,
                                                 preprocess = TRUE,
                                                 polydeg = 3L,
                                                 demean = TRUE,
                                                 unif = TRUE) {
  checkmate::assert_file_exists(path)
  checkmate::assert_flag(preprocess)
  checkmate::assert_flag(demean)
  checkmate::assert_flag(unif)
  if (isTRUE(preprocess)) checkmate::assert_count(polydeg)
  invisible(gc(FALSE))
  on.exit(invisible(gc(FALSE)), add = TRUE)

  image_dims <- pp_nifti_dims4(path)
  spatial_dims <- image_dims[1:3]
  if (!identical(as.integer(spatial_dims), as.integer(dim(mask3d)))) {
    stop("Image and smoothness mask dimensions do not match.", call. = FALSE)
  }
  total_volumes <- image_dims[4]
  volume_idx <- pp_smoothness_volume_indices(total_volumes, max_volumes)
  mask_vec <- as.vector(mask3d)
  # RNifti performs the temporal subsetting while reading. This avoids
  # materializing a complete long 4D run merely to retain a small sample.
  prepared <- pp_read_volume_matrix(path, volume_idx, spatial_dims)[
    mask_vec, , drop = FALSE
  ]

  if (isTRUE(preprocess)) {
    detrended <- pp_detrend_voxels(prepared, degree = polydeg, demean = demean)
    rm(prepared)
    invisible(gc(FALSE))
    prepared <- detrended
    rm(detrended)
    if (isTRUE(unif)) {
      scaled <- pp_mad_scale_matrix(prepared)
      rm(prepared)
      invisible(gc(FALSE))
      prepared <- scaled
      rm(scaled)
    }
  }

  estimate <- pp_estimate_classic_masked_matrix(
    prepared, mask3d, pp_pixdim_mm(path)
  )
  list(
    per_axis = estimate$per_axis,
    geom_axes = estimate$geom_axes,
    geom = estimate$geom,
    volumes_used = length(volume_idx),
    total_volumes = total_volumes,
    volume_indices = volume_idx,
    volume_sampling = if (length(volume_idx) == total_volumes) "all" else "distributed"
  )
}

median_over_time <- function(arr4d) {
  return(apply(arr4d, c(1, 2, 3), stats::median, na.rm = TRUE))
}

mad_over_time <- function(arr4d) {
  return(apply(arr4d, c(1, 2, 3), stats::mad, constant = 1.4826, na.rm = TRUE))
}


#' @keywords internal
#' @noRd
pp_pixdim_mm <- function(path) {
  h <- RNifti::niftiHeader(path)
  return(as.numeric(h$pixdim[2:4]))
}

#' @keywords internal
#' @noRd
pp_read_4d <- function(path) {
  img <- RNifti::readNifti(path)
  d <- dim(img)
  if (length(d) == 3L) {
    d <- c(d, 1L)
    dim(img) <- d
  }
  return(img)
}

#' @keywords internal
#' @noRd
pp_max_abs_diff <- function(a, b) {
  return(max(abs(as.numeric(a) - as.numeric(b)), na.rm = TRUE))
}

#' Validate run-scalar or denominator-guarded voxelwise PSC normalization
#'
#' Checks dimensions, finite-value locations, and exact application of either a
#' single positive multiplier or a positive 3D PSC multiplier map. A guarded
#' PSC map encodes ordinary `100 / local_baseline` factors plus denominator-floor
#' and run-reference fallback factors; guarding does not imply observation
#' clipping or voxel masking. For scalar normalization, an optional reference
#' mask also verifies the achieved target.
#'
#' @details `reference_location` is the run reference intensity before scaling:
#'   the spatial median across reference voxels of their 10%-trimmed temporal
#'   means. Therefore, `reference_location * scale_factor` should equal
#'   `target`. If `core_file` is supplied in `run_scalar` mode, the function independently repeats
#'   this two-stage calculation on `post_file`, using `include_frames` to select
#'   the same baseline-estimation volumes. In `voxel_psc` mode, `target` must be
#'   100 and `scale_file` must be a finite positive 3D map matching the spatial
#'   BOLD grid. No binary PSC validity mask is expected or applied.
#'
#' @param pre_file Path to the 4D NIfTI image immediately before scaling.
#' @param post_file Path to the 4D NIfTI image immediately after scaling.
#' @param reference_location Positive run reference intensity measured after
#'   masking and smoothing and before temporal denoising.
#' @param target Desired value of the run reference intensity after scaling.
#' @param mode Either `"run_scalar"` or denominator-guarded `"voxel_psc"`.
#' @param scale_factor Positive constant applied to every voxel and volume in
#'   `run_scalar` mode.
#' @param scale_file Path to the 3D multiplier map used in `voxel_psc` mode.
#' @param core_file Optional path to the fixed 3D reference-region mask. The
#'   BrainGnomes pipeline supplies this to verify the achieved target directly.
#' @param include_frames Optional logical vector with one value per volume.
#'   `TRUE` identifies a volume used to estimate the temporal baselines. The
#'   pipeline supplies the same vector used for `reference_location`.
#' @param tolerance Maximum allowed relative numerical error for each check.
#' @param chunk_size Maximum number of volumes read together. All volumes are
#'   checked; chunking bounds memory independently of run length.
#' @param scratch_directory Writable directory for temporary decompressed
#'   images. Compressed images are streamed to disk once so successive chunks
#'   do not repeatedly decompress the beginning of a long run. Copies are
#'   removed on success or failure. Defaults to the R temporary directory.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   The `message` attribute gives a readable summary. The `details` attribute
#'   reports the target, multiplier, expected and remeasured reference
#'   intensities, and relative multiplication errors.
#'
#' @keywords internal
#' @importFrom RNifti readNifti
validate_intensity_normalize <- function(pre_file, post_file,
                                         reference_location, target,
                                         scale_factor = NULL,
                                         mode = "run_scalar",
                                         scale_file = NULL,
                                         core_file = NULL,
                                         include_frames = NULL,
                                         tolerance = 1e-5,
                                         chunk_size = 8L,
                                         scratch_directory = tempdir()) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_choice(mode, c("run_scalar", "voxel_psc"))
  checkmate::assert_number(reference_location, finite = TRUE)
  checkmate::assert_number(target, finite = TRUE)
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)

  pre_post_grid <- pp_compare_nifti_grid(
    pre_file, post_file, "pre", "post", tolerance = tolerance
  )
  if (!isTRUE(pre_post_grid$passed)) return(pp_grid_failure(pre_post_grid))

  if (reference_location <= 0 || target <= 0) {
    out <- FALSE
    attr(out, "message") <- "Run reference intensity and target must be positive."
    attr(out, "details") <- list(
      reference_location = reference_location,
      target = target
    )
    return(out)
  }

  if (identical(mode, "run_scalar")) {
    checkmate::assert_number(scale_factor, finite = TRUE)
    if (scale_factor <= 0) {
      out <- FALSE
      attr(out, "message") <- "The run-wise intensity multiplier must be positive."
      attr(out, "details") <- list(scale_factor = scale_factor)
      return(out)
    }
  } else {
    checkmate::assert_file_exists(scale_file)
    pre_scale_grid <- pp_compare_nifti_grid(
      pre_file, scale_file, "pre", "PSC scale map", tolerance = tolerance
    )
    if (!isTRUE(pre_scale_grid$passed)) return(pp_grid_failure(pre_scale_grid))
    if (abs(target - 100) > tolerance) {
      out <- FALSE
      attr(out, "message") <- "voxel_psc requires a fixed target of 100."
      attr(out, "details") <- list(target = target)
      return(out)
    }
  }

  if (identical(mode, "run_scalar") && !is.null(core_file)) {
    if (!checkmate::test_string(core_file, min.chars = 1L)) {
      out <- FALSE
      attr(out, "message") <-
        "Intensity-reference core must be NULL or a nonempty path."
      attr(out, "details") <- list(core_file = core_file)
      return(out)
    }
    checkmate::assert_file_exists(core_file)
    pre_core_grid <- pp_compare_nifti_grid(
      pre_file, core_file, "pre", "intensity-reference core",
      tolerance = tolerance
    )
    if (!isTRUE(pre_core_grid$passed)) return(pp_grid_failure(pre_core_grid))
  }

  checkmate::assert_count(chunk_size, positive = TRUE)
  pre_dims <- pp_nifti_dims4(pre_file)
  post_dims <- pp_nifti_dims4(post_file)
  if (!identical(pre_dims, post_dims)) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      "Pre/post dimensions mismatch: [%s] vs [%s].",
      paste(pre_dims, collapse = "x"), paste(post_dims, collapse = "x")
    )
    attr(out, "details") <- list(pre_dim = pre_dims, post_dim = post_dims)
    return(out)
  }

  expected_target <- NA_real_
  target_relative_error <- NA_real_
  if (identical(mode, "run_scalar")) {
    expected_target <- reference_location * scale_factor
    target_relative_error <- abs(expected_target - target) /
      max(1, abs(target))
    multipliers <- scale_factor
  } else {
    scale_map <- RNifti::readNifti(scale_file)
    if (!identical(as.integer(dim(scale_map)), pre_dims[1:3])) {
      out <- FALSE
      attr(out, "message") <- sprintf(
        "PSC scale-map dimensions [%s] do not match BOLD spatial dimensions [%s].",
        paste(dim(scale_map), collapse = "x"),
        paste(pre_dims[1:3], collapse = "x")
      )
      attr(out, "details") <- list(
        scale_dim = dim(scale_map), bold_spatial_dim = pre_dims[1:3]
      )
      return(out)
    }
    multipliers <- as.numeric(scale_map)
    rm(scale_map)
    if (any(!is.finite(multipliers)) || any(multipliers <= 0)) {
      out <- FALSE
      attr(out, "message") <- "PSC multiplier map contains nonfinite or nonpositive values."
      attr(out, "details") <- list()
      return(out)
    }
  }

  # The spatial multiplier repeats down each matrix column without allocating
  # a 4D multiplier array. Read every volume, including the final partial chunk.
  value_relative_error <- 0
  finite_values <- 0
  volume_chunks <- split(
    seq_len(pre_dims[4]), ceiling(seq_len(pre_dims[4]) / chunk_size)
  )
  # Gzip cannot seek cheaply to later volumes. Decompress each input once to
  # scratch with a fixed buffer, then release both copies on every exit path.
  staged_files <- character()
  on.exit(unlink(staged_files), add = TRUE)
  pre_chunk_file <- pp_stage_nifti_for_chunks(pre_file, scratch_directory)
  if (!identical(pre_chunk_file, pre_file)) staged_files <- c(staged_files, pre_chunk_file)
  post_chunk_file <- pp_stage_nifti_for_chunks(post_file, scratch_directory)
  if (!identical(post_chunk_file, post_file)) staged_files <- c(staged_files, post_chunk_file)
  invisible(gc(FALSE))
  for (volumes in volume_chunks) {
    pre_values <- pp_read_volume_matrix(pre_chunk_file, volumes, pre_dims[1:3])
    post_values <- pp_read_volume_matrix(post_chunk_file, volumes, pre_dims[1:3])
    finite <- is.finite(pre_values)
    if (!identical(finite, is.finite(post_values))) {
      out <- FALSE
      attr(out, "message") <- "Pre/post finite-value patterns differ after intensity normalization."
      attr(out, "details") <- list(volume_indices = volumes)
      return(out)
    }
    finite_values <- finite_values + sum(finite)
    if (any(finite)) {
      expected_values <- (pre_values * multipliers)[finite]
      value_relative_error <- max(value_relative_error, max(
        abs(post_values[finite] - expected_values) / pmax(1, abs(expected_values))
      ))
      rm(expected_values)
    }
    rm(pre_values, post_values, finite)
    invisible(gc(FALSE))
  }
  if (finite_values == 0) {
    out <- FALSE
    attr(out, "message") <- "No jointly finite pre/post values are available for validation."
    attr(out, "details") <- list()
    return(out)
  }
  observed_target <- NA_real_
  observed_target_relative_error <- NA_real_
  if (identical(mode, "run_scalar") && !is.null(core_file)) {
    checkmate::assert_file_exists(core_file)
    observed <- measure_reference_location(
      img = post_file,
      core_mask = core_file,
      include_frames = include_frames,
      baseline_method = "trimmed_mean",
      baseline_trim = 0.10,
      min_valid_frames = 20L
    )
    observed_target <- unname(observed$reference_location)
    observed_target_relative_error <- abs(observed_target - target) /
      max(1, abs(target))
  }
  observed_ok <- is.na(observed_target_relative_error) ||
    observed_target_relative_error <= tolerance
  target_ok <- is.na(target_relative_error) || target_relative_error <= tolerance
  passed <- target_ok && value_relative_error <= tolerance && observed_ok
  if (identical(mode, "run_scalar")) {
    msg <- sprintf(
      paste0(
        "Fixed multiplier %.8g maps run reference intensity %.8g to %.8g ",
        "(target %.8g); remeasured normalized reference intensity %s; ",
        "maximum relative multiplication error %.6g (tol %.6g)."
      ),
      scale_factor, reference_location, expected_target, target,
      if (is.na(observed_target)) "not requested" else format(observed_target, digits = 8),
      value_relative_error, tolerance
    )
  } else {
    msg <- sprintf(
      paste0(
        "Denominator-guarded voxelwise PSC map is finite, positive, and ",
        "matches the BOLD spatial grid; no clipping or masking was applied; ",
        "maximum relative multiplication error %.6g (tol %.6g)."
      ),
      value_relative_error, tolerance
    )
  }
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- list(
    reference_location = reference_location,
    target = target,
    mode = mode,
    scale_factor = scale_factor,
    scale_file = scale_file,
    expected_target = expected_target,
    target_relative_error = target_relative_error,
    observed_target = observed_target,
    observed_target_relative_error = observed_target_relative_error,
    value_relative_error = value_relative_error,
    volumes_compared = pre_dims[4],
    chunk_size = as.integer(chunk_size)
  )
  return(out)
}

#' Empirical calibration models for classic FWHM after spatial smoothing
#'
#' The Gaussian and diagnostic no-mask models were derived from three real-BOLD
#' datasets spanning 2.41--3.12 mm voxels. The production masked-SUSAN model was
#' subsequently recalibrated on three fMRIPrep cohorts by invoking
#' `spatial_smooth()` directly with the same `automask()` configuration as
#' `postprocess_subject()`. The final subject from each dataset was held out, and
#' leave-one-dataset-out checks were used to assess transfer across resolutions.
#' The definitive SUSAN calibration evaluates raw, detrended, and
#' detrended-plus-MAD estimators instead of assuming one preparation a priori,
#' and crosses the threshold mask with the mask already applied to the BOLD.
#' The promoted models reproduce the full-run SUSAN threshold, temporal mean,
#' and extents while estimating smoothness from 96 timepoints distributed over
#' the complete run. Each model stores its exact estimator, mask condition, and
#' volume-sampling rule; these cannot be changed independently of its
#' coefficients.
#'
#' The primary model predicts post-smoothing FWHM by Gaussian quadrature while
#' allowing the program's effective kernel gain to depend on the dimensionless
#' voxel-to-kernel ratio:
#' `gain = coeffs[1] + coeffs[2] * voxel_mm / kernel_mm` and
#' `post = sqrt(pre^2 + (gain * kernel_mm)^2)`.
#'
#' Structure: `smoother -> method -> mask/nomask -> model`
#'   - `type = "quadrature_ratio_linear"`: baseline-conditioned model above
#'   - `type = "linear"`: `coeffs[1] + coeffs[2] * kernel`
#'   - `type = "poly"`:   polynomial in kernel (`sum(coeffs * kernel^(0:p))`)
#'
#' @keywords internal
#' @noRd
pp_calibration_coeffs <- list(
  gaussian = list(
    classic = list(
      mask = list(
        model_version = "smoothness-calibration-v1",
        type = "quadrature_ratio_linear", coeffs = c(1.18475396, -0.47521770),
        tolerance_mm = 0.8, mode = "afni_blurinmask",
        kernel_range_mm = c(3, 8), voxel_range_mm = c(2.408688, 3.116644),
        estimator = "detrend_mad", preprocess = TRUE, polydeg = 3L,
        demean = TRUE, unif = TRUE
      ),
      nomask = list(
        model_version = "smoothness-calibration-v1",
        type = "quadrature_ratio_linear", coeffs = c(1.13001562, -0.11747280),
        tolerance_mm = 1.0, mode = "afni_3dmerge",
        kernel_range_mm = c(3, 8), voxel_range_mm = c(2.408688, 3.116644),
        estimator = "detrend_mad", preprocess = TRUE, polydeg = 3L,
        demean = TRUE, unif = TRUE
      )
    )
  ),
  susan = list(
    classic = list(
      mask = list(
        none = list(
          model_version = "smoothness-calibration-v4-fullcontext-distributed96-k3-8",
          input_mask = "none",
          type = "quadrature_ratio_linear",
          coeffs = c(1.27820370989578, -0.520302740093367),
          tolerance_mm = 0.7, mode = "fsl_susan_mask",
          kernel_range_mm = c(3, 8),
          voxel_range_mm = c(2.40865896, 3.11664432),
          max_volumes = 96L, volume_sampling = "distributed_full_run",
          smoothing_context = "full_run",
          estimator = "detrend_mad", preprocess = TRUE, polydeg = 3L,
          demean = TRUE, unif = TRUE
        ),
        fmriprep = list(
          model_version = "smoothness-calibration-v4-fullcontext-distributed96-k3-8",
          input_mask = "fmriprep",
          type = "quadrature_ratio_linear",
          coeffs = c(1.23406155442799, -0.451054336264315),
          tolerance_mm = 0.8, mode = "fsl_susan_mask",
          kernel_range_mm = c(3, 8),
          voxel_range_mm = c(2.40865896, 3.11664432),
          max_volumes = 96L, volume_sampling = "distributed_full_run",
          smoothing_context = "full_run",
          estimator = "detrend_mad", preprocess = TRUE, polydeg = 3L,
          demean = TRUE, unif = TRUE
        ),
        template = list(
          model_version = "smoothness-calibration-v4-fullcontext-distributed96-k3-8",
          input_mask = "template",
          type = "quadrature_ratio_linear",
          coeffs = c(1.16494890257513, -0.381080413733339),
          support_version = "v4-pp-entrypoints-2mm-k5-2026-10-05",
          external_support = list(list(
            kernel_range_mm = c(5, 5), voxel_range_mm = c(2, 2),
            voxel_spacing_mm = c(2, 2, 2),
            source = "pp_entrypoints_2026-10-05",
            n_validation = 50L, n_subjects = 10L,
            max_abs_error_mm = 0.409227471374049
          )),
          tolerance_mm = 0.6, mode = "fsl_susan_mask",
          kernel_range_mm = c(3, 8),
          voxel_range_mm = c(2.40865896, 3.11664432),
          max_volumes = 96L, volume_sampling = "distributed_full_run",
          smoothing_context = "full_run",
          estimator = "detrend_mad", preprocess = TRUE, polydeg = 3L,
          demean = TRUE, unif = TRUE
        )
      ),
      nomask = list(
        model_version = "smoothness-calibration-v1",
        type = "quadrature_ratio_linear", coeffs = c(-0.06022293, 0.80840240),
        tolerance_mm = 0.5, mode = "fsl_susan_nomask",
        kernel_range_mm = c(3, 8), voxel_range_mm = c(2.408688, 3.116644),
        estimator = "detrend_mad", preprocess = TRUE, polydeg = 3L,
        demean = TRUE, unif = TRUE
      )
    )
  )
)

#' Resolve the exact estimator preparation stored with a calibration model
#' @keywords internal
#' @noRd
pp_calibration_preparation <- function(model = NULL, preprocess = NULL,
                                        polydeg = NULL, demean = NULL,
                                        unif = NULL) {
  calibrated <- !is.null(model)
  expected <- list(
    preprocess = if (calibrated && !is.null(model$preprocess)) {
      isTRUE(model$preprocess)
    } else {
      TRUE
    },
    polydeg = if (calibrated && !is.null(model$polydeg)) {
      as.integer(model$polydeg)
    } else {
      3L
    },
    demean = if (calibrated && !is.null(model$demean)) isTRUE(model$demean) else TRUE,
    unif = if (calibrated && !is.null(model$unif)) isTRUE(model$unif) else TRUE
  )
  supplied <- list(
    preprocess = preprocess, polydeg = polydeg, demean = demean, unif = unif
  )
  if (calibrated) {
    mismatched <- names(supplied)[vapply(names(supplied), function(name) {
      !is.null(supplied[[name]]) &&
        !isTRUE(all.equal(supplied[[name]], expected[[name]], check.attributes = FALSE))
    }, logical(1))]
    if (length(mismatched)) {
      stop(
        "Calibration model '", model$model_version,
        "' requires estimator '", model$estimator,
        "'; incompatible override(s): ", paste(mismatched, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }
  resolved <- lapply(names(expected), function(name) {
    if (is.null(supplied[[name]])) expected[[name]] else supplied[[name]]
  })
  names(resolved) <- names(expected)
  checkmate::assert_flag(resolved$preprocess)
  if (isTRUE(resolved$preprocess)) checkmate::assert_count(resolved$polydeg)
  checkmate::assert_flag(resolved$demean)
  checkmate::assert_flag(resolved$unif)
  resolved$estimator <- if (calibrated && !is.null(model$estimator)) {
    model$estimator
  } else if (!isTRUE(resolved$preprocess)) {
    "raw"
  } else if (isTRUE(resolved$unif)) {
    "detrend_mad"
  } else {
    "detrend"
  }
  resolved
}

#' Predict the expected FWHM delta from a calibration model
#' @keywords internal
#' @noRd
pp_calibration_gain <- function(model, kernel_fwhm, voxel_mm) {
  checkmate::assert_number(kernel_fwhm, lower = 1e-6, finite = TRUE)
  checkmate::assert_numeric(voxel_mm, lower = 1e-6, finite = TRUE, min.len = 1L)
  voxel_geom_mm <- exp(mean(log(voxel_mm)))
  gain <- if (identical(model$type, "quadrature_ratio_linear")) {
    model$coeffs[[1]] + model$coeffs[[2]] * voxel_geom_mm / kernel_fwhm
  } else {
    stop("Calibration model does not define a quadrature gain: ", model$type,
         call. = FALSE)
  }
  if (!is.finite(gain) || gain <= 0) {
    stop("Calibration predicts a non-positive effective kernel gain.", call. = FALSE)
  }
  gain
}

#' @keywords internal
#' @noRd
pp_predict_calibration <- function(model, kernel_fwhm, pre_fwhm = NULL,
                                    voxel_mm = NULL) {
  coeffs <- model$coeffs
  if (model$type == "quadrature_ratio_linear") {
    checkmate::assert_number(pre_fwhm, lower = 0, finite = TRUE)
    gain <- pp_calibration_gain(model, kernel_fwhm, voxel_mm)
    expected_post <- sqrt(pre_fwhm^2 + (gain * kernel_fwhm)^2)
    return(expected_post - pre_fwhm)
  } else if (model$type == "linear") {
    return(coeffs[1] + coeffs[2] * kernel_fwhm)
  } else if (model$type == "poly") {
    powers <- seq(0, length(coeffs) - 1)
    return(sum(coeffs * kernel_fwhm^powers))
  } else {
    stop("Unknown calibration model type: ", model$type, call. = FALSE)
  }
}

#' Classify smoothing support in the fitted domain or an external validation case
#'
#' @param model Selected mask-specific calibration model.
#' @param kernel_fwhm Requested smoothing kernel in millimeters.
#' @param voxel_mm Spatial voxel sizes in millimeters.
#' @return One of `interpolated`, `externally_validated`, or `EXTRAPOLATED`.
#' @noRd
pp_calibration_support <- function(model, kernel_fwhm, voxel_mm) {
  if (isTRUE(model$input_mask_extrapolated)) return("EXTRAPOLATED")
  voxel_geom_mm <- exp(mean(log(voxel_mm)))
  # NIfTI spacing is stored in floating point; permit representation rounding
  # at the documented boundaries without widening the scientific domain.
  # Check numeric values against one inclusive support interval.
  in_range <- function(value, bounds) {
    all(value >= bounds[1] - 1e-6 & value <= bounds[2] + 1e-6)
  }
  if (in_range(kernel_fwhm, model$kernel_range_mm) &&
      in_range(voxel_geom_mm, model$voxel_range_mm)) return("interpolated")
  for (support in model$external_support) {
    if (in_range(kernel_fwhm, support$kernel_range_mm) &&
        in_range(voxel_geom_mm, support$voxel_range_mm) &&
        (is.null(support$voxel_spacing_mm) ||
         all(abs(voxel_mm - support$voxel_spacing_mm) <= 1e-6))) {
      return("externally_validated")
    }
  }
  "EXTRAPOLATED"
}

#' Select the calibration model for a given smoother and mask usage
#' @keywords internal
#' @noRd
pp_select_calibration <- function(smoother, used_mask, input_mask = "none") {
  checkmate::assert_choice(input_mask, c("none", "fmriprep", "template", "custom"))
  smooth_entry <- pp_calibration_coeffs[[smoother]]
  if (is.null(smooth_entry)) {
    warning("No calibration table for smoother '", smoother,
            "'; falling back to gaussian.", call. = FALSE)
    smooth_entry <- pp_calibration_coeffs[["gaussian"]]
  }
  method_entry <- smooth_entry[["classic"]]
  if (is.null(method_entry)) {
    stop("No classic calibration for smoother '", smoother, "'.", call. = FALSE)
  }
  key <- if (isTRUE(used_mask)) "mask" else "nomask"
  model <- method_entry[[key]]
  if (is.null(model)) {
    fallback_key <- setdiff(c("mask", "nomask"), key)
    model <- method_entry[[fallback_key]]
    if (is.null(model)) {
      stop("Calibration entry for '", smoother, "' classic is malformed.", call. = FALSE)
    }
    warning("Calibration '", key, "' missing; using '", fallback_key, "' fallback.", call. = FALSE)
  }
  calibrated_input_mask <- if (!is.null(model$input_mask)) {
    model$input_mask
  } else {
    "none"
  }
  input_mask_extrapolated <- FALSE
  if (is.null(model$type)) {
    input_key <- input_mask
    if (identical(input_key, "custom") || is.null(model[[input_key]])) {
      fallback_input <- if (!is.null(model$template)) {
        "template"
      } else if (!is.null(model$fmriprep)) {
        "fmriprep"
      } else {
        "none"
      }
      warning(
        "No exact smoothness calibration for input mask '", input_key,
        "'; using '", fallback_input, "' as an extrapolation.",
        call. = FALSE
      )
      input_key <- fallback_input
      input_mask_extrapolated <- TRUE
    }
    model <- model[[input_key]]
    calibrated_input_mask <- input_key
  } else if (!identical(input_mask, calibrated_input_mask)) {
    warning(
      "No exact smoothness calibration for input mask '", input_mask,
      "'; using '", calibrated_input_mask, "' as an extrapolation.",
      call. = FALSE
    )
    input_mask_extrapolated <- TRUE
  }
  if (is.null(model) || is.null(model$type)) {
    stop("Calibration entry for input mask '", input_mask, "' is malformed.",
         call. = FALSE)
  }
  model$calibrated_input_mask <- calibrated_input_mask
  model$input_mask_extrapolated <- input_mask_extrapolated
  model
}

#' Validate spatial smoothing (classic FWHM pre vs post, calibration-corrected)
#'
#' Measures the observed FWHM change using `estimate_classic_fwhm()` and compares
#' it to the calibration-predicted delta for the requested kernel size. The
#' calibration accounts for the fact that fMRI data are non-Gaussian and the
#' naive first-differences FWHM estimate has a systematic bias that depends on
#' smoother type and whether masking was used. The calibrated preprocessing mode
#' is selected together with the calibration model. The production masked-SUSAN
#' calibration stores and enforces the estimator preparation selected by
#' cross-dataset validation; the estimator is not chosen at validation time.
#'
#' Classic first-difference FWHM is used intentionally as a local
#' gradient-variance statistic. A mixed Gaussian-plus-exponential ACF can
#' describe fMRI's longer spatial tail, but its scalar half-height FWHM does not
#' obey Gaussian quadrature when a Gaussian kernel is added: the fitted core and
#' tail change differently. The real-BOLD calibration therefore absorbs the
#' non-Gaussian core behavior without treating the full ACF as Gaussian.
#'
#' Optional preprocessing can remove a low-order polynomial trend, the temporal
#' mean, and voxelwise temporal scale before the classic estimate. It is retained
#' for diagnostic and legacy calibration use, but it must match the selected
#' calibration. `preprocess = NULL` enforces that model-specific choice.
#' Requests outside the model's fitted domain and explicit external support
#' are reported as extrapolations and cannot pass validation. The template-mask model also
#' supports the externally validated 5 mm kernel on isotropic 2 mm images;
#' this case does not extend support to other kernels at that resolution.
#'
#' @param pre_file Path to 4D BOLD before `spatial_smooth`.
#' @param post_file Path to 4D BOLD after `spatial_smooth`.
#' @param mask_file 3D mask (same space as BOLD).
#' @param fwhm_mm Requested smoothing kernel FWHM in mm (`cfg$spatial_smooth$fwhm_mm`).
#' @param smoother Character; `"susan"` (default, matches `spatial_smooth()`) or `"gaussian"`.
#' @param used_mask Logical; whether a mask was used to calculate SUSAN's
#'   brightness threshold (default `TRUE`). SUSAN itself is not spatially
#'   restricted to this mask. For Gaussian calibration modes this instead
#'   distinguishes masked `3dBlurInMask` from unmasked `3dmerge`.
#' @param input_mask Character input-mask condition: `"none"`, `"fmriprep"`,
#'   `"template"`, or `"custom"`. This describes masking already applied to
#'   the BOLD before smoothing, independently of `used_mask`. A model from a
#'   different input-mask condition is reported as an extrapolation but cannot
#'   pass validation, because masking materially changes the calibrated gain.
#' @param tolerance_mm Tolerance in mm for `|observed_post - expected_post|`.
#'   `NULL` (the default) uses the program/mask-specific cross-validation
#'   tolerance stored with the calibration model.
#' @param preprocess Logical or `NULL`. `NULL` (the default) uses the preprocessing
#'   mode recorded by the selected calibration model. An explicit value must
#'   match that model; without `fwhm_mm`, `TRUE` applies diagnostic detrending.
#' @param polydeg Optional polynomial detrending degree. `NULL` uses the model.
#' @param demean Optional logical mean-removal setting. `NULL` uses the model.
#' @param unif Optional logical temporal-MAD scaling setting. `NULL` uses the model.
#' @param max_volumes Maximum number of timepoints used for validation.
#'   Timepoints are deterministically distributed over the complete run;
#'   shorter runs use every timepoint, and `Inf` uses all volumes. A calibrated
#'   model may require its stored cap and reject a different override.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details` (pre/post/delta/expected_delta/diff FWHM mm).
#'
#' @keywords internal
validate_spatial_smooth <- function(pre_file, post_file, mask_file, fwhm_mm = NA_real_,
                                    smoother = "susan", used_mask = TRUE,
                                    input_mask = "none",
                                    tolerance_mm = NULL, preprocess = NULL,
                                    polydeg = NULL, demean = NULL, unif = NULL,
                                    max_volumes = 96L) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(mask_file)

  pre_post_grid <- pp_compare_nifti_grid(
    pre_file, post_file, "pre", "post"
  )
  if (!isTRUE(pre_post_grid$passed)) return(pp_grid_failure(pre_post_grid))
  pre_mask_grid <- pp_compare_nifti_grid(
    pre_file, mask_file, "pre", "smoothness mask"
  )
  if (!isTRUE(pre_mask_grid$passed)) return(pp_grid_failure(pre_mask_grid))

  msk <- RNifti::readNifti(mask_file)
  mask_logical <- (msk != 0) & is.finite(msk)
  pre_header <- RNifti::niftiHeader(pre_file)
  post_header <- RNifti::niftiHeader(post_file)
  pre_dim <- as.integer(pre_header$dim[2:5])
  post_dim <- as.integer(post_header$dim[2:5])

  if (!all(pre_dim[1:3] == post_dim[1:3]) ||
      !all(pre_dim[1:3] == dim(msk))) {
    out <- FALSE
    attr(out, "message") <- "Pre/post/mask spatial dimensions mismatch."
    attr(out, "details") <- list()
    return(out)
  }
  if (pre_dim[4] != post_dim[4]) {
    out <- FALSE
    attr(out, "message") <- "Pre and post must have same number of timepoints."
    attr(out, "details") <- list()
    return(out)
  }

  vox_mm <- pp_pixdim_mm(pre_file)
  has_kernel <- checkmate::test_number(fwhm_mm, lower = 1e-6, finite = TRUE)
  cal_model <- if (has_kernel) {
    pp_select_calibration(smoother, used_mask, input_mask = input_mask)
  } else {
    NULL
  }
  if (has_kernel && !is.null(cal_model$max_volumes)) {
    supplied_cap <- suppressWarnings(as.integer(max_volumes))
    if (length(supplied_cap) != 1L || is.na(supplied_cap) ||
        !identical(supplied_cap, as.integer(cal_model$max_volumes))) {
      stop(
        "Calibration model '", cal_model$model_version,
        "' requires max_volumes=", as.integer(cal_model$max_volumes), ".",
        call. = FALSE
      )
    }
  }
  preparation <- pp_calibration_preparation(
    cal_model, preprocess = preprocess, polydeg = polydeg,
    demean = demean, unif = unif
  )
  estimate_file <- function(path) {
    pp_estimate_classic_smoothness_file(
      path, mask_logical, max_volumes = max_volumes,
      preprocess = preparation$preprocess,
      polydeg = preparation$polydeg, demean = preparation$demean,
      unif = preparation$unif
    )
  }
  pre_estimate <- estimate_file(pre_file)
  pre_f <- pre_estimate$geom
  volumes_used <- pre_estimate$volumes_used
  total_volumes <- pre_estimate$total_volumes
  volume_indices <- pre_estimate$volume_indices
  volume_sampling <- pre_estimate$volume_sampling
  rm(pre_estimate)
  invisible(gc(FALSE))
  post_f <- estimate_file(post_file)$geom

  if (!is.finite(pre_f) || !is.finite(post_f) || pre_f <= 0 || post_f <= 0) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      "Could not estimate classic FWHM (pre=%s, post=%s).",
      format(pre_f, digits = 5), format(post_f, digits = 5)
    )
    attr(out, "details") <- list(pre_fwhm_mm = pre_f, post_fwhm_mm = post_f)
    return(out)
  }

  delta_observed <- post_f - pre_f

  # --- calibration-based comparison ---
  if (has_kernel) {
    delta_expected <- pp_predict_calibration(
      cal_model, fwhm_mm, pre_fwhm = pre_f, voxel_mm = vox_mm
    )
    expected_post <- pre_f + delta_expected
    calibration_gain <- pp_calibration_gain(cal_model, fwhm_mm, vox_mm)
    calibration_support <- pp_calibration_support(cal_model, fwhm_mm, vox_mm)
    calibration_extrapolated <- identical(calibration_support, "EXTRAPOLATED")
    if (is.null(tolerance_mm)) tolerance_mm <- cal_model$tolerance_mm
    checkmate::assert_number(tolerance_mm, lower = 0, finite = TRUE)
    diff_cal <- delta_observed - delta_expected
    within_tol <- abs(diff_cal) <= tolerance_mm
    exact_input_mask_calibration <-
      !isTRUE(cal_model$input_mask_extrapolated)
    passed <- within_tol && !calibration_extrapolated
    status <- if (!exact_input_mask_calibration) {
      "FAIL (no exact input-mask calibration)"
    } else if (calibration_extrapolated) {
      "FAIL (outside calibration support)"
    } else if (passed) {
      "PASS"
    } else {
      "FAIL"
    }
    msg <- sprintf(
      paste0(
        "Classic geom FWHM: pre=%.4f mm, post=%.4f mm, delta_observed=%.4f mm. ",
        "Expected post=%.4f mm, delta=%.4f mm (smoother=%s, threshold_mask=%s, input_mask=%s, model=%s, estimator=%s, version=%s). ",
        "Volumes used=%d/%d. Calibration support=%s. ",
        "|obs-exp|=%.4f mm (tol=%.4f mm). %s"
      ),
      pre_f, post_f, delta_observed,
      expected_post, delta_expected, smoother, used_mask, input_mask, cal_model$type,
      preparation$estimator, cal_model$model_version,
      volumes_used, total_volumes,
      calibration_support,
      abs(diff_cal), tolerance_mm,
      status
    )
    details <- list(
      pre_fwhm_mm = pre_f,
      post_fwhm_mm = post_f,
      delta_observed_mm = delta_observed,
      post_expected_mm = expected_post,
      delta_expected_mm = delta_expected,
      delta_diff_mm = diff_cal,
      tolerance_mm = tolerance_mm,
      smoother = smoother,
      used_mask = used_mask,
      input_mask = input_mask,
      calibration_input_mask = cal_model$calibrated_input_mask,
      calibration_input_mask_extrapolated =
        isTRUE(cal_model$input_mask_extrapolated),
      exact_input_mask_calibration = exact_input_mask_calibration,
      within_tolerance = within_tol,
      calibration_mode = cal_model$mode,
      calibration_model_version = cal_model$model_version,
      calibration_type = cal_model$type,
      calibration_estimator = preparation$estimator,
      calibration_coeffs = cal_model$coeffs,
      calibration_gain = calibration_gain,
      expected_effective_kernel_mm = calibration_gain * fwhm_mm,
      observed_effective_kernel_mm = sqrt(max(0, post_f^2 - pre_f^2)),
      calibration_kernel_range_mm = cal_model$kernel_range_mm,
      calibration_voxel_range_mm = cal_model$voxel_range_mm,
      calibration_max_volumes = cal_model$max_volumes,
      calibration_volume_sampling = cal_model$volume_sampling,
      calibration_smoothing_context = cal_model$smoothing_context,
      calibration_extrapolated = calibration_extrapolated,
      calibration_support = calibration_support,
      calibration_support_version = cal_model$support_version,
      calibration_external_support = cal_model$external_support,
      voxel_spacing_mm = vox_mm,
      voxel_geom_mm = exp(mean(log(vox_mm))),
      volumes_used = volumes_used,
      total_volumes = total_volumes,
      max_volumes = max_volumes,
      volume_indices = volume_indices,
      volume_sampling = volume_sampling,
      preprocessing = list(
        enabled = isTRUE(preparation$preprocess),
        polydeg = preparation$polydeg,
        demean = isTRUE(preparation$demean),
        unif = isTRUE(preparation$unif)
      )
    )
  } else {
    # no kernel specified: just check that smoothness did not decrease
    passed <- delta_observed >= 0
    msg <- sprintf(
      paste0(
        "Classic geom FWHM: pre=%.4f mm, post=%.4f mm, delta=%.4f mm. ",
        "Volumes used=%d/%d (no kernel specified; directional check only)."
      ),
      pre_f, post_f, delta_observed, volumes_used, total_volumes
    )
    details <- list(
      pre_fwhm_mm = pre_f,
      post_fwhm_mm = post_f,
      delta_observed_mm = delta_observed,
      volumes_used = volumes_used,
      total_volumes = total_volumes,
      max_volumes = max_volumes,
      volume_indices = volume_indices,
      volume_sampling = volume_sampling,
      preprocessing = list(
        enabled = isTRUE(preparation$preprocess),
        polydeg = preparation$polydeg,
        demean = isTRUE(preparation$demean),
        unif = isTRUE(preparation$unif)
      )
    )
  }

  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- details
  return(out)
}

#' Deterministically sample voxels and replay regression
#'
#' Shared helper for `validate_apply_aroma` and `validate_confound_regression`.
#' Reads pre/post 4D images, selects up to `n_sample` finite, non-constant
#' pre-step voxels across normalized 3D image space, replays the regression in
#' pure R, and returns the maximum absolute difference. Post-step values never
#' influence sample eligibility.
#'
#' @param pre_file Path to 4D BOLD before the step.
#' @param post_file Path to 4D BOLD after the step.
#' @param X Design matrix (timepoints x regressors).
#' @param include_rows Logical vector of rows used in fitting.
#' @param add_intercept Passed to `lmfit_residuals_mat`.
#' @param preserve_mean Passed to `lmfit_residuals_mat`.
#' @param set_mean Passed to `lmfit_residuals_mat`.
#' @param regress_cols Passed to `lmfit_residuals_mat` (1-based).
#' @param exclusive Passed to `lmfit_residuals_mat`.
#' @param n_sample Number of voxels to sample (default 100).
#' @param mask_file Optional 3D brain mask used to constrain pre-step sampling.
#'
#' @return A list with `max_abs_diff` and `n_sampled`.
#' @keywords internal
#' @noRd
pp_sample_and_replay <- function(pre_file, post_file, X, include_rows,
                                  add_intercept = FALSE,
                                  preserve_mean = FALSE, set_mean = 0.0,
                                  regress_cols = NULL, exclusive = FALSE,
                                  n_sample = 100L, mask_file = NULL) {
  failure <- function(reason, n_sampled = 0L, sampled_indices = integer(),
                      normalized_coords = matrix(numeric(), ncol = 3L),
                      n_nonfinite_post = 0L,
                      n_unexpected_constant_post = 0L) {
    list(
      valid = FALSE, max_abs_diff = Inf, n_sampled = as.integer(n_sampled),
      sampled_indices = sampled_indices,
      normalized_coords = normalized_coords,
      n_nonfinite_post = as.integer(n_nonfinite_post),
      n_unexpected_constant_post = as.integer(n_unexpected_constant_post),
      failure_reason = reason
    )
  }
  checkmate::assert_count(n_sample, positive = TRUE)
  pre_img <- pp_read_4d(pre_file)
  post_img <- pp_read_4d(post_file)
  d <- dim(pre_img)
  if (!identical(d, dim(post_img))) {
    return(failure(sprintf(
      "Pre/post dimensions differ: [%s] vs [%s].",
      paste(d, collapse = "x"), paste(dim(post_img), collapse = "x")
    )))
  }
  nx <- d[1]; ny <- d[2]; nz <- d[3]; nt <- d[4]
  if (!is.matrix(X) || nrow(X) != nt) {
    return(failure(sprintf(
      "Regression design has %d rows but BOLD has %d timepoints.",
      if (is.matrix(X)) nrow(X) else 0L, nt
    )))
  }
  if (length(include_rows) != nt || anyNA(include_rows)) {
    return(failure("Regression inclusion vector does not match BOLD timepoints."))
  }

  use_replay_mask <- !is.null(mask_file)
  if (use_replay_mask &&
      !checkmate::test_string(mask_file, min.chars = 1L)) {
    return(failure("Replay mask path must be a single nonempty string."))
  }
  replay_mask <- rep(TRUE, nx * ny * nz)
  if (use_replay_mask) {
    if (!checkmate::test_file_exists(mask_file)) {
      return(failure("Replay mask file does not exist."))
    }
    replay_mask_image <- RNifti::readNifti(mask_file)
    if (!identical(as.integer(dim(replay_mask_image)), c(nx, ny, nz))) {
      return(failure("Replay mask dimensions do not match the BOLD grid."))
    }
    replay_mask <- as.vector(
      is.finite(replay_mask_image) & replay_mask_image > 0
    )
    if (!any(replay_mask)) {
      return(failure("Replay mask contains no voxels."))
    }
  }

  # Determine replay eligibility from the pre-step matrix only. rowVars() also
  # excludes series containing NA/Inf, while the nonzero requirement avoids
  # spending the replay sample on empty background.
  pre_mat_full <- matrix(as.numeric(pre_img), nrow = nx * ny * nz, ncol = nt)
  pre_variance <- matrixStats::rowVars(pre_mat_full)
  candidate_idx <- which(
    replay_mask & is.finite(pre_variance) &
      pre_variance > 2 * .Machine$double.eps &
      matrixStats::rowAnys(pre_mat_full != 0, na.rm = TRUE)
  )
  if (length(candidate_idx) == 0L) {
    return(failure(
      "No finite, non-constant pre-step voxels are available for replay."
    ))
  }

  sel <- pp_select_spatial_replay_voxels(
    candidate_idx = candidate_idx,
    spatial_dims = c(nx, ny, nz),
    n_voxels = n_sample
  )
  if (!length(sel$indices)) {
    return(failure(
      "No finite, non-constant pre-step voxels are available for replay."
    ))
  }

  # Extract pre and post time series for selected voxels (nt x n_voxels)
  Y_pre <- pre_mat_full[sel$indices, , drop = FALSE]  # n_voxels x nt
  Y_pre <- t(Y_pre)                                    # nt x n_voxels
  Y_post <- matrix(NA_real_, nrow = nt, ncol = length(sel$indices))
  post_mat_full <- matrix(as.numeric(post_img), nrow = nx * ny * nz, ncol = nt)
  Y_post <- t(post_mat_full[sel$indices, , drop = FALSE])

  # Replay regression in pure R
  expected <- lmfit_residuals_mat(
    Y = Y_pre,
    X = X,
    include_rows = include_rows,
    add_intercept = add_intercept,
    preserve_mean = preserve_mean,
    set_mean = set_mean,
    regress_cols = regress_cols,
    exclusive = exclusive
  )

  n_nonfinite_post <- sum(!is.finite(Y_post))
  expected_variable <- vapply(
    seq_len(ncol(expected)),
    function(column) pp_is_valid_series(expected[, column]),
    logical(1)
  )
  post_constant <- vapply(
    seq_len(ncol(Y_post)),
    function(column) {
      series <- Y_post[, column]
      all(is.finite(series)) && !pp_is_valid_series(series)
    },
    logical(1)
  )
  n_unexpected_constant_post <- sum(expected_variable & post_constant)
  if (n_nonfinite_post > 0L || n_unexpected_constant_post > 0L) {
    reason <- paste(
      c(
        if (n_nonfinite_post > 0L) {
          sprintf("%d sampled post-step values are nonfinite", n_nonfinite_post)
        },
        if (n_unexpected_constant_post > 0L) {
          sprintf(
            "%d sampled post-step series are unexpectedly constant",
            n_unexpected_constant_post
          )
        }
      ),
      collapse = "; "
    )
    return(failure(
      reason = reason,
      n_sampled = length(sel$indices),
      sampled_indices = sel$indices,
      normalized_coords = sel$normalized_coords,
      n_nonfinite_post = n_nonfinite_post,
      n_unexpected_constant_post = n_unexpected_constant_post
    ))
  }

  mad_val <- max(abs(Y_post - expected))
  return(list(
    valid = is.finite(mad_val),
    max_abs_diff = mad_val,
    n_sampled = length(sel$indices),
    sampled_indices = sel$indices,
    normalized_coords = sel$normalized_coords,
    n_nonfinite_post = n_nonfinite_post,
    n_unexpected_constant_post = n_unexpected_constant_post,
    failure_reason = if (is.finite(mad_val)) NULL else "Replay difference is nonfinite."
  ))
}

#' Validate AROMA by deterministic spatial replay
#'
#' Selects approximately 100 pre-step voxels across normalized image space and
#' replays the AROMA regression via `lmfit_residuals_mat`; passes if the maximum
#' absolute difference is below 0.05. If there are no noise ICs, validation
#' instead requires the complete post-step image to be unchanged.
#'
#' @param pre_file Path to 4D BOLD before `apply_aroma`.
#' @param post_file Path to 4D BOLD after `apply_aroma`.
#' @param mixing_file MELODIC mixing matrix (no header).
#' @param noise_ics Noise IC indices (1-based), same as pipeline.
#' @param nonaggressive Same as `apply_aroma`.
#' @param n_sample Number of voxels to sample (default 100).
#' @param mask_file Optional 3D brain mask used to constrain pre-step sampling.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details`.
#'
#' @keywords internal
validate_apply_aroma <- function(pre_file, post_file, mixing_file, noise_ics,
                                 nonaggressive = TRUE, n_sample = 100L,
                                 mask_file = NULL) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(mixing_file)

  pre_post_grid <- pp_compare_nifti_grid(
    pre_file, post_file, "pre", "post"
  )
  if (!isTRUE(pre_post_grid$passed)) return(pp_grid_failure(pre_post_grid))
  has_mask <- checkmate::test_string(mask_file, min.chars = 1L)
  if (!is.null(mask_file) && !has_mask) {
    out <- FALSE
    attr(out, "message") <- "AROMA replay mask must be NULL or a nonempty path."
    attr(out, "details") <- list(failure_reason = "Invalid replay mask path.")
    return(out)
  }
  if (has_mask) {
    checkmate::assert_file_exists(mask_file)
    pre_mask_grid <- pp_compare_nifti_grid(
      pre_file, mask_file, "pre", "AROMA replay mask"
    )
    if (!isTRUE(pre_mask_grid$passed)) return(pp_grid_failure(pre_mask_grid))
  }

  if (is.null(noise_ics) || length(noise_ics) == 0L) {
    unchanged <- pp_compare_nifti_identity(pre_file, post_file)
    out <- isTRUE(unchanged$passed)
    attr(out, "message") <- paste0(
      "No noise ICs; verified that AROMA was a no-op. ", unchanged$message
    )
    unchanged$no_op <- TRUE
    unchanged$skipped <- FALSE
    attr(out, "details") <- unchanged
    return(out)
  }

  if (!checkmate::test_integerish(
    noise_ics, lower = 1L, any.missing = FALSE
  )) {
    out <- FALSE
    attr(out, "message") <-
      "AROMA noise IC indices must be finite positive integers."
    attr(out, "details") <- list(
      skipped = FALSE, failure_reason = "Invalid noise IC values."
    )
    return(out)
  }

  mixing_mat <- as.matrix(data.table::fread(mixing_file, header = FALSE, data.table = FALSE))
  storage.mode(mixing_mat) <- "double"
  requested_idx <- sort(unique(as.integer(noise_ics)))
  invalid_idx <- requested_idx[requested_idx > ncol(mixing_mat)]
  comp_idx <- setdiff(requested_idx, invalid_idx)
  if (length(comp_idx) == 0L) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      paste0(
        "No requested AROMA noise IC indices are valid for a %d-column ",
        "mixing matrix; requested: %s."
      ),
      ncol(mixing_mat), paste(requested_idx, collapse = ", ")
    )
    attr(out, "details") <- list(
      skipped = FALSE, requested_noise_ics = requested_idx,
      invalid_noise_ics = invalid_idx,
      failure_reason = "No valid noise IC indices."
    )
    return(out)
  }

  exclusive_flag <- !isTRUE(nonaggressive)
  include_rows <- rep(TRUE, nrow(mixing_mat))

  replay <- pp_sample_and_replay(
    pre_file = pre_file,
    post_file = post_file,
    X = mixing_mat,
    include_rows = include_rows,
    add_intercept = TRUE,
    preserve_mean = TRUE,
    set_mean = 0.0,
    regress_cols = comp_idx,
    exclusive = exclusive_flag,
    n_sample = n_sample,
    mask_file = mask_file
  )

  mad <- replay$max_abs_diff
  tol <- 0.05
  passed <- isTRUE(replay$valid) && replay$n_sampled > 0L &&
    is.finite(mad) && mad < tol
  msg <- sprintf(
    paste0(
      "AROMA deterministic spatial replay (%d voxels): max abs diff %.6g ",
      "(tol %.3g); nonaggressive=%s%s."
    ),
    replay$n_sampled, mad, tol, nonaggressive,
    if (is.null(replay$failure_reason)) "" else
      paste0("; ", replay$failure_reason)
  )
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- list(
    max_abs_diff = mad, n_noise_ic = length(comp_idx),
    requested_noise_ics = requested_idx,
    invalid_noise_ics = invalid_idx,
    n_sampled = replay$n_sampled,
    sampled_indices = replay$sampled_indices,
    normalized_coords = replay$normalized_coords,
    n_nonfinite_post = replay$n_nonfinite_post,
    n_unexpected_constant_post = replay$n_unexpected_constant_post,
    failure_reason = replay$failure_reason
  )
  return(out)
}

#' Validate confound regression by deterministic spatial replay
#'
#' Selects approximately 100 pre-step voxels across normalized image space and
#' replays the regression via `lmfit_residuals_mat` (`preserve_mean = TRUE`);
#' passes if the maximum absolute difference is below 0.05.
#'
#' @param pre_file Path to 4D BOLD before `confound_regression`.
#' @param post_file Path to 4D BOLD after `confound_regression`.
#' @param to_regress Regressor TSV (no header).
#' @param censor_file Optional censor file (1 = keep TR).
#' @param n_sample Number of voxels to sample (default 100).
#' @param mask_file Optional 3D brain mask used to constrain pre-step sampling.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details` (`max_abs_diff`).
#'
#' @keywords internal
validate_confound_regression <- function(pre_file, post_file, to_regress,
                                         censor_file = NULL, n_sample = 100L,
                                         mask_file = NULL) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(to_regress)

  pre_post_grid <- pp_compare_nifti_grid(
    pre_file, post_file, "pre", "post"
  )
  if (!isTRUE(pre_post_grid$passed)) return(pp_grid_failure(pre_post_grid))
  has_mask <- checkmate::test_string(mask_file, min.chars = 1L)
  if (!is.null(mask_file) && !has_mask) {
    out <- FALSE
    attr(out, "message") <-
      "Confound-regression replay mask must be NULL or a nonempty path."
    attr(out, "details") <- list(
      max_abs_diff = Inf, n_sampled = 0L,
      failure_reason = "Invalid replay mask path."
    )
    return(out)
  }
  if (has_mask) {
    checkmate::assert_file_exists(mask_file)
    pre_mask_grid <- pp_compare_nifti_grid(
      pre_file, mask_file, "pre", "confound-regression replay mask"
    )
    if (!isTRUE(pre_mask_grid$passed)) return(pp_grid_failure(pre_mask_grid))
  }

  Xmat <- as.matrix(data.table::fread(to_regress, sep = "\t", header = FALSE, data.table = FALSE))
  good_vols <- rep(TRUE, nrow(Xmat))
  use_censor <- !is.null(censor_file)
  if (use_censor &&
      !checkmate::test_string(censor_file, min.chars = 1L)) {
    out <- FALSE
    attr(out, "message") <-
      "Confound-regression censor path must be a single nonempty string."
    attr(out, "details") <- list(
      max_abs_diff = Inf, n_sampled = 0L,
      failure_reason = "Invalid censor path."
    )
    return(out)
  }
  if (use_censor) {
    if (!checkmate::test_file_exists(censor_file)) {
      out <- FALSE
      attr(out, "message") <- "Confound-regression censor file does not exist."
      attr(out, "details") <- list(
        max_abs_diff = Inf, n_sampled = 0L,
        failure_reason = "Censor file does not exist."
      )
      return(out)
    }
    censor_check <- pp_validate_censor(readLines(censor_file), nrow(Xmat))
    if (!isTRUE(censor_check$valid)) {
      out <- FALSE
      attr(out, "message") <- censor_check$message
      attr(out, "details") <- list(
        max_abs_diff = Inf, n_sampled = 0L,
        failure_reason = censor_check$message
      )
      return(out)
    }
    good_vols <- as.logical(censor_check$censor)
  }

  replay <- pp_sample_and_replay(
    pre_file = pre_file,
    post_file = post_file,
    X = Xmat,
    include_rows = good_vols,
    preserve_mean = TRUE,
    set_mean = 0.0,
    regress_cols = NULL,
    exclusive = FALSE,
    n_sample = n_sample,
    mask_file = mask_file
  )

  mad <- replay$max_abs_diff
  tol <- 0.05
  passed <- isTRUE(replay$valid) && replay$n_sampled > 0L &&
    is.finite(mad) && mad < tol
  msg <- sprintf(
    paste0(
      "Confound regression deterministic spatial replay (%d voxels): ",
      "max abs diff %.6g (tol %.3g)%s."
    ),
    replay$n_sampled, mad, tol,
    if (is.null(replay$failure_reason)) "" else
      paste0("; ", replay$failure_reason)
  )
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- list(
    max_abs_diff = mad, n_sampled = replay$n_sampled,
    sampled_indices = replay$sampled_indices,
    normalized_coords = replay$normalized_coords,
    n_nonfinite_post = replay$n_nonfinite_post,
    n_unexpected_constant_post = replay$n_unexpected_constant_post,
    failure_reason = replay$failure_reason
  )
  return(out)
}

#' Validate scrub interpolation by exact preservation and sampled spline replay
#'
#' The pre/post images must have the same shape, uncensored volumes must be
#' unchanged, and interpolated values must match the production natural-spline
#' implementation at deterministic, spatially distributed voxels.
#'
#' @param pre_file Path to 4D BOLD before `scrub_interpolate`.
#' @param post_file Path to 4D BOLD after `scrub_interpolate`.
#' @param censor_file Censor file (1 = keep, 0 = interpolate).
#' @param n_sample Number of pre-step brain/data voxels used for spline replay.
#' @param tolerance Maximum relative numerical error for exact comparisons.
#' @param chunk_size Number of volumes compared at a time.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details`.
#'
#' @keywords internal
validate_scrub_interpolate <- function(pre_file, post_file, censor_file,
                                       n_sample = 100L, tolerance = 1e-5,
                                       chunk_size = 100L) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(censor_file)
  checkmate::assert_count(n_sample, positive = TRUE)
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
  checkmate::assert_count(chunk_size, positive = TRUE)

  pre_post_grid <- pp_compare_nifti_grid(
    pre_file, post_file, "pre", "post", tolerance = tolerance
  )
  if (!isTRUE(pre_post_grid$passed)) return(pp_grid_failure(pre_post_grid))

  pre_dims <- pp_nifti_dims4(pre_file)
  post_dims <- pp_nifti_dims4(post_file)
  if (!identical(pre_dims, post_dims)) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      "Pre/post dimensions differ after scrub_interpolate: [%s] vs [%s].",
      paste(pre_dims, collapse = "x"), paste(post_dims, collapse = "x")
    )
    attr(out, "details") <- list(pre_dim = pre_dims, post_dim = post_dims)
    return(out)
  }

  censor_check <- pp_validate_censor(readLines(censor_file), pre_dims[4])
  if (!isTRUE(censor_check$valid)) {
    out <- FALSE
    attr(out, "message") <- censor_check$message
    attr(out, "details") <- list()
    return(out)
  }
  censor <- censor_check$censor
  t_interp <- which(censor == 0L)
  t_keep <- which(censor == 1L)
  if (length(t_interp) > 0L && length(t_keep) < 3L) {
    out <- FALSE
    attr(out, "message") <-
      "Fewer than three retained timepoints are available for spline replay."
    attr(out, "details") <- list(
      n_interpolated = length(t_interp), n_retained = length(t_keep)
    )
    return(out)
  }

  summary <- list(
    max_absolute_error = 0, max_relative_error = 0,
    n_mismatched = 0L, n_nonfinite_observed = 0L,
    finite_pattern_mismatches = 0L
  )
  update_summary <- function(current, comparison) {
    current$max_absolute_error <- max(
      current$max_absolute_error, comparison$max_absolute_error
    )
    current$max_relative_error <- max(
      current$max_relative_error, comparison$max_relative_error
    )
    for (name in c(
      "n_mismatched", "n_nonfinite_observed", "finite_pattern_mismatches"
    )) {
      current[[name]] <- current[[name]] + comparison[[name]]
    }
    current
  }

  n_voxels <- prod(pre_dims[1:3])
  pre_min <- rep(Inf, n_voxels)
  pre_max <- rep(-Inf, n_voxels)
  retained_all_finite <- rep(TRUE, n_voxels)
  if (length(t_keep)) {
    keep_groups <- split(
      t_keep, ceiling(seq_along(t_keep) / as.integer(chunk_size))
    )
    for (volumes in keep_groups) {
      pre_matrix <- pp_read_volume_matrix(pre_file, volumes, pre_dims[1:3])
      post_matrix <- pp_read_volume_matrix(post_file, volumes, pre_dims[1:3])
      summary <- update_summary(
        summary,
        pp_compare_numeric(
          post_matrix, pre_matrix, tolerance = tolerance, require_finite = TRUE
        )
      )
      retained_all_finite <- retained_all_finite &
        matrixStats::rowAlls(is.finite(pre_matrix))
      pre_min <- pmin(
        pre_min, matrixStats::rowMins(pre_matrix, na.rm = TRUE)
      )
      pre_max <- pmax(
        pre_max, matrixStats::rowMaxs(pre_matrix, na.rm = TRUE)
      )
    }
  }

  spline_comparison <- list(
    passed = TRUE, max_absolute_error = 0, max_relative_error = 0,
    n_mismatched = 0L, n_nonfinite_observed = 0L,
    finite_pattern_mismatches = 0L
  )
  selected <- list(
    indices = integer(), normalized_coords = matrix(numeric(), ncol = 3L)
  )
  if (length(t_interp)) {
    variable_candidates <- which(
      retained_all_finite & is.finite(pre_min) & is.finite(pre_max) &
        (pre_max - pre_min) > 2 * .Machine$double.eps
    )
    candidates <- if (length(variable_candidates)) {
      variable_candidates
    } else {
      which(retained_all_finite & is.finite(pre_min) & is.finite(pre_max))
    }
    selected <- pp_select_spatial_replay_voxels(
      candidates, spatial_dims = pre_dims[1:3], n_voxels = n_sample
    )
    if (!length(selected$indices)) {
      out <- FALSE
      attr(out, "message") <-
        "No finite pre-step voxels are available for spline replay."
      attr(out, "details") <- list(
        n_interpolated = length(t_interp), n_retained = length(t_keep)
      )
      return(out)
    }

    pre_sample <- matrix(
      NA_real_, nrow = length(selected$indices), ncol = pre_dims[4]
    )
    post_sample <- pre_sample
    all_groups <- split(
      seq_len(pre_dims[4]),
      ceiling(seq_len(pre_dims[4]) / as.integer(chunk_size))
    )
    for (volumes in all_groups) {
      pre_matrix <- pp_read_volume_matrix(pre_file, volumes, pre_dims[1:3])
      post_matrix <- pp_read_volume_matrix(post_file, volumes, pre_dims[1:3])
      pre_sample[, volumes] <- pre_matrix[selected$indices, , drop = FALSE]
      post_sample[, volumes] <- post_matrix[selected$indices, , drop = FALSE]
    }

    expected_interp <- matrix(
      NA_real_, nrow = length(selected$indices), ncol = length(t_interp)
    )
    first_valid <- min(t_keep)
    last_valid <- max(t_keep)
    for (voxel in seq_len(nrow(pre_sample))) {
      retained_values <- pre_sample[voxel, t_keep]
      if (all(retained_values == retained_values[[1L]])) {
        # Production skips constant retained series and leaves censored values
        # exactly as they were in the input image.
        expected <- pre_sample[voxel, t_interp]
      } else {
        expected <- natural_spline_interp(
          as.numeric(t_keep), retained_values, as.numeric(t_interp)
        )
        expected[t_interp < first_valid] <- pre_sample[voxel, first_valid]
        expected[t_interp > last_valid] <- pre_sample[voxel, last_valid]
      }
      expected_interp[voxel, ] <- expected
    }
    spline_comparison <- pp_compare_numeric(
      post_sample[, t_interp, drop = FALSE], expected_interp,
      tolerance = tolerance, require_finite = TRUE
    )
  }

  msg <- sprintf(
    paste0(
      "Scrub interpolation replay: %d retained volumes unchanged ",
      "(max relative error %.6g); %d interpolated volumes replayed at %d ",
      "spatially stratified voxels (max relative error %.6g; tol %.6g); ",
      "%d mismatched values."
    ),
    length(t_keep), summary$max_relative_error,
    length(t_interp), length(selected$indices),
    spline_comparison$max_relative_error, tolerance,
    summary$n_mismatched + spline_comparison$n_mismatched
  )
  passed <- summary$n_mismatched == 0L &&
    summary$n_nonfinite_observed == 0L && isTRUE(spline_comparison$passed)
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- list(
    n_interpolated = length(t_interp), n_retained = length(t_keep),
    n_sampled = length(selected$indices),
    sampled_indices = selected$indices,
    normalized_coords = selected$normalized_coords,
    retained_max_absolute_error = summary$max_absolute_error,
    retained_max_relative_error = summary$max_relative_error,
    retained_mismatches = summary$n_mismatched,
    spline_max_absolute_error = spline_comparison$max_absolute_error,
    spline_max_relative_error = spline_comparison$max_relative_error,
    spline_mismatches = spline_comparison$n_mismatched,
    tolerance = tolerance
  )
  return(out)
}

#' Validate scrub timepoints by exact retained-volume replay
#'
#' The post image must contain exactly the pre-image volumes whose censor values
#' are one, in their original order. Pass the censor vector read **before** the
#' step if the file is overwritten.
#'
#' @param pre_file Path to 4D BOLD before `scrub_timepoints`.
#' @param post_file Path to 4D BOLD after `scrub_timepoints`.
#' @param censor_vec Censor vector length = pre TRs (1 = keep, 0 = drop).
#' @param tolerance Maximum relative numerical error for retained volumes.
#' @param chunk_size Number of retained volumes compared at a time.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details` (`n_pre_t`, `n_post_t`, `n_removed`).
#'
#' @keywords internal
validate_scrub_timepoints <- function(pre_file, post_file, censor_vec,
                                      tolerance = 1e-5, chunk_size = 100L) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
  checkmate::assert_count(chunk_size, positive = TRUE)

  pre_post_grid <- pp_compare_nifti_grid(
    pre_file, post_file, "pre", "post", tolerance = tolerance
  )
  if (!isTRUE(pre_post_grid$passed)) return(pp_grid_failure(pre_post_grid))

  pre_dims <- pp_nifti_dims4(pre_file)
  post_dims <- pp_nifti_dims4(post_file)
  censor_check <- pp_validate_censor(censor_vec, pre_dims[4])
  if (!isTRUE(censor_check$valid)) {
    out <- FALSE
    attr(out, "message") <- censor_check$message
    attr(out, "details") <- list()
    return(out)
  }
  censor <- censor_check$censor
  keep <- which(censor == 1L)
  removed <- which(censor == 0L)
  expected_t <- length(keep)
  if (!expected_t) {
    out <- FALSE
    attr(out, "message") <- "Censor vector would remove every timepoint."
    attr(out, "details") <- list(n_pre_t = pre_dims[4], n_removed = length(removed))
    return(out)
  }
  if (!identical(pre_dims[1:3], post_dims[1:3]) || post_dims[4] != expected_t) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      paste0(
        "Scrub timepoint dimensions mismatch: pre=[%s], post=[%s], ",
        "expected post T=%d."
      ),
      paste(pre_dims, collapse = "x"), paste(post_dims, collapse = "x"),
      expected_t
    )
    attr(out, "details") <- list(
      pre_dim = pre_dims, post_dim = post_dims, expected_post_t = expected_t
    )
    return(out)
  }

  aggregate <- list(
    max_absolute_error = 0, max_relative_error = 0,
    n_mismatched = 0L, n_nonfinite_observed = 0L,
    finite_pattern_mismatches = 0L
  )
  post_positions <- seq_len(expected_t)
  groups <- split(
    post_positions, ceiling(post_positions / as.integer(chunk_size))
  )
  for (post_volumes in groups) {
    pre_volumes <- keep[post_volumes]
    pre_matrix <- pp_read_volume_matrix(pre_file, pre_volumes, pre_dims[1:3])
    post_matrix <- pp_read_volume_matrix(
      post_file, post_volumes, post_dims[1:3]
    )
    comparison <- pp_compare_numeric(
      post_matrix, pre_matrix, tolerance = tolerance, require_finite = TRUE
    )
    aggregate$max_absolute_error <- max(
      aggregate$max_absolute_error, comparison$max_absolute_error
    )
    aggregate$max_relative_error <- max(
      aggregate$max_relative_error, comparison$max_relative_error
    )
    for (name in c(
      "n_mismatched", "n_nonfinite_observed", "finite_pattern_mismatches"
    )) {
      aggregate[[name]] <- aggregate[[name]] + comparison[[name]]
    }
  }
  passed <- aggregate$n_mismatched == 0L &&
    aggregate$n_nonfinite_observed == 0L
  msg <- sprintf(
    paste0(
      "Scrub timepoint replay: pre T=%d, post T=%d, removed=%d; ",
      "all %d retained volumes match in order with max relative error %.6g ",
      "(tol %.6g); %d mismatched values."
    ),
    pre_dims[4], post_dims[4], length(removed), expected_t,
    aggregate$max_relative_error, tolerance, aggregate$n_mismatched
  )
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- c(
    list(
      n_pre_t = pre_dims[4], n_post_t = post_dims[4],
      n_removed = length(removed), n_retained = expected_t,
      tolerance = tolerance
    ),
    aggregate
  )
  return(out)
}
