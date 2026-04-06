### Postprocessing validation functions

#' Validate that a brain mask was correctly applied to 4D fMRI data
#'
#' Checks that voxels outside the mask are zero (no signal leakage) and
#' optionally reports voxels inside the mask that are all zero (potentially
#' problematic).
#'
#' @param mask_file Path to the binary mask NIfTI file (1s = brain, 0s = non-brain).
#' @param data_file Path to the masked 4D fMRI data file (after `apply_mask` was run).
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes:
#'   - `message`: Character string describing the validation result.
#'   - `external_violations`: Integer count of voxels outside mask with non-zero signal.
#'   - `internal_zeros`: Integer count of voxels inside mask that are all zero.
#'
#' @details
#' This function verifies that the masking step was applied correctly by checking:
#' - External violations: voxels where mask == 0 (outside brain) but data has non-zero signal.
#' - Internal zeros: voxels where mask > 0 (inside brain) but all timepoints are zero.
#'
#' Validation passes if `external_violations == 0`. Internal zeros are reported
#' but do not cause validation to fail.
#'
#' @keywords internal
#' @importFrom RNifti readNifti
#' @importFrom matrixStats rowAnys
validate_apply_mask <- function(mask_file, data_file) {
  checkmate::assert_file_exists(mask_file)
  checkmate::assert_file_exists(data_file)

  mask <- RNifti::readNifti(mask_file)
  data4d <- RNifti::readNifti(data_file)

  img_dims <- dim(data4d)
  if (length(img_dims) == 3L) {
    img_dims <- c(img_dims, 1L)
    dim(data4d) <- img_dims
  }
  n_vox <- prod(img_dims[1:3])
  n_t <- img_dims[4]

  # reshape 4D time series into voxels x time matrix
  img_matrix <- array(data4d, dim = c(n_vox, n_t))
  mask_vec <- as.vector(mask)
  outside <- mask_vec == 0
  inside <- mask_vec > 0

  # External violation: mask == 0 but ANY timepoint is nonzero
  any_nonzero <- matrixStats::rowAnys(img_matrix != 0)
  external_violations <- sum(outside & any_nonzero)

  # Internal zeros: mask > 0 but ALL timepoints are zero
  all_zero <- !any_nonzero
  internal_zeros <- sum(inside & all_zero)

  passed <- external_violations == 0L

  msg <- paste0(
    internal_zeros, " voxels = 0 inside the mask. ",
    external_violations, " voxels > 0 outside the mask."
  )

  result <- passed
  attr(result, "message") <- msg
  attr(result, "external_violations") <- external_violations
  attr(result, "internal_zeros") <- internal_zeros

  return(result)
}

# --- multitaper helpers ---

#' Compute multitaper power spectral density estimate for a single time series
#' @importFrom signal sgolayfilt
#' @keywords internal
#' @noRd
.pp_power_multitaper <- function(
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
  if (smooth_psd) {
    window_length <- 2 * floor((length(p_db) / 20) / 2) + 1 # 1/20th of the series
    p_db <- signal::sgolayfilt(p_db, p = 3, n = window_length)
    #p_db <- stats::filter(p_db, rep(1/5, 5), sides = 2)  # 5-point moving average
  }
  
  return(data.frame(f = f_hz, power = p_db))
  
}


.pp_mtm_bandpower <- function(
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
.pp_is_valid_series <- function(x, tol = 2 * .Machine$double.eps) {
  return(all(is.finite(x)) && stats::var(x) > tol)
}

#' @keywords internal
#' @noRd
.pp_select_nonconstant_voxels <- function(
    mask_idx,
    get_pre_ts,
    get_post_ts,
    n_voxels,
    var_tol = 2 * .Machine$double.eps) {

  candidate_positions <- seq_along(mask_idx)

  if (is.null(n_voxels) || is.na(n_voxels)) {
    valid_mask <- vapply(
      candidate_positions,
      function(pos) {
        .pp_is_valid_series(get_pre_ts(pos), tol = var_tol) &&
          .pp_is_valid_series(get_post_ts(pos), tol = var_tol)
      },
      logical(1)
    )
    valid_positions <- candidate_positions[valid_mask]
    if (!length(valid_positions)) {
      stop("No non-constant voxels available in both pre and post series.", call. = FALSE)
    }
    return(list(indices = mask_idx[valid_positions], positions = valid_positions))
  }

  candidate_order <- sample(candidate_positions, length(candidate_positions))
  valid_positions <- integer(n_voxels)
  found <- 0L
  for (pos in candidate_order) {
    if (.pp_is_valid_series(get_pre_ts(pos), tol = var_tol) &&
        .pp_is_valid_series(get_post_ts(pos), tol = var_tol)) {
      found <- found + 1L
      valid_positions[found] <- pos
      if (found == n_voxels) break
    }
  }
  if (found < n_voxels) {
    stop(
      "Fewer than ", n_voxels, " non-constant voxels available in both pre and post series.",
      call. = FALSE
    )
  }
  return(list(indices = mask_idx[valid_positions], positions = valid_positions))
}

#' @keywords internal
#' @noRd
.pp_make_ts_extractor <- function(img, coords_matrix) {
  function(position) {
    coord <- coords_matrix[position, ]
    return(img[coord[1], coord[2], coord[3], , drop = TRUE])
  }
}

#' @keywords internal
#' @noRd
.pp_average_multitaper_spectra <- function(spec_list) {
  if (!length(spec_list)) stop("No spectra supplied for averaging.", call. = FALSE)
  dt <- data.table::rbindlist(spec_list, idcol = "voxel")
  data.table::setnames(dt, c("f", "power"), c("freq", "power_db"))
  as.data.frame(dt[, .(power_db = mean(power_db, na.rm = TRUE)), by = freq])
}

#' @keywords internal
#' @noRd
.pp_average_bandpower <- function(bp_list) {
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
    passband_loss_fail_db = 3
) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_number(tr, lower = 1e-4, upper = 100)
  checkmate::assert_int(n_voxels, lower = 1L)

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

  pre_img <- RNifti::readNifti(pre_file)
  pre_img_dims <- dim(pre_img)
  if (length(pre_img_dims) == 3L) {
    pre_img_dims <- c(pre_img_dims, 1L)
    dim(pre_img) <- pre_img_dims
  }
  pre_n_t <- pre_img_dims[4]
  pre_n_vox <- prod(pre_img_dims[1:3])

  mask_dims <- NULL
  if (!is.null(mask_file) && nzchar(mask_file)) {
    checkmate::assert_file_exists(mask_file)
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

  if (!is.null(mask_file) && nzchar(mask_file) && !all(mask_dims == post_img_dims[1:3])) {
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

  var_tol <- if (is.null(mask_file) || !nzchar(mask_file)) 1e-3 else 2 * .Machine$double.eps
  default_sample_size <- min(as.integer(n_voxels), length(mask_idx))
  voxels_to_sample <- if (length(mask_idx) <= default_sample_size) length(mask_idx) else default_sample_size

  get_pre_ts <- .pp_make_ts_extractor(pre_img, mask_coords)
  get_post_ts <- .pp_make_ts_extractor(post_img, mask_coords)

  selection <- tryCatch(
    .pp_select_nonconstant_voxels(
      mask_idx = mask_idx,
      get_pre_ts = get_pre_ts,
      get_post_ts = get_post_ts,
      n_voxels = voxels_to_sample,
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

  pre_spectra <- lapply(selected_positions, function(pos) {
    .pp_power_multitaper(get_pre_ts(pos), dt = dt)
  })
  post_spectra <- lapply(selected_positions, function(pos) {
    .pp_power_multitaper(get_post_ts(pos), dt = dt)
  })

  rm(pre_img, post_img)

  nyquist <- 1 / (2 * dt)
  outside_bands <- list()
  avg_reduction <- NA_real_
  fail_outside <- FALSE

  if (!is.na(band_low) && band_low > 0) {
    outside_bands$below <- c(0, max(0, band_low))
  }
  if (!is.na(band_high) && band_high < nyquist) {
    outside_bands$above <- c(min(band_high, nyquist), nyquist)
  }

  if (length(outside_bands) > 0) {
    pre_bp_list <- lapply(selected_positions, function(pos) {
      .pp_mtm_bandpower(
        get_pre_ts(pos),
        dt = dt,
        bands = outside_bands,
        detrend = "linear",
        exclude_dc = TRUE,
        total_band = c(0, nyquist)
      )
    })
    post_bp_list <- lapply(selected_positions, function(pos) {
      .pp_mtm_bandpower(
        get_post_ts(pos),
        dt = dt,
        bands = outside_bands,
        detrend = "linear",
        exclude_dc = TRUE,
        total_band = c(0, nyquist)
      )
    })

    pre_bp_avg <- .pp_average_bandpower(pre_bp_list)
    post_bp_avg <- .pp_average_bandpower(post_bp_list)

    bandpower_diff <- merge(
      pre_bp_avg[, c("label", "low", "high", "power_db", "relative_power")],
      post_bp_avg[, c("label", "low", "high", "power_db", "relative_power")],
      by = c("label", "low", "high"), suffixes = c("_pre", "_post")
    )
    bandpower_diff$power_db_change <- bandpower_diff$power_db_post - bandpower_diff$power_db_pre
    bandpower_diff$band_type <- "outside"

    avg_reduction <- mean(bandpower_diff$power_db_pre - bandpower_diff$power_db_post, na.rm = TRUE)
    fail_outside <- is.finite(avg_reduction) && avg_reduction <= 0
  }

  passband_low <- if (!is.na(band_low)) max(0, band_low) else 0
  passband_high <- if (!is.na(band_high)) min(nyquist, band_high) else nyquist
  has_passband_bounds <- (!is.na(band_low) || !is.na(band_high)) && passband_high > passband_low

  avg_change_db <- NA_real_
  fail_passband <- FALSE

  if (has_passband_bounds) {
    passband <- list(passband = c(passband_low, passband_high))
    pre_pass_list <- lapply(selected_positions, function(pos) {
      .pp_mtm_bandpower(
        get_pre_ts(pos),
        dt = dt,
        bands = passband,
        detrend = "linear",
        exclude_dc = TRUE,
        total_band = c(0, nyquist)
      )
    })
    post_pass_list <- lapply(selected_positions, function(pos) {
      .pp_mtm_bandpower(
        get_post_ts(pos),
        dt = dt,
        bands = passband,
        detrend = "linear",
        exclude_dc = TRUE,
        total_band = c(0, nyquist)
      )
    })

    pre_pass_avg <- .pp_average_bandpower(pre_pass_list)
    post_pass_avg <- .pp_average_bandpower(post_pass_list)

    passband_diff <- merge(
      pre_pass_avg[, c("label", "low", "high", "power_db", "relative_power")],
      post_pass_avg[, c("label", "low", "high", "power_db", "relative_power")],
      by = c("label", "low", "high"), suffixes = c("_pre", "_post")
    )
    passband_diff$power_db_change <- passband_diff$power_db_post - passband_diff$power_db_pre
    passband_diff$band_type <- "passband"

    power_changes <- passband_diff$power_db_change
    avg_change_db <- if (all(is.na(power_changes))) NA_real_ else mean(power_changes, na.rm = TRUE)
    fail_passband <- !is.na(avg_change_db) && is.finite(avg_change_db) &&
      avg_change_db < -passband_loss_fail_db
  }

  passed <- !fail_outside && !fail_passband

  msg_parts <- character()
  if (is.finite(avg_reduction)) {
    msg_parts <- c(msg_parts, sprintf("avg outside-band power reduction (dB): %s", signif(avg_reduction, 4)))
  }
  if (!is.na(avg_change_db) && is.finite(avg_change_db)) {
    msg_parts <- c(msg_parts, sprintf("avg passband power change post-pre (dB): %s", signif(avg_change_db, 4)))
  }
  if (!length(msg_parts)) {
    msg_parts <- "multitaper bandpower summaries (limited band spec)."
  }

  msg <- paste(msg_parts, collapse = "; ")
  if (fail_outside) {
    msg <- paste0(msg, "; FAIL: no net power reduction outside configured stopbands.")
  }
  if (fail_passband) {
    msg <- paste0(
      msg,
      "; FAIL: large power loss in passband (threshold ",
      passband_loss_fail_db,
      " dB)."
    )
  }

  details <- list(
    nyquist_hz = nyquist,
    n_voxels_used = length(selected_positions),
    avg_reduction_outside_db = avg_reduction,
    avg_passband_change_db = avg_change_db,
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

median_over_time <- function(arr4d) {
  return(apply(arr4d, c(1, 2, 3), stats::median, na.rm = TRUE))
}

mad_over_time <- function(arr4d) {
  return(apply(arr4d, c(1, 2, 3), stats::mad, constant = 1.4826, na.rm = TRUE))
}


#' @keywords internal
#' @noRd
.pp_pixdim_mm <- function(path) {
  h <- RNifti::niftiHeader(path)
  return(as.numeric(h$pixdim[2:4]))
}

#' @keywords internal
#' @noRd
.pp_read_4d <- function(path) {
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
.pp_max_abs_diff <- function(a, b) {
  return(max(abs(as.numeric(a) - as.numeric(b)), na.rm = TRUE))
}

#' Validate intensity normalization (global median in mask)
#'
#' Global median of in-mask values vs `target` within `tolerance`.
#'
#' @param data_file 4D NIfTI after `intensity_normalize`.
#' @param mask_file 3D mask.
#' @param target Target median (`cfg$intensity_normalize$global_median`).
#' @param tolerance Absolute tolerance on `abs(median - target)`.
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details` (`global_median`, `target`, `abs_diff`).
#'
#' @keywords internal
#' @importFrom RNifti readNifti
validate_intensity_normalize <- function(data_file, mask_file, target, tolerance = 0) {
  checkmate::assert_file_exists(data_file)
  checkmate::assert_file_exists(mask_file)
  checkmate::assert_number(target, finite = TRUE)
  checkmate::assert_number(tolerance, lower = 0, finite = TRUE)

  img <- RNifti::readNifti(data_file)
  msk <- RNifti::readNifti(mask_file)

  if (length(dim(img)) < 3L) {
    out <- FALSE
    attr(out, "message") <- "Input image must be at least 3D."
    attr(out, "details") <- list()
    return(out)
  }
  if (length(dim(img)) == 3L) dim(img) <- c(dim(img), 1L)
  if (length(dim(msk)) != 3L) {
    out <- FALSE
    attr(out, "message") <- "Mask must be 3D."
    attr(out, "details") <- list()
    return(out)
  }
  if (!all(dim(img)[1:3] == dim(msk))) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      "Spatial dims mismatch: image [%s] vs mask [%s]",
      paste(dim(img)[1:3], collapse = "x"),
      paste(dim(msk), collapse = "x")
    )
    attr(out, "details") <- list()
    return(out)
  }

  mask_logical <- (msk != 0) & is.finite(msk)
  n_mask_vox <- sum(mask_logical)
  if (n_mask_vox == 0L) {
    out <- FALSE
    attr(out, "message") <- "Mask contains zero in-brain voxels."
    attr(out, "details") <- list()
    return(out)
  }

  img_dims <- dim(img)
  n_vox <- prod(img_dims[1:3])
  n_t <- img_dims[4]
  img_matrix <- array(img, dim = c(n_vox, n_t))
  vals <- as.vector(img_matrix[mask_logical, , drop = FALSE])
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0L) {
    out <- FALSE
    attr(out, "message") <- "No finite values found within mask."
    attr(out, "details") <- list()
    return(out)
  }

  gmed <- stats::median(vals)
  diff <- abs(gmed - target)
  passed <- diff <= tolerance
  msg <- sprintf(
    "Global median within mask over %d voxels x %d timepoints = %.6f; target = %.6f; abs diff = %.6g (tol %.6g).",
    n_mask_vox, n_t, gmed, target, diff, tolerance
  )
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- list(global_median = gmed, target = target, abs_diff = diff)
  return(out)
}

#' Empirical calibration coefficients for classic FWHM delta estimation
#'
#' These were derived from simulations applying known smoothing kernels to
#' fMRI-like data and measuring the resulting change in classic FWHM.
#' Because fMRI data are not Gaussian, the naive FWHM delta from first
#' differences systematically deviates from the requested kernel; these
#' regressions correct for that bias.
#'
#' Structure: `smoother -> method -> mask/nomask -> list(type, coeffs)`
#'   - `type = "linear"`: `coeffs[1] + coeffs[2] * kernel`
#'   - `type = "poly"`:   polynomial in kernel (`sum(coeffs * kernel^(0:p))`)
#'
#' @keywords internal
#' @noRd
.pp_calibration_coeffs <- list(
  gaussian = list(
    classic = list(
      mask   = list(type = "linear", coeffs = c(-2.942579, 1.198781)),
      nomask = list(type = "linear", coeffs = c(-2.141319, 1.143082))
    )
  ),
  susan = list(
    classic = list(
      mask   = list(type = "poly", coeffs = c(-3.6270403, 1.4369376, -0.03108286)),
      nomask = list(type = "poly", coeffs = c(-3.6270403, 1.4369376, -0.03108286))
    )
  )
)

#' Predict the expected FWHM delta from a calibration model
#' @keywords internal
#' @noRd
.pp_predict_calibration <- function(model, kernel_fwhm) {
  coeffs <- model$coeffs
  if (model$type == "linear") {
    return(coeffs[1] + coeffs[2] * kernel_fwhm)
  } else if (model$type == "poly") {
    powers <- seq(0, length(coeffs) - 1)
    return(sum(coeffs * kernel_fwhm^powers))
  } else {
    stop("Unknown calibration model type: ", model$type, call. = FALSE)
  }
}

#' Select the calibration model for a given smoother and mask usage
#' @keywords internal
#' @noRd
.pp_select_calibration <- function(smoother, used_mask) {
  smooth_entry <- .pp_calibration_coeffs[[smoother]]
  if (is.null(smooth_entry)) {
    warning("No calibration table for smoother '", smoother,
            "'; falling back to gaussian.", call. = FALSE)
    smooth_entry <- .pp_calibration_coeffs[["gaussian"]]
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
  model
}

#' Validate spatial smoothing (classic FWHM pre vs post, calibration-corrected)
#'
#' Measures the observed FWHM change using `estimate_classic_fwhm()` and compares
#' it to the calibration-predicted delta for the requested kernel size. The
#' calibration accounts for the fact that fMRI data are non-Gaussian and the
#' naive first-differences FWHM estimate has a systematic bias that depends on
#' smoother type and whether masking was used.
#'
#' @param pre_file Path to 4D BOLD before `spatial_smooth`.
#' @param post_file Path to 4D BOLD after `spatial_smooth`.
#' @param mask_file 3D mask (same space as BOLD).
#' @param fwhm_mm Requested smoothing kernel FWHM in mm (`cfg$spatial_smooth$fwhm_mm`).
#' @param smoother Character; `"susan"` (default, matches `spatial_smooth()`) or `"gaussian"`.
#' @param used_mask Logical; whether smoothing was performed inside a mask (default `TRUE`).
#' @param tolerance_mm Tolerance in mm for `|observed_delta - expected_delta|` (default 0.5).
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details` (pre/post/delta/expected_delta/diff FWHM mm).
#'
#' @keywords internal
validate_spatial_smooth <- function(pre_file, post_file, mask_file, fwhm_mm = NA_real_,
                                    smoother = "susan", used_mask = TRUE,
                                    tolerance_mm = 0.5) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(mask_file)

  pre <- .pp_read_4d(pre_file)
  post <- .pp_read_4d(post_file)
  msk <- RNifti::readNifti(mask_file)
  mask_logical <- (msk != 0) & is.finite(msk)

  if (!all(dim(pre)[1:3] == dim(post)[1:3]) || !all(dim(pre)[1:3] == dim(msk))) {
    out <- FALSE
    attr(out, "message") <- "Pre/post/mask spatial dimensions mismatch."
    attr(out, "details") <- list()
    return(out)
  }
  if (dim(pre)[4] != dim(post)[4]) {
    out <- FALSE
    attr(out, "message") <- "Pre and post must have same number of timepoints."
    attr(out, "details") <- list()
    return(out)
  }

  vox_mm <- .pp_pixdim_mm(pre_file)
  pre_f <- estimate_classic_fwhm(pre, mask_logical, vox_mm)$geom
  post_f <- estimate_classic_fwhm(post, mask_logical, vox_mm)$geom

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
  has_kernel <- checkmate::test_number(fwhm_mm, lower = 1e-6, finite = TRUE)
  if (has_kernel) {
    cal_model <- .pp_select_calibration(smoother, used_mask)
    delta_expected <- .pp_predict_calibration(cal_model, fwhm_mm)
    diff_cal <- delta_observed - delta_expected
    within_tol <- abs(diff_cal) <= tolerance_mm
    passed <- within_tol
    msg <- sprintf(
      paste0(
        "Classic geom FWHM: pre=%.4f mm, post=%.4f mm, delta_observed=%.4f mm. ",
        "Calibrated expected delta=%.4f mm (smoother=%s, mask=%s). ",
        "|obs-exp|=%.4f mm (tol=%.4f mm). %s"
      ),
      pre_f, post_f, delta_observed,
      delta_expected, smoother, used_mask,
      abs(diff_cal), tolerance_mm,
      if (passed) "PASS" else "FAIL"
    )
    details <- list(
      pre_fwhm_mm = pre_f,
      post_fwhm_mm = post_f,
      delta_observed_mm = delta_observed,
      delta_expected_mm = delta_expected,
      delta_diff_mm = diff_cal,
      tolerance_mm = tolerance_mm,
      smoother = smoother,
      used_mask = used_mask
    )
  } else {
    # no kernel specified: just check that smoothness did not decrease
    passed <- delta_observed >= 0
    msg <- sprintf(
      "Classic geom FWHM: pre=%.4f mm, post=%.4f mm, delta=%.4f mm (no kernel specified; directional check only).",
      pre_f, post_f, delta_observed
    )
    details <- list(
      pre_fwhm_mm = pre_f,
      post_fwhm_mm = post_f,
      delta_observed_mm = delta_observed
    )
  }

  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- details
  return(out)
}

#' Sample voxels and replay regression via `lmfit_residuals_mat`
#'
#' Shared helper for `validate_apply_aroma` and `validate_confound_regression`.
#' Reads pre/post 4D images, samples up to `n_sample` non-constant voxels,
#' replays the regression in pure R, and returns the max absolute difference.
#'
#' @param pre_file Path to 4D BOLD before the step.
#' @param post_file Path to 4D BOLD after the step.
#' @param X Design matrix (timepoints x regressors).
#' @param include_rows Logical vector of rows used in fitting.
#' @param preserve_mean Passed to `lmfit_residuals_mat`.
#' @param set_mean Passed to `lmfit_residuals_mat`.
#' @param regress_cols Passed to `lmfit_residuals_mat` (1-based).
#' @param exclusive Passed to `lmfit_residuals_mat`.
#' @param n_sample Number of voxels to sample (default 100).
#'
#' @return A list with `max_abs_diff` and `n_sampled`.
#' @keywords internal
#' @noRd
.pp_sample_and_replay <- function(pre_file, post_file, X, include_rows,
                                  preserve_mean = FALSE, set_mean = 0.0,
                                  regress_cols = NULL, exclusive = FALSE,
                                  n_sample = 100L) {
  pre_img <- .pp_read_4d(pre_file)
  post_img <- .pp_read_4d(post_file)
  d <- dim(pre_img)
  nx <- d[1]; ny <- d[2]; nz <- d[3]; nt <- d[4]

  # Build mask: voxels where pre has any nonzero value across time
  pre_mat_full <- matrix(as.numeric(pre_img), nrow = nx * ny * nz, ncol = nt)
  mask_idx <- which(matrixStats::rowAnys(pre_mat_full != 0))
  if (length(mask_idx) == 0L) {
    return(list(max_abs_diff = 0, n_sampled = 0L))
  }

  # Convert mask indices to xyz coords for .pp_select_nonconstant_voxels
  coords <- arrayInd(mask_idx, .dim = c(nx, ny, nz))

  get_pre_ts <- .pp_make_ts_extractor(pre_img, coords)
  get_post_ts <- .pp_make_ts_extractor(post_img, coords)

  n_want <- min(n_sample, length(mask_idx))
  sel <- .pp_select_nonconstant_voxels(
    mask_idx = mask_idx,
    get_pre_ts = get_pre_ts,
    get_post_ts = get_post_ts,
    n_voxels = n_want
  )

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
    add_intercept = FALSE,
    preserve_mean = preserve_mean,
    set_mean = set_mean,
    regress_cols = regress_cols,
    exclusive = exclusive
  )

  mad_val <- max(abs(Y_post - expected), na.rm = TRUE)
  return(list(max_abs_diff = mad_val, n_sampled = length(sel$indices)))
}

#' Validate AROMA (voxel-sampling replay vs output)
#'
#' Samples ~100 voxels and replays the AROMA regression via `lmfit_residuals_mat`;
#' passes if max abs diff < 0.05 (or skips if no noise ICs).
#'
#' @param pre_file Path to 4D BOLD before `apply_aroma`.
#' @param post_file Path to 4D BOLD after `apply_aroma`.
#' @param mixing_file MELODIC mixing matrix (no header).
#' @param noise_ics Noise IC indices (1-based), same as pipeline.
#' @param nonaggressive Same as `apply_aroma`.
#' @param n_sample Number of voxels to sample (default 100).
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details`.
#'
#' @keywords internal
validate_apply_aroma <- function(pre_file, post_file, mixing_file, noise_ics,
                                 nonaggressive = TRUE, n_sample = 100L) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(mixing_file)

  if (is.null(noise_ics) || length(noise_ics) == 0L) {
    out <- TRUE
    attr(out, "message") <- "No noise ICs; AROMA left data unchanged - validation skipped."
    attr(out, "details") <- list(skipped = TRUE)
    return(out)
  }

  mixing_mat <- as.matrix(data.table::fread(mixing_file, header = FALSE, data.table = FALSE))
  storage.mode(mixing_mat) <- "double"
  comp_idx <- sort(unique(as.integer(noise_ics)))
  comp_idx <- comp_idx[comp_idx >= 1L & comp_idx <= ncol(mixing_mat)]
  if (length(comp_idx) == 0L) {
    out <- TRUE
    attr(out, "message") <- "No valid noise IC indices - validation skipped."
    attr(out, "details") <- list(skipped = TRUE)
    return(out)
  }

  exclusive_flag <- !isTRUE(nonaggressive)
  include_rows <- rep(TRUE, nrow(mixing_mat))

  replay <- .pp_sample_and_replay(
    pre_file = pre_file,
    post_file = post_file,
    X = mixing_mat,
    include_rows = include_rows,
    preserve_mean = FALSE,
    set_mean = 0.0,
    regress_cols = comp_idx,
    exclusive = exclusive_flag,
    n_sample = n_sample
  )

  mad <- replay$max_abs_diff
  tol <- 0.05
  passed <- is.finite(mad) && mad < tol
  msg <- sprintf(
    "AROMA voxel-sample replay (%d voxels): max abs diff %.6g (tol %.3g); nonaggressive=%s.",
    replay$n_sampled, mad, tol, nonaggressive
  )
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- list(max_abs_diff = mad, n_noise_ic = length(comp_idx),
                                n_sampled = replay$n_sampled)
  return(out)
}

#' Validate confound regression (voxel-sampling replay vs output)
#'
#' Samples ~100 voxels and replays the regression via `lmfit_residuals_mat`
#' (`preserve_mean = TRUE`); passes if max abs diff < 0.05.
#'
#' @param pre_file Path to 4D BOLD before `confound_regression`.
#' @param post_file Path to 4D BOLD after `confound_regression`.
#' @param to_regress Regressor TSV (no header).
#' @param censor_file Optional censor file (1 = keep TR).
#' @param n_sample Number of voxels to sample (default 100).
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details` (`max_abs_diff`).
#'
#' @keywords internal
validate_confound_regression <- function(pre_file, post_file, to_regress,
                                         censor_file = NULL, n_sample = 100L) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(to_regress)

  Xmat <- as.matrix(data.table::fread(to_regress, sep = "\t", header = FALSE, data.table = FALSE))
  good_vols <- rep(TRUE, nrow(Xmat))
  if (checkmate::test_file_exists(censor_file)) {
    good_vols <- as.logical(as.integer(readLines(censor_file)))
  }

  replay <- .pp_sample_and_replay(
    pre_file = pre_file,
    post_file = post_file,
    X = Xmat,
    include_rows = good_vols,
    preserve_mean = TRUE,
    set_mean = 0.0,
    regress_cols = NULL,
    exclusive = FALSE,
    n_sample = n_sample
  )

  mad <- replay$max_abs_diff
  tol <- 0.05
  passed <- is.finite(mad) && mad < tol
  msg <- sprintf(
    "Confound regression voxel-sample replay (%d voxels): max abs diff %.6g (tol %.3g).",
    replay$n_sampled, mad, tol
  )
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- list(max_abs_diff = mad, n_sampled = replay$n_sampled)
  return(out)
}

#' Validate scrub interpolate (dims + finite at filled TRs)
#'
#' Same 4D shape as pre; interpolated TRs (censor 0) must be finite in post.
#'
#' @param pre_file Path to 4D BOLD before `scrub_interpolate`.
#' @param post_file Path to 4D BOLD after `scrub_interpolate`.
#' @param censor_file Censor file (1 = keep, 0 = interpolate).
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details`.
#'
#' @keywords internal
validate_scrub_interpolate <- function(pre_file, post_file, censor_file) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_file_exists(censor_file)

  pre <- .pp_read_4d(pre_file)
  post <- .pp_read_4d(post_file)
  censor <- as.integer(readLines(censor_file))
  t_interp <- which(1L - censor == 1L)

  if (!all(dim(pre) == dim(post))) {
    out <- FALSE
    attr(out, "message") <- "Pre/post dimensions differ after scrub_interpolate."
    attr(out, "details") <- list(pre_dim = dim(pre), post_dim = dim(post))
    return(out)
  }

  if (length(t_interp) == 0L) {
    out <- TRUE
    attr(out, "message") <- "No censored timepoints to interpolate; validation trivially passed."
    attr(out, "details") <- list()
    return(out)
  }

  ok <- all(is.finite(post[, , , t_interp]))
  msg <- sprintf(
    "Scrub interpolate: %d interpolated timepoints; all finite in post: %s.",
    length(t_interp), ok
  )
  out <- ok
  attr(out, "message") <- msg
  attr(out, "details") <- list(n_interpolated = length(t_interp))
  return(out)
}

#' Validate scrub timepoints (output TR count vs censor)
#'
#' Post TRs should equal pre TRs minus scrubbed count. Pass censor read **before** the step if the file is overwritten.
#'
#' @param pre_file Path to 4D BOLD before `scrub_timepoints`.
#' @param post_file Path to 4D BOLD after `scrub_timepoints`.
#' @param censor_vec Censor vector length = pre TRs (1 = keep, 0 = drop).
#'
#' @return A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
#'   Attributes: `message`, `details` (`n_pre_t`, `n_post_t`, `n_removed`).
#'
#' @keywords internal
validate_scrub_timepoints <- function(pre_file, post_file, censor_vec) {
  checkmate::assert_file_exists(pre_file)
  checkmate::assert_file_exists(post_file)
  checkmate::assert_integerish(censor_vec, any.missing = FALSE)

  pre <- .pp_read_4d(pre_file)
  post <- .pp_read_4d(post_file)
  censor <- as.integer(censor_vec)
  n_pre <- dim(pre)[4]
  if (length(censor) != n_pre) {
    out <- FALSE
    attr(out, "message") <- sprintf(
      "Censor length (%d) does not match pre image T (%d).",
      length(censor), n_pre
    )
    attr(out, "details") <- list()
    return(out)
  }
  t_scrub <- which(1L - censor == 1L)
  n_post <- dim(post)[4]
  expected <- n_pre - length(t_scrub)
  passed <- n_post == expected && all(dim(pre)[1:3] == dim(post)[1:3])
  msg <- sprintf(
    "Scrub timepoints: pre T=%d, post T=%d, removed=%d (expected post T=%d). Match: %s.",
    n_pre, n_post, length(t_scrub), expected, passed
  )
  out <- passed
  attr(out, "message") <- msg
  attr(out, "details") <- list(n_pre_t = n_pre, n_post_t = n_post, n_removed = length(t_scrub))
  return(out)
}
