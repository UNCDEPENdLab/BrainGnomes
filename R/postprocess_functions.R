###################################################
### POSTPROCESSING FUNCTIONS FOR SPECIFIC STEPS ###
###################################################

#' Apply a brain mask to a 4D NIfTI image
#'
#' Multiplies a NIfTI image by a binary mask using FSL's \code{fslmaths -mas} to zero out non-brain voxels.
#' This is typically used to restrict processing to brain tissue.
#'
#' @param in_file Path to the input 4D NIfTI image.
#' @param mask_file Path to a binary mask NIfTI file (same dimensions as \code{in_file}).
#' @param out_file The full path for the file output by this step
#' @param overwrite Logical; whether to overwrite the output file if it already exists.
#' @param lg Optional lgr object used for logging messages
#' @param fsl_img Optional path to a Singularity image to execute the command in a container.
#'
#' @return Path to the masked output NIfTI file.
#'
#' @keywords internal
#' @importFrom checkmate assert_file_exists assert_string
#' @importFrom glue glue
apply_mask <- function(in_file, mask_file, out_file, overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  checkmate::assert_file_exists(mask_file)
  checkmate::assert_string(out_file)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  to_log(lg, "info", "Apply mask {mask_file} to {in_file}")

  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -mas {mask_file} {file_sans_ext(out_file)} -odt float"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, mask_file, out_file)))
  return(out_file)
}

#' Interpolate Over Censored Timepoints in a 4D NIfTI Image
#'
#' Applies cubic spline interpolation to a 4D fMRI image to replace censored
#' (scrubbed) timepoints, as defined in a censor file. Timepoints with a value
#' of `0` in the censor file are interpolated across using voxelwise natural
#' splines, with nearest-neighbor extrapolation at the edges.
#'
#' @param in_file Path to the input 4D NIfTI image file.
#' @param censor_file Path to a 1D censor file (e.g., from fMRI preprocessing) 
#'   containing a binary vector of `1`s (keep) and `0`s (scrub) for each timepoint.
#' @param out_file The full path for the file output by this step
#' @param confound_files Optional character vector of confound or regressor
#'   files to update alongside the fMRI data. Rows corresponding to interpolated
#'   volumes are filled in using natural splines with nearest-neighbor
#'   extrapolation.
#' @param overwrite Logical indicating whether to overwrite an existing interpolated
#'   file (default is `FALSE`).
#' @param lg Optional `Logger` object (from the `lgr` package) for logging output.
#'   If not provided, the root logger is used.
#'
#' @return A character string giving the path to the interpolated output NIfTI file.
#'   If the file already exists and `overwrite = FALSE`, the existing file path is returned.
#'
#' @details
#' Timepoints to interpolate are identified as those with a `0` in the `censor_file`.
#' These are replaced using voxelwise cubic spline interpolation across the remaining
#' timepoints. Extrapolation at the beginning or end of the series uses the nearest
#' valid value (i.e., `edge_nn = TRUE`).
#'
#' This function relies on a lower-level Rcpp function `natural_spline_4d()`
#' that performs the actual interpolation.
#'
#' @importFrom stats splinefun
#' @importFrom data.table fread
#' @keywords internal
scrub_interpolate <- function(in_file, censor_file, out_file,
                             confound_files = NULL, overwrite=FALSE, lg=NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_file_exists(censor_file)
  checkmate::assert_string(out_file)
  checkmate::assert_flag(overwrite)
  checkmate::assert_character(confound_files, any.missing = FALSE, null.ok = TRUE)
  
  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }
  
  to_log(lg, "info", "Applying spline interpolate to scrubbed timepoints")
  to_log(lg, "debug", "in_file: {in_file}")
  to_log(lg, "debug", "censor_file: {censor_file}")

  censor <- as.integer(readLines(censor_file))
  t_interpolate <- which(1L - censor == 1L) # bad timepoints are 0 in the censor file
  
  if (!any(t_interpolate)) {
    to_log(lg, "info", "No timepoints to scrub found in {censor_file}. Interpolation will have no effect.")
  } else {
    to_log(lg, "info", "Applying voxelwise natural spline interpolation for {length(t_interpolate)} timepoints to {in_file}.")
  }

  # run 4D interpolation with Rcpp function

  run_logged(
    natural_spline_4d,
    infile = in_file,
    t_interpolate = t_interpolate,
    edge_nn = TRUE,
    outfile = out_file,
    internal = TRUE,
    logger = lg
  )

  if (length(confound_files) > 0 && length(t_interpolate) > 0) {
    good_idx <- which(censor == 1L)
    first_valid <- min(good_idx)
    last_valid <- max(good_idx)
    for (cf in confound_files) {
      if (!checkmate::test_file_exists(cf)) {
        to_log(lg, "warn", "Confound file {cf} not found; skipping")
        next
      }
      df <- data.table::fread(cf)
      for (jj in seq_along(df)) {
        sfun <- splinefun(good_idx, df[[jj]][good_idx], method = "natural")
        yint <- sfun(t_interpolate)
        yint[t_interpolate < first_valid] <- df[[jj]][first_valid]
        yint[t_interpolate > last_valid] <- df[[jj]][last_valid]
        df[[jj]][t_interpolate] <- yint
      }
      has_header <- !all(grepl("^V[0-9]+$", names(df)))
      data.table::fwrite(df, file = cf, sep = "\t", col.names = has_header)
    }
  }

  return(out_file)
}


#' Remove Censored Volumes and Update Confounds
#'
#' Removes timepoints flagged in a censor file from a 4D NIfTI image. When
#' \code{confound_files} are provided, the corresponding rows in those files
#' are removed so that the time series remain aligned.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param censor_file Path to the 1D censor vector used to identify volumes to
#'   remove.
#' @param out_file The full path for the file output by this step
#' @param confound_files Optional character vector of confound or regressor
#'   files to update alongside the fMRI data.
#'
#' @param overwrite Logical; overwrite the output NIfTI if it exists.
#' @param lg Optional \code{Logger} object for message output.
#'
#' @return The path to the scrubbed NIfTI image.
#' @keywords internal
scrub_timepoints <- function(in_file, censor_file = NULL, out_file,
                             confound_files = NULL,
                             overwrite = FALSE, lg = NULL) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_string(out_file)
  checkmate::assert_flag(overwrite)
  checkmate::assert_character(confound_files, any.missing = FALSE, null.ok = TRUE)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  if (!checkmate::test_file_exists(censor_file)) {
    msg <- glue("In scrub_timepoints, cannot locate censor file: {censor_file}")
    to_log(lg, "fatal", msg)
  }

  censor <- as.integer(readLines(censor_file))
  t_scrub <- which(1L - censor == 1L) # bad timepoints are 0 in the censor file

  if (!any(t_scrub)) {
    to_log(lg, "info", "No timepoints to scrub found in {censor_file}. Scrubbing will not change the length of the output data.")
  } else {
    to_log(lg, "info", "Applying timepoint scrubbing, removing {length(t_scrub)} timepoints from {in_file}.")
  }

  # run 4D interpolation with Rcpp function
  run_logged(
    remove_nifti_volumes,
    infile = in_file,
    remove_tpts = t_scrub,
    outfile = out_file,
    logger = lg
  )

  if (length(confound_files) > 0 && length(t_scrub) > 0) {
    for (cf in confound_files) {
      if (!checkmate::test_file_exists(cf)) {
        to_log(lg, "warn", "Confound file {cf} not found; skipping")
        next
      }
      df <- data.table::fread(cf)
      df <- df[-t_scrub, , drop = FALSE]
      new_len <- nrow(df)
      has_header <- !all(grepl("^V[0-9]+$", names(df)))
      data.table::fwrite(df, file = cf, sep = "\t", col.names = has_header)
    }
    writeLines(rep("1", new_len), con = censor_file)
  }

  return(out_file)
}
filter_confounds <- function(confounds_df = NULL,
                             tr = NULL,
                             filter_type = c("notch", "lowpass"),
                             bandstop_min_bpm = NULL,
                             bandstop_max_bpm = NULL,
                             low_pass_hz = NULL,
                             filter_order = NULL,
                             columns = c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z"),
                             add_poly = TRUE,
                             out_file = NULL,
                             padtype = "constant",
                             padlen = NULL,
                             use_zi = TRUE,
                             lg = NULL) {
  checkmate::assert_data_frame(confounds_df, all.missing = TRUE)
  checkmate::assert_number(tr, lower = 0.01)
  filter_type <- match.arg(filter_type)
  checkmate::assert_character(columns, any.missing = FALSE, min.len = 1L)
  checkmate::assert_flag(add_poly)
  checkmate::assert_string(out_file, null.ok = TRUE)
  checkmate::assert_choice(padtype, c("constant", "odd", "even", "zero"))
  checkmate::assert_flag(use_zi)
  if (!is.null(padlen)) checkmate::assert_integerish(padlen, len = 1L, lower = -1L)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes")
  }

  # polynomial expansion -- first derivatives and squares
  poly_expand <- function(df, cols = NULL) {
    if (!is.null(cols)) checkmate::assert_subset(cols, names(df))

    # 1) first differences for each numeric column
    original_cols <- if (is.null(cols)) names(df) else cols
    for (col in original_cols) {
      if (is.numeric(df[[col]])) df[[paste0(col, "_derivative1")]] <- c(NA, diff(df[[col]]))
    }

    # 2) squares of originals + derivatives
    all_cols <- paste0(original_cols, "_derivative1")
    for (col in all_cols) {
      if (is.numeric(df[[col]])) df[[paste0(col, "_power2")]] <- df[[col]]^2
    }

    df
  }

  available_cols <- intersect(columns, names(confounds_df))
  missing_cols <- setdiff(columns, available_cols)
  if (length(missing_cols) > 0L) {
    to_log(lg, "fatal", "Missing motion columns in confounds: {paste(missing_cols, collapse = ', ')}")
  }

  fs <- 1 / tr
  nyquist <- fs / 2
  nyquist_bpm <- nyquist * 60
  padlen_val <- if (is.null(padlen)) -1L else as.integer(padlen)

  if (filter_type == "notch") {
    checkmate::assert_number(bandstop_min_bpm, lower = 0)
    checkmate::assert_number(bandstop_max_bpm, lower = 0)
    if (is.null(filter_order)) filter_order <- 4L
    checkmate::assert_integerish(filter_order, len = 1L, lower = 4L)
    filter_order <- as.integer(filter_order)
    if (filter_order %% 4L != 0L) {
      to_log(lg, "fatal", "filter_order must be divisible by 4 for notch filtering; received {filter_order}.")
    }
    n_filter_applications <- filter_order %/% 4L
    if (bandstop_max_bpm <= bandstop_min_bpm) {
      to_log(lg, "fatal", "bandstop_max_bpm ({bandstop_max_bpm}) must be greater than bandstop_min_bpm ({bandstop_min_bpm}).")
    }

    stopband_hz <- c(bandstop_min_bpm, bandstop_max_bpm) / 60
    stopband_hz_adjusted <- abs(stopband_hz - (floor((stopband_hz + nyquist) / fs) * fs))
    stopband_adjusted <- stopband_hz_adjusted * 60
    if (any(abs(stopband_adjusted - c(bandstop_min_bpm, bandstop_max_bpm)) > 1e-6)) {
      to_log(lg, "warn", "One or both filter frequencies are above Nyquist frequency ({round(nyquist_bpm, 2)} BPM), so they have been changed ({round(bandstop_min_bpm, 2)} --> {round(stopband_adjusted[1], 2)}, {round(bandstop_max_bpm, 2)} --> {round(stopband_adjusted[2], 2)} BPM).")
      bandstop_min_bpm <- stopband_adjusted[1]
      bandstop_max_bpm <- stopband_adjusted[2]
    }
    if (bandstop_max_bpm <= bandstop_min_bpm) {
      to_log(lg, "warn", "Adjusted notch band is invalid after Nyquist correction; skipping motion filtering.")
      return(confounds_df)
    }

    stopband_hz <- c(bandstop_min_bpm, bandstop_max_bpm) / 60
    f0 <- mean(stopband_hz)
    bandwidth <- abs(diff(stopband_hz))

    if (bandwidth <= 0) {
      to_log(lg, "warn", "Adjusted notch band has zero bandwidth; skipping motion filtering.")
      return(confounds_df)
    }

    if (f0 >= nyquist) {
      to_log(lg, "warn", "Adjusted notch center {round(f0, 4)} Hz exceeds the Nyquist frequency {round(nyquist, 4)} Hz for TR = {tr} s; skipping motion filtering.")
      return(confounds_df)
    }

    Q <- f0 / bandwidth
    coeffs <- iirnotch_r(f0 = f0, Q = Q, fs = fs)
    to_log(lg, "info", "Applying notch filter centered at {round(f0, 4)} Hz (Q = {round(Q, 2)}, order {filter_order}, passes {n_filter_applications}) to {length(available_cols)} motion columns.")
  } else {
    if (is.null(filter_order)) filter_order <- 2L
    checkmate::assert_number(low_pass_hz, lower = 0.001)
    if (low_pass_hz >= nyquist) {
      low_pass_hz_adjusted <- abs(low_pass_hz - (floor((low_pass_hz + nyquist) / fs) * fs))
      if (abs(low_pass_hz_adjusted - low_pass_hz) > 1e-6) {
        to_log(lg, "warn", "Low-pass filter frequency is above Nyquist frequency ({round(nyquist_bpm, 2)} BPM), so it has been changed ({round(low_pass_hz * 60, 2)} --> {round(low_pass_hz_adjusted * 60, 2)} BPM).")
      }
      low_pass_hz <- low_pass_hz_adjusted
    }
    if (low_pass_hz <= 0 || low_pass_hz >= nyquist) {
      to_log(lg, "warn", "Adjusted low-pass cutoff is invalid for TR = {tr} s; skipping motion filtering.")
      return(confounds_df)
    }
    checkmate::assert_integerish(filter_order, len = 1L, lower = 2L)
    if (!requireNamespace("signal", quietly = TRUE)) {
      to_log(lg, "fatal", "The 'signal' package must be installed for low-pass filtering.")
    }
    filter_order <- as.integer(filter_order)
    if (filter_order %% 2L != 0L) {
      to_log(lg, "fatal", "filter_order must be divisible by 2 for low-pass filtering; received {filter_order}.")
    }
    n_filter_applications <- filter_order %/% 2L
    coeffs <- signal::butter(n_filter_applications, low_pass_hz / nyquist, type = "low")
    to_log(lg, "info", "Applying low-pass filter (cutoff {round(low_pass_hz, 4)} Hz, order {filter_order}, effective {n_filter_applications}) to {length(available_cols)} motion columns.")
  }

  for (col_name in columns) {
    series <- as.numeric(confounds_df[[col_name]])
    if (anyNA(series)) {
      to_log(lg, "warn", "Column {col_name} contains missing values; replacing NAs with zeros before filtering.")
      series[is.na(series)] <- 0
    }
    filtered <- series
    if (filter_type == "notch") {
      for (ii in seq_len(n_filter_applications)) {
        filtered <- filtfilt_cpp(
          filtered,
          b = coeffs$b,
          a = coeffs$a,
          padlen = padlen_val,
          padtype = padtype,
          use_zi = use_zi
        )
      }
    } else {
      filtered <- filtfilt_cpp(
        filtered,
        b = coeffs$b,
        a = coeffs$a,
        padlen = padlen_val,
        padtype = padtype,
        use_zi = use_zi
      )
    }
    confounds_df[[col_name]] <- filtered
  }

  # always recompute derivatives and quadratics after filtering
  if (add_poly) confounds_df <- poly_expand(confounds_df, columns)

  if (!is.null(out_file)) {
    data.table::fwrite(confounds_df, file = out_file, sep = "\t", na = "n/a")
    return(invisible(out_file))
  }

  return(confounds_df)
}

#' Notch filter regressors (esp. motion parameters) from the confounds file
#'
#' Uses `iirnotch_r()` to design a single-frequency notch filter that
#' suppresses respiratory-related oscillations in the six rigid-body
#' motion parameters. The function filters the requested columns with
#' [`filtfilt_cpp()`] (zero-phase), and optionally writes the updated
#' confounds table to disk.
#'
#' @param confounds_df A data frame containing confound regressors to filter.
#' @param tr The repetition time of the scan sequence in seconds. Used
#'  to check that the stop band falls within the
#' @param bandstop_min_bpm Lower bound of the notch stop-band, in breaths
#'  per minute. This will be converted to Hz internally.
#' @param bandstop_max_bpm Upper bound of the notch stop-band, in breaths
#'  per minute. This will be converted to Hz internally.
#' @param columns Columns in `confounds_df` to filter. Defaults to the standard six
#'   rigid-body parameters: `rot_x`, `rot_y`, `rot_z`, `trans_x`,
#'   `trans_y`, `trans_z`.
#' @param out_file Optional output path. When provided, the filtered
#'   confounds table is written here (tab-delimited). If `NULL`, the
#'   modified table is returned invisibly.
#' @param padtype Passed to [`filtfilt_cpp()`]; governs how the edges are
#'   padded before filtering. One of `"constant"`, `"odd"`, `"even"`, or
#'   `"zero"`. Defaults to `"constant"` to match SciPy.
#' @param padlen Optional integer pad length forwarded to
#'   [`filtfilt_cpp()`]. If `NULL`, the default inside `filtfilt_cpp()`
#'   (`-1L`) is used.
#' @param use_zi Logical; whether to initialise the filter state using
#'   steady-state conditions (the default in `filtfilt_cpp()`).
#' @param lg Optional `Logger` (from `lgr`) used for status messages.
#'
#' @return If `out_file` is `NULL`, the filtered confounds are returned
#'   as a data frame. Otherwise the path to `out_file` is returned
#'   invisibly.
#'
#' @keywords internal
#' @importFrom checkmate assert_file_exists assert_number assert_character assert_string assert_choice assert_flag
#' @importFrom data.table fread fwrite
notch_filter <- function(confounds_df = NULL, tr = NULL, bandstop_min_bpm = NULL, bandstop_max_bpm = NULL,
    columns = c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z"), add_poly = TRUE,
    out_file = NULL, padtype = "constant",  padlen = NULL, use_zi = TRUE, lg = NULL) {

  filter_confounds(
    confounds_df = confounds_df,
    tr = tr,
    filter_type = "notch",
    bandstop_min_bpm = bandstop_min_bpm,
    bandstop_max_bpm = bandstop_max_bpm,
    columns = columns,
    add_poly = add_poly,
    out_file = out_file,
    padtype = padtype,
    padlen = padlen,
    use_zi = use_zi,
    lg = lg
  )
}

#' Compute framewise displacement from motion parameters
#'
#' Calculates Power-style framewise displacement (FD) from a matrix or
#' data frame containing the six rigid-body motion parameters. Rotations
#' are converted to displacements on the surface of a sphere with radius
#' `head_radius` (in millimetres) before summation. Optional arguments
#' mirror typical preprocessing steps (dropping initial dummy scans,
#' truncating to a final volume, or handling rotations recorded in
#' degrees).
#'
#' @param motion Numeric matrix or data frame with columns for
#'   `rot_x`, `rot_y`, `rot_z`, `trans_x`, `trans_y`, and `trans_z`.
#' @param head_radius Radius of the head (in millimetres) used to convert
#'   rotational parameters to linear displacements. Defaults to 50 mm.
#' @param columns Character vector giving the column order expected in
#'   `motion`. Defaults to the six canonical fMRIPrep motion columns.
#' @param rot_units Unit of the rotation parameters (`"rad"` or
#'   `"deg"`). Values in degrees are internally converted to radians
#'   prior to differencing.
#' @param drop_volumes Number of leading timepoints to discard (e.g., if
#'   dummy scans were dropped from the fMRI data). Defaults to 0.
#' @param last_volume Optional index of the final volume to retain. If
#'   supplied, frames beyond `last_volume` are excluded before FD is
#'   computed.
#' @param na_action Replacement applied to any remaining `NA` values
#'   after subsetting. Defaults to `0`, matching the behavior used in
#'   `notch_filter()`.
#'
#' @return Numeric vector of framewise displacement values (same length
#'   as the number of rows in `motion`). The first entry is zero by
#'   definition.
#'
#' @keywords internal
#' @importFrom checkmate assert_data_frame assert_number assert_character
framewise_displacement <- function(motion,
                                   head_radius = 50,
                                   columns = c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z"),
                                   rot_units = c("rad", "deg"),
                                   drop_volumes = 0L,
                                   last_volume = NULL,
                                   na_action = 0) {
  if (is.matrix(motion)) {
    if (is.null(colnames(motion))) stop("Matrix input must have column names matching the required motion parameters.")
    motion_mat <- motion
  } else {
    checkmate::assert_data_frame(motion, any.missing = TRUE)
    motion_mat <- as.matrix(motion)
  }

  checkmate::assert_number(head_radius, lower = 1)
  checkmate::assert_character(columns, any.missing = FALSE, len = 6L)
  rot_units <- match.arg(rot_units)
  checkmate::assert_integerish(drop_volumes, len = 1L, lower = 0L)
  if (is.null(last_volume)) {
    last_volume <- nrow(motion_mat)
  } else {
    checkmate::assert_integerish(last_volume, len = 1L, lower = 1L, upper = nrow(motion_mat))
  }

  start_idx <- drop_volumes + 1L
  if (start_idx > last_volume) stop("drop_volumes exceeds last_volume; no rows remain to compute framewise displacement.")
  motion_mat <- motion_mat[start_idx:last_volume, , drop = FALSE]

  missing_cols <- setdiff(columns, colnames(motion_mat))
  if (length(missing_cols)) stop(sprintf("Missing motion columns required for FD: %s", paste(missing_cols, collapse = ", ")))

  motion_mat <- motion_mat[, columns, drop = FALSE]
  storage.mode(motion_mat) <- "double"
  if (anyNA(motion_mat)) {
    warning("NAs detected in motion matrix; replacing missing values before FD calculation.", call. = FALSE)
    motion_mat[is.na(motion_mat)] <- na_action
  }
  if (nrow(motion_mat) == 0L) return(numeric())

  if (rot_units == "deg") { # convert degrees to radians if needed
    motion_mat[, columns[1:3]] <- motion_mat[, columns[1:3], drop = FALSE] * (pi / 180)
  }

  diffs <- rbind(rep(0, ncol(motion_mat)), diff(motion_mat))
  rot_contrib <- rowSums(abs(diffs[, columns[1:3], drop = FALSE])) * head_radius
  trans_contrib <- rowSums(abs(diffs[, columns[4:6], drop = FALSE]))

  rot_contrib + trans_contrib
}


#' Apply temporal filtering to a 4D NIfTI image
#'
#' Apply high-pass and/or low-pass temporal filtering to an fMRI time series.
#' By default this calls FSL's \code{fslmaths -bptf} but a Butterworth filter
#' implemented in \code{butterworth_filter_4d} can also be used. Filter cutoffs
#' are specified in Hz; for the FSL implementation they are internally converted
#' to sigma values in volumes using a standard FWHM-to-sigma transformation.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param out_file The full path for the file output by this step
#' @param low_pass_hz Upper frequency cutoff in Hz. Frequencies above this are removed (low-pass).
#'   Use \code{NULL} to omit the low-pass component (internally treated as \code{Inf}).
#' @param high_pass_hz Lower frequency cutoff in Hz. Frequencies below this are removed (high-pass).
#'   Use \code{NULL} or a non-positive value to omit the high-pass component (internally treated as \code{-Inf}).
#' @param tr Repetition time (TR) in seconds. Required to convert Hz to volumes.
#' @param overwrite Logical; whether to overwrite the output file if it exists.
#' @param lg Optional lgr object used for logging messages
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#' @param method Character. "fslmaths" to use FSL's -bptf or "butterworth" for a Butterworth filter.
#'
#' @return The path to the temporally filtered output NIfTI file.
#'
#' @details The mean image is added back after filtering to preserve signal intensity. Filtering
#' is skipped if the output file already exists and \code{overwrite = FALSE}.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom lgr get_logger
#' @importFrom checkmate assert_string assert_number assert_flag
temporal_filter <- function(in_file, out_file, low_pass_hz=NULL, high_pass_hz=NULL, tr=NULL,
                            overwrite=FALSE, lg=NULL, fsl_img = NULL,
                            method=c("fslmaths","butterworth")) {
  method <- match.arg(method)
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(out_file)
  checkmate::assert_number(low_pass_hz, null.ok = TRUE, na.ok = FALSE)
  checkmate::assert_number(high_pass_hz, null.ok = TRUE, na.ok = FALSE)
  if (is.null(low_pass_hz) && is.null(high_pass_hz)) stop("low_pass_hz and high_pass_hz are NULL, so no filtering can occur")
  if (is.null(low_pass_hz)) low_pass_hz <- Inf
  if (is.null(high_pass_hz) || abs(high_pass_hz) < 1e-6) high_pass_hz <- -Inf
  
  checkmate::assert_number(tr, lower = 0.01)
  checkmate::assert_flag(overwrite)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  if (is.infinite(low_pass_hz) && !is.infinite(high_pass_hz)) {
    to_log(lg, "info", "Applying high-pass filter with cutoff: {high_pass_hz} Hz (removes frequencies below this), TR = {tr}s")
  } else if (!is.infinite(low_pass_hz) && is.infinite(high_pass_hz)) {
    to_log(lg, "info", "Applying low-pass filter with cutoff: {low_pass_hz} Hz (removes frequencies above this), TR = {tr}s")
  } else if (!is.infinite(low_pass_hz) && !is.infinite(high_pass_hz)) {
    to_log(lg, "info", "Applying band-pass filter: {high_pass_hz} Hz - {low_pass_hz} Hz, TR = {tr}s")
  } else {
    to_log(lg, "fatal", "Problem with temporal_filter settings. Both low_pass_hz and high_pass_hz are infinite or invalid.")
  }
  to_log(lg, "debug", "in_file: {in_file}")
  
  if (method == "fslmaths") {
    # bptf specifies its filter cutoffs in terms of volumes, not frequencies
    fwhm_to_sigma <- sqrt(8 * log(2)) # Details here: https://www.mail-archive.com/hcp-users@humanconnectome.org/msg01393.html

    # set volumes to -1 to skip that side of filter in -bptf
    hp_volumes <- if (is.infinite(high_pass_hz)) -1 else 1 / (high_pass_hz * fwhm_to_sigma * tr)
    lp_volumes <- if (is.infinite(low_pass_hz)) -1 else 1 / (low_pass_hz * fwhm_to_sigma * tr)

    temp_tmean <- tempfile(pattern="tmean")
    on.exit(rm_niftis(temp_tmean), add = TRUE)
    run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {temp_tmean}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean)))
    run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -bptf {hp_volumes} {lp_volumes} -add {temp_tmean} {file_sans_ext(out_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean, out_file)))
  } else {
    to_log(lg, "info", "Using internal Butterworth temporal filtering function")
    # Note that butterworth_filter_4d accepts frequency cutoffs -- anything below low_hz is cut (high-pass), anything above high_hz is cut (low-pass)
    bw_low <- if (is.infinite(high_pass_hz)) NULL else high_pass_hz
    bw_high <- if (is.infinite(low_pass_hz)) NULL else low_pass_hz
    butterworth_filter_4d(infile = in_file, tr = tr, low_hz = bw_low, high_hz = bw_high, outfile = out_file, lg = lg)
  }
  
  return(out_file)
}

#' Apply AROMA-based denoising to an fMRI image
#'
#' Performs ICA-AROMA denoising by regressing out identified noise components from an fMRI
#' time series using the internal \code{lmfit_residuals_4d()} helper. When \code{nonaggressive = TRUE}
#' (the default) only the unique variance attributable to the specified components is removed,
#' matching FSL's non-aggressive \code{fsl_regfilt} behavior. Set \code{nonaggressive = FALSE} for
#' aggressive regression that fully removes the listed components. An intercept
#' is included and each voxel's pre-AROMA temporal mean is added back so that
#' denoising preserves the positive baseline intensity.
#' @param in_file Path to the input 4D NIfTI file.
#' @param out_file The full path for the file output by this step
#' @param mixing_file Path to the MELODIC mixing matrix (e.g., \code{*_desc-MELODIC_mixing.tsv}).
#' @param noise_ics Vector of ICA components to regress out (usually pulled from relevant aroma_timeseries.tsv file).
#' @param overwrite Logical; whether to overwrite the output file if it exists.
#' @param lg Optional lgr object used for logging messages
#' @param nonaggressive Logical; \code{TRUE} (default) performs partial regression to emulate
#'   non-aggressive AROMA. Set to \code{FALSE} for aggressive regression.
#'
#' @return Path to the denoised output NIfTI file. If required files are missing, returns \code{in_file} unmodified.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom checkmate assert_string assert_flag test_file_exists test_integerish
#' @importFrom data.table fread
apply_aroma <- function(in_file, out_file, mixing_file, noise_ics, overwrite = FALSE,
                        lg = NULL, nonaggressive = TRUE) {
  checkmate::assert_string(out_file)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(nonaggressive)
  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  to_log(lg, "debug", "in_file: {in_file}")
  to_log(lg, "debug", "mixing_file: {mixing_file}")

  if (isFALSE(checkmate::test_file_exists(mixing_file))) {
    to_log(lg, "warn", "Cannot find mixing file corresponding to {in_file}. Skipping AROMA regression")
    return(in_file)
  }

  if (!overwrite && checkmate::test_file_exists(out_file)) {
    to_log(lg, "info", "Output already exists at {out_file}; skipping AROMA regression.")
    return(out_file)
  }

  if (is.null(noise_ics) || length(noise_ics) == 0) {
    to_log(lg, "info", "No AROMA noise components provided; leaving data unchanged.")
    return(in_file)
  }

  if (isFALSE(checkmate::test_integerish(noise_ics, lower = 1, any.missing = FALSE))) {
    to_log(lg, "warn", "noise_ics must be a vector of positive integers identifying components to regress out. Skipping AROMA regression")
    return(in_file)
  }

  mixing_dt <- data.table::fread(mixing_file, header = FALSE, data.table = FALSE)
  mixing_mat <- as.matrix(mixing_dt)

  if (nrow(mixing_mat) == 0 || ncol(mixing_mat) == 0) {
    to_log(lg, "warn", "Mixing matrix {mixing_file} is empty; skipping AROMA regression")
    return(in_file)
  }

  storage.mode(mixing_mat) <- "double"

  comp_idx <- sort(unique(as.integer(noise_ics)))
  invalid_idx <- comp_idx[comp_idx < 1 | comp_idx > ncol(mixing_mat)]
  if (length(invalid_idx) > 0) {
    to_log(lg, "warn", "Dropping invalid AROMA component indices: {paste(invalid_idx, collapse = ', ')}")
    comp_idx <- setdiff(comp_idx, invalid_idx)
  }

  if (length(comp_idx) == 0) {
    to_log(lg, "info", "No valid AROMA noise components remain after filtering; leaving data unchanged.")
    return(in_file)
  }

  include_rows <- rep(TRUE, nrow(mixing_mat))
  exclusive_flag <- !isTRUE(nonaggressive)
  mode_label <- if (exclusive_flag) "aggressive" else "non-aggressive"

  to_log(lg, "info", "Regressing {length(comp_idx)} AROMA noise components using {mode_label} internal lmfit implementation.")
  run_logged(
    lmfit_residuals_4d,
    infile = in_file,
    X = mixing_mat,
    include_rows = include_rows,
    add_intercept = TRUE,
    outfile = out_file,
    preserve_mean = TRUE,
    regress_cols = comp_idx,
    exclusive = exclusive_flag,
    logger = lg
  )

  return(out_file)
}

#' Apply SUSAN-based spatial smoothing to a 4D fMRI image
#'
#' Performs spatial smoothing using FSL's \code{susan} algorithm, which adapts smoothing based
#' on local image intensity structure. A smoothing kernel defined by \code{fwhm_mm} is applied
#' and the extents mask is re-applied post-smoothing to constrain the result to original data extents.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param out_file The full path for the file output by this step
#' @param fwhm_mm Full-width at half-maximum (FWHM) of the Gaussian kernel in millimeters.
#' @param brain_mask Optional brain mask to guide intensity thresholding. If \code{NULL}, the whole image is used.
#' @param overwrite Logical; whether to overwrite the output file if it already exists.
#' @param lg Optional lgr object used for logging messages
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#'
#' @return Path to the spatially smoothed output NIfTI file.
#'
#' @details The SUSAN threshold is computed based on the 2nd and 50th percentiles of intensity values.
#' An extents mask is created prior to smoothing to ensure no new voxels are introduced in the output.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom checkmate assert_string assert_number assert_file_exists
spatial_smooth <- function(in_file, out_file, fwhm_mm = 6, brain_mask = NULL, overwrite = FALSE, lg = NULL, fsl_img=NULL) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_string(out_file)
  checkmate::assert_number(fwhm_mm, lower = 0.1)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  to_log(lg, "info", "Spatial smoothing with FHWM {fwhm_mm}mm kernel")
  to_log(lg, "debug", "in_file: {in_file}")

  fwhm_to_sigma <- sqrt(8 * log(2)) # Details here: https://www.mail-archive.com/hcp-users@humanconnectome.org/msg01393.html
  sigma <- fwhm_mm / fwhm_to_sigma

  p2_intensity <- image_quantile(in_file, brain_mask, .02)
  median_intensity <- image_quantile(in_file, brain_mask, .5)
  susan_thresh <- (median_intensity - p2_intensity) * .75 # also see featlib.tcl

  # always compute extents mask that is reapplied to data post-smoothing to avoid any "new" voxels
  extents_mask <- tempfile(pattern = "extents_mask")
  # susan generates a _usan_size.nii.gz file implicitly based on the out_file, track that too
  usan_file <- glue("{file_sans_ext(out_file)}_usan_size")
  on.exit(rm_niftis(c(extents_mask, usan_file)), add = TRUE)
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmin -bin {extents_mask} -odt char"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, extents_mask))) # save extents to temp file

  # compute mean functional image used in susan
  temp_tmean <- tempfile(pattern = "tmean")
  on.exit(rm_niftis(temp_tmean), add = TRUE)
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {temp_tmean}"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, temp_tmean))) # save tmean to temporary file
  run_fsl_command(glue("susan {file_sans_ext(in_file)} {susan_thresh} {sigma} 3 1 1 {temp_tmean} {susan_thresh} {file_sans_ext(out_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, temp_tmean, out_file)))

  # apply extents mask
  run_fsl_command(glue("fslmaths {file_sans_ext(out_file)} -mul {extents_mask} {file_sans_ext(out_file)} -odt float"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, extents_mask, out_file)))

  return(out_file)
}


#' Resolve the configured intensity-normalization mode
#'
#' Missing modes retain the historical run-wise scalar behavior. In the
#' denominator-guarded voxelwise PSC pathway, a reliable local baseline is
#' scaled to 100, very low positive baselines use a lower denominator bound,
#' and unidentified baselines use a conservative run-level fallback. These
#' guards do not clip observations or mask voxels. Both modes use the same
#' filename prefix and processing checkpoint; the mode is recorded in
#' normalization provenance rather than encoded in the prefix.
#'
#' @param intensity_cfg Intensity-normalization configuration list.
#' @return One of `"run_scalar"` or `"voxel_psc"`.
#' @keywords internal
resolve_intensity_normalization_mode <- function(intensity_cfg) {
  mode <- intensity_cfg$mode
  if (is.null(mode)) mode <- "run_scalar"
  checkmate::assert_choice(mode, c("run_scalar", "voxel_psc"))
  mode
}

#' Resolve the configured intensity-normalization target
#'
#' The target is the desired spatial median across reference-region voxels of
#' their 10%-trimmed temporal means for `run_scalar`. `target` is the canonical
#' configuration field and `global_median` remains accepted for backward
#' compatibility. Denominator-guarded `voxel_psc` always uses 100: reliable
#' local baselines are scaled to 100, while floor or fallback denominators
#' prevent unstable division without clipping or masking data. The fixed target
#' is required for regression coefficients to be expressed in approximate
#' percent-signal-change units; any configured scalar target is ignored in that
#' mode.
#'
#' @param intensity_cfg Intensity-normalization configuration list.
#' @param mode Optional resolved normalization mode.
#' @return A finite positive numeric scalar.
#' @keywords internal
resolve_intensity_normalization_target <- function(
    intensity_cfg,
    mode = resolve_intensity_normalization_mode(intensity_cfg)) {
  if (identical(mode, "voxel_psc")) return(100)

  target <- intensity_cfg$target
  if (!checkmate::test_number(target, finite = TRUE, lower = 0.1)) {
    target <- intensity_cfg$global_median
  }
  if (!checkmate::test_number(target, finite = TRUE, lower = 0.1)) {
    stop("intensity_normalize requires a finite positive target (legacy field: global_median).")
  }
  as.numeric(target)
}

#' Identify volumes used to estimate the run reference intensity
#'
#' All volumes are included when no matching metadata are available. When an
#' fMRIPrep confounds table is present, volumes marked as non-steady-state are
#' excluded. A matching pipeline censor vector is also honored. These
#' exclusions affect estimation of the voxelwise temporal baselines and run
#' reference intensity; the eventual multiplier is still applied to every
#' volume in the run.
#'
#' @param in_file Path to the input 4D BOLD NIfTI image.
#' @param confounds_file Optional path to an fMRIPrep confounds TSV file.
#' @param censor_file Optional path to a censor file containing one value per
#'   volume, where a positive value means that the volume is included.
#' @param lg Optional logger for warnings about incompatible metadata.
#' @return Logical vector with one value per BOLD volume. `TRUE` means that the
#'   volume is used to estimate the intensity reference.
#' @keywords internal
intensity_reference_frames <- function(in_file, confounds_file = NULL,
                                       censor_file = NULL, lg = NULL) {
  header <- RNifti::niftiHeader(in_file)
  dims <- header$dim
  if (length(dims) < 5L || dims[[1L]] < 4L || dims[[5L]] < 1L) {
    stop("Intensity-reference estimation requires a 4D BOLD image.")
  }
  n_volumes <- as.integer(dims[[5L]])
  include <- rep(TRUE, n_volumes)

  if (checkmate::test_file_exists(confounds_file)) {
    confounds <- data.table::fread(
      confounds_file, na.strings = c("n/a", "NA", "."),
      data.table = FALSE, showProgress = FALSE
    )
    if (nrow(confounds) == n_volumes) {
      nonsteady_cols <- grep("^non_steady_state_outlier", names(confounds), value = TRUE)
      if (length(nonsteady_cols) > 0L) {
        nonsteady <- rowSums(as.matrix(confounds[, nonsteady_cols, drop = FALSE]), na.rm = TRUE) > 0
        include[nonsteady] <- FALSE
      }
    } else if (!is.null(lg)) {
      to_log(lg, "warn", "Ignoring non-steady-state columns for intensity normalization: confounds have {nrow(confounds)} rows but BOLD has {n_volumes} volumes.")
    }
  }

  if (checkmate::test_file_exists(censor_file)) {
    censor <- suppressWarnings(as.numeric(readLines(censor_file, warn = FALSE)))
    if (length(censor) == n_volumes && all(is.finite(censor))) {
      include <- include & censor > 0
    } else if (!is.null(lg)) {
      to_log(lg, "warn", "Ignoring intensity-normalization censor file because it does not contain one finite value per BOLD volume: {censor_file}")
    }
  }

  include
}

#' Prepare robust run-scalar or denominator-guarded voxelwise PSC normalization
#'
#' Selects a fixed region of stable, positive-signal functional voxels from the
#' input BOLD image. It then measures the reference intensity after enabled
#' masking and smoothing: a 10% trimmed temporal mean is calculated for each
#' reference voxel, followed by the spatial median across voxels. In
#' `run_scalar` mode, the function returns `target / reference_location`. In
#' `voxel_psc` mode, it calculates a robust temporal baseline at every voxel and
#' writes a denominator-guarded multiplier map targeting 100.
#'
#' @details This is the single internal reference-selection policy used by the
#'   postprocessing pipeline. Its conservative quality thresholds are fixed to
#'   support consistent scaling across projects rather than exposed as tuning
#'   options. The input image must retain a finite, positive temporal baseline;
#'   an image that has already been demeaned or residualized is unsuitable.
#'
#'   "Guarded PSC" refers specifically to protection against unstable division
#'   by a local baseline. A reliable voxel uses `100 / local_baseline`; a finite
#'   positive baseline below 20% of the core reference location uses that lower
#'   denominator bound; and a baseline that is nonfinite, nonpositive, or based
#'   on too few eligible frames uses the conservative run multiplier
#'   `100 / reference_location`. The floor and fallback voxels remain in the
#'   output but are not exact local PSC. The guards do not clip observations,
#'   impute data, or create a binary validity mask. Only `apply_mask` controls
#'   spatial inclusion in the output.
#'
#'   When a logger is supplied, the function reports PSC category counts and
#'   percentages within the conservative functional automask at info level.
#'   Counts across the complete image grid are reported at debug level because
#'   they commonly include many background voxels and are less useful for
#'   routine QA. Both summaries distinguish ordinary PSC, denominator-floor,
#'   insufficient-frame fallback, and invalid-baseline fallback voxels.
#'
#' @param in_file Path to the input 4D BOLD NIfTI image used to choose the
#'   reference voxels and baseline-estimation volumes.
#' @param target Desired run reference intensity after scaling. Specifically,
#'   this is the desired spatial median across reference voxels of their
#'   10%-trimmed temporal means for `run_scalar`. It is fixed to 100 for
#'   `voxel_psc`.
#' @param mode Either `"run_scalar"` (default) or `"voxel_psc"`. The latter
#'   uses ordinary local PSC where possible and denominator floor/fallback
#'   factors elsewhere; it does not clip or mask the image.
#' @param calibration_file Path to the 4D BOLD image after enabled masking and
#'   smoothing but before temporal denoising. The run reference intensity is
#'   measured on this image and the multiplier is applied to it. Defaults to
#'   `in_file` for direct use and backward compatibility.
#' @param calibration_steps Character vector naming processing steps completed
#'   before `calibration_file`.
#' @param confounds_file Optional path to an fMRIPrep confounds TSV file used to
#'   identify non-steady-state volumes.
#' @param censor_file Optional path to the pipeline censor vector used to omit
#'   censored volumes from reference estimation.
#' @param automask_file Optional path for the temporary conservative automask.
#'   If `NULL`, a temporary file is created and removed automatically.
#' @param core_file Output path for the fixed reference-region mask, named the
#'   reference-core mask in saved files and metadata.
#' @param sidecar_file Optional JSON provenance sidecar path.
#' @param scale_file Optional output path for the 3D voxelwise multiplier map.
#'   Used only for `voxel_psc`; an empty string keeps the map in memory only.
#' @param lg Optional logger.
#' @return List containing paths to the reference-region outputs, the run
#'   reference intensity (`reference_location`), requested `target`, calculated
#'   scalar factor or PSC multiplier map, logical baseline-estimation volume
#'   vector (`include_frames`), and QA summaries.
#' @keywords internal
prepare_intensity_reference <- function(in_file, target = 10000,
                                        mode = "run_scalar",
                                        calibration_file = in_file,
                                        calibration_steps = character(),
                                        confounds_file = NULL,
                                        censor_file = NULL,
                                        automask_file = NULL,
                                        core_file = "", sidecar_file = NULL,
                                        scale_file = "",
                                        lg = NULL) {
  checkmate::assert_file_exists(in_file)
  checkmate::assert_file_exists(calibration_file)
  checkmate::assert_choice(mode, c("run_scalar", "voxel_psc"))
  if (identical(mode, "voxel_psc")) target <- 100
  checkmate::assert_number(target, finite = TRUE, lower = 0.1)
  checkmate::assert_character(calibration_steps, any.missing = FALSE)
  cleanup_automask <- is.null(automask_file)
  if (cleanup_automask) automask_file <- tempfile(fileext = ".nii.gz")
  checkmate::assert_string(automask_file)
  checkmate::assert_string(core_file)
  checkmate::assert_string(scale_file)
  if (cleanup_automask) on.exit(unlink(automask_file), add = TRUE)

  dir.create(dirname(automask_file), recursive = TRUE, showWarnings = FALSE)
  if (nzchar(core_file)) dir.create(dirname(core_file), recursive = TRUE, showWarnings = FALSE)

  automask(
    in_file, outfile = automask_file, clfrac = 0.5, NN = 1L,
    SIhh = 0, peels = 1L, fill_holes = FALSE, dilate_steps = 0L
  )

  include_frames <- intensity_reference_frames(
    in_file, confounds_file = confounds_file,
    censor_file = censor_file, lg = lg
  )

  # These are implementation constants, not user-facing tuning parameters.
  core_result <- derive_reference_core(
    img = in_file,
    candidate_mask = automask_file,
    include_frames = include_frames,
    baseline_method = "trimmed_mean",
    baseline_trim = 0.10,
    baseline_floor_fraction = 0.20,
    relative_noise_mad_cutoff = 5.0,
    spike_mad_cutoff = 6.0,
    max_spike_fraction = 0.05,
    min_valid_frames = 20L,
    outfile = core_file
  )

  core <- as.vector(as.array(core_result$core)) > 0
  candidate_count <- unname(core_result$counts[["agreement"]])
  core_count <- sum(core)
  core_fraction <- core_count / candidate_count

  if (candidate_count < 100L) {
    stop("The normalization automask contains fewer than 100 candidate voxels.")
  }
  if (core_count == 0L || !is.finite(core_fraction) || core_fraction < 0.50) {
    stop(sprintf(
      paste0(
        "The stable intensity-reference region retained only %d of %d ",
        "candidate voxels; intensity normalization cannot be estimated safely."
      ),
      core_count, candidate_count
    ))
  }

  calibration <- measure_reference_location(
    img = calibration_file,
    core_mask = core_result$core,
    include_frames = include_frames,
    baseline_method = "trimmed_mean",
    baseline_trim = 0.10,
    min_valid_frames = 20L
  )
  if (!is.finite(calibration$usable_core_fraction) ||
      calibration$usable_core_fraction < 0.50) {
    stop(sprintf(
      paste0(
        "Only %d of %d fixed reference voxels have a finite positive baseline ",
        "at the normalization point after spatial processing."
      ),
      calibration$usable_core_voxels, calibration$core_voxels
    ))
  }
  reference_location <- unname(calibration$reference_location)
  if (!is.finite(reference_location) || reference_location <= 0) {
    stop(paste(
      "Could not estimate a finite positive run reference intensity for",
      "intensity normalization."
    ))
  }

  scale_factor <- NULL
  psc_result <- NULL
  if (identical(mode, "run_scalar")) {
    scale_factor <- target / reference_location
    if (!is.finite(scale_factor) || scale_factor <= 0) {
      stop("Could not estimate a finite positive run intensity multiplier.")
    }
  } else {
    if (nzchar(scale_file)) {
      dir.create(dirname(scale_file), recursive = TRUE, showWarnings = FALSE)
    }
    psc_result <- derive_voxel_psc_scale(
      img = calibration_file,
      reference_location = reference_location,
      qa_mask = automask_file,
      include_frames = include_frames,
      target = 100,
      baseline_trim = 0.10,
      baseline_floor_fraction = 0.20,
      min_valid_frames = 20L,
      outfile = scale_file
    )
  }

  summary <- list(
    method = "automask_reference_core_v1",
    normalization_method = if (identical(mode, "run_scalar")) {
      "run_scalar_reference_v1"
    } else {
      "guarded_voxel_psc_v1"
    },
    normalization_mode = mode,
    source_file = normalizePath(in_file, winslash = "/", mustWork = FALSE),
    calibration_file = normalizePath(calibration_file, winslash = "/", mustWork = FALSE),
    calibration_stage = "post_spatial_pre_temporal",
    calibration_steps = calibration_steps,
    target = as.numeric(target),
    reference_location = reference_location,
    scale_factor = scale_factor,
    scale_file = if (identical(mode, "voxel_psc") && nzchar(scale_file)) {
      basename(scale_file)
    } else {
      NULL
    },
    eligible_frames = sum(include_frames),
    total_frames = length(include_frames),
    candidate_voxels = candidate_count,
    core_voxels = core_count,
    core_fraction = core_fraction,
    calibration_usable_core_voxels = unname(calibration$usable_core_voxels),
    calibration_usable_core_fraction = unname(calibration$usable_core_fraction),
    internal_settings = list(
      automask_clfrac = 0.5,
      automask_connectivity = 1L,
      automask_peels = 1L,
      automask_fill_holes = FALSE,
      automask_dilate_steps = 0L,
      temporal_location = "trimmed_mean",
      temporal_trim = 0.10,
      baseline_floor_fraction = 0.20,
      relative_noise_mad_cutoff = 5.0,
      spike_mad_cutoff = 6.0,
      max_spike_fraction = 0.05
    ),
    thresholds = core_result$thresholds,
    counts = as.list(core_result$counts)
  )

  if (identical(mode, "voxel_psc")) {
    summary$psc_guard <- list(
      definition = paste(
        "Denominator-guarded baseline-to-100 scaling: reliable local",
        "baselines use 100/baseline, low positive baselines use a fixed",
        "lower denominator bound, and unidentified baselines use the",
        "run-reference fallback. No observation clipping or voxel masking."
      ),
      target = 100,
      denominator_floor_fraction = 0.20,
      denominator_floor = unname(psc_result$denominator_floor),
      reference_scale = unname(psc_result$reference_scale),
      maximum_scale = unname(psc_result$maximum_scale),
      grid_counts = as.list(psc_result$grid_counts),
      automask_qa_counts = as.list(psc_result$qa_mask_counts),
      categories = list(
        ordinary_psc = paste(
          "Finite positive trimmed baseline at or above the floor;",
          "multiplier = 100 / local baseline; exact robust local PSC."
        ),
        denominator_floor = paste(
          "Finite positive trimmed baseline below the floor; multiplier =",
          "100 / floor; bounded amplification but not exact local PSC."
        ),
        insufficient_frames_fallback = paste(
          "Fewer than 20 finite eligible frames; multiplier = 100 /",
          "reference location; retained but not exact local PSC."
        ),
        invalid_baseline_fallback = paste(
          "Nonfinite or nonpositive trimmed baseline; multiplier = 100 /",
          "reference location; retained but not exact local PSC."
        )
      ),
      policy = paste(
        "Reliable voxels use 100 / trimmed baseline; positive low baselines",
        "use the denominator floor; unidentified baselines use 100 /",
        "reference_location. Guards alter denominators only: observations are",
        "not clipped and no PSC validity mask is applied."
      )
    )
  }

  if (checkmate::test_string(sidecar_file) && nzchar(sidecar_file)) {
    dir.create(dirname(sidecar_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(
      jsonlite::toJSON(summary, auto_unbox = TRUE, pretty = TRUE, digits = NA),
      con = sidecar_file, useBytes = TRUE
    )
  }

  if (!is.null(lg)) {
    if (identical(mode, "run_scalar")) {
      to_log(lg, "info", paste0(
        "Selected {core_count}/{candidate_count} candidate voxels for the intensity-reference region; ",
        "run reference intensity at the normalization point={format(reference_location, digits=8)}, ",
        "target={target}, multiplier={format(scale_factor, digits=8)}"
      ))
    } else {
      qa_counts <- psc_result$qa_mask_counts
      grid_counts <- psc_result$grid_counts
      qa_total <- as.numeric(qa_counts[["qa_mask_voxels"]])
      grid_total <- sum(as.numeric(grid_counts))
      count_pct <- function(count, total) {
        count <- as.numeric(count)
        pct <- if (is.finite(total) && total > 0) 100 * count / total else NA_real_
        sprintf("%d (%.3f%%)", count, pct)
      }

      to_log(lg, "info", paste0(
        "Prepared denominator-guarded voxelwise PSC scaling from {core_count}/{candidate_count} reference-core voxels; ",
        "reference intensity={format(reference_location, digits=8)}, ",
        "denominator floor={format(psc_result$denominator_floor, digits=8)}, ",
        "PSC target=100. Guards bound denominators; they do not clip observations or apply an output mask."
      ))
      to_log(lg, "info", paste0(
        "PSC denominator categories within conservative automask (n=", qa_total,
        "): ordinary PSC=", count_pct(qa_counts[["ordinary_psc"]], qa_total),
        "; denominator floor=", count_pct(qa_counts[["denominator_floor"]], qa_total),
        "; insufficient-frame fallback=",
        count_pct(qa_counts[["insufficient_frames_fallback"]], qa_total),
        "; invalid-baseline fallback=",
        count_pct(qa_counts[["invalid_baseline_fallback"]], qa_total), "."
      ))
      to_log(lg, "debug", paste0(
        "PSC denominator categories across full image grid (n=", grid_total,
        "): ordinary PSC=", count_pct(grid_counts[["ordinary_psc"]], grid_total),
        "; denominator floor=", count_pct(grid_counts[["denominator_floor"]], grid_total),
        "; insufficient-frame fallback=",
        count_pct(grid_counts[["insufficient_frames_fallback"]], grid_total),
        "; invalid-baseline fallback=",
        count_pct(grid_counts[["invalid_baseline_fallback"]], grid_total), "."
      ))
    }
  }

  summary$include_frames <- include_frames
  summary$core_file <- core_file
  summary$automask_file <- automask_file
  summary$sidecar_file <- sidecar_file
  summary$scale_file <- scale_file
  summary$scale_map <- if (identical(mode, "voxel_psc")) psc_result$scale else NULL
  summary
}

#' Apply run-scalar or denominator-guarded voxelwise PSC normalization
#'
#' Applies either one previously estimated positive run multiplier or a positive
#' 3D voxelwise multiplier map. BrainGnomes estimates both after masking and
#' smoothing and applies them before temporal denoising.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param out_file Full path for the intensity-normalized output file.
#' @param scale_factor Finite positive multiplier calculated as `target / L`,
#'   where `L` is the spatial median across reference voxels of their
#'   10%-trimmed temporal means. Required for `run_scalar`.
#' @param mode Either `"run_scalar"` (default) or `"voxel_psc"`.
#' @param scale_file Path to the 3D denominator-guarded PSC multiplier map.
#'   Reliable voxels use `100 / local_baseline`; floor and fallback multipliers
#'   are already encoded in this map. Required for `voxel_psc`.
#' @param overwrite Logical; whether to overwrite the output file if it exists.
#' @param lg Optional lgr logger used for messages.
#' @param fsl_img Optional Singularity image used to execute FSL commands.
#'
#' @return Path to the intensity-normalized output NIfTI file.
#'
#' @details The input is rescaled using \code{fslmaths -mul}; FSL broadcasts a
#' 3D PSC map across the 4D series. This function only applies the supplied
#' multiplier. "Guarded" describes how the map's denominators were bounded or
#' replaced during calibration; this application step does not clip
#' observations, apply the reference core as a mask, or re-estimate a baseline
#' from filtered or residualized data.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom checkmate assert_string assert_number
intensity_normalize <- function(in_file, out_file, scale_factor = NULL,
                                mode = "run_scalar", scale_file = NULL,
                                overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(out_file)
  checkmate::assert_choice(mode, c("run_scalar", "voxel_psc"))
  if (identical(mode, "run_scalar")) {
    checkmate::assert_number(
      scale_factor, finite = TRUE, lower = .Machine$double.eps
    )
  } else {
    checkmate::assert_file_exists(scale_file)
  }

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  if (identical(mode, "run_scalar")) {
    to_log(lg, "info", "Applying fixed run-wise intensity multiplier: {format(scale_factor, digits=8)}")
    multiplier <- scale_factor
    bind_paths <- dirname(c(in_file, out_file))
  } else {
    to_log(lg, "info", "Applying fixed denominator-guarded voxelwise PSC multiplier map (no clipping or masking): {scale_file}")
    multiplier <- file_sans_ext(scale_file)
    bind_paths <- dirname(c(in_file, out_file, scale_file))
  }

  run_fsl_command(
    glue("fslmaths {file_sans_ext(in_file)} -mul {multiplier} {file_sans_ext(out_file)} -odt float"),
    log_file = log_file, fsl_img = fsl_img, bind_paths = bind_paths
  )
  return(out_file)
}

#' Residualize matrix time series using lm()
#'
#' Applies the same confound regression logic as \code{lmfit_residuals_4d} but operates on an
#' in-memory matrix rather than a NIfTI image on disk. Each column of \code{Y} is treated as a
#' separate time series, and nuisance regressors supplied in \code{X} are regressed out using
#' \code{stats::lm.fit()} on the uncensored timepoints.
#'
#' Constant columns in the design matrix are removed automatically (with the first intercept
#' column preserved). Optional censoring allows the fit to use only valid rows while predictions
#' are generated for the full series.
#'
#' @param Y Numeric matrix of time series to residualize (rows = timepoints, columns = signals).
#' @param X Numeric design matrix with the same number of rows as \code{Y}.
#' @param include_rows Optional logical vector marking rows to use during fitting.
#' @param add_intercept Logical; add an intercept column when the design lacks one.
#' @param preserve_mean Logical; keep the original mean of uncensored timepoints.
#' @param set_mean Numeric; shift residuals so every column has this mean (ignored when \code{preserve_mean = TRUE}).
#' @param regress_cols Optional integer vector (1-based) selecting columns of \code{X} to regress out.
#' @param exclusive Logical; if \code{TRUE}, the fit only uses columns listed in \code{regress_cols}
#'   (and the intercept, if present).
#'
#' @return A numeric matrix of residuals with the same dimensions as \code{Y}.
#' @keywords internal
#' @seealso lmfit_residuals_4d
lmfit_residuals_mat <- function(Y, X, include_rows = NULL, add_intercept = FALSE,
                                preserve_mean = FALSE, set_mean = 0,
                                regress_cols = NULL, exclusive = FALSE) {
  Y <- as.matrix(Y)
  X <- as.matrix(X)
  checkmate::assert_matrix(Y, mode = "numeric", any.missing = FALSE)
  checkmate::assert_matrix(X, mode = "numeric", nrows = nrow(Y), any.missing = FALSE)

  n_t <- nrow(Y)
  use_set <- abs(set_mean) > 1e-8
  if (use_set && preserve_mean) {
    warning("Cannot use preserve_mean = TRUE and have a non-zero value for set_mean. The set_mean will be ignored.")
    use_set <- FALSE
  }

  if (!is.null(include_rows)) {
    if (length(include_rows) != n_t) {
      stop(sprintf("include_rows must be length %d (number of timepoints) or empty.", n_t))
    }
    if (!is.logical(include_rows)) {
      include_rows <- as.logical(include_rows)
    }
    checkmate::assert_logical(include_rows, len = n_t, any.missing = FALSE)
    include_idx <- which(include_rows)
  } else {
    include_idx <- seq_len(n_t)
  }

  if (length(include_idx) == 0) {
    stop("No timepoints selected for fitting (include_rows has no TRUE values).")
  }

  X_valid <- X[include_idx, , drop = FALSE]
  keep_cols <- integer(0)
  dropped_constant <- integer(0)
  has_intercept <- FALSE
  intercept_original <- NA_integer_

  for (j in seq_len(ncol(X))) {
    col <- X_valid[, j]
    min_val <- min(col)
    max_val <- max(col)
    range_val <- abs(max_val - min_val)
    is_intercept <- abs(max_val - 1) < 1e-8 && abs(min_val - 1) < 1e-8

    if (is_intercept && !has_intercept) {
      has_intercept <- TRUE
      intercept_original <- j
      keep_cols <- c(keep_cols, j)
    } else if (range_val < 1e-8) {
      dropped_constant <- c(dropped_constant, j)
    } else {
      keep_cols <- c(keep_cols, j)
    }
  }

  if (length(dropped_constant) > 0) {
    warning(sprintf("Dropping constant column(s) from design matrix: %s", paste(dropped_constant, collapse = ", ")))
  }

  if (length(keep_cols) > 0) {
    X_use <- X[, keep_cols, drop = FALSE]
  } else {
    X_use <- matrix(0, nrow = n_t, ncol = 0)
  }

  original_to_kept <- rep(NA_integer_, ncol(X))
  if (length(keep_cols) > 0) {
    original_to_kept[keep_cols] <- seq_along(keep_cols)
  }

  intercept_idx <- if (!is.na(intercept_original)) original_to_kept[intercept_original] else NA_integer_

  if (length(regress_cols)) {
    checkmate::assert_integerish(regress_cols, lower = 1, any.missing = FALSE)
    regress_cols_vec <- as.integer(regress_cols)
  } else {
    regress_cols_vec <- integer(0)
  }

  if (exclusive && length(regress_cols_vec) == 0) {
    stop("exclusive = TRUE requires regress_cols to specify at least one column.")
  }

  regress_mask <- rep(FALSE, ncol(X_use))
  if (length(regress_cols_vec) > 0) {
    for (col in regress_cols_vec) {
      if (col < 1 || col > length(original_to_kept)) next
      mapped <- original_to_kept[col]
      if (!is.na(mapped)) {
        regress_mask[mapped] <- TRUE
      }
    }
  } else if (!exclusive && ncol(X_use) > 0) {
    regress_mask[] <- TRUE
  }

  if (!has_intercept && add_intercept) {
    X_use <- cbind("(Intercept)" = 1, X_use)
    if (length(original_to_kept) > 0) {
      mapped_idx <- which(!is.na(original_to_kept))
      original_to_kept[mapped_idx] <- original_to_kept[mapped_idx] + 1L
    }
    regress_mask <- if (length(regress_mask) > 0) c(TRUE, regress_mask) else TRUE
    has_intercept <- TRUE
    intercept_idx <- 1L
  }

  regress_indices <- which(regress_mask)
  if (length(regress_indices) == 0 && ncol(X_use) > 0 && !exclusive && length(regress_cols_vec) == 0) {
    regress_indices <- seq_len(ncol(X_use))
  }

  if (exclusive && length(regress_indices) == 0) {
    stop("exclusive = TRUE requires at least one usable regressor column after preprocessing.")
  }

  if (ncol(X_use) == 0) {
    stop("Design matrix has no columns after preprocessing.")
  }

  fit_cols <- if (exclusive) {
    unique(c(regress_indices, if (!is.na(intercept_idx)) intercept_idx else integer(0)))
  } else {
    seq_len(ncol(X_use))
  }
  fit_cols <- fit_cols[fit_cols >= 1 & fit_cols <= ncol(X_use)]

  if (length(fit_cols) == 0) {
    stop("Design matrix has no columns after preprocessing.")
  }

  X_fit <- X_use[, fit_cols, drop = FALSE]
  if (length(regress_indices) > 0) {
    X_regress <- X_use[, regress_indices, drop = FALSE]
  } else {
    X_regress <- matrix(0, nrow = n_t, ncol = 0)
  }

  predict_indices <- integer(length(regress_indices))
  if (length(regress_indices) > 0) {
    matches <- match(regress_indices, fit_cols)
    if (any(is.na(matches))) {
      stop("Internal error: regression column not present in fitting matrix.")
    }
    predict_indices <- matches
  }

  n_t_sub <- length(include_idx)
  if (n_t_sub < ncol(X_fit)) {
    stop(sprintf("Not enough uncensored timepoints to estimate model: %d available, but need at least %d", n_t_sub, ncol(X_fit)))
  }

  X_sub <- X_fit[include_idx, , drop = FALSE]
  residuals_mat <- matrix(NA_real_, nrow = n_t, ncol = ncol(Y))
  const_tol <- 1e-6

  for (col_idx in seq_len(ncol(Y))) {
    y <- Y[, col_idx]
    y_sub <- y[include_idx]

    if (max(y_sub) - min(y_sub) < const_tol) {
      residuals <- rep(0, n_t)
    } else {
      fit <- stats::lm.fit(x = X_sub, y = y_sub)
      coef_fit <- fit$coefficients
      if (length(coef_fit) != ncol(X_fit)) {
        coef_fit <- rep_len(coef_fit, ncol(X_fit))
      }
      coef_fit[is.na(coef_fit)] <- 0
      if (length(regress_indices) > 0) {
        beta_sub <- coef_fit[predict_indices]
        fitted <- as.vector(X_regress %*% beta_sub)
      } else {
        fitted <- rep(0, n_t)
      }
      residuals <- y - fitted
    }

    if (preserve_mean) {
      residuals <- residuals + mean(y_sub)
    } else if (use_set) {
      residuals <- residuals + set_mean
    }

    residuals_mat[, col_idx] <- residuals
  }

  residuals_mat
}

#' Regress confound time series from a 4D fMRI image
#'
#' Uses FSL's \code{fsl_glm} to remove nuisance regressors from a 4D NIfTI image. The residuals
#' from the regression are re-centered by adding back the temporal mean of the original image.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param to_regress Path to a text file containing nuisance regressors (one column per regressor).
#' @param out_file The full path for the file output by this step
#' @param censor_file An optional censor file (1s indicate volumes to keep) that is used to 
#' @param overwrite Logical; whether to overwrite the output file if it already exists.
#' @param lg Optional lgr object used for logging messages
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#'
#' @return Path to the residualized output NIfTI file.
#'
#' @details The regressors are converted to FSL's binary matrix format using \code{Text2Vest}.
#' The residuals are computed using \code{fsl_glm}, and the temporal mean of the original image is
#' added back to preserve baseline signal intensity.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom checkmate assert_file_exists assert_string
confound_regression <- function(in_file, out_file, to_regress=NULL, censor_file = NULL, overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_file_exists(to_regress)
  checkmate::assert_string(out_file)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  method <- "lmfit" # default -- supports fitting coefficients to good timepoints

  if (method == "fsl") {
    # convert text file to FSL vest file for fsl_glm to accept it
    vest_file <- tempfile(pattern = "regressors", fileext = ".mat")
    on.exit(unlink(vest_file), add = TRUE)
    run_fsl_command(glue("Text2Vest {to_regress} {vest_file}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(to_regress, vest_file)))
    
    # because the residuals will be demeaned and intensity normalization should follow this step, add back in the temporal mean from the pre-regression image
    temp_tmean <- tempfile(pattern="tmean")
    on.exit(rm_niftis(temp_tmean), add = TRUE)
    run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {temp_tmean}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean)))
    run_fsl_command(glue("fsl_glm -i {file_sans_ext(in_file)} -d {vest_file} --out_res={file_sans_ext(out_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, vest_file, out_file)))
    run_fsl_command(glue("fslmaths {file_sans_ext(out_file)} -add {temp_tmean} {file_sans_ext(out_file)}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(out_file, temp_tmean)))

    # 3dTproject for regression (deprecated to keep all commands in FSL)
    # regress_cmd <- glue("3dTproject -input {in_file} -prefix {out_file}_afni -ort {to_regress} -polort 0")
  } else if (method == "lmfit") {
    to_log(lg, "info", "Using internal lmfit confound regression function")
    Xmat <- data.table::fread(to_regress, sep = "\t", header = FALSE)
    good_vols <- rep(TRUE, nrow(Xmat))
    if (checkmate::test_file_exists(censor_file)) {
      good_vols <- as.logical(as.integer(readLines(censor_file))) # bad timepoints are 0 in the censor file
      if (sum(good_vols) < length(good_vols)) {
        to_log(
          lg,
          "info",
          "Censor file {censor_file} excludes {length(good_vols) - sum(good_vols)} volumes; fitting confound regression with {sum(good_vols)} of {length(good_vols)} volumes (all volumes are retained in the output)."
        )
      }
    }
    
    run_logged(
      lmfit_residuals_4d,
      infile = in_file,
      X = as.matrix(Xmat),
      include_rows = good_vols,
      outfile = out_file,
      preserve_mean = TRUE,
      logger = lg
    )
  }
  
  return(out_file)
}


#' Compute a loose brain mask from functional MRI data using FSL
#'
#' Generates a brain mask from a functional image using a modified FSL approach
#' based on the 98-2 percentile intensity method. This method combines BET skull-stripping
#' with percentile thresholding and binary dilation to produce a conservative mask.
#'
#' @param in_file Path to the input 4D NIfTI functional image.
#' @param lg Optional lgr object used for logging messages
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#'
#' @return File path to the computed binary brain mask (not yet dilated). A dilated version
#'   of the mask is also saved with a `_dil1x` suffix.
#'
#' @details This function replicates the "98-2" heuristic used in FSL's featlib.tcl:
#'   it computes the 2nd and 98th percentiles from a skull-stripped mean image and thresholds
#'   at 10% above the 2nd percentile. A final mask is formed by applying this threshold,
#'   binarizing, and performing one dilation iteration.
#'
#' @keywords internal
compute_brain_mask <- function(in_file, lg = NULL, fsl_img = NULL) {
  # use the 98 - 2 method from FSL (featlib.tcl ca. line 5345)
  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  to_log(lg, "info", "Computing brain mask from fMRI data using FSL's 98-2 percentile method")

  # first use FSL bet on the mean functional to get a starting point
  tmean_file <- tempfile(pattern="tmean")
  temp_bet <- tempfile()
  temp_stripped <- tempfile(pattern="epi_bet")
  # Use on.exit to guarantee cleanup even if run_fsl_command fails
  on.exit(rm_niftis(c(tmean_file, temp_bet, glue::glue("{temp_bet}_mask"), temp_stripped)), add = TRUE)
  
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {file_sans_ext(tmean_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, tmean_file)))
  run_fsl_command(glue("bet {tmean_file} {temp_bet} -R -f 0.3 -m -n"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(tmean_file, temp_bet)))
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -mas {temp_bet}_mask {temp_stripped}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_bet, temp_stripped)))

  # now compute 2nd and 98th percentiles on skull-stripped image
  p2 <- image_quantile(temp_stripped, quantiles=.02, exclude_zero = FALSE)
  p98 <- image_quantile(temp_stripped, quantiles=.98, exclude_zero = FALSE)
  
  thresh <- p2 + (p98 - p2)/10

  # apply this threshold to the epi_bet image, then take Tmin and binarize to form mask
  temp_mask <- tempfile(pattern = "mask_98_2")
  run_fsl_command(glue("fslmaths {temp_stripped} -thr {thresh} -Tmin -bin {temp_mask}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(temp_stripped, temp_mask)))

  # create dil1x copy as well if this is used elsewhere
  run_fsl_command(glue("fslmaths {temp_mask} -dilF {temp_mask}_dil1x"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(temp_mask))
  
  return(temp_mask)
}

#' Resample TemplateFlow Mask to fMRIPrep Image Using Python
#'
#' This function uses Python (via `reticulate`) to identify and resample a TemplateFlow mask
#' to match the resolution and spatial dimensions of an fMRIPrep BOLD image.
#'
#' @param in_file Path to the BIDS-compliant NIfTI file (e.g., an fMRIPrep preprocessed BOLD image).
#' @param output Optional path to write the resampled image. If NULL, a BIDS-style filename is constructed.
#' @param template_resolution Integer specifying the TemplateFlow resolution index (e.g., 1 = 1mm).
#' @param suffix TemplateFlow suffix (e.g., "mask", "T1w").
#' @param desc TemplateFlow descriptor (e.g., "brain").
#' @param extension File extension for the template image (default is ".nii.gz").
#' @param interpolation Interpolation method to use during resampling. Options are
#'   "nearest", "linear", or "continuous".
#' @param install_dependencies Logical. If \code{TRUE} (default), attempts to automatically install
#'   required Python packages (nibabel, nilearn, templateflow) if they are missing from the active environment.
#'   When the active Python environment is not writable, BrainGnomes will fall back to a managed
#'   reticulate environment; set \code{options(BrainGnomes.py_force_managed_env = TRUE)} to always
#'   prefer the managed environment.
#'   If \code{FALSE}, the function will raise an error if dependencies are not found.
#' @param overwrite Logical. If \code{TRUE}, overwrite the existing output file (if present).
#' @param lg Optional lgr logger for emitting warnings/info to the postprocess log.
#'
#' @details
#' The appropriate template is inferred from the `space-` entity of the BIDS-formatted input filename.
#' For example, an input such as:
#' \code{sub-221256_task-trust_run-1_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz}
#' will lead to selection of the MNI152NLin2009cAsym template.
#'
#' This function depends on a companion Python script (\code{fetch_matched_template_image.py})
#' that is bundled with the BrainGnomes package and sourced at runtime.
#'
#' @return Invisibly returns \code{TRUE} on success. A new NIfTI file is written to \code{output}.
#'
#' @importFrom reticulate source_python py_module_available py_install
#' @importFrom filelock unlock lock
#' @export
resample_template_to_img <- function(
    in_file,
    output = NULL,
    template_resolution = 1,
    suffix = "mask",
    desc = "brain",
    extension = ".nii.gz",
    interpolation = "nearest",
    install_dependencies = TRUE,
    overwrite = FALSE,
    lg = NULL) {
  checkmate::assert_string(in_file)
  checkmate::assert_file_exists(in_file)
  checkmate::assert_string(output, null.ok = TRUE)
  checkmate::assert_string(suffix)
  checkmate::assert_string(desc)
  checkmate::assert_string(extension)
  checkmate::assert_string(interpolation)
  checkmate::assert_flag(install_dependencies)
  checkmate::assert_flag(overwrite)

  install_dependencies <- isTRUE(getOption("BrainGnomes.install_py_deps", install_dependencies))
  force_managed_env <- isTRUE(getOption("BrainGnomes.py_force_managed_env", FALSE))

  f_info <- as.list(extract_bids_info(in_file))
  normalize_templateflow_entity <- function(value) {
    if (length(value) == 0L || is.null(value) || is.na(value) || !nzchar(value)) return(NULL)
    if (grepl("^[0-9]+$", value)) return(as.integer(value))
    value
  }
  template_spaces <- c( # https://www.templateflow.org/browse/
    "Fischer344",
    "MNI152Lin",
    "MNI152NLin2009aAsym",
    "MNI152NLin2009aSym",
    "MNI152NLin2009bAsym",
    "MNI152NLin2009bSym",
    "MNI152NLin2009cAsym",
    "MNI152NLin2009cSym",
    "MNI152NLin6Asym",
    "MNI152NLin6Sym",
    "MNI305",
    "MNIColin27",
    "MNIInfant",
    "MNIPediatricAsym",
    "MouseIn",
    "NKI",
    "NMT31Sym",
    "OASIS30ANTs",
    "PNC",
    "RESILIENT",
    "SUIT",
    "UNCInfant",
    "VALiDATe29",
    "WHS",
    "dhcpAsym",
    "dhcpSym",
    "dhcpVol",
    "fsLR",
    "fsaverage",
    "onavg"
  )

  # default to same name as input file, but change suffix to templatemask
  if (is.null(output)) {
    output <- file.path(dirname(in_file), construct_bids_filename(modifyList(f_info, list(suffix = "templatemask"))))
  }

  # If we're in native/anatomical space, prefer the subject-specific fMRIPrep mask
  template_space <- f_info$space
  template_cohort <- normalize_templateflow_entity(f_info$cohort)
  space_label <- if (is.null(template_space) || is.na(template_space)) "unknown" else template_space
  is_template_space <- !is.null(template_space) && !is.na(template_space) && template_space %in% template_spaces
  if (!is_template_space) {
    native_mask <- file.path(
      dirname(in_file),
      construct_bids_filename(modifyList(f_info, list(description = "brain", suffix = "mask")))
    )
    to_log(lg, "warn", "Requested template mask, but space '{space_label}' is not a TemplateFlow space. Using the run-specific brain mask instead.")
    if (file.exists(native_mask)) return(invisible(native_mask))
    to_log(lg, "error", "Cannot fetch a template mask for non-template space '{space_label}'. Expected subject mask at: {native_mask}")
  }

  if (file.exists(output) && !overwrite) {
    return(invisible(output)) # don't recreate existing image
  }

  # based on postprocess log, determine log root for parking lock file
  logger_logs_dir <- function(logger) {
    log_dir <- path.expand("~") # this is the fallback if the logger doesn't match expectations
    if (!checkmate::test_class(logger, "Logger")) return(log_dir)
    primary_dest <- tryCatch(logger$appenders$postprocess_log$destination, error = function(...) NULL)
    if (!is.null(primary_dest) && nzchar(primary_dest)) {
      return(tryCatch(
        suppressWarnings(normalizePath(dirname(primary_dest), winslash = "/", mustWork = FALSE)),
        error = function(...) {
          to_log(lg, "warn", "Cannot find log root for lock file based on {primary_dest}")
          log_dir
        }
      ))
    } else {
      # fall back to user home directory
      return(log_dir)
    }
  }

  required_modules <- c("nibabel", "nilearn", "templateflow")
  py_initialized <- reticulate::py_available(initialize = FALSE)

  python_env_root <- function() {
    reticulate_python <- Sys.getenv("RETICULATE_PYTHON", "")
    if (nzchar(reticulate_python) && file.exists(reticulate_python)) {
      return(dirname(dirname(reticulate_python)))
    }
    virtual_env <- Sys.getenv("VIRTUAL_ENV", "")
    if (nzchar(virtual_env)) return(virtual_env)
    conda_prefix <- Sys.getenv("CONDA_PREFIX", "")
    if (nzchar(conda_prefix)) return(conda_prefix)
    ""
  }

  env_is_writable <- function(path) {
    nzchar(path) && dir.exists(path) && file.access(path, 2) == 0
  }

  with_unset_env <- function(vars, expr) {
    old_env <- Sys.getenv(vars, unset = NA)
    on.exit({
      for (nm in names(old_env)) {
        val <- old_env[[nm]]
        if (is.na(val)) {
          Sys.unsetenv(nm)
        } else {
          do.call(Sys.setenv, setNames(list(val), nm))
        }
      }
    }, add = TRUE)
    Sys.unsetenv(vars)
    force(expr)
  }

  use_managed_env <- force_managed_env && !py_initialized
  if (force_managed_env && py_initialized) {
    to_log(lg, "warn", "BrainGnomes.py_force_managed_env is TRUE, but Python is already initialized; using the active Python environment.")
  }

  if (!use_managed_env && install_dependencies && !py_initialized) {
    env_root <- python_env_root()
    if (nzchar(env_root) && !env_is_writable(env_root)) {
      use_managed_env <- TRUE
      to_log(lg, "warn", "Active Python environment '{env_root}' is not writable. Falling back to a managed reticulate environment.")
    }
  }

  missing <- character()
  if (install_dependencies) {
    project_logs_dir <- logger_logs_dir(lg)
    if (!dir.exists(project_logs_dir)) dir.create(project_logs_dir, recursive = TRUE, showWarnings = FALSE)

    lock_file <- file.path(project_logs_dir, "resample_template_to_img.lock")
    lock_handle <- NULL
    release_lock <- function() {
      if (!is.null(lock_handle)) {
        filelock::unlock(lock_handle)
        lock_handle <<- NULL
      }
    }
    lock_timeout <- getOption("BrainGnomes.py_install_lock_timeout", 600)
    lock_handle <- tryCatch(
      filelock::lock(lock_file, timeout = lock_timeout),
      error = function(e) {
        err_msg <- glue(
          "Unable to acquire dependency installation lock at {lock_file}. ",
          "Another process may still be installing Python packages. ",
          "Increase option 'BrainGnomes.py_install_lock_timeout' to wait longer if needed. ",
          "Original error: {conditionMessage(e)}"
        )
        to_log(lg, "error", err_msg)
      }
    )
    on.exit(release_lock(), add = TRUE)

    if (use_managed_env && !py_initialized) {
      managed_env_vars <- c("RETICULATE_PYTHON", "RETICULATE_PYTHON_ENV", "VIRTUAL_ENV", "CONDA_PREFIX")
      missing <- with_unset_env(managed_env_vars, {
        reticulate::py_require(required_modules)
        required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]
      })
    } else {
      missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]
      if (length(missing) > 0) {
        message("Installing missing Python packages into the active environment...")
        install_error <- NULL
        tryCatch(
          reticulate::py_install(missing),
          error = function(e) install_error <<- e
        )
        if (!is.null(install_error)) {
          err_msg <- conditionMessage(install_error)
          hint <- "If the active Python environment is not writable, set options(BrainGnomes.py_force_managed_env = TRUE) or point RETICULATE_PYTHON to a user-writable environment."
          to_log(lg, "error", glue("Python package installation failed: {err_msg}. {hint}"))
          stop(glue("Python package installation failed. {hint}"))
        }
        missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]
      }
    }
    release_lock()
  } else {
    missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]
  }

  if (length(missing) > 0) {
    to_log(lg, "error",
      glue("The following required Python modules are missing: {paste(missing, collapse = ', ')}. ",
      "Please install them in your Python environment (e.g., with pip or reticulate::virtualenv_install), ",
      "or set options(BrainGnomes.py_force_managed_env = TRUE) to use a managed environment.")
    )
  }

  # Load Python module from script
  script_path <- system.file("fetch_matched_template_image.py", package = "BrainGnomes")
  if (!file.exists(script_path)) stop("Required python script not found: ", script_path)
  reticulate::source_python(script_path)

  img <- resample_template_to_bold(
    in_file = in_file,
    output = output,
    template_resolution = template_resolution,
    template_space = template_space,
    template_cohort = template_cohort,
    suffix = suffix,
    desc = desc,
    extension = extension,
    interpolation = interpolation
  )

  return(invisible(img))
}

# helper file to identify scrubbing censor file from input nifti
get_censor_file <- function(bids_info) {
  checkmate::assert_list(bids_info)
  construct_bids_filename(
    modifyList(bids_info, list(suffix = "censor", ext = ".1D")),
    full.names = TRUE
  )
}

#' Apply a Butterworth Filter to a 4D NIfTI Image
#'
#' This function performs voxelwise temporal filtering of a 4D fMRI image using
#' a Butterworth IIR filter (low-pass, high-pass, or bandpass).
#'
#' @param infile Character string. Path to the input 4D NIfTI file.
#' @param tr Numeric. The repetition time (TR) in seconds.
#' @param low_hz Numeric or NULL. Low cutoff frequency in Hz for high-pass or bandpass filtering.
#' @param high_hz Numeric or NULL. High cutoff frequency in Hz for low-pass or bandpass filtering.
#' @param outfile Character string. If provided, the filtered image is written to this file.
#' @param internal Logical. If FALSE (default), returns a `niftiImage` object with voxel values;
#'   if TRUE, returns a minimal metadata internal object (see RNifti).
#' @param order Integer. Filter order (default = 4).
#' @param padtype Character string. Padding strategy: "even", "odd", "constant", or "zero". Default is "even".
#' @param use_zi Logical. Whether to use steady-state initial conditions (default = TRUE).
#' @param demean Logical. Whether to demean the timeseries prior to filtering. Usually a good to remove 
#'   DC (mean) component (default = true).
#'
#' @return A 4D NIfTI image, either written to `outfile` or returned as an object.
#'
#' @details This function uses the `signal` package to compute IIR filter coefficients, and then
#' applies a zero-phase forward-backward filter to each voxel using C++ code via Rcpp.
#'
#' @examples
#' \dontrun{
#' butterworth_filter_4d("bold.nii.gz", tr = 2, low_hz = 0.01, high_hz = 0.1,
#'                        outfile = "bold_filtered.nii.gz")
#' }
#'
#' @importFrom signal butter
#' @param lg Optional logger for status and debug messages.
#' @export
butterworth_filter_4d <- function(infile, tr, low_hz = NULL, high_hz = NULL,
                                  outfile = "", internal = FALSE,
                                  order = 2L, padtype = "even", use_zi = TRUE, demean = TRUE,
                                  lg = NULL) {
  checkmate::assert_file_exists(infile)
  checkmate::assert_number(tr, lower=0.01, upper = 100)
  checkmate::assert_number(low_hz, null.ok = TRUE)
  checkmate::assert_number(high_hz, null.ok=TRUE)
  checkmate::assert_integerish(order, len=1L, lower=2L, upper=50L)
  
  if (!requireNamespace("signal", quietly = TRUE)) stop("The 'signal' package must be installed.")
  
  if (order %% 2 != 0) stop("filter order must be even because we use forward-reverse passes")
  order <- order / 2 # If you design a 4th-order filter, filtfilt applies it forward and then again backward, resulting in an effective 8th-order magnitude response.

  fs <- 1 / tr  # sampling frequency in Hz
  nyq <- fs / 2
  
  if (is.null(low_hz) && is.null(high_hz)) {
    stop("Must specify at least one of 'low_hz' or 'high_hz'.")
  }
  
  # if (is.null(low_hz) || low_hz < 0) low_hz <- 0 # 0 indicates no filtering
  # if (is.null(high_hz) || is.infinite(high_hz)) high_hz <- Inf # 0 indicates no filtering
  
  if (!is.null(low_hz) && low_hz <= 0) stop("'low_hz' must be > 0")
  if (!is.null(high_hz) && high_hz >= nyq) stop("'high_hz' must be < Nyquist frequency")
  
  # Design Butterworth filter
  if (!is.null(low_hz) && !is.null(high_hz)) {
    W <- c(low_hz, high_hz) / nyq
    type <- "pass"
  } else if (!is.null(low_hz)) {
    W <- low_hz / nyq
    type <- "high"
  } else {
    W <- high_hz / nyq
    type <- "low"
  }
  
  butter_coeff <- signal::butter(order, W, type = type)
  b <- butter_coeff$b
  a <- butter_coeff$a

  # Call C++ function for voxelwise filtering
  run_logged(
    butterworth_filter_cpp,
    infile = infile,
    b = b,
    a = a,
    outfile = outfile,
    internal = internal,
    padtype = padtype,
    use_zi = use_zi,
    demean = demean,
    logger = lg
  )
}

#' iirnotch implementation in R (RBJ biquad design)
#' @param f0 notch center frequency in Hz
#' @param Q quality factor (higher = narrower notch)
#' @param fs sampling rate in Hz
#' @return filter coefficients `list(b, a)` normalized so `a[1] == 1`
#' @keywords internal
#' @noRd
iirnotch_r <- function(f0, Q, fs) {
  stopifnot(is.numeric(f0), is.numeric(Q), is.numeric(fs),
            f0 > 0, Q > 0, fs > 0, f0 < fs/2)

  w0 <- 2 * pi * (f0 / fs)           # normalized radian frequency
  cw  <- cos(w0)
  sw  <- sin(w0)
  alpha <- sw / (2 * Q)

  # RBJ cookbook notch (constant 0 dB)
  b <- c(1, -2 * cw, 1)
  a <- c(1 + alpha, -2 * cw, 1 - alpha)

  # normalize so a[1] == 1
  b <- b / a[1]
  a <- a / a[1]

  list(b = b, a = a)
}
