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

  lg$info("Apply mask {mask_file} to {in_file}")

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
  
  lg$info("Applying spline interpolate to scrubbed timepoints")
  lg$debug("in_file: {in_file}")
  lg$debug("censor_file: {censor_file}")

  censor <- as.integer(readLines(censor_file))
  t_interpolate <- which(1L - censor == 1L) # bad timepoints are 0 in the censor file
  
  if (!any(t_interpolate)) {
    lg$info("No timepoints to scrub found in {censor_file}. Interpolation will have no effect.")
  } else {
    lg$info("Applying voxelwise natural spline interpolation for {length(t_interpolate)} timepoints to {in_file}.")
  }

  # run 4D interpolation with Rcpp function

  natural_spline_4d(in_file, t_interpolate = t_interpolate, edge_nn=TRUE, outfile = out_file, internal = TRUE)

  if (length(confound_files) > 0 && length(t_interpolate) > 0) {
    good_idx <- which(censor == 1L)
    first_valid <- min(good_idx)
    last_valid <- max(good_idx)
    for (cf in confound_files) {
      if (!checkmate::test_file_exists(cf)) {
        lg$warn("Confound file {cf} not found; skipping")
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
    lg$error(msg)
    stop(msg)
  }

  censor <- as.integer(readLines(censor_file))
  t_scrub <- which(1L - censor == 1L) # bad timepoints are 0 in the censor file

  if (!any(t_scrub)) {
    lg$info("No timepoints to scrub found in {censor_file}. Scrubbing will not change the length of the output data.")
  } else {
    lg$info("Applying timepoint scrubbing, removing {length(t_scrub)} timepoints from {in_file}.")
  }

  # run 4D interpolation with Rcpp function
  remove_nifti_volumes(in_file, t_scrub, out_file)

  if (length(confound_files) > 0 && length(t_scrub) > 0) {
    for (cf in confound_files) {
      if (!checkmate::test_file_exists(cf)) {
        lg$warn("Confound file {cf} not found; skipping")
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
  
  stopifnot(low_pass_hz > high_pass_hz)
  checkmate::assert_number(tr, lower = 0.01)
  checkmate::assert_flag(overwrite)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  if (is.infinite(low_pass_hz) && !is.infinite(high_pass_hz)) {
    lg$info("Applying high-pass filter with cutoff: {high_pass_hz} Hz (removes frequencies below this), TR = {tr}s")
  } else if (!is.infinite(low_pass_hz) && is.infinite(high_pass_hz)) {
    lg$info("Applying low-pass filter with cutoff: {low_pass_hz} Hz (removes frequencies above this), TR = {tr}s")
  } else if (!is.infinite(low_pass_hz) && !is.infinite(high_pass_hz)) {
    lg$info("Applying band-pass filter: {high_pass_hz} Hz - {low_pass_hz} Hz, TR = {tr}s")
  } else {
    lg$warn("No filtering applied — both low_pass_hz and high_pass_hz are infinite or invalid.")
  }
  lg$debug("in_file: {in_file}")
  
  if (method == "fslmaths") {
    # bptf specifies its filter cutoffs in terms of volumes, not frequencies
    fwhm_to_sigma <- sqrt(8 * log(2)) # Details here: https://www.mail-archive.com/hcp-users@humanconnectome.org/msg01393.html

    # set volumes to -1 to skip that side of filter in -bptf
    hp_volumes <- if (is.infinite(high_pass_hz)) -1 else 1 / (high_pass_hz * fwhm_to_sigma * tr)
    lp_volumes <- if (is.infinite(low_pass_hz)) -1 else 1 / (low_pass_hz * fwhm_to_sigma * tr)

    temp_tmean <- tempfile(pattern="tmean")
    run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {temp_tmean}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean)))
    run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -bptf {hp_volumes} {lp_volumes} -add {temp_tmean} {file_sans_ext(out_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean, out_file)))

    rm_niftis(temp_tmean) # clean up temporal mean image
  } else {
    lg$info("Using internal Butterworth temporal filtering function")
    # Note that butterworth_filter_4d accepts frequency cutoffs -- anything below low_hz is cut (high-pass), anything above high_hz is cut (low-pass)
    bw_low <- if (is.infinite(high_pass_hz)) NULL else high_pass_hz
    bw_high <- if (is.infinite(low_pass_hz)) NULL else low_pass_hz
    butterworth_filter_4d(infile = in_file, tr = tr, low_hz = bw_low, high_hz = bw_high, outfile = out_file)
  }
  
  return(out_file)
}

#' Apply AROMA-based denoising to an fMRI image
#'
#' Performs non-aggressive ICA-AROMA denoising by regressing out identified noise components
#' from an fMRI time series using FSL's \code{fsl_regfilt}. Falls back to an R-based wrapper script
#' if the standard FSL command fails due to dimensionality issues.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param out_file The full path for the file output by this step
#' @param mixing_file Path to the MELODIC mixing matrix (e.g., \code{*_desc-MELODIC_mixing.tsv}).
#' @param noise_ics Vector of ICA components to regress out (usually pulled from relevant aroma_timeseries.tsv file).
#' @param overwrite Logical; whether to overwrite the output file if it exists.
#' @param lg Optional lgr object used for logging messages
#' @param use_R Logical; if \code{TRUE}, use an R wrapper script (\code{fsl_regfilt.R}) instead of \code{fsl_regfilt}.
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#'
#' @return Path to the denoised output NIfTI file. If required files are missing, returns \code{in_file} unmodified.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom checkmate assert_string test_file_exists
apply_aroma <- function(in_file, out_file, mixing_file, noise_ics, overwrite = FALSE, lg = NULL, use_R = FALSE, fsl_img = NULL) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_string(out_file)
  checkmate::assert_flag(overwrite)
  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  lg$debug("in_file: {in_file}")
  lg$debug("mixing_file: {mixing_file}")

  if (isFALSE(checkmate::test_file_exists(mixing_file))) {
    to_log(lg, "warn", "Cannot find mixing file corresponding to {in_file}. Skipping AROMA regression")
    return(in_file)
  }

  if (isFALSE(checkmate::test_integerish(noise_ics, lower=1))) {
    to_log(lg, "warn", "noise_ics must be a vector of integers identifying components to regress out. Skipping AROMA regression")
    return(in_file)
  }

  # just read in the comma-separated noise ICs
  noise_ics <- paste(noise_ics, collapse=",") # fsl_regfilt requires comma-separated list

  # for some reason, fsl_regfilt blows up when we try to feed a regressors x 1 x 1 x timepoints NIfTI
  # fall back to R in this case
  if (isTRUE(use_R)) {
    regfilt_rscript <- system.file("fsl_regfilt.R", package = "BrainGnomes")
    if (!file.exists(regfilt_rscript)) stop("Cannot find fsl_regfilt.R script in the BrainGnomes installation folder")

    cmd <- glue("{Sys.getenv('R_HOME')}/bin/Rscript --vanilla {regfilt_rscript} --input={in_file} --melodic_mix={mixing_file} --filter={noise_ics} --njobs=1 --output={out_file}")
    to_log(lg, "info", "Running fsl_regfilt.R: {cmd}")
    system(cmd)
  } else {
    cmd <- glue("fsl_regfilt -i {file_sans_ext(in_file)} -o {file_sans_ext(out_file)} -d {mixing_file} -f {noise_ics}")
    run_fsl_command(cmd, log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, mixing_file, out_file)))
  }
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

  lg$info("Spatial smoothing with FHWM {fwhm_mm}mm kernel")
  lg$debug("in_file: {in_file}")

  fwhm_to_sigma <- sqrt(8 * log(2)) # Details here: https://www.mail-archive.com/hcp-users@humanconnectome.org/msg01393.html
  sigma <- fwhm_mm / fwhm_to_sigma

  p2_intensity <- image_quantile(in_file, brain_mask, .02)
  median_intensity <- image_quantile(in_file, brain_mask, .5)
  susan_thresh <- (median_intensity - p2_intensity) * .75 # also see featlib.tcl

  # always compute extents mask that is reapplied to data post-smoothing to avoid any "new" voxels
  extents_mask <- tempfile(pattern = "extents_mask")
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmin -bin {extents_mask} -odt char"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, extents_mask))) # save extents to temp file

  # compute mean functional image used in susan
  temp_tmean <- tempfile(pattern = "tmean")
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {temp_tmean}"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, temp_tmean))) # save tmean to temporary file
  run_fsl_command(glue("susan {file_sans_ext(in_file)} {susan_thresh} {sigma} 3 1 1 {temp_tmean} {susan_thresh} {file_sans_ext(out_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, temp_tmean, out_file)))

  # apply extents mask
  run_fsl_command(glue("fslmaths {file_sans_ext(out_file)} -mul {extents_mask} {file_sans_ext(out_file)} -odt float"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, extents_mask, out_file)))

  rm_niftis(c(temp_tmean, extents_mask, glue("{file_sans_ext(out_file)}_usan_size"))) # cleanup temp files

  return(out_file)
}


#' Normalize global intensity of a 4D fMRI image
#'
#' Rescales the intensity of a 4D NIfTI image so that the median voxel intensity within a brain mask
#' matches a specified global target. This operation is commonly used to standardize signal across runs or subjects.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param out_file The full path for the file output by this step
#' @param brain_mask Optional path to a brain mask NIfTI file. If \code{NULL}, the entire image is used.
#' @param global_median Target median intensity value to normalize to (default is 10000).
#' @param overwrite Logical; whether to overwrite the output file if it exists.
#' @param lg Optional lgr object used for logging messages
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#'
#' @return Path to the intensity-normalized output NIfTI file.
#'
#' @details The 50th percentile intensity is estimated using \code{fslstats}, and the input image is
#' rescaled using \code{fslmaths -mul}. If the output file exists and \code{overwrite = FALSE}, the step is skipped.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom checkmate assert_string assert_number
intensity_normalize <- function(in_file, out_file, brain_mask=NULL, global_median=10000, overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(out_file)
  checkmate::assert_number(global_median)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue("BrainGnomes") # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  lg$info("Intensity normalizing fMRI data to global median: {global_median}")

  median_intensity <- image_quantile(in_file, brain_mask, .5)

  # a heuristic for now, but we must have a small positive median to avoid wild scaling values (divide by ~0) and accidental sign flips (if median is negative)
  # long-term, we need to decide whether to additive scaling x + (10000 - median) or to add a number to all voxels to bring the min > 0.01, then do multiplicative median scaling
  if (median_intensity < 1) median_intensity <- 1.0

  rescaling_factor <- global_median / median_intensity

  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -mul {rescaling_factor} {file_sans_ext(out_file)} -odt float"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, out_file)))
  return(out_file)
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
    run_fsl_command(glue("Text2Vest {to_regress} {vest_file}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(to_regress, vest_file)))
    
    # because the residuals will be demeaned and intensity normalization should follow this step, add back in the temporal mean from the pre-regression image
    temp_tmean <- tempfile(pattern="tmean")
    run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {temp_tmean}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean)))
    run_fsl_command(glue("fsl_glm -i {file_sans_ext(in_file)} -d {vest_file} --out_res={file_sans_ext(out_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, vest_file, out_file)))
    run_fsl_command(glue("fslmaths {file_sans_ext(out_file)} -add {temp_tmean} {file_sans_ext(out_file)}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(out_file, temp_tmean)))

    # 3dTproject for regression (deprecated to keep all commands in FSL)
    # regress_cmd <- glue("3dTproject -input {in_file} -prefix {out_file}_afni -ort {to_regress} -polort 0")

    rm_niftis(temp_tmean)
  } else if (method == "lmfit") {
    lg$info("Using internal lmfit confound regression function")
    Xmat <- data.table::fread(to_regress, sep = "\t", header = FALSE)
    if (checkmate::test_file_exists(censor_file)) {
      good_vols <- as.logical(as.integer(readLines(censor_file))) # bad timepoints are 0 in the censor file
      if (sum(good_vols) < length(good_vols)) lg$info("Fitting confound regression with {sum(good_vols)} of {length(good_vols)} volumes.")
    }
    
    lmfit_residuals_4d(in_file, X = as.matrix(Xmat), include_rows = good_vols, outfile = out_file, preserve_mean = TRUE)
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

  lg$info("Computing brain mask from fMRI data using FSL's 98-2 percentile method")

  # first use FSL bet on the mean functional to get a starting point
  tmean_file <- tempfile(pattern="tmean")
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {file_sans_ext(tmean_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, tmean_file)))
  
  temp_bet <- tempfile()
  run_fsl_command(glue("bet {tmean_file} {temp_bet} -R -f 0.3 -m -n"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(tmean_file, temp_bet)))

  temp_stripped <- tempfile(pattern="epi_bet")
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

  # cleanup temp files
  rm_niftis(c(tmean_file, temp_bet, temp_stripped))
  
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
#'   If \code{FALSE}, the function will raise an error if dependencies are not found.
#' @param overwrite Logical. If \code{TRUE}, overwrite the existing output file (if present).
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
  overwrite = FALSE
) {
  checkmate::assert_file_exists(in_file)
  checkmate::assert_string(output, null.ok = TRUE)
  checkmate::assert_string(suffix)
  checkmate::assert_string(desc)
  checkmate::assert_string(extension)
  checkmate::assert_string(interpolation)
  checkmate::assert_flag(install_dependencies)
  checkmate::assert_flag(overwrite)

  # default to same name as input file, but change suffix to templatemask
  if (is.null(output)) {
    f_info <- as.list(extract_bids_info(in_file))
    output <- file.path(dirname(in_file), construct_bids_filename(modifyList(f_info, list(suffix = "templatemask"))))
  }

  if (file.exists(output) && !overwrite) {
    return(invisible(output)) # don't recreate existing image
  }

  required_modules <- c("nibabel", "nilearn", "templateflow")
  missing <- required_modules[!vapply(required_modules, reticulate::py_module_available, logical(1))]

  if (length(missing) > 0) {
    if (install_dependencies) {
      message("Installing missing Python packages into the active environment...")
      reticulate::py_install(missing)
    } else {
      stop(
        "The following required Python modules are missing: ", paste(missing, collapse = ", "), "\n",
        "Please install them in your Python environment (e.g., with pip or reticulate::virtualenv_install).",
        call. = FALSE
      )
    } 
  }

  # Load Python module from script
  script_path <- system.file("fetch_matched_template_image.py", package = "BrainGnomes")
  if (!file.exists(script_path)) stop("Required python script not found: ", script_path)
  reticulate::source_python(script_path)

  img <- resample_template_to_bold(
    in_file = in_file,
    output = output,
    template_resolution = template_resolution,
    suffix = suffix,
    desc = desc,
    extension = extension,
    interpolation = interpolation
  )

  return(invisible(img))
}
utils::globalVariables("resample_template_to_bold") # avoid R CMD CHECK complaint about global function -- here, created by source_python

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
#' @export
butterworth_filter_4d <- function(infile, tr, low_hz = NULL, high_hz = NULL,
                                  outfile = "", internal = FALSE,
                                  order = 2L, padtype = "even", use_zi = TRUE, demean = TRUE) {
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
  butterworth_filter_cpp(infile = infile, b = b, a = a,
                         outfile = outfile, internal = internal,
                         padtype = padtype, use_zi = use_zi, demean=demean)
}
