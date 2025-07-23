
#' Postprocess a single fMRI BOLD image using a configured pipeline
#'
#' Applies a sequence of postprocessing operations to a single subject-level BOLD NIfTI file, as specified by
#' the user-defined configuration object. Operations may include brain masking, spatial smoothing, ICA-AROMA denoising,
#' temporal filtering, confound regression, and intensity normalization. The function also optionally computes and saves
#' a filtered confounds file for downstream analyses.
#'
#' The processing sequence can be enforced by the user (`force_processing_order = TRUE`) or determined dynamically based
#' on the `enable` flags in the configuration. Intermediate NIfTI and confound files may be deleted to save disk space,
#' depending on the `keep_intermediates` setting. Logging is handled via the `lgr` package and is directed to subject-specific
#' log files inferred from BIDS metadata.
#'
#' @param in_file Path to a subject-level BOLD NIfTI file output by fMRIPrep.
#' @param cfg A list containing configuration options, including TR (`cfg$tr`), enabled processing steps (`cfg$<step>$enable`),
#'   logging (`cfg$log_file`), and paths to resources such as brain masks or singularity images (`cfg$fsl_img`, `cfg$brain_mask`).
#'
#' @return The path to the final postprocessed BOLD NIfTI file. Side effects include writing a confounds TSV file (if enabled),
#'   and logging to a subject-level log file.
#'
#' @details
#' Required `cfg` entries:
#' - `tr`: Repetition time in seconds.
#' - `bids_desc`: A BIDS-compliant `desc` label for the output filename.
#' - `processing_steps`: Optional character vector specifying processing order (if `force_processing_order = TRUE`).
#'
#' Optional steps controlled by `cfg$<step>$enable`:
#' - `apply_mask`
#' - `spatial_smooth`
#' - `apply_aroma`
#' - `temporal_filter`
#' - `confound_regression`
#' - `intensity_normalize`
#'
#' @importFrom checkmate assert_list assert_file_exists test_character test_number
#' @export
postprocess_subject <- function(in_file, cfg=NULL) {
  checkmate::assert_file_exists(in_file)
  checkmate::assert_list(cfg)
  if (!checkmate::test_character(cfg$bids_desc)) {
    stop("postprocess_subject requires a bids_desc field containing the intended description field of the postprocessed filename.")
  }
  
  # checkmate::assert_list(processing_sequence)
  proc_files <- get_fmriprep_outputs(in_file)

  # determine if input is in a stereotaxic space
  input_bids_info <- as.list(extract_bids_info(in_file))
  native_space <- is.na(input_bids_info$space) || input_bids_info$space %in% c("T1w", "T2w", "anat")

  # log_file should come through as an environment variable, pointing to the subject-level log.
  # Use this to get the location of the subject log directory
  sub_log_file <- Sys.getenv("log_file")
  if (!nzchar(sub_log_file)) {
    warning("Cannot find log_file as an environment variable. Logs may not appear in the expected location!")
    attempt_dir <- normalizePath(file.path(dirname(in_file), glue("../../../logs/sub-{input_bids_info$sub}")))
    log_dir <- if (dir.exists(attempt_dir)) attempt_dir else dirname(in_file)
  } else {
    log_dir <- dirname(sub_log_file)
  }
  
  # Setup default log file -- need to make sure it always goes in the subject log folder
  if (is.null(cfg$log_file)) {
    cfg$log_file <- construct_bids_filename(modifyList(input_bids_info, list(ext=".log", description=cfg$bids_desc)), full.names=FALSE)
  } else {
    cfg$log_file <- glue(cfg$log_file) # evaluate location of log, allowing for glue expressions
  }

  # force log file to be in the right directory
  log_file <- file.path(log_dir, basename(cfg$log_file))

  lg <- lgr::get_logger_glue(c("postprocess", input_bids_info$sub))
  lg$add_appender(lgr::AppenderFile$new(log_file), name = "postprocess_log")

  # Reconstruct expected output file
  output_bids_info <- modifyList(input_bids_info, list(description = cfg$bids_desc)) # set desc to new description
  final_filename <- construct_bids_filename(output_bids_info, full.names = TRUE)

  # determine if final output file already exists
  if (checkmate::test_file_exists(final_filename)) {
    lg$info("Postprocessed file already exists: {final_filename}")

    if (isTRUE(cfg$overwrite)) {
      lg$info("Removing {final_filename} because overwrite is TRUE")
      file.remove(final_filename)
    } else {
      lg$info("Skipping postprocessing for {in_file} because postprocessed file already exists")
      return(final_filename)
    }
  }

  # location of FSL singularity container
  fsl_img <- cfg$fsl_img

  if (!checkmate::test_number(cfg$tr, lower = 0.01, upper = 30)) {
    stop("YAML config must contain a tr field specifying the repetition time in seconds")
  }

  # default to not enforcing user-specified order of processing steps
  if (!checkmate::test_flag(cfg$force_processing_order)) cfg$force_processing_order <- FALSE

  start_time <- Sys.time()
  lg$info("Start preprocessing: {as.character(start_time)}")
  
  # determine brain mask to be used for computing intensity thresholds for susan and normalization
  # Consider user-specified brain mask
  brain_mask <- NULL
  if (checkmate::test_string(cfg$brain_mask)) {
    if (cfg$brain_mask == "template") {
      brain_mask <- resample_template_to_img(in_file) # call Python helper for TemplateFlow
    } else if (checkmate::test_file_exists(cfg$brain_mask)) {
      brain_mask <- cfg$brain_mask
    } else {
      lg$warn("Cannot find brain_mask: ", cfg$brain_mask, ". Will try to find an alternative.")
    }
  }

  # Handle fallback cases (if user mask input was invalid or unspecified)
  if (is.null(brain_mask)) {
    if (native_space) {
      if (!is.null(proc_files$brain_mask)) {
        brain_mask <- proc_files$brain_mask
      } else {
        brain_mask <- compute_brain_mask(in_file, log_file)
      }
    } else {
      brain_mask <- resample_template_to_img(in_file) # call Python helper for TemplateFlow
    }
  }

  cur_file <- proc_files$bold
  file_set <- cur_file # tracks all of the files used in the postprocessing stream

  ## setup order of processing steps
  if (isTRUE(cfg$force_processing_order)) {

    checkmate::assert_character(cfg$processing_steps) # ensure we have a character vector
    cfg$processing_steps <- tolower(cfg$processing_steps) # avoid case issues

    # handle small glitches in nomenclature
    cfg$processing_steps <- sub("spatial_smoothing", "spatial_smooth", cfg$processing_steps, fixed=TRUE)
    cfg$processing_steps <- sub("temporal_filtering", "temporal_filter", cfg$processing_steps, fixed=TRUE)
    cfg$processing_steps <- sub("confound_regress", "confound_regression", cfg$processing_steps, fixed = TRUE)
    cfg$processing_steps <- sub("intensity_normalization", "intensity_normalize", cfg$processing_steps, fixed = TRUE)

    processing_sequence <- cfg$processing_steps
    lg$info("We will follow the user-specified processing order, with no guarantees on data validity.")
  } else {
    processing_sequence <- c()
    if (isTRUE(cfg$apply_mask$enable)) processing_sequence <- c(processing_sequence, "apply_mask")
    if (isTRUE(cfg$spatial_smooth$enable)) processing_sequence <- c(processing_sequence, "spatial_smooth")
    if (isTRUE(cfg$apply_aroma$enable)) processing_sequence <- c(processing_sequence, "apply_aroma")
    if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$interpolate)) processing_sequence <- c(processing_sequence, "scrub_interpolate")
    if (isTRUE(cfg$temporal_filter$enable)) processing_sequence <- c(processing_sequence, "temporal_filter")
    if (isTRUE(cfg$confound_regression$enable)) processing_sequence <- c(processing_sequence, "confound_regression")
    if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$apply)) processing_sequence <- c(processing_sequence, "scrub_timepoints")
    if (isTRUE(cfg$intensity_normalize$enable)) processing_sequence <- c(processing_sequence, "intensity_normalize")
  }

  lg$info("Processing will proceed in the following order: {paste(processing_sequence, collapse=', ')}")
  
  #### handle confounds, filtering to match MRI data. This will also calculate scrubbing information, if requested
  to_regress <- postprocess_confounds(
    proc_files = proc_files,
    cfg = cfg,
    processing_sequence = processing_sequence,
    input_bids_info = input_bids_info,
    fsl_img = fsl_img,
    lg = lg
  )

  # expected censor file for scrubbing
  censor_file <- get_censor_file(input_bids_info)

  #### Loop over fMRI processing steps in sequence
  for (step in processing_sequence) {
    if (step == "apply_mask") {
      lg$info("Masking fMRI data using file: {brain_mask}")
      cur_file <- apply_mask(cur_file, prefix = cfg$apply_mask$prefix,
        mask_file = brain_mask,
        overwrite=cfg$overwrite, lg = lg, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "spatial_smooth") {
      lg$info("Spatial smoothing with FHWM {cfg$spatial_smooth$fwhm_mm}mm kernel")
      cur_file <- spatial_smooth(cur_file,
        brain_mask = brain_mask, prefix = cfg$spatial_smooth$prefix, fwhm_mm = cfg$spatial_smooth$fwhm_mm, 
        overwrite = cfg$overwrite, lg = lg, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "apply_aroma") {
      cur_file <- apply_aroma(cur_file, prefix = cfg$apply_aroma$prefix,
        mixing_file = proc_files$melodic_mix,
        noise_ics = proc_files$noise_ics,
        overwrite=cfg$overwrite, lg=lg, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "scrub_interpolate") {
      lg$info("Applying spline interpolate to scrubbed timepoints")
      cur_file <- scrub_interpolate(
        cur_file, censor_file = censor_file, 
        prefix = cfg$scrubbing$interpolate_prefix, overwrite=cfg$overwrite, lg=lg
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "temporal_filter") {
      lg$info("Temporal filtering with low-pass: {cfg$temporal_filter$low_pass_hz} Hz, high-pass: {cfg$temporal_filter$high_pass_hz} Hz given TR: {cfg$tr}")
      cur_file <- temporal_filter(cur_file, prefix = cfg$temporal_filter$prefix,
        tr = cfg$tr, low_pass_hz = cfg$temporal_filter$low_pass_hz,
        high_pass_hz = cfg$temporal_filter$high_pass_hz,
        overwrite=cfg$overwrite, lg=lg, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "confound_regression") {
      lg$info("Removing confound regressors from fMRI data using file: {to_regress}")
      cur_file <- confound_regression(cur_file, prefix = cfg$confound_regression$prefix,
        to_regress = to_regress, censor_file = censor_file,
        overwrite=cfg$overwrite, lg = lg, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "scrub_timepoints") {
      cur_file <- scrub_timepoints(cur_file,
        censor_file = censor_file,
        prefix = cfg$scrubbing$prefix,
        overwrite = cfg$overwrite, lg = lg, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "intensity_normalize") {
      lg$info("Intensity normalizing fMRI data to global median: {cfg$intensity_normalize$global_median}")
      cur_file <- intensity_normalize(cur_file, prefix = cfg$intensity_normalize$prefix,
        brain_mask = brain_mask,
        global_median = cfg$intensity_normalize$global_median,
        overwrite=cfg$overwrite, lg=lg, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else {
      stop("Unknown step: ", step)
    }
  }

  # clean up intermediate NIfTIs
  if (isFALSE(cfg$keep_intermediates) && length(file_set) > 2L) {
    # initial file is the BOLD input from fmriprep, last file is the final processed image
    to_delete <- file_set[2:(length(file_set) - 1)]
    for (ff in to_delete) {
      if (file.exists(ff)) {
        lg$debug("Removing intermediate file: {ff}")
        unlink(ff)
      }
    }
  }

  # clean up confound regressors file
  if (isFALSE(cfg$keep_intermediates) && isTRUE(cfg$confound_regression$enable)) {
    if (file.exists(to_regress)) {
      lg$debug("Removing intermediate confound regression file: {to_regress}")
      unlink(to_regress)
    }
  }

  # move the final file into a BIDS-friendly file name with a desc field
  lg$debug("Renaming last file in stream: {cur_file} to postprocessed file name: {final_filename}")
  file.rename(cur_file, final_filename)
  
  end_time <- Sys.time()
  lg$info("End postprocessing: {as.character(end_time)}")
  return(final_filename)
}

####################################
### FUNCTIONS FOR SPECIFIC STEPS ###
####################################

#' Apply a brain mask to a 4D NIfTI image
#'
#' Multiplies a NIfTI image by a binary mask using FSL's \code{fslmaths -mas} to zero out non-brain voxels.
#' This is typically used to restrict processing to brain tissue.
#'
#' @param in_file Path to the input 4D NIfTI image.
#' @param mask_file Path to a binary mask NIfTI file (same dimensions as \code{in_file}).
#' @param prefix Prefix to prepend to the output file name.
#' @param overwrite Logical; whether to overwrite the output file if it already exists.
#' @param lg Optional lgr object used for logging messages
#' @param fsl_img Optional path to a Singularity image to execute the command in a container.
#'
#' @return Path to the masked output NIfTI file.
#'
#' @keywords internal
#' @importFrom checkmate assert_file_exists assert_string
#' @importFrom glue glue
apply_mask <- function(in_file, mask_file, prefix="m", overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  checkmate::assert_file_exists(mask_file)
  checkmate::assert_string(prefix)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip existing file
  } else {
    out_file <- res$out_file
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
#' @param prefix Optional character prefix to prepend to the interpolated output
#'   filename (default is `"i"`).
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
#' @keywords internal
scrub_interpolate <- function(in_file, censor_file, prefix="i", overwrite=FALSE, lg=NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_file_exists(censor_file)
  checkmate::assert_string(prefix)
  checkmate::assert_flag(overwrite)
  
  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }
  
  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip existing file
  } else  {
    out_file <- res$out_file
  }
  
  censor <- as.integer(readLines(censor_file))
  t_interpolate <- which(1L - censor) # bad timepoints are 0 in the censor file
  
  if (!any(t_interpolate)) {
    lg$info("No timepoints to scrub found in {censor_file}. Interpolation will have no effect.")
  } else {
    lg$info("Applying voxelwise natural spline interpolation for {length(t_interpolate)} timepoints to {in_file}.")
  }

  # run 4D interpolation with Rcpp function
  natural_spline_4d(in_file, t_interpolate = t_interpolate, edge_nn=TRUE, outfile = out_file, internal = TRUE)
  
  return(out_file)
}


scrub_timepoints <- function(in_file, censor_file = NULL, prefix="i", overwrite=FALSE, lg=NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_flag(overwrite)
  
  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  if (!checkmate::test_file_exists(censor_file)) {
    msg <- glue("In scrub_timepoints, cannot locate censor file: {censor_file}")
    lg$error(msg)
    stop(msg)
  }
  
  # handle extant output file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip existing file
  } else  {
    out_file <- res$out_file
  }
  
  censor <- as.integer(readLines(censor_file))
  t_scrub <- which(1L - censor) # bad timepoints are 0 in the censor file
  
  if (!any(t_scrub)) {
    lg$info("No timepoints to scrub found in {censor_file}. Scrubbing will not change the length of the output data.")
  } else {
    lg$info("Applying timepoint scrubbing, removing {length(t_scrub)} timepoints from {in_file}.")
  }

  # run 4D interpolation with Rcpp function
  remove_nifti_volumes(in_file, t_scrub, out_file)

  # TODO: scrub other outputs -- confounds etc.
  
  return(out_file)
}



#' Apply temporal filtering to a 4D NIfTI image
#'
#' Uses FSL's \code{fslmaths -bptf} to apply high-pass and/or low-pass temporal filtering
#' to an fMRI time series. The filter cutoffs are specified in Hz and internally converted
#' to sigma values in volumes using a standard FWHM-to-sigma transformation.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param prefix Character string prefix to prepend to the output file.
#' @param low_pass_hz Low-pass filter cutoff in Hz. Use \code{0} to skip.
#' @param high_pass_hz High-pass filter cutoff in Hz. Use \code{Inf} to skip.
#' @param tr Repetition time (TR) in seconds. Required to convert Hz to volumes.
#' @param overwrite Logical; whether to overwrite the output file if it exists.
#' @param lg Optional lgr object used for logging messages
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
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
temporal_filter <- function(in_file, prefix="f", low_pass_hz=0, high_pass_hz=1/120, tr=NULL, overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_number(low_pass_hz)
  checkmate::assert_number(high_pass_hz)
  stopifnot(low_pass_hz < high_pass_hz)
  checkmate::assert_number(tr, lower = 0.01)
  checkmate::assert_flag(overwrite)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip existing file
  } else {
    out_file <- res$out_file
  }

  # bptf specifies its filter cutoffs in terms of volumes, not frequencies
  fwhm_to_sigma <- sqrt(8 * log(2)) # Details here: https://www.mail-archive.com/hcp-users@humanconnectome.org/msg01393.html

  if (is.infinite(high_pass_hz)) {
    #message("Low-pass filtering")
    hp_volumes <- -1 # do not apply high-pass
  } else {
    hp_volumes <- 1 / (high_pass_hz * fwhm_to_sigma * tr)
  }

  if (is.infinite(low_pass_hz) || low_pass_hz==0) {
    #message("High-pass filtering")
    lp_volumes <- -1 # do not apply low-pass
  } else {
    lp_volumes <- 1 / (low_pass_hz * fwhm_to_sigma * tr)
  }

  temp_tmean <- tempfile(pattern="tmean")
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -Tmean {temp_tmean}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean)))
  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -bptf {hp_volumes} {lp_volumes} -add {temp_tmean} {file_sans_ext(out_file)}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean, out_file)))

  rm_niftis(temp_tmean) # clean up temporal mean image
  
  return(out_file)
}

#' Apply AROMA-based denoising to an fMRI image
#'
#' Performs non-aggressive ICA-AROMA denoising by regressing out identified noise components
#' from an fMRI time series using FSL's \code{fsl_regfilt}. Falls back to an R-based wrapper script
#' if the standard FSL command fails due to dimensionality issues.
#'
#' @param in_file Path to the input 4D NIfTI file.
#' @param prefix Prefix to prepend to the output file name.
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
apply_aroma <- function(in_file, prefix = "a", mixing_file, noise_ics, overwrite = FALSE, lg = NULL, use_R = FALSE, fsl_img = NULL) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_flag(overwrite)
  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  lg$info("Removing AROMA noise components from fMRI data")

  checkmate::assert_string(prefix)
  if (isFALSE(checkmate::test_file_exists(mixing_file))) {
    warning(glue("Cannot find mixing file corresponding to {in_file}. Skipping AROMA regression"))
    return(in_file)
  }

  if (isFALSE(checkmate::test_integerish(noise_ics, lower=1))) {
    warning(glue("noise_ics must be a vector of integers identifying components to regress out. Skipping AROMA regression"))
    return(in_file)
  }

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip existing file
  } else {
    out_file <- res$out_file
  }

  # just read in the comma-separated noise ICs
  noise_ics <- paste(noise_ics, collapse=",") # fsl_regfilt requires comma-separated list

  # for some reason, fsl_regfilt blows up when we try to feed a regressors x 1 x 1 x timepoints NIfTI
  # fall back to R in this case
  if (isTRUE(use_R)) {
    regfilt_rscript <- system.file("fsl_regfilt.R", package = "BrainGnomes")
    if (!file.exists(regfilt_rscript)) stop("Cannot find fsl_regfilt.R script in the BrainGnomes installation folder")

    cmd <- glue("Rscript --vanilla {regfilt_rscript} --input={in_file} --melodic_mix={mixing_file} --filter={noise_ics} --njobs=1 --output={out_file}")
    lg$info("Running fsl_regfilt.R: {cmd}")
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
#' @param prefix Prefix to prepend to the output file name.
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
spatial_smooth <- function(in_file, prefix = "s", fwhm_mm = 6, brain_mask = NULL, overwrite = FALSE, lg = NULL, fsl_img=NULL) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_number(fwhm_mm, lower = 0.1)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip existing file
  } else {
    out_file <- res$out_file
  }

  fwhm_to_sigma <- sqrt(8 * log(2)) # Details here: https://www.mail-archive.com/hcp-users@humanconnectome.org/msg01393.html
  sigma <- fwhm_mm / fwhm_to_sigma

  p2_intensity <- get_image_quantile(in_file, brain_mask, 2, log_file = log_file, fsl_img = fsl_img)
  median_intensity <- get_image_quantile(in_file, brain_mask, 50, log_file = log_file, fsl_img = fsl_img)
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
#' @param prefix Prefix to prepend to the output file name.
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
intensity_normalize <- function(in_file, prefix="n", brain_mask=NULL, global_median=10000, overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_number(global_median)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }  

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip existing file
  } else {
    out_file <- res$out_file
  }

  median_intensity <- get_image_quantile(in_file, brain_mask, 50, log_file=log_file, fsl_img=fsl_img)
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
#' @param prefix Prefix to prepend to the output file name.
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
confound_regression <- function(in_file, to_regress=NULL, censor_file = NULL, prefix="r", overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_file_exists(to_regress)
  checkmate::assert_string(prefix)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip existing file
  } else {
    out_file <- res$out_file
  }

  method <- "lmfit" # for testing

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
      good_vols <- as.logical(readLines(censor_file)) # bad timepoints are 0 in the censor file
      if (sum(good_vols) < length(good_vols)) lg$info("Fitting confound regression with {sum(good_vols)} of {length(good_vols)} volumes.")
    }
    
    lmfit_residuals_4d(in_file, X = Xmat, include_rows = good_vols, outfile = out_file, preserve_mean = TRUE)
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
#' @details This function replicates the "98-2" heuristic used in FSL’s featlib.tcl:
#'   it computes the 2nd and 98th percentiles from a skull-stripped mean image and thresholds
#'   at 10% above the 2nd percentile. A final mask is formed by applying this threshold,
#'   binarizing, and performing one dilation iteration.
#'
#' @keywords internal
compute_brain_mask <- function(in_file, lg = NULL, fsl_img = NULL) {
  # use the 98 - 2 method from FSL (featlib.tcl ca. line 5345)
  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger_glue() # use root logger
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
  p2 <- get_image_quantile(temp_stripped, quantile=2, exclude_zero = FALSE, log_file = log_file, fsl_img = fsl_img)
  p98 <- get_image_quantile(temp_stripped, quantile=98, exclude_zero = FALSE, log_file = log_file, fsl_img = fsl_img)
  
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
#' @param in_file Path to the BIDS-compliant NIfTI file.
#' @param output Path to output resampled image.
#' @param template_resolution Resolution index (e.g., 1 = 1mm).
#' @param suffix TemplateFlow suffix (e.g., "mask", "T1w").
#' @param desc TemplateFlow descriptor (e.g., "brain").
#' @param extension File extension (default: ".nii.gz").
#' @param interpolation Interpolation method ("nearest", "linear", "continuous").
#' @param overwrite Logical. If `TRUE`, overwrite existing `output`.
#'
#' @details
#'   The relevant template will be identified using the space- entity for the BIDS-compliant input image.
#'   For example, sub-221256_task-trust_run-1_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz
#'   will use the MNI152NLin2009cAsym template image from TemplateFlow.
#' @return Invisible `TRUE` on success. Writes output to `output`.
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


#' Compute spike regressors for volume censoring
#'
#' Evaluates user-supplied expressions against a confounds data.frame to
#' generate spike (one-hot) regressors. Expressions may optionally include
#' a semicolon-separated range of volumes to also flag (e.g. "-1:1; framewise_displacement > 0.5").
#'
#' @param confounds_df Data frame of confounds with one row per volume.
#' @param spike_volume Character vector of expressions to evaluate.
#' @param lg Logger object for messages.
#' @return Matrix of spike regressors or NULL if none detected.
#' @keywords internal
compute_spike_regressors <- function(confounds_df = NULL, spike_volume = NULL, lg = NULL) {
  if (is.null(confounds_df) || is.null(spike_volume)) return(NULL)
  if (!checkmate::test_class(lg, "Logger")) lg <- lgr::get_logger_glue()
  checkmate::assert_character(spike_volume, null.ok = TRUE)

  spikes <- do.call(cbind, lapply(seq_along(spike_volume), function(ii) {
    has_bounds <- grepl(";", spike_volume[ii], fixed = TRUE)
    if (isTRUE(has_bounds)) {
      esplit <- strsplit(spike_volume[ii], "\\s*;\\s*", perl = TRUE)[[1L]]
      stopifnot(length(esplit) == 2L)
      spike_bounds <- as.integer(eval(parse(text = paste0("c(", esplit[1], ")"))))
      expr <- esplit[2L]
    } else {
      spike_bounds <- 0L
      expr <- spike_volume[ii]
    }

    spike_vec <- tryCatch(with(confounds_df, eval(parse(text = expr))), error = function(e) {
      lg$error("Problem evaluating spike expression: %s", expr)
      return(NULL)
    })

    which_spike <- which(spike_vec)
    if (length(which_spike) == 0L) return(NULL)

    spike_df <- do.call(cbind, lapply(which_spike, function(xx) {
      vec <- rep(0, nrow(confounds_df))
      vec[xx] <- 1
      vec
    }))
    colnames(spike_df) <- paste0("spike_", seq_len(ncol(spike_df)))

    if (!identical(spike_bounds, 0L)) {
      shifts <- spike_bounds[spike_bounds != 0L]
      res <- do.call(cbind, lapply(shifts, function(ss) {
        shift_mat <- apply(spike_df, 2, function(col) {
          if (ss < 0) {
            lead(col, abs(ss), default = 0)
          } else {
            lag(col, ss, default = 0)
          }
        })
        colnames(shift_mat) <- paste0("spike_", ifelse(ss < 0, "m", "p"), abs(ss), "_", 1:ncol(shift_mat))
        shift_mat
      }))
      spike_df <- cbind(spike_df, res)
    }

    if (is.null(names(spike_volume)[ii]) || names(spike_volume)[ii] == "") {
      colnames(spike_df) <- paste0("expr", ii, "_", colnames(spike_df))
    } else {
      colnames(spike_df) <- paste0(names(spike_volume)[ii], "_", colnames(spike_df))
    }
    spike_df
  }))

  if (is.null(spikes)) return(NULL)
  spikes <- spikes[, !duplicated(spikes, MARGIN = 2)]
  spikes
}


## -------------------------------------------------------------------------
## Confound Handling Helper -------------------------------------------------
## -------------------------------------------------------------------------

#' Expand confound column patterns
#'
#' Supports perl-compatible regular expressions and simple integer
#' range syntax using angle brackets. For example, the pattern
#' `motion_param_<1-3>` expands to `motion_param_1`,
#' `motion_param_2`, `motion_param_3`, while
#' `motion_param_<1,25>` expands to `motion_param_1` and
#' `motion_param_25`. Patterns without angle brackets are treated as
#' regular expressions evaluated against the available column names.
#'
#' @param patterns Character vector of column patterns.
#' @param available Character vector of available column names.
#' @return Character vector of expanded column names.
#' @keywords internal
expand_confound_columns <- function(patterns, available) {
  checkmate::assert_character(patterns, any.missing = FALSE, null.ok = TRUE)
  if (length(patterns) == 0 || all(is.na(patterns))) return(character())

  res <- unlist(lapply(patterns, function(pat) {
    if (is.na(pat) || pat == "") return(character())

    has_index <- grepl("<[^>]+>", pat)
    if (has_index) {
      pre <- sub("^([^<]*)<[^>]+>.*$", "\\1", pat)
      post <- sub("^[^<]*<[^>]+>(.*)$", "\\1", pat)
      idx_str <- sub("^[^<]*<([^>]+)>.*$", "\\1", pat)
      idx_parts <- strsplit(idx_str, ",")[[1]]
      idx <- unlist(lapply(idx_parts, function(x) {
        if (grepl("-", x)) {
          rng <- as.integer(strsplit(x, "-")[[1]])
          if (length(rng) == 2 && !any(is.na(rng))) seq(rng[1], rng[2]) else as.integer(x)
        } else {
          as.integer(x)
        }
      }))
      return(paste0(pre, idx, post))
    } else {
      matches <- grep(pat, available, value = TRUE, perl = TRUE)
      if (length(matches) > 0) return(matches)
      return(pat)
    }
  }))

  unique(res)
}

# helper file to identify scrubbing censor file from input nifti
get_censor_file <- function(input_bids_info) {
  checkmate::assert_list(input_bids_info)
  construct_bids_filename(
    modifyList(input_bids_info, list(description = cfg$bids_desc, suffix = "censor", ext = ".1D")),
    full.names = TRUE
  )
}

#' Postprocess confounds to match fMRI operations
#'
#' Handles optional spike regression file creation, filtering of confound
#' time series and writing of postprocessed confounds/regressor files.
#' This is used internally by `postprocess_subject()`.
#'
#' @param proc_files List of files returned by `get_fmriprep_outputs()`.
#' @param cfg Configuration list passed to `postprocess_subject`.
#' @param processing_sequence Character vector of enabled processing steps.
#' @param input_bids_info Named list of BIDS entities for the current file.
#' @param fsl_img Optional path to a Singularity image with FSL installed.
#' @param lg Logger object for messages.
#' @return Path to the nuisance regressor file or `NULL` if not created.
#' @keywords internal
postprocess_confounds <- function(proc_files, cfg, processing_sequence,
                                  input_bids_info, fsl_img = NULL, lg = NULL) {
  if (!checkmate::test_class(lg, "Logger")) lg <- lgr::get_logger_glue()

  to_regress <- NULL
  if (isTRUE(cfg$confound_regression$enable) ||
      isTRUE(cfg$confound_calculate$enable) ||
      isTRUE(cfg$scrubbing$enable)) {

    confounds <- data.table::fread(proc_files$confounds,
                                   na.strings = c("n/a", "NA", "."))

    # handle regular expression and range expansion in column names
    cfg$confound_regression$columns <- expand_confound_columns(
      cfg$confound_regression$columns, names(confounds)
    )
    cfg$confound_calculate$columns <- expand_confound_columns(
      cfg$confound_calculate$columns, names(confounds)
    )
    cfg$confound_regression$noproc_columns <- expand_confound_columns(
      cfg$confound_regression$noproc_columns, names(confounds)
    )
    cfg$confound_calculate$noproc_columns <- expand_confound_columns(
      cfg$confound_calculate$noproc_columns, names(confounds)
    )

    confound_cols <- as.character(union(cfg$confound_regression$columns,
                                         cfg$confound_calculate$columns))
    noproc_cols <- as.character(union(cfg$confound_regression$noproc_columns,
                                       cfg$confound_calculate$noproc_columns))
    if (any(noproc_cols %in% confound_cols)) {
      stop("Cannot handle overlaps in noproc_columns and columns for confounds")
    }

    if (isTRUE(cfg$scrubbing$enable)) {
      lg$info("Computing spike regressors using expression: {paste(cfg$scrubbing$expression, collapse=', ')}")
      spike_mat <- compute_spike_regressors(confounds, cfg$scrubbing$expression, lg = lg)
      scrub_file <- construct_bids_filename(
        modifyList(input_bids_info, list(description = cfg$bids_desc, suffix = "scrub", ext = ".tsv")),
        full.names = TRUE
      )
      
      censor_file <- get_censor_file(input_bids_info)
      if (!is.null(spike_mat)) {
        data.table::fwrite(as.data.frame(spike_mat), file = scrub_file, sep = "\t", col.names = FALSE)
        censor_vec <- ifelse(rowSums(spike_mat) > 0, 0, 1)
        writeLines(as.character(censor_vec), con = censor_file)
      } else {
        lg$info("No spikes detected; censor file will contain all 1s")
        writeLines(rep("1", nrow(confounds)), con = censor_file)
      }
    }

    confounds_to_filt <- subset(confounds, select = confound_cols)
    tmp_out <- file.path(tempdir(), sub(".tsv$", "", basename(proc_files$confounds)))
    confound_nii <- mat_to_nii(confounds_to_filt, ni_out = tmp_out, fsl_img = fsl_img)

    if ("apply_aroma" %in% processing_sequence) {
      lg$info("Removing AROMA noise components from confounds")
      confound_nii <- apply_aroma(confound_nii,
        mixing_file = proc_files$melodic_mix, noise_ics = proc_files$noise_ics,
        overwrite = cfg$overwrite, lg = lg, use_R = TRUE, fsl_img = fsl_img
      )
    }

    if ("temporal_filter" %in% processing_sequence) {
      lg$info("Temporally filtering confounds")
      confound_nii <- temporal_filter(confound_nii,
        tr = cfg$tr,
        low_pass_hz = cfg$temporal_filter$low_pass_hz,
        high_pass_hz = cfg$temporal_filter$high_pass_hz,
        overwrite = cfg$overwrite, lg = lg, fsl_img = fsl_img
      )
    }

    filtered_confounds <- data.frame(nii_to_mat(confound_nii))
    filtered_confounds <- setNames(filtered_confounds, confound_cols)

    if (isTRUE(cfg$confound_calculate$enable)) {
      confile <- construct_bids_filename(
        modifyList(input_bids_info, list(description = cfg$bids_desc, suffix = "confounds", ext = ".tsv")),
        full.names = TRUE
      )

      df <- subset(filtered_confounds, select = cfg$confound_calculate$columns)

      if (!is.null(cfg$confound_calculate$noproc_columns) && !is.na(cfg$confound_calculate$noproc_columns)) {
        present_cols <- intersect(cfg$confound_calculate$noproc_columns, names(confounds))
        missing_cols <- setdiff(cfg$confound_calculate$noproc_columns, names(confounds))

        if (length(missing_cols) > 0L) {
          lg$warn(
            "The following confound_calculate$noproc_columns were not found in the confounds file and will be ignored: ",
            paste(missing_cols, collapse = ", ")
          )
        }

        if (length(present_cols) > 0L) {
          noproc_df <- confounds[, present_cols, drop = FALSE]
          noproc_df[is.na(noproc_df)] <- 0
          df <- cbind(df, noproc_df)
        }
      }

      if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$add_to_confounds) && exists("spike_mat") && !is.null(spike_mat)) {
        df <- cbind(df, spike_mat)
      }

      if (isTRUE(cfg$confound_calculate$demean)) {
        df[, cfg$confound_calculate$columns] <- lapply(
          df[, cfg$confound_calculate$columns, drop = FALSE],
          function(x) x - mean(x, na.rm = TRUE)
        )
      }

      lg$info("Writing postprocessed confounds to: {confile}")
      lg$info("Columns are: {paste(names(df), collapse=', ')}")
      data.table::fwrite(df, file = confile, sep = "\t", col.names = FALSE)
    }

    if (isTRUE(cfg$confound_regression$enable)) {
      df <- subset(filtered_confounds, select = cfg$confound_regression$columns)
      df <- as.data.frame(lapply(df, function(cc) cc - mean(cc, na.rm = TRUE)))

      if (!is.null(cfg$confound_regression$noproc_columns) && !is.na(cfg$confound_regression$noproc_columns)) {
        present_cols <- intersect(cfg$confound_regression$noproc_columns, names(confounds))
        missing_cols <- setdiff(cfg$confound_regression$noproc_columns, names(confounds))

        if (length(missing_cols) > 0L) {
          lg$warn(
            "The following confound_regression$noproc_columns were not found in the confounds file and will be ignored: ",
            paste(missing_cols, collapse = ", ")
          )
        }

        if (length(present_cols) > 0L) {
          noproc_df <- confounds[, present_cols, drop = FALSE]
          noproc_df[is.na(noproc_df)] <- 0
          df <- cbind(df, noproc_df)
        }
      }

      to_regress <- construct_bids_filename(
        modifyList(input_bids_info, list(description = cfg$bids_desc, suffix = "regressors", ext = ".tsv")),
        full.names = TRUE
      )

      const_cols <- sapply(df, function(x) all(x == x[1L]))
      if (any(const_cols)) df <- df[, !const_cols, drop = FALSE]
      df <- cbind(1, df)

      data.table::fwrite(df, file = to_regress, sep = "\t", col.names = FALSE)
    }
  }

  to_regress
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
                                  order = 2L, padtype = "even", use_zi = TRUE) {
  checkmate::assert_file_exists(infile)
  checkmate::assert_number(tr, lower=0.01, upper = 100)
  checkmate::assert_number(low_hz, null.ok = TRUE)
  checkmate::assert_number(high_hz, null.ok=TRUE)
  checkmate::assert_integerish(order, len=1L, lower=1L, upper=50L)
  
  
  if (!requireNamespace("signal", quietly = TRUE)) stop("The 'signal' package must be installed.")
  
  fs <- 1 / tr  # sampling frequency in Hz
  nyq <- fs / 2
  
  if (is.null(low_hz) && is.null(high_hz)) {
    stop("Must specify at least one of 'low_hz' or 'high_hz'.")
  }
  
  # if (is.null(low_hz) || low_hz < 0) low_hz <- 0 # 0 indicates no filtering
  # if (is.null(high_hz) || is.infinite(high_hz)) high_hz <- Inf # 0 indicates no filtering
  # 
  
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
                         padtype = padtype, use_zi = use_zi)
}