### primary function to process a given fmriprep subject dataset
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
    if (isTRUE(cfg$temporal_filter$enable)) processing_sequence <- c(processing_sequence, "temporal_filter")
    if (isTRUE(cfg$confound_regression$enable)) processing_sequence <- c(processing_sequence, "confound_regression")
    if (isTRUE(cfg$intensity_normalize$enable)) processing_sequence <- c(processing_sequence, "intensity_normalize")
  }

  lg$info("Processing will proceed in the following order: {paste(processing_sequence, collapse=', ')}")
  
  #### handle confounds, filtering to match MRI data
  if (isTRUE(cfg$confound_regression$enable) || isTRUE(cfg$confound_calculate$enable)) {
    confounds <- data.table::fread(proc_files$confounds, na.strings = c("n/a", "NA", "."))
    confound_cols <- as.character(union(cfg$confound_regression$columns, cfg$confound_calculate$columns))
    noproc_cols <- as.character(union(cfg$confound_regression$noproc_columns, cfg$confound_calculate$noproc_columns)) # no AROMA or filter
    if (any(noproc_cols %in% confound_cols)) {
      stop("Cannot handle overlaps in noproc_columns and columns for confounds")
    }
    
    confounds_to_filt <- subset(confounds, select = confound_cols)
    confound_nii <- mat_to_nii(confounds_to_filt, ni_out = tempfile(pattern = "confounds"))

    # apply AROMA denoising to confounds if AROMA is applied to MRI data
    if ("apply_aroma" %in% processing_sequence) {
      lg$info("Removing AROMA noise components from confounds")
      confound_nii <- apply_aroma(confound_nii,
        mixing_file = proc_files$melodic_mix, noise_ics = proc_files$noise_ics, 
        overwrite=cfg$overwrite, lg=lg, use_R=TRUE, fsl_img = fsl_img
      )
    }

    # apply temporal filter to confounds if temporal filter is applied to MRI data
    if ("temporal_filter" %in% processing_sequence) {
      lg$info("Temporally filtering confounds")
      confound_nii <- temporal_filter(confound_nii,
        tr = cfg$tr, low_pass_hz = cfg$temporal_filter$low_pass_hz, high_pass_hz = cfg$temporal_filter$high_pass_hz, 
        overwrite=cfg$overwrite, lg=lg, fsl_img = fsl_img
      )
    }

    # read in processed confounds and convert back to time x signals data.frame
    filtered_confounds <- data.frame(nii_to_mat(confound_nii))
    filtered_confounds <- setNames(filtered_confounds, confound_cols)
    
    # handle confound calculation
    if (isTRUE(cfg$confound_calculate$enable)) {
      confile <- construct_bids_filename(
        modifyList(input_bids_info, list(description = cfg$bids_desc, suffix = "confounds", ext = ".tsv")),
        full.names = TRUE
      )

      df <- subset(filtered_confounds, select = cfg$confound_calculate$columns)
      if (!is.null(cfg$confound_calculate$noproc_columns) && length(cfg$confound_calculate$noproc_columns) > 0L) {
        noproc_df <- subset(confounds, select = cfg$confound_calculate$noproc_columns)
        noproc_df[is.na(noproc_df)] <- 0 # force 0 value -- NAs don't work as regressors
        df <- cbind(df, noproc_df)
      }

      if (isTRUE(cfg$confound_calculate$demean)) {
        df[, cfg$confound_calculate$columns] <- lapply(df[, cfg$confound_calculate$columns], function(x) x - mean(x, na.rm = TRUE))
      }
      
      lg$info("Writing postprocessed confounds to: {confile}")
      lg$info("Columns are: {paste(names(df), collapse=', ')}")
      data.table::fwrite(df, file = confile, sep = "\t", col.names = FALSE)
    }

    if (isTRUE(cfg$confound_regression$enable)) {
      df <- subset(filtered_confounds, select = cfg$confound_regression$columns)

      # mean center columns
      df <- as.data.frame(lapply(df, function(cc) cc - mean(cc, na.rm = TRUE)))
      
      if (!is.null(cfg$confound_regression$noproc_columns) && length(cfg$confound_regression$noproc_columns) > 0L) {
        noproc_df <- subset(confounds, select=cfg$confound_regression$noproc_columns)
        noproc_df[is.na(noproc_df)] <- 0 # force 0 value -- NAs don't work as regressors
        df <- cbind(df, noproc_df)
      }

      to_regress <- construct_bids_filename(
        modifyList(input_bids_info, list(description = cfg$bids_desc, suffix = "regressors", ext = ".tsv")),
        full.names = TRUE
      )

      const_cols <- sapply(df, function(x) all(x == x[1L]))
      if (any(const_cols)) df <- df[, !const_cols] # remove any constant columns
      df <- cbind(1, df) # add intercept

      data.table::fwrite(df, file = to_regress, sep = "\t", col.names = FALSE)
    }
  }
  
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
      lg$info("Removing AROMA noise components from fMRI data")
      cur_file <- apply_aroma(cur_file, prefix = cfg$apply_aroma$prefix,
        mixing_file = proc_files$melodic_mix,
        noise_ics = proc_files$noise_ics,
        overwrite=cfg$overwrite, lg=lg, fsl_img = fsl_img
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
        to_regress = to_regress,
        overwrite=cfg$overwrite, lg = lg, fsl_img = fsl_img
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
  return(cur_file)
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
    lg <- lgr::get_logger() # use root logger
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

  run_fsl_command(glue("fslmaths {file_sans_ext(in_file)} -mas {mask_file} {file_sans_ext(out_file)} -odt float"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, mask_file, out_file)))
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
    lg <- lgr::get_logger() # use root logger
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
    lg <- lgr::get_logger() # use root logger
    log_file <- NULL # no log file to write
  } else {
    log_file <- lg$appenders$postprocess_log$destination
  }

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
    stop("Need to change R script to accept comma-separated list")
    #cmd <- glue("fsl_regfilt.R {in_file} {mixing_file} {noise_file} 1 {out_file}")
    lg$info(cmd)
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
    lg <- lgr::get_logger() # use root logger
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
    lg <- lgr::get_logger() # use root logger
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
confound_regression <- function(in_file, to_regress=NULL, prefix="r", overwrite=FALSE, lg=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_file_exists(to_regress)
  checkmate::assert_string(prefix)

  if (!checkmate::test_class(lg, "Logger")) {
    lg <- lgr::get_logger() # use root logger
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
    lg <- lgr::get_logger() # use root logger
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
