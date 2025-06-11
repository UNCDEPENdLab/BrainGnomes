#' Resample TemplateFlow Mask to fMRIPrep Image Using Python
#'
#' @param in_file Path to the fMRIPrep-derived NIfTI file.
#' @param output Path to output resampled image.
#' @param transform Optional path to inverse transform (for native-space images).
#' @param template_resolution Resolution index (e.g., 1 = 1mm).
#' @param template_space TemplateFlow space (e.g., "MNI152NLin2009cAsym").
#' @param suffix TemplateFlow suffix (e.g., "mask", "T1w").
#' @param desc TemplateFlow descriptor (e.g., "brain").
#' @param extension File extension (default: ".nii.gz").
#' @param interpolation Interpolation method ("nearest", "linear", "continuous").
#'
#' @return Invisible `TRUE` on success. Writes output to `output`.
#' @importFrom reticulate source_python
#' @keywords internal
resample_template <- function(
  in_file,
  output = NULL,
  template_resolution = 1,
  suffix = "mask",
  desc = "brain",
  extension = ".nii.gz",
  interpolation = "nearest",
  install_dependencies = TRUE
) {
  checkmate::assert_file_exists(in_file)
  checkmate::assert_string(output, null.ok = TRUE)
  checkmate::assert_string(template_space)
  checkmate::assert_flag(install_dependencies)

  # default to same name as input file, but change suffix to templatemask
  if (is.null(output)) {
    f_info <- as.list(extract_bids_info(in_file))
    output <- file.path(dirname(in_file), construct_bids_filename(modifyList(f_info, list(suffix = "templatemask"))))
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
    transform = transform,
    template_resolution = template_resolution,
    suffix = suffix,
    desc = desc,
    extension = extension,
    interpolation = interpolation
  )

  return(invisible(img))
}


### primary function to process a given fmriprep subject dataset
postprocess_subject <- function(in_file, cfg=NULL) {
  checkmate::assert_file_exists(in_file)
  checkmate::assert_list(cfg)
  
  # checkmate::assert_list(processing_sequence)
  proc_files <- get_fmriprep_outputs(in_file)

  # determine if input is in a stereotaxic space
  bids_info <- as.list(extract_bids_info(in_file))
  native_space <- is.na(bids_info$space) || bids_info$space %in% c("T1w", "T2w", "anat")

  # default configuration settings -- not sure whether this should be allowed?
  default_cfg <- list(
    tr = NULL,
    force_processing_order = FALSE,
    log_file = "{proc_files$prefix}_post_fmriprep.log",
    brain_mask = "template",
    overwrite = FALSE, keep_intermediates = FALSE,
    processing_steps = c(
      "apply_mask", "spatial_smooth", "apply_aroma",
      "temporal_filter", "intensity_normalize", "confound_calculate"
    ),
    apply_mask = list(prefix = "m"),
    spatial_smooth = list(prefix = "s", fwhm_mm = 6),
    apply_aroma = list(prefix = "a", aggressive = FALSE),
    temporal_filter = list(prefix = "f", low_pass_hz = 0, high_pass_hz = 0.008333333),
    intensity_normalize = list(prefix = "n", global_median = 10000),
    confound_regression = list(
      prefix = "r", columns = c("csf", "csf_derivative1", "white_matter", "white_matter_derivative1"),
      noproc_columns = list(),
      output_file = "{proc_files$prefix}_confound_regressors.txt"
    ),
    confound_calculate = list(
      columns = c("csf", "csf_derivative1", "white_matter", "white_matter_derivative1"),
      noproc_columns = "framewise_displacement",
      output_file = "{proc_files$prefix}_postprocessed_confounds.txt",
      demean = FALSE
    )
  )

  # location of FSL singularity container
  fsl_img <- cfg$fsl_img

  sdir <- dirname(in_file)

  # add any defaults if user's config is incomplete
  cfg <- populate_defaults(cfg, default_cfg)

  if (!checkmate::test_number(cfg$tr, lower = 0.01, upper = 30)) {
    stop("YAML config must contain a tr field specifying the repetition time in seconds")
  }

  # default to not enforcing user-specified order of processing steps
  if (!checkmate::test_flag(cfg$force_processing_order)) cfg$force_processing_order <- FALSE

  log_file <- glue(cfg$log_file) #evaluate location of log
  complete_file <- paste0(".", basename(proc_files$prefix), "_complete") # evaluate location of log

  if (isFALSE(cfg$overwrite) && file.exists(complete_file)) {
    message(glue("Already completed postprocessing for {in_file}. Skipping"))
    return(NULL)
  }

  start_time <- Sys.time()
  to_log(paste0("# Start fmriprep postprocessing: ", as.character(start_time)), log_file = log_file)
  
  # determine brain mask to be used for computing intensity thresholds for susan and normalization
  # Step 1: Handle user-specified brain mask
  brain_mask <- NULL
  if (checkmate::test_string(cfg$brain_mask)) {
    if (cfg$brain_mask == "template") {
      brain_mask <- get_template_mask(in_file, log_file)
    } else if (checkmate::test_file_exists(cfg$brain_mask)) {
      brain_mask <- cfg$brain_mask
    } else {
      warning("Cannot find brain_mask: ", cfg$brain_mask, ". Will try to find an alternative.")
    }
  }

  # Step 2: Handle fallback cases (if user input was invalid or unspecified)
  if (is.null(brain_mask)) {
    if (native_space) {
      if (!is.null(proc_files$brain_mask)) {
        brain_mask <- proc_files$brain_mask # fixed typo here
      } else {
        brain_mask <- compute_brain_mask(in_file, log_file)
      }
    } else {
      brain_mask <- get_template_mask(in_file, log_file)
    }
  }

  cur_file <- proc_files$bold
  file_set <- cur_file

  ## setup order of processing steps
  checkmate::assert_character(cfg$processing_steps) # ensure we have a character vector
  cfg$processing_steps <- tolower(cfg$processing_steps) # avoid case issues

  # handle small glitches in nomenclature
  cfg$processing_steps <- sub("spatial_smoothing", "spatial_smooth", cfg$processing_steps, fixed=TRUE)
  cfg$processing_steps <- sub("temporal_filtering", "temporal_filter", cfg$processing_steps, fixed=TRUE)
  cfg$processing_steps <- sub("confound_regress", "confound_regression", cfg$processing_steps, fixed = TRUE)
  cfg$processing_steps <- sub("intensity_normalization", "intensity_normalize", cfg$processing_steps, fixed = TRUE)

  if (isTRUE(cfg$force_processing_order)) {
    processing_sequence <- cfg$processing_steps
    to_log("# We will follow the user-specified processing order, with no guarantees about whether this may yield data problems.", log_file = log_file)
  } else {
    processing_sequence <- c()
    if ("apply_mask" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "apply_mask")
    if ("spatial_smooth" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "spatial_smooth")
    if ("apply_aroma" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "apply_aroma")
    if ("temporal_filter" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "temporal_filter")
    if ("confound_regression" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "confound_regression")
    if ("intensity_normalize" %in% cfg$processing_steps) processing_sequence <- c(processing_sequence, "intensity_normalize")
  }

  to_log(glue("# Processing will proceed in the following order: {paste(processing_sequence, collapse=', ')}"), log_file = log_file)
  
  #### handle confounds, filtering to match MRI data
  if ("confound_regression" %in% cfg$processing_steps || "confound_calculate" %in% cfg$processing_steps) {
    confounds <- data.table::fread(proc_files$confounds, na.strings = c("n/a", "NA", "."))
    confound_cols <- as.character(union(cfg$confound_regression$columns, cfg$confound_calculate$columns))
    noproc_cols <- as.character(union(cfg$confound_regression$noproc_columns, cfg$confound_calculate$noproc_columns)) # no AROMA or filter
    if (any(noproc_cols %in% confound_cols)) {
      stop("Cannot handle overlaps in noproc_columns and columns for confounds")
    }
    
    confounds_to_filt <- subset(confounds, select = confound_cols)
    confound_nii <- mat_to_nii(confounds_to_filt, ni_out = tempfile(pattern = "confounds"))

    # apply AROMA denoising to confounds if AROMA is applied to MRI data
    if ("apply_aroma" %in% cfg$processing_steps) {
      to_log("# Removing AROMA noise components from confounds", log_file=log_file)
      confound_nii <- apply_aroma(confound_nii,
        mixing_file = proc_files$melodic_mix, noise_ics = proc_files$noise_ics, 
        overwrite=cfg$overwrite, log_file=log_file, use_R=TRUE, fsl_img = fsl_img
      )
    }

    # apply temporal filter to confounds if temporal filter is applied to MRI data
    if ("temporal_filter" %in% cfg$processing_steps) {
      to_log("# Temporally filtering confounds", log_file=log_file)
      confound_nii <- temporal_filter(confound_nii,
        tr = cfg$tr, low_pass_hz = cfg$temporal_filter$low_pass_hz, high_pass_hz = cfg$temporal_filter$high_pass_hz, 
        overwrite=cfg$overwrite, log_file=log_file, fsl_img = fsl_img
      )
    }

    # read in processed confounds and convert back to time x signals data.frame
    filtered_confounds <- data.frame(nii_to_mat(confound_nii))
    filtered_confounds <- setNames(filtered_confounds, confound_cols)
    
    if ("confound_calculate" %in% cfg$processing_steps) {
      df <- subset(filtered_confounds, select = cfg$confound_calculate$columns)
      if (!is.null(cfg$confound_calculate$noproc_columns) && length(cfg$confound_calculate$noproc_columns) > 0L) {
        noproc_df <- subset(confounds, select = cfg$confound_calculate$noproc_columns)
        noproc_df[is.na(noproc_df)] <- 0 # force 0 value -- NAs don't work as regressors
        df <- cbind(df, noproc_df)
      }

      if (isTRUE(cfg$confound_calculate$demean)) {
        df[, cfg$confound_calculate$columns] <- lapply(df[, cfg$confound_calculate$columns], function(x) x - mean(x, na.rm = TRUE))
      }
      confile <- glue(cfg$confound_calculate$output_file)
      to_log(glue("# Writing filtered confounds to: {confile}"), log_file = log_file)
      to_log(glue("# Columns are: {paste(names(df), collapse=', ')}"), log_file = log_file)
      data.table::fwrite(df, file = confile, sep = "\t", col.names = FALSE)
    }

    if ("confound_regression" %in% cfg$processing_steps) {
      df <- subset(filtered_confounds, select = cfg$confound_regression$columns)

      # mean center columns
      df <- as.data.frame(lapply(df, function(cc) cc - mean(cc)))
      
      if (!is.null(cfg$confound_regression$noproc_columns) && length(cfg$confound_regression$noproc_columns) > 0L) {
        noproc_df <- subset(confounds, select=cfg$confound_regression$noproc_columns)
        noproc_df[is.na(noproc_df)] <- 0 # force 0 value -- NAs don't work as regressors
        df <- cbind(df, noproc_df)
      }
      to_regress <- glue(cfg$confound_regression$output_file)
      
      const_cols <- sapply(df, function(x) all(x == x[1L]))
      if (any(const_cols)) df <- df[, !const_cols] # remove any constant columns
      df <- cbind(1, df) # add intercept

      data.table::fwrite(df, file = to_regress, sep = "\t", col.names = FALSE)
    }
  }
  
  #### loop over fMRI processing steps in sequence
  proc_prefix <- "-"

  for (step in processing_sequence) {
    if (step == "spatial_smooth") {
      proc_prefix <- paste0(cfg$spatial_smooth$prefix, proc_prefix)
      to_log(glue("# Spatial smoothing with FHWM {cfg$spatial_smooth$fwhm_mm}mm kernel"), log_file = log_file)
      cur_file <- spatial_smooth(cur_file,
        brain_mask = brain_mask, prefix = cfg$spatial_smooth$prefix, fwhm_mm = cfg$spatial_smooth$fwhm_mm, 
        overwrite = cfg$overwrite, log_file = log_file, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "apply_aroma") {
      proc_prefix <- paste0(cfg$apply_aroma$prefix, proc_prefix)
      to_log("# Removing AROMA noise components from fMRI data", log_file=log_file)
      cur_file <- apply_aroma(cur_file, prefix = cfg$apply_aroma$prefix,
        mixing_file = proc_files$melodic_mix,
        noise_ics = proc_files$noise_ics,
        overwrite=cfg$overwrite, log_file=log_file, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "temporal_filter") {
      proc_prefix <- paste0(cfg$temporal_filter$prefix, proc_prefix)
      to_log(glue("# Temporal filtering with lp: {cfg$temporal_filter$low_pass_hz} Hz, hp: {cfg$temporal_filter$high_pass_hz} Hz given TR: {cfg$tr}"), log_file = log_file)
      cur_file <- temporal_filter(cur_file, prefix = cfg$temporal_filter$prefix,
        tr = cfg$tr, low_pass_hz = cfg$temporal_filter$low_pass_hz,
        high_pass_hz = cfg$temporal_filter$high_pass_hz,
        overwrite=cfg$overwrite, log_file=log_file, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "intensity_normalize") {
      proc_prefix <- paste0(cfg$intensity_normalize$prefix, proc_prefix)
      to_log(glue("# Intensity normalizing fMRI data to global median: {cfg$intensity_normalize$global_median}"), log_file = log_file)
      cur_file <- intensity_normalize(cur_file, prefix = cfg$intensity_normalize$prefix,
        brain_mask = brain_mask,
        global_median = cfg$intensity_normalize$global_median,
        overwrite=cfg$overwrite, log_file=log_file, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "confound_regression") {
      proc_prefix <- paste0(cfg$confound_regression$prefix, proc_prefix)
      to_log(glue("# Removing confound regressors from fMRI data using file: {to_regress}"), log_file = log_file)
      cur_file <- confound_regression(cur_file, prefix = cfg$confound_regression$prefix,
        to_regress = to_regress,
        overwrite=cfg$overwrite, log_file = log_file, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    } else if (step == "apply_mask") {
      proc_prefix <- paste0(cfg$apply_mask$prefix, proc_prefix)
      to_log(glue("# Masking fMRI data using file: {brain_mask}"), log_file = log_file)
      cur_file <- apply_mask(cur_file, prefix = cfg$apply_mask$prefix,
        mask_file = brain_mask,
        overwrite=cfg$overwrite, log_file = log_file, fsl_img = fsl_img
      )
      file_set <- c(file_set, cur_file)
    }
  }

  if (isFALSE(cfg$keep_intermediates) && length(file_set) > 2L) {
    # initial file is the BOLD input from fmriprep, last file is the final processed image
    to_delete <- file_set[2:(length(file_set) - 1)]
    for (ff in to_delete) {
      cat("# Removing", ff, "\n", file = log_file, append=TRUE)
      if (file.exists(ff)) unlink(ff)
    }
  }

  # move the final file into a BIDS-friendly file name with a desc field
  if (!is.null(cfg$bids_desc) && is.character(cfg$bids_desc)) {
    final_filename <- sub(proc_prefix, "", cur_file) # strip prefix
    if (grepl("_desc-", final_filename)) final_filename <- sub("_desc-[^_\\.]+", paste0("_desc-", cfg$bids_desc), final_filename)
    else final_filename <- sub("((_bold)?\\.nii(\\.gz)?)", paste0("_desc-", cfg$bids_desc, "\\1"), final_filename)
    file.rename(cur_file, final_filename)
  }

  end_time <- Sys.time()
  to_log(glue("# End fmriprep postprocessing: {as.character(end_time)}"), log_file = log_file)
  cat(as.character(start_time), as.character(end_time), file = complete_file, sep = "\n")
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
#' @param log_file Optional path to a file for logging FSL command output.
#' @param fsl_img Optional path to a Singularity image to execute the command in a container.
#'
#' @return Path to the masked output NIfTI file.
#'
#' @keywords internal
#' @importFrom checkmate assert_file_exists assert_string
#' @importFrom glue glue
apply_mask <- function(in_file, mask_file, prefix="m", overwrite=FALSE, log_file=NULL, fsl_img = NULL) {
  checkmate::assert_file_exists(mask_file)
  checkmate::assert_string(prefix)

  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else {
    out_file <- res$out_file
  }

  run_fsl_command(glue("fslmaths {in_file} -mas {mask_file} {out_file} -odt float"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, mask_file, out_file)))
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
#' @param log_file Optional path to a log file for command output.
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#'
#' @return The path to the temporally filtered output NIfTI file.
#'
#' @details The mean image is added back after filtering to preserve signal intensity. Filtering
#' is skipped if the output file already exists and \code{overwrite = FALSE}.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom checkmate assert_string assert_number assert_flag
temporal_filter <- function(in_file, prefix="f", low_pass_hz=0, high_pass_hz=1/120, tr=NULL, overwrite=FALSE, log_file=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_number(low_pass_hz)
  checkmate::assert_number(high_pass_hz)
  stopifnot(low_pass_hz < high_pass_hz)
  checkmate::assert_number(tr, lower = 0.01)
  checkmate::assert_flag(overwrite)

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
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
  run_fsl_command(glue("fslmaths {in_file} -Tmean {temp_tmean}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean)))
  run_fsl_command(glue("fslmaths {in_file} -bptf {hp_volumes} {lp_volumes} -add {temp_tmean} {out_file}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean, out_file)))

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
#' @param log_file Optional path to a file for logging FSL command output.
#' @param use_R Logical; if \code{TRUE}, use an R wrapper script (\code{fsl_regfilt.R}) instead of \code{fsl_regfilt}.
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#'
#' @return Path to the denoised output NIfTI file. If required files are missing, returns \code{in_file} unmodified.
#'
#' @keywords internal
#' @importFrom glue glue
#' @importFrom checkmate assert_string test_file_exists
apply_aroma <- function(in_file, prefix = "a", mixing_file, noise_ics, overwrite = FALSE, log_file = NULL, use_R = FALSE, fsl_img = NULL) {
  # checkmate::assert_file_exists(in_file)
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
    return(res$out_file) # skip out
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
    to_log(cmd, log_file=log_file)
    system(cmd)
  } else {
    cmd <- glue("fsl_regfilt -i {in_file} -o {out_file} -d {mixing_file} -f {noise_ics}")
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
#' @param log_file Optional path to a file for logging the FSL command output.
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
spatial_smooth <- function(in_file, prefix = "s", fwhm_mm = 6, brain_mask = NULL, overwrite = FALSE, log_file = NULL, fsl_img=NULL) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_number(fwhm_mm, lower = 0.1)

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
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
  run_fsl_command(glue("fslmaths {in_file} -Tmin -bin {extents_mask} -odt char"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, extents_mask))) # save extents to temp file

  # compute mean functional image used in susan
  temp_tmean <- tempfile(pattern = "tmean")
  run_fsl_command(glue("fslmaths {in_file} -Tmean {temp_tmean}"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, temp_tmean))) # save tmean to temporary file
  run_fsl_command(glue("susan {in_file} {susan_thresh} {sigma} 3 1 1 {temp_tmean} {susan_thresh} {out_file}"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, temp_tmean, out_file)))

  # apply extents mask
  run_fsl_command(glue("fslmaths {out_file} -mul {extents_mask} {out_file} -odt float"), log_file = log_file, fsl_img = fsl_img, bind_paths = dirname(c(in_file, extents_mask, out_file)))

  rm_niftis(c(temp_tmean, extents_mask, glue("{out_file}_usan_size"))) # cleanup temp files

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
#' @param log_file Optional path to a log file for command output.
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
intensity_normalize <- function(in_file, prefix="n", brain_mask=NULL, global_median=10000, overwrite=FALSE, log_file=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_string(prefix)
  checkmate::assert_number(global_median)

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else {
    out_file <- res$out_file
  }

  median_intensity <- get_image_quantile(in_file, brain_mask, 50, log_file=log_file, fsl_img=fsl_img)
  rescaling_factor <- global_median / median_intensity

  run_fsl_command(glue("fslmaths {in_file} -mul {rescaling_factor} {out_file} -odt float"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, out_file)))
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
#' @param log_file Optional path to a log file for recording command output.
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
confound_regression <- function(in_file, to_regress=NULL, prefix="r", overwrite=FALSE, log_file=NULL, fsl_img = NULL) {
  #checkmate::assert_file_exists(in_file)
  checkmate::assert_file_exists(to_regress)
  checkmate::assert_string(prefix)

  # handle extant file
  res <- out_file_exists(in_file, prefix, overwrite)
  if (isTRUE(res$skip)) {
    return(res$out_file) # skip out
  } else {
    out_file <- res$out_file
  }

  # convert text file to FSL vest file for fsl_glm to accept it
  vest_file <- tempfile(pattern = "regressors", fileext = ".mat")
  run_fsl_command(glue("Text2Vest {to_regress} {vest_file}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(to_regress, vest_file)))
  
  # because the residuals will be demeaned and intensity normalization should follow this step, add back in the temporal mean from the pre-regression image
  temp_tmean <- tempfile(pattern="tmean")
  run_fsl_command(glue("fslmaths {in_file} -Tmean {temp_tmean}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, temp_tmean)))
  run_fsl_command(glue("fsl_glm -i {in_file} -d {vest_file} --out_res={out_file}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, vest_file, out_file)))
  run_fsl_command(glue("fslmaths {out_file} -add {temp_tmean} {out_file}"), log_file=log_file, fsl_img = fsl_img, bind_paths=dirname(c(out_file, temp_tmean)))

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
#' @param log_file Optional path to a log file for command output.
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
compute_brain_mask <- function(in_file, log_file = NULL, fsl_img = NULL) {
  # use the 98 - 2 method from FSL (featlib.tcl ca. line 5345)

  to_log("# Computing brain mask from fMRI data using FSL's 98-2 percentile method", log_file=log_file)

  # first use FSL bet on the mean functional to get a starting point
  tmean_file <- tempfile(pattern="tmean")
  run_fsl_command(glue("fslmaths {in_file} -Tmean {tmean_file}"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(in_file, tmean_file)))
  
  temp_bet <- tempfile()
  run_fsl_command(glue("bet {tmean_file} {temp_bet} -R -f 0.3 -m -n"), log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(c(tmean_file, temp_bet)))

  temp_stripped <- tempfile(pattern="epi_bet")
  run_fsl_command(glue("fslmaths {in_file} -mas {temp_bet}_mask {temp_stripped}"), log_file = log_file, fsl_img, bind_paths=dirname(c(in_file, temp_bet, temp_stripped)))

  # now compute 2nd and 98th percentiles on skull-stripped image
  p2 <- get_image_quantile(temp_stripped, quantile=2, exclude_zero = FALSE, log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(temp_stripped))
  p98 <- get_image_quantile(temp_stripped, quantile=98, exclude_zero = FALSE, log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(temp_stripped))
  
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
