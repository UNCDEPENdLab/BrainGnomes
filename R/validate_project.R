# helper to convert NULL, empty list, or "", character(0) to NA for conformity
validate_char <- function(arg) {
  if (is.null(arg) || identical(arg, list()) || length(arg) == 0L || (length(arg) == 1L && (is.na(arg[1L]) || arg[1L] == ""))) {
    arg <- NA_character_
  }
  return(arg)
}

# check memgb, nhours, ncores, cli_options, and sched_args for all jobs
validate_job_settings <- function(scfg, job_name = NULL) {
  gaps <- c()

  if (is.null(job_name)) stop("Invalid NULL job_name")
  if (!is.list(scfg[[job_name]])) stop("scfg[[job_name]] is not a list")

  if (!checkmate::test_number(scfg[[job_name]]$memgb, lower = 1, upper = 1000)) {
    message("Invalid memgb setting in ", job_name, ". We will ask you for a valid value")
    gaps <- c(gaps, paste(job_name, "memgb", sep = "/"))
    scfg[[job_name]]$memgb <- NULL
  }

  if (!checkmate::test_number(scfg[[job_name]]$nhours, lower = 1, upper = 1000)) {
    message("Invalid nhours setting in ", job_name, ". We will ask you for a valid value")
    gaps <- c(gaps, paste(job_name, "nhours", sep = "/"))
    scfg[[job_name]]$nhours <- NULL
  }

  if (!checkmate::test_number(scfg[[job_name]]$ncores, lower = 1, upper = 250)) {
    message("Invalid ncores setting in ", job_name, ". We will ask you for a valid value")
    gaps <- c(gaps, paste(job_name, "ncores", sep = "/"))
    scfg[[job_name]]$ncores <- NULL
  }

  # conform cli_options to NA_character_ on empty
  scfg[[job_name]]$cli_options <- validate_char(get_nested_values(scfg, job_name)$cli_options)
  scfg[[job_name]]$sched_args <- validate_char(get_nested_values(scfg, job_name)$sched_args)

  attr(scfg, "gaps") <- gaps
  return(scfg)
}

#' Validate the structure of a project configuration object
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @importFrom checkmate assert_flag test_class test_directory_exists test_file_exists
#' @keywords internal
validate_bids_conversion <- function(scfg = list(), quiet = FALSE) {
  # BIDS conversion validation -- only relevant if enabled
  if (!checkmate::test_flag(scfg$bids_conversion$enable)) {
    if (!quiet) message("Invalid bids_conversion/enable flag. You will be asked for this.")
    attr(scfg, "gaps") <- "bids_conversion/enable"
    scfg$bids_conversion$enable <- NULL
    return(scfg)
  }

  if (isFALSE(scfg$bids_conversion$enable)) return(scfg) # no validation

  gaps <- c()

  scfg <- validate_job_settings(scfg, "bids_conversion")
  gaps <- c(gaps, attr(scfg, "gaps"))

  for (rr in c("metadata/dicom_directory", "metadata/bids_directory")) {
    if (!checkmate::test_directory_exists(get_nested_values(scfg, rr))) {
      message("Config file is missing valid directory for ", rr, ".")
      gaps <- c(gaps, rr)
    }
  }

  for (rr in c("compute_environment/heudiconv_container", "bids_conversion/heuristic_file")) {
    if (!checkmate::test_file_exists(get_nested_values(scfg, rr))) {
      message("Config file is missing valid ", rr, ". You will be asked for this.")
      gaps <- c(gaps, rr)
    }
  }

  # validate BIDS conversion sub_regex
  if (!checkmate::test_string(scfg$bids_conversion$sub_regex)) {
    message("Missing sub_regex in $bids_conversion You will be asked for this.")
    gaps <- c(gaps, "bids_conversion/sub_regex")
    scfg$bids_conversion$sub_regex <- NULL
  }

  # validate bids_conversion sub_id_match
  if (!checkmate::test_string(scfg$bids_conversion$sub_id_match)) {
    message("Missing sub_id_match in $bids_conversion You will be asked for this.")
    gaps <- c(gaps, "bids_conversion/sub_id_match")
    scfg$bids_conversion$sub_id_match <- NULL
  }

  if (!checkmate::test_string(scfg$bids_conversion$ses_regex, na.ok = TRUE)) {
    message("Invalid ses_regex in $bids_conversion. You will be asked for this.")
    gaps <- c(gaps, "bids_conversion/ses_regex")
    scfg$bids_conversion$ses_regex <- NULL
  }

  if (!checkmate::test_string(scfg$bids_conversion$ses_id_match, na.ok = TRUE)) {
    message("Invalid ses_id_match in $bids_conversion. You will be asked for this.")
    gaps <- c(gaps, "bids_conversion/ses_id_match")
    scfg$bids_conversion$ses_id_match <- NULL
  }

  if (!checkmate::test_flag(scfg$bids_conversion$overwrite)) {
    message("Invalid overwrite flag in $bids_conversion. You will be asked for this.")
    gaps <- c(gaps, "bids_conversion/overwrite")
    scfg$bids_conversion$overwrite <- NULL
  }

  if (!checkmate::test_flag(scfg$bids_conversion$clear_cache)) {
    message("Invalid clear_cache flag in $bids_conversion. You will be asked for this.")
    gaps <- c(gaps, "bids_conversion/clear_cache")
    scfg$bids_conversion$clear_cache <- NULL
  }

  attr(scfg, "gaps") <- gaps
  return(scfg)
}

#' Validate the structure of a project configuration object
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @importFrom checkmate assert_flag test_class test_directory_exists test_file_exists
#' @keywords internal
validate_project <- function(scfg = list(), quiet = FALSE) {
  if (!checkmate::test_class(scfg, "bg_project_cfg")) {
    if (inherits(scfg, "list")) {
      class(scfg) <- c(class(scfg), "bg_project_cfg")
    } else {
      stop("scfg must be a list of bg_project_cfg object")
    }
  }

  checkmate::assert_flag(quiet)

  gaps <- c()

  if (is.null(scfg$metadata$project_name)) {
    if (!quiet) message("Config file is missing project_name. You will be asked for this.")
    gaps <- c(gaps, "metadata/project_name")
  }

  # required directories not tied to a specific step
  core_dirs <- c(
    "metadata/project_directory", "metadata/log_directory",
    "metadata/scratch_directory", "metadata/templateflow_home"
  )
  for (rr in core_dirs) {
    if (!checkmate::test_directory_exists(get_nested_values(scfg, rr))) {
      message("Config file is missing valid directory for ", rr, ".")
      gaps <- c(gaps, rr)
    }
  }

  # step-specific directories
  if (any(scfg$fmriprep$enable, scfg$aroma$enable, scfg$postprocess$enable, na.rm = TRUE)) {
    fmriprep_dir <- get_nested_values(scfg, "metadata/fmriprep_directory")
    if (is.null(fmriprep_dir) || !nzchar(fmriprep_dir) || !checkmate::test_directory_exists(fmriprep_dir)) {
      message("Config file is missing valid directory for metadata/fmriprep_directory.")
      gaps <- c(gaps, "metadata/fmriprep_directory")
    }
  }

  if (any(scfg$postprocess$enable, scfg$extract_rois$enable, na.rm = TRUE)) {
    if (!checkmate::test_directory_exists(get_nested_values(scfg, "metadata/postproc_directory"))) {
      message("Config file is missing valid directory for metadata/postproc_directory.")
      gaps <- c(gaps, "metadata/postproc_directory")
    }
  }

  if (isTRUE(scfg$mriqc$enable)) {
    if (!checkmate::test_directory_exists(get_nested_values(scfg, "metadata/mriqc_directory"))) {
      message("Config file is missing valid directory for metadata/mriqc_directory.")
      gaps <- c(gaps, "metadata/mriqc_directory")
    }

    if (!checkmate::test_file_exists(scfg$compute_environment$mriqc_container)) {
      message("MRIQC is enabled but mriqc_container is missing. You will be asked for this.")
      gaps <- c(gaps, "compute_environment/mriqc_container")
    }
  }

  if (isTRUE(scfg$flywheel_sync$enable)) {
    if (is.null(scfg$flywheel_sync$source_url)) {
      message("Config file is missing Flywheel source_url. You will be asked for this.")
      gaps <- c(gaps, "flywheel_sync/source_url")
    }

    if (!checkmate::test_directory_exists(scfg$flywheel_sync$dropoff_directory)) {
      message("Config file is missing valid directory for flywheel_sync/dropoff_directory.")
      gaps <- c(gaps, "flywheel_sync/dropoff_directory")
    }

    if (!checkmate::test_directory_exists(scfg$flywheel_sync$temp_directory)) {
      message("Config file is missing valid directory for flywheel_sync/temp_directory.")
      gaps <- c(gaps, "flywheel_sync/temp_directory")
    }
  }

  # step-specific required files
  if (isTRUE(scfg$fmriprep$enable)) {
    for (rr in c("compute_environment/fmriprep_container", "fmriprep/fs_license_file")) {
      if (!checkmate::test_file_exists(get_nested_values(scfg, rr))) {
        message("Config file is missing valid ", rr, ". You will be asked for this.")
        gaps <- c(gaps, rr)
      }
    }

    # BIDS directory required for fmriprep
    if (!checkmate::test_directory_exists(get_nested_values(scfg, "metadata/bids_directory"))) {
      message("Config file is missing valid directory for metadata/bids_directory.")
      gaps <- c(gaps, "metadata/bids_directory")
    }
  }

  if (!checkmate::test_subset(scfg$compute_environment$scheduler, c("slurm", "torque"), empty.ok = FALSE)) {
    message("Invalid scheduler setting. You will be asked for this.")
    gaps <- c(gaps, "compute_environment/scheduler")
    scfg$compute_environment$scheduler <- NULL
  }

  if (isTRUE(scfg$aroma$enable) && !checkmate::test_file_exists(scfg$compute_environment$aroma_container)) {
    message("AROMA is enabled but aroma_container is missing. You will be asked for this.")
    gaps <- c(gaps, "compute_environment/aroma_container")
  }

  # validate job settings only for enabled steps
  for (job in c("fmriprep", "mriqc", "aroma")) {
    if (!checkmate::test_flag(scfg[[job]]$enable)) {
      message("Invalid enable flag in ", job, ". You will be asked for this.")
      gaps <- c(gaps, paste0(job, "/enable"))
      scfg[[job]]$enable <- NULL
      next
    }
    if (isTRUE(scfg[[job]]$enable)) {
      scfg <- validate_job_settings(scfg, job)
      gaps <- c(gaps, attr(scfg, "gaps"))
    }
  }

  if (!checkmate::test_flag(scfg$bids_validation$enable)) {
    message("Invalid bids_validation/enable flag. You will be asked for this.")
    gaps <- c(gaps, "bids_validation/enable")
    scfg$bids_validation$enable <- NULL
  } else if (isTRUE(scfg$bids_validation$enable)) {
    if (!checkmate::test_file_exists(scfg$compute_environment$bids_validator)) {
      message("BIDS validation is enabled but bids_validator is missing. You will be asked for this.")
      gaps <- c(gaps, "compute_environment/bids_validator")
    }
    scfg <- validate_job_settings(scfg, "bids_validation")
    gaps <- c(gaps, attr(scfg, "gaps"))

    scfg$bids_validation$outfile <- validate_char(scfg$bids_validation$outfile)
    if (!checkmate::test_string(scfg$bids_validation$outfile)) {
      message("Missing outfile in $bids_validation. You will be asked for this.")
      gaps <- c(gaps, "bids_validation/outfile")
      scfg$bids_validation$outfile <- NULL
    }
  }

  # validate bids conversion
  scfg <- validate_bids_conversion(scfg, quiet = quiet)
  gaps <- c(gaps, attr(scfg, "gaps"))


  # Postprocessing settings validation (function in setup_postproc.R)
  if (!checkmate::test_flag(scfg$postprocess$enable)) {
    message("Invalid postprocess/enable flag. You will be asked for this.")
    gaps <- c(gaps, "postprocess/enable")
    scfg$postprocess$enable <- NULL
  } else if (isTRUE(scfg$postprocess$enable)) {
    # enforce presence of fsl container
    if (!checkmate::test_file_exists(scfg$compute_environment$fsl_container)) {
      message("Postprocessing is enabled but fsl_container is missing. You will be asked for this.")
      gaps <- c(gaps, "compute_environment/fsl_container")
    }
    postprocess_result <- validate_postprocess_configs(scfg$postprocess, quiet)
    scfg$postprocess <- postprocess_result$postprocess
    gaps <- c(gaps, postprocess_result$gaps)
  }

  if (!checkmate::test_flag(scfg$extract_rois$enable)) {
    message("Invalid extract_rois/enable flag. You will be asked for this.")
    gaps <- c(gaps, "extract_rois/enable")
    scfg$extract_rois$enable <- NULL
  } else if (isTRUE(scfg$extract_rois$enable)) {
    extract_result <- validate_extract_configs(scfg$extract_rois, quiet)
    scfg$extract_rois <- extract_result$extract_rois
    gaps <- c(gaps, extract_result$gaps)
  }

  attr(scfg, "gaps") <- gaps

  return(scfg)
}


#' Validate postprocess configuration block
#' @param ppcfg a postprocess configuration block
#' @param quiet a flag indicating whether to suppress messages
#' @keywords internal
#' @noRd
validate_postprocess_config_single <- function(ppcfg, cfg_name = NULL, quiet = FALSE) {
  gaps <- c()

  # postprocess/input_regex
  if (!"input_regex" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/input_regex")
  } else if (!checkmate::test_string(ppcfg$input_regex)) {
    if (!quiet) message(glue("Invalid input_regex in $postprocess${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "postprocess/input_regex")
    ppcfg$input_regex <- NULL
  }

  # postprocess/bids_desc
  if (!"bids_desc" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/bids_desc")
  } else if (!checkmate::test_string(ppcfg$bids_desc)) {
    if (!quiet) message(glue("Invalid bids_desc in $postprocess${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "postprocess/bids_desc")
    ppcfg$bids_desc <- NULL
  }

  # postprocess/keep_intermediates
  if (!"keep_intermediates" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/keep_intermediates")
  } else if (!checkmate::test_flag(ppcfg$keep_intermediates)) {
    if (!quiet) message(glue("Invalid keep_intermediates in $postprocess${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "postprocess/keep_intermediates")
    ppcfg$keep_intermediates <- NULL
  }

  # postprocess/overwrite
  if (!"overwrite" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/overwrite")
  } else if (!checkmate::test_flag(ppcfg$overwrite)) {
    if (!quiet) message(glue("Invalid overwrite in $postprocess${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "postprocess/overwrite")
    ppcfg$overwrite <- NULL
  }

  # postprocess/tr
  if (!"tr" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/tr")
  } else if (!checkmate::test_number(ppcfg$tr, lower = 0.01, upper = 100)) {
    if (!quiet) message(glue("Invalid tr in $postprocess${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "postprocess/tr")
    ppcfg$tr <- NULL
  }

  # validate temporal filtering
  if (is.null(ppcfg$temporal_filter$enable)) gaps <- c(gaps, "postprocess/temporal_filter/enable")
  if ("temporal_filter" %in% names(ppcfg) && isTRUE(ppcfg$temporal_filter$enable)) {
    if (!checkmate::test_number(ppcfg$temporal_filter$low_pass_hz, lower=0)) {
      if (!quiet) message(glue("Missing low_pass_hz in $postprocess${cfg_name}. You will be asked for this."))
      gaps <- c(gaps, "postprocess/temporal_filter/low_pass_hz")
      ppcfg$temporal_filter$low_pass_hz <- NULL
    }
    if (!checkmate::test_number(ppcfg$temporal_filter$high_pass_hz, lower = 0)) {
      if (!quiet) message(glue("Missing high_pass_hz in $postprocess${cfg_name}. You will be asked for this."))
      gaps <- c(gaps, "postprocess/temporal_filter/high_pass_hz")
      ppcfg$temporal_filter$high_pass_hz <- NULL
    }
    if (!is.null(ppcfg$temporal_filter$low_pass_hz) && !is.null(ppcfg$temporal_filter$high_pass_hz) && 
        ppcfg$temporal_filter$high_pass_hz < ppcfg$temporal_filter$low_pass_hz) {
      if (!quiet) message("high_pass_hz is greater than low_pass_hz $postprocess${cfg_name}$temporal_filter. You will be asked to respecify valid values.")
      gaps <- unique(c(gaps, "postprocess/temporal_filter/low_pass_hz", "postprocess/temporal_filter/high_pass_hz"))
      ppcfg$temporal_filter$low_pass_hz <- NULL
      ppcfg$temporal_filter$high_pass_hz <- NULL
    }
    if (!checkmate::test_string(ppcfg$temporal_filter$prefix)) {
      if (!quiet) message(glue("No valid prefix found for $postprocess${cfg_name}$temporal_filter. Defaulting to 'f'"))
      ppcfg$temporal_filter$prefix <- "f"
    }
    if (!checkmate::test_string(ppcfg$temporal_filter$method) || !(ppcfg$temporal_filter$method %in% c("fslmaths", "butterworth"))) {
      if (!quiet) message(glue("Invalid method in $postprocess${cfg_name}$temporal_filter. You will be asked for this."))
      gaps <- c(gaps, "postprocess/temporal_filter/method")
      ppcfg$temporal_filter$method <- NULL
    }
  }

  # validate spatial smoothing
  if (is.null(ppcfg$spatial_smooth$enable)) gaps <- c(gaps, "postprocess/spatial_smooth/enable")
  if ("spatial_smooth" %in% names(ppcfg) && isTRUE(ppcfg$spatial_smooth$enable)) {
    if (!checkmate::test_number(ppcfg$spatial_smooth$fwhm_mm, lower = 0.1)) {
      if (!quiet) message(glue("Missing fwhm_mm in $postprocess${cfg_name}$spatial_smooth. You will be asked for this."))
      gaps <- c(gaps, "postprocess/spatial_smooth/fwhm_mm")
      ppcfg$spatial_smooth$fwhm_mm <- NULL
    }
    if (!checkmate::test_string(ppcfg$spatial_smooth$prefix)) {
      if (!quiet) message("No valid prefix found for $postprocess${cfg_name}$spatial_smooth. Defaulting to 's'")
      ppcfg$spatial_smooth$prefix <- "s"
    }
  }

  # validate intensity normalization
  if (is.null(ppcfg$intensity_normalize$enable)) gaps <- c(gaps, "postprocess/intensity_normalize/enable")
  if ("intensity_normalize" %in% names(ppcfg) && isTRUE(ppcfg$intensity_normalize$enable)) {
    if (!checkmate::test_number(ppcfg$intensity_normalize$global_median, lower = 0.1)) {
      if (!quiet) message(glue("Invalid global_median in $postprocess${cfg_name}$intensity_normalize. You will be asked for this."))
      gaps <- c(gaps, "postprocess/intensity_normalize/global_median")
      ppcfg$intensity_normalize$global_median <- NULL
    }
    if (!checkmate::test_string(ppcfg$intensity_normalize$prefix)) {
      if (!quiet) message(glue("No valid prefix found for $postprocess${cfg_name}$intensity_normalize. Defaulting to 'n'"))
      ppcfg$intensity_normalize$prefix <- "n"
    }
  }

  # validate confound calculation
  if (is.null(ppcfg$confound_calculate$enable)) gaps <- c(gaps, "postprocess/confound_calculate/enable")
  if ("confound_calculate" %in% names(ppcfg) && isTRUE(ppcfg$confound_calculate$enable)) {
    if (!checkmate::test_flag(ppcfg$confound_calculate$demean)) {
      if (!quiet) message(glue("Invalid demean field in $postprocess${cfg_name}$confound_calculate. You will be asked for this."))
      gaps <- c(gaps, "postprocess/confound_calculate/demean")
      ppcfg$confound_calculate$demean <- NULL
    }
    if (!checkmate::test_character(ppcfg$confound_calculate$columns)) {
      if (!quiet) message(glue("Invalid columns field in $postprocess${cfg_name}$confound_calculate"))
      gaps <- c(gaps, "postprocess/confound_calculate/columns")
      ppcfg$confound_calculate$columns <- NULL
    }
    if (!checkmate::test_character(ppcfg$confound_calculate$noproc_columns)) {
      if (!quiet) message(glue("Invalid noproc_columns field in $postprocess${cfg_name}$confound_calculate"))
      gaps <- c(gaps, "postprocess/confound_calculate/noproc_columns")
      ppcfg$confound_calculate$noproc_columns <- NULL
    }
  }

  # validate scrubbing
  if (is.null(ppcfg$scrubbing$enable)) gaps <- c(gaps, "postprocess/scrubbing/enable")
  if ("scrubbing" %in% names(ppcfg) && isTRUE(ppcfg$scrubbing$enable)) {
    if (!checkmate::test_string(ppcfg$scrubbing$expression)) {
      if (!quiet) message(glue("Invalid expression field in $postprocess${cfg_name}$scrubbing"))
      gaps <- c(gaps, "postprocess/scrubbing/expression")
      ppcfg$scrubbing$expression <- NULL
    }
    if (!checkmate::test_flag(ppcfg$scrubbing$add_to_confounds)) {
      if (!quiet) message(glue("Invalid add_to_confounds field in $postprocess${cfg_name}$scrubbing"))
      gaps <- c(gaps, "postprocess/scrubbing/add_to_confounds")
      ppcfg$scrubbing$add_to_confounds <- NULL
    }
    if (!checkmate::test_flag(ppcfg$scrubbing$interpolate)) {
      if (!quiet) message(glue("Invalid interpolate field in $postprocess${cfg_name}$scrubbing"))
      gaps <- c(gaps, "postprocess/scrubbing/interpolate")
      ppcfg$scrubbing$interpolate <- NULL
    }
    if (!checkmate::test_string(ppcfg$scrubbing$interpolate_prefix)) {
      if (!quiet) message(glue("No valid interpolate_prefix found for $postprocess${cfg_name}$scrubbing. Defaulting to 'i'"))
      ppcfg$scrubbing$interpolate_prefix <- "i"
    }
    if (!checkmate::test_flag(ppcfg$scrubbing$apply)) {
      if (!quiet) message(glue("Invalid apply field in $postprocess${cfg_name}$scrubbing"))
      gaps <- c(gaps, "postprocess/scrubbing/apply")
      ppcfg$scrubbing$apply <- NULL
    }
    if (!checkmate::test_string(ppcfg$scrubbing$prefix)) {
      if (!quiet) message(glue("No valid prefix found for $postprocess${cfg_name}$scrubbing. Defaulting to 'x'"))
      ppcfg$scrubbing$prefix <- "x"
    }
  }

  if (is.null(ppcfg$confound_regression$enable)) gaps <- c(gaps, "postprocess/confound_regression/enable")
  if ("confound_regression" %in% names(ppcfg) && isTRUE(ppcfg$confound_regression$enable)) {
    if (!checkmate::test_character(ppcfg$confound_regression$columns)) {
      if (!quiet) message(glue("Invalid columns field in $postprocess${cfg_name}$confound_regression"))
      gaps <- c(gaps, "postprocess/confound_regression/columns")
      ppcfg$confound_regression$columns <- NULL
    }
    if (!checkmate::test_character(ppcfg$confound_regression$noproc_columns)) {
      if (!quiet) message(glue("Invalid noproc_columns field in $postprocess${cfg_name}$confound_regression."))
      gaps <- c(gaps, "postprocess/confound_regression/noproc_columns")
      ppcfg$confound_regression$noproc_columns <- NULL
    }
    if (!checkmate::test_string(ppcfg$confound_regression$prefix)) {
      if (!quiet) message(glue("No valid prefix found for $postprocess${cfg_name}$confound_regression. Defaulting to 'r'"))
      ppcfg$confound_regression$prefix <- "r"
    }
  }

  # validate apply_mask
  if (is.null(ppcfg$apply_mask$enable)) gaps <- c(gaps, "postprocess/apply_mask/enable")
  if ("apply_mask" %in% names(ppcfg) && isTRUE(ppcfg$apply_mask$enable)) {
    if (!checkmate::test_string(ppcfg$apply_mask$mask_file, null.ok = TRUE, na.ok = TRUE) || (!ppcfg$apply_mask$mask_file[1L] == "template" && !checkmate::test_file_exists(ppcfg$apply_mask$mask_file))) {
      if (!quiet) message(glue("Invalid mask_file in $postprocess${cfg_name}$apply_mask. You will be asked for this."))
      gaps <- c(gaps, "postprocess/apply_mask/mask_file")
      ppcfg$apply_mask$mask_file <- NULL
    }
    if (!checkmate::test_string(ppcfg$apply_mask$prefix)) {
      if (!quiet) message(glue("No valid prefix found for $postprocess${cfg_name}$apply_mask. Defaulting to 'm'"))
      ppcfg$apply_mask$prefix <- "m"
    }
  }

  # validate AROMA application
  if (is.null(ppcfg$apply_aroma$enable)) gaps <- c(gaps, "postprocess/apply_aroma/enable")
  if ("apply_aroma" %in% names(ppcfg) && isTRUE(ppcfg$apply_aroma$enable)) {
    if (!checkmate::test_flag(ppcfg$apply_aroma$nonaggressive)) {
      if (!quiet) message(glue("Invalid nonaggressive field in $postprocess${cfg_name}$apply_aroma. You will be asked for this."))
      gaps <- c(gaps, "postprocess/apply_aroma/nonaggressive")
      ppcfg$apply_aroma$nonaggressive <- NULL
    }
    if (!checkmate::test_string(ppcfg$apply_aroma$prefix)) {
      if (!quiet) message(glue("No valid prefix found for $postprocess${cfg_name}$apply_aroma. Defaulting to 'a'"))
      ppcfg$apply_aroma$prefix <- "a"
    }
  }

  if (!checkmate::test_flag(ppcfg$force_processing_order)) {
    gaps <- c(gaps, "postprocess/force_processing_order")
    ppcfg$force_processing_order <- FALSE
  }

  if (isTRUE(ppcfg$force_processing_order) && !checkmate::test_character(ppcfg$processing_steps)) {
    gaps <- c(gaps, "postprocess/processing_steps")
    ppcfg$processing_steps <- NULL
  }

  return(list(postprocess = ppcfg, gaps = gaps))
}

validate_postprocess_configs <- function(ppcfg, quiet = FALSE) {
  reserved <- c("enable")
  cfg_names <- setdiff(names(ppcfg), reserved)
  gaps <- c()
  for (nm in cfg_names) {
    # validate stream job settings
    ppcfg <- validate_job_settings(ppcfg, nm)
    if (!is.null(attr(ppcfg, "gaps"))) gaps <- c(gaps, paste0("postprocess/", attr(ppcfg, "gaps")))

    res <- validate_postprocess_config_single(ppcfg[[nm]], nm, quiet)
    ppcfg[[nm]] <- res$postprocess

    # rename gaps by config, like postprocess/ppcfg1/temporal_filter/prefix
    gaps <- c(gaps, paste0("postprocess/", nm, "/", sub("^postprocess/", "", res$gaps)))
  }
  return(list(postprocess = ppcfg, gaps = gaps))
}

validate_extract_configs <- function(ecfg, quiet = FALSE) {
  reserved <- c("enable")
  cfg_names <- setdiff(names(ecfg), reserved)
  gaps <- c()
  for (nm in cfg_names) {
    # validate stream job settings
    ecfg <- validate_job_settings(ecfg, nm)
    if (!is.null(attr(ecfg, "gaps"))) gaps <- c(gaps, paste0("extract_rois/", attr(ecfg, "gaps")))

    res <- validate_extract_config_single(ecfg[[nm]], nm, quiet)
    ecfg[[nm]] <- res$extract_rois

    # rename gaps by config, like extract_rois/ecfg1/correlation
    gaps <- c(gaps, paste0("extract_rois/", nm, "/", sub("^extract_rois/", "", res$gaps)))
  }
  return(list(extract_rois = ecfg, gaps = gaps))
}

validate_extract_config_single <- function(ecfg, cfg_name = NULL, quiet = FALSE) {
  gaps <- c()

  if (!"input_streams" %in% names(ecfg)) {
    gaps <- c(gaps, "extract_rois/input_streams")
  } else if (!checkmate::test_character(ecfg$input_streams)) {
    if (!quiet) message(glue("Invalid input_streams in $extract_rois${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "extract_rois/input_streams")
    ecfg$input_streams <- NULL
  }

  if (!"atlases" %in% names(ecfg)) {
    gaps <- c(gaps, "extract_rois/atlases")
  } else {
    atlas_exists <- sapply(ecfg$atlases, validate_exists)
    if (any(!atlas_exists)) {
      which_bad <- ecfg$atlases[!atlas_exists]
      message(glue("Invalid atlases in $extract_rois${cfg_name}. You will be asked for these: {paste(which_bad, collapse=', ')}"))
      gaps <- c(gaps, "extract_rois/atlases") # would be nice to have more fine-grained control
    }
  }

  if (!"roi_reduce" %in% names(ecfg)) {
    gaps <- c(gaps, "extract_rois/roi_reduce")
  } else if (!checkmate::test_string(ecfg$roi_reduce) ||
    !checkmate::test_subset(ecfg$roi_reduce, c("mean", "median", "pca", "huber"))) {

    message(glue("Invalid roir_reduce method in $extract_rois${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "extract_rois/roi_reduce")
  }

  if (!"rtoz" %in% names(ecfg)) {
    gaps <- c(gaps, "extract_rois/rtoz")
  } else if (!checkmate::test_flag(ecfg$rtoz)) {
    message(glue("Invalid rtoz in $extract_rois${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "extract_rois/rtoz")
  }

  if (!"min_vox_per_roi" %in% names(ecfg)) {
    gaps <- c(gaps, "extract_rois/min_vox_per_roi")
  } else if (!checkmate::test_integerish(ecfg$min_vox_per_roi, len=1L, lower=1L)) {
    message(glue("Invalid min_vox_per_roi in $extract_rois${cfg_name}. You will be asked for this."))
    gaps <- c(gaps, "extract_rois/min_vox_per_roi")
  }

  return(list(extract_rois = ecfg, gaps = gaps))

}