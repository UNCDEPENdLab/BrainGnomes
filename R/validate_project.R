# helper to convert NULL, empty list, or "" to character(0) for conformity
validate_char <- function(arg) {
  if (is.null(arg) || identical(arg, list()) || length(arg) == 0L || arg[1L] == "") {
    arg <- character(0)
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

  # conform cli_options to character(0) on empty
  scfg[[job_name]]$cli_options <- validate_char(get_nested_values(scfg, job_name)$cli_options)
  scfg[[job_name]]$sched_args <- validate_char(get_nested_values(scfg, job_name)$sched_args)

  attr(scfg, "gaps") <- gaps
  return(scfg)
}


#' Validate the structure of a study configuration object
#' @param scfg a study configuration file as produced by `load_project` or `setup_project`
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

  required_dirs <- c(
    "metadata/project_directory", "metadata/dicom_directory", "metadata/bids_directory",
    "metadata/fmriprep_directory", "metadata/mriqc_directory", "metadata/log_directory",
    "metadata/scratch_directory", "metadata/templateflow_home"
  )
  for (rr in required_dirs) {
    if (!checkmate::test_directory_exists(get_nested_values(scfg, rr))) {
      message("Config file is missing valid directory for ", rr, ".")
      gaps <- c(gaps, rr)
    }
  }

  required_files <- c("compute_environment/fmriprep_container", "compute_environment/heudiconv_container", "bids_conversion/heuristic_file", "fmriprep/fs_license_file")
  for (rr in required_files) {
    if (!checkmate::test_file_exists(get_nested_values(scfg, rr))) {
      message("Config file is missing valid ", rr, ". You will be asked for this.")
      gaps <- c(gaps, rr)
    }
  }

  # optional files
  if (!is.null(scfg$compute_environment$bids_validator) && !checkmate::test_file_exists(scfg$compute_environment$bids_validator)) {
    message("Cannot find bids_validator at ", scfg$compute_environment$bids_validator, ". You will be asked for this.")
    gaps <- c(gaps, "compute_environment/bids_validator")
    scfg$compute_environment$bids_validator <- NULL
  }

  if (!is.null(scfg$compute_environment$mriqc_container) && !checkmate::test_file_exists(scfg$compute_environment$mriqc_container)) {
    message("Cannot find MRIQC container at ", scfg$compute_environment$mriqc_container, ". You will be asked for this.")
    gaps <- c(gaps, "compute_environment/mriqc_container")
    scfg$compute_environment$mriqc_container <- NULL
  }

  if (!is.null(scfg$compute_environment$aroma_container) && !checkmate::test_file_exists(scfg$compute_environment$aroma_container)) {
    message("Cannot find AROMA container at ", scfg$compute_environment$aroma_container, ". You will be asked for this.")
    gaps <- c(gaps, "compute_environment/aroma_container")
    scfg$compute_environment$aroma_container <- NULL
  }

  if (!checkmate::test_subset(scfg$compute_environment$scheduler, c("slurm", "torque"), empty.ok = FALSE)) {
    message("Invalid scheduler setting. You will be asked for this.")
    gaps <- c(gaps, "compute_environment/scheduler")
    scfg$compute_environment$scheduler <- NULL
  }

  if (isTRUE(scfg$mriqc$enable) && !checkmate::test_file_exists(scfg$compute_environment$mriqc_container)) {
    message("MRIQC is enabled but mriqc_container is missing. You will be asked for this.")
    gaps <- c(gaps, "compute_environment/mriqc_container")
  }

  if (isTRUE(scfg$aroma$enable) && !checkmate::test_file_exists(scfg$compute_environment$aroma_container)) {
    message("AROMA is enabled but aroma_container is missing. You will be asked for this.")
    gaps <- c(gaps, "compute_environment/aroma_container")
  }

  if (isTRUE(scfg$bids_validation$enable) && !checkmate::test_file_exists(scfg$compute_environment$bids_validator)) {
    message("BIDS validation is enabled but bids_validator is missing. You will be asked for this.")
    gaps <- c(gaps, "compute_environment/bids_validator")
  }

  # validate job settings
  for (job in c("bids_conversion", "fmriprep", "mriqc", "aroma")) {
    scfg <- validate_job_settings(scfg, job)
    gaps <- c(gaps, attr(scfg, "gaps"))
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

  # Postprocessing settings validation (function in setup_postproc.R)
  postprocess_result <- validate_postprocess_configs(scfg$postprocess, quiet)
  scfg$postprocess <- postprocess_result$postprocess
  gaps <- c(gaps, postprocess_result$gaps)

  attr(scfg, "gaps") <- gaps

  return(scfg)
}


#' Validate postprocess configuration block
#' @param ppcfg a postprocess configuration block
#' @param quiet a flag indicating whether to suppress messages
#' @keywords internal
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

  if ("scrubbing" %in% names(ppcfg) && isTRUE(ppcfg$scrubbing$enable)) {
    if (!checkmate::test_character(ppcfg$scrubbing$expression)) {
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

  if ("apply_mask" %in% names(ppcfg) && isTRUE(ppcfg$apply_mask$enable)) {
    if (!checkmate::test_string(ppcfg$apply_mask$mask_file, null.ok = TRUE) || !checkmate::test_file_exists(ppcfg$apply_mask$mask_file)) {
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
    gaps <- c(gaps, paste0("postprocess/", attr(ppcfg, "gaps")))

    res <- validate_postprocess_config_single(ppcfg[[nm]], nm, quiet)
    ppcfg[[nm]] <- res$postprocess

    # rename gaps by config, like postprocess/ppcfg1/temporal_filter/prefix
    gaps <- c(gaps, paste0("postprocess/", nm, "/", sub("^postprocess/", "", res$gaps)))
  }
  return(list(postprocess = ppcfg, gaps = gaps))
}
