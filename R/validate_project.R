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
    "metadata/fmriprep_directory", "metadata/scratch_directory", "metadata/templateflow_home"
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

  # helper subfunction to convert NULL, empty list, or "" to character(0) for conformity
  validate_char <- function(arg) {
    if (is.null(arg) || identical(arg, list()) || length(arg) == 0L || arg[1L] == "") {
      arg <- character(0)
    }
    return(arg)
  }

  # check memgb, nhours, ncores, cli_options, and sched_args for all jobs
  validate_job_settings <- function(job_name) {
    if (!checkmate::test_number(scfg[[job_name]]$memgb, lower=1, upper=1000)) {
      message("Invalid memgb setting in ", job_name, ". We will ask you for a valid value")
      gaps <- c(gaps, paste(job_name, "memgb", sep="/"))
      scfg[[job_name]]$memgb <- NULL
    }

    if (!checkmate::test_number(scfg[[job_name]]$nhours, lower=1, upper=1000)) {
      message("Invalid nhours setting in ", job_name, ". We will ask you for a valid value")
      gaps <- c(gaps, paste(job_name, "nhours", sep="/"))
      scfg[[job_name]]$nhours <- NULL
    }

    if (!checkmate::test_number(scfg[[job_name]]$ncores, lower = 1, upper = 250)) {
      message("Invalid ncores setting in ", job_name, ". We will ask you for a valid value")
      gaps <- c(gaps, paste(job_name, "ncores", sep = "/"))
      scfg[[job_name]]$ncores <- NULL
    }
    
    # conform cli_options to character(0) on empty
    scfg[[job_name]]$cli_options <- validate_char(scfg[[job_name]]$cli_options)
    scfg[[job_name]]$sched_args <- validate_char(scfg[[job_name]]$sched_args)
    
  }

  # validate job settings
  for (job in c("bids_conversion", "fmriprep", "mriqc", "aroma", "postprocess")) {
    validate_job_settings(job)
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

  # Postprocessing settings validation (function in setup_postproc.R)
  postprocess_result <- validate_postprocess_config(scfg$postprocess, quiet)
  scfg$postprocess <- postprocess_result$postprocess
  gaps <- c(gaps, postprocess_result$gaps)

  attr(scfg, "gaps") <- gaps

  return(scfg)
}


#' Validate postprocess configuration block
#' @param ppcfg a postprocess configuration block
#' @param quiet a flag indicating whether to suppress messages
#' @keywords internal
validate_postprocess_config <- function(ppcfg, quiet = FALSE) {
  gaps <- c()

  # postprocess/input_regex
  if (!"input_regex" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/input_regex")
  } else if (!checkmate::test_string(ppcfg$input_regex)) {
    if (!quiet) message("Invalid input_regex in $postprocess. You will be asked for this.")
    gaps <- c(gaps, "postprocess/input_regex")
    ppcfg$input_regex <- NULL
  }

  # postprocess/bids_desc
  if (!"bids_desc" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/bids_desc")
  } else if (!checkmate::test_string(ppcfg$bids_desc)) {
    if (!quiet) message("Invalid bids_desc in $postprocess. You will be asked for this.")
    gaps <- c(gaps, "postprocess/bids_desc")
    ppcfg$bids_desc <- NULL
  }

  # postprocess/keep_intermediates
  if (!"keep_intermediates" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/keep_intermediates")
  } else if (!checkmate::test_flag(ppcfg$keep_intermediates)) {
    if (!quiet) message("Invalid keep_intermediates in $postprocess. You will be asked for this.")
    gaps <- c(gaps, "postprocess/keep_intermediates")
    ppcfg$keep_intermediates <- NULL
  }

  # postprocess/overwrite
  if (!"overwrite" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/overwrite")
  } else if (!checkmate::test_flag(ppcfg$overwrite)) {
    if (!quiet) message("Invalid overwrite in $postprocess. You will be asked for this.")
    gaps <- c(gaps, "postprocess/overwrite")
    ppcfg$overwrite <- NULL
  }

  # postprocess/tr
  if (!"tr" %in% names(ppcfg)) {
    gaps <- c(gaps, "postprocess/tr")
  } else if (!checkmate::test_number(ppcfg$tr, lower = 0.01, upper = 100)) {
    if (!quiet) message("Invalid tr in $postprocess. You will be asked for this.")
    gaps <- c(gaps, "postprocess/tr")
    ppcfg$tr <- NULL
  }

  # validate temporal filtering
  if ("temporal_filter" %in% names(ppcfg)) {
    if (!checkmate::test_number(ppcfg$temporal_filter$low_pass_hz, lower=0)) {
      if (!quiet) message("Missing low_pass_hz in $postprocess. You will be asked for this.")
      gaps <- c(gaps, "postprocess/temporal_filter/low_pass_hz")
      ppcfg$temporal_filter$low_pass_hz <- NULL
    }
    if (!checkmate::test_number(ppcfg$temporal_filter$high_pass_hz, lower = 0)) {
      if (!quiet) message("Missing high_pass_hz in $postprocess. You will be asked for this.")
      gaps <- c(gaps, "postprocess/temporal_filter/high_pass_hz")
      ppcfg$temporal_filter$high_pass_hz <- NULL
    }
    if (!is.null(ppcfg$temporal_filter$low_pass_hz) && !is.null(ppcfg$temporal_filter$high_pass_hz) && 
        ppcfg$temporal_filter$high_pass_hz < ppcfg$temporal_filter$low_pass_hz) {
      if (!quiet) message("high_pass_hz is greater than low_pass_hz $postprocess$temporal_filter. You will be asked to respecify valid values.")
      gaps <- unique(c(gaps, "postprocess/temporal_filter/low_pass_hz", "postprocess/temporal_filter/high_pass_hz"))
      ppcfg$temporal_filter$low_pass_hz <- NULL
      ppcfg$temporal_filter$high_pass_hz <- NULL
    }
    if (!checkmate::test_string(ppcfg$temporal_filter$prefix)) {
      if (!quiet) message("No valid prefix found for $postprocess$temporal_filter")
      gaps <- c(gaps, "postprocess/temporal_filter/prefix")
      ppcfg$temporal_filter$prefix <- NULL
    }
  }

  # validate spatial smoothing
  if ("spatial_smooth" %in% names(ppcfg)) {
    if (!checkmate::test_number(ppcfg$spatial_smooth$fwhm_mm, lower = 0.1)) {
      if (!quiet) message("Missing fwhm_mm in $postprocess$spatial_smooth. You will be asked for this.")
      gaps <- c(gaps, "postprocess/spatial_smooth/fwhm_mm")
      ppcfg$spatial_smooth$fwhm_mm <- NULL
    }
    if (!checkmate::test_string(ppcfg$spatial_smooth$prefix)) {
      if (!quiet) message("No valid prefix found for $postprocess$spatial_smooth")
      gaps <- c(gaps, "postprocess/spatial_smooth/prefix")
      ppcfg$spatial_smooth$prefix <- NULL
    }
  }

  # validate intensity normalization
  if ("intensity_normalize" %in% names(ppcfg)) {
    if (!checkmate::test_number(ppcfg$intensity_normalize$global_median, lower = 0.1)) {
      if (!quiet) message("Invalid global_median in $postprocess$intensity_normalize. You will be asked for this.")
      gaps <- c(gaps, "postprocess/intensity_normalize/global_median")
      ppcfg$intensity_normalize$global_median <- NULL
    }
    if (!checkmate::test_string(ppcfg$intensity_normalize$prefix)) {
      if (!quiet) message("No valid prefix found for $postprocess$intensity_normalize")
      gaps <- c(gaps, "postprocess/intensity_normalize/prefix")
      ppcfg$intensity_normalize$prefix <- NULL
    }
  }

  # validate confound calculation
  if ("confound_calculate" %in% names(ppcfg)) {
    if (!checkmate::test_flag(ppcfg$confound_calculate$demean)) {
      if (!quiet) message("Invalid demean field in $postprocess$confound_calculate. You will be asked for this.")
      gaps <- c(gaps, "postprocess/confound_calculate/demean")
      ppcfg$confound_calculate$demean <- NULL
    }
    if (!checkmate::test_string(ppcfg$confound_calculate$output_file)) {
      if (!quiet) message("Invalid output_file field in $postprocess$confound_calculate")
      gaps <- c(gaps, "postprocess/confound_calculate/output_file")
      ppcfg$confound_calculate$output_file <- NULL
    }
    if (!checkmate::test_character(ppcfg$confound_calculate$columns)) {
      if (!quiet) message("Invalid columns field in $postprocess$confound_calculate")
      gaps <- c(gaps, "postprocess/confound_calculate/columns")
      ppcfg$confound_calculate$columns <- NULL
    }
    if (!checkmate::test_character(ppcfg$confound_calculate$noproc_columns)) {
      if (!quiet) message("Invalid noproc_columns field in $postprocess$confound_calculate")
      gaps <- c(gaps, "postprocess/confound_calculate/noproc_columns")
      ppcfg$confound_calculate$noproc_columns <- NULL
    }
  }

  # validate AROMA application
  if ("apply_aroma" %in% names(ppcfg)) {
    if (!checkmate::test_flag(ppcfg$apply_aroma$nonaggressive)) {
      if (!quiet) message("Invalid nonaggressive field in $postprocess$apply_aroma. You will be asked for this.")
      gaps <- c(gaps, "postprocess/apply_aroma/nonaggressive")
      ppcfg$apply_aroma$nonaggressive <- NULL
    }
    if (!checkmate::test_string(ppcfg$apply_aroma$prefix)) {
      if (!quiet) message("No valid prefix found for $postprocess$apply_aroma")
      gaps <- c(gaps, "postprocess/apply_aroma/prefix")
      ppcfg$apply_aroma$prefix <- NULL
    }
  }

  return(list(postprocess = ppcfg, gaps = gaps))
}
