
#' Run the processing pipeline
#' @param scfg A list containing the study configuration.
#' @param steps Character vector of pipeline steps to execute (or `"all"` to run all steps).
#'   Options are c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess").
#'   If `NULL`, the user will be prompted for which steps to run.
#' @param debug A logical value indicating whether to run in debug mode (verbose output for debugging, no true processing).
#' @param force A logical value indicating whether to force the execution of all steps, regardless of their current status.
#' @param subject_filter Optional character vector or data.frame specifying which
#'   subjects (and optionally sessions) to process. When a data.frame is
#'   provided, it must contain a `sub_id` column and may include a `ses_id`
#'   column to filter on specific subject/session combinations.
#' @param postprocess_streams Optional character vector specifying which postprocessing streams should be run. If
#'   `"postprocess"`` is included in `steps`, then this setting lets the user choose streams. If NULL, all postprocess
#'   streams will be run.
#' 
#' @return A logical value indicating whether the processing pipeline was successfully run.
#' @export
#' @examples
#'   \dontrun{
#'     # Assuming you have a valid study configuration list named `study_config`
#'     run_project(study_config, prompt = TRUE, force = FALSE)
#'   }
#' @importFrom glue glue
#' @importFrom checkmate assert_list assert_flag assert_directory_exists
#' @importFrom lgr get_logger_glue
run_project <- function(scfg, steps = NULL, subject_filter = NULL, postprocess_streams = NULL,
    debug = FALSE, force = FALSE) {

  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_character(steps, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_character(subject_filter, any.missing = FALSE, null.ok = TRUE),
    checkmate::check_data_frame(subject_filter, null.ok = TRUE)
  )
  checkmate::assert_character(postprocess_streams, null.ok = TRUE)
  checkmate::assert_subset(
    postprocess_streams,
    choices = c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess"),
    empty.ok = TRUE
  )
  checkmate::assert_flag(debug)
  checkmate::assert_flag(force)
  
  if (is.null(scfg$metadata$project_name)) stop("Cannot run a nameless project. Have you run setup_project() yet?")
  if (is.null(scfg$metadata$project_directory)) stop("Cannot run a project lacking a project directory. Have you run setup_project() yet?")

  all_streams <- get_postprocess_stream_names(scfg) # vector of potential postprocessing streams

  cat(glue("
    \nRunning processing pipeline for: {scfg$metadata$project_name}
      Project directory:   {pretty_arg(scfg$metadata$project_directory)}
      DICOM directory:     {pretty_arg(scfg$metadata$dicom_directory)}
      BIDS directory:      {pretty_arg(scfg$metadata$bids_directory)}
      fmriprep directory:  {pretty_arg(scfg$metadata$fmriprep_directory)}\n
      "))

  # by passing steps, user is asking for unattended execution
  if (!is.null(steps)) prompt <- FALSE

  if (isFALSE(prompt)) {
    if ("bids_conversion" %in% steps) {
      if (!isTRUE(scfg$bids_conversion$enable)) stop("bids_conversion was requested, but it is disabled in the configuration.")
      if (is.null(scfg$bids_conversion$sub_regex)) stop("Cannot run BIDS conversion without a subject regex.")
      if (is.null(scfg$bids_conversion$ses_regex)) stop("Cannot run BIDS conversion without a session regex.")
      if (is.null(scfg$compute_environment$heudiconv_container)) stop("Cannot run BIDS conversion without a heudiconv container.")
    }

    if ("mriqc" %in% steps) {
      if (!isTRUE(scfg$mriqc$enable)) stop("mriqc was requested, but it is disabled in the configuration.")
      if (!validate_exists(scfg$compute_environment$mriqc_container)) {
        stop("Cannot run MRIQC without a valid MRIQC container.")
      }
    }

    if ("fmriprep" %in% steps) {
      if (!isTRUE(scfg$fmriprep$enable)) stop("fmriprep was requested, but it is disabled in the configuration.")
      if (!validate_exists(scfg$compute_environment$fmriprep_container)) {
        stop("Cannot run fmriprep without a valid fmriprep container.")
      }
    }

    if ("aroma" %in% steps) {
      if (!isTRUE(scfg$aroma$enable)) stop("aroma was requested in steps, but it is disabled in your configuration. Use edit_project to fix this.")
      if (!validate_exists(scfg$compute_environment$aroma_container)) {
        stop("Cannot run AROMA without a valid AROMA container.")
      }
    }

    if ("postprocess" %in% steps) {
      if (!isTRUE(scfg$postprocess$enable)) stop("postprocess was requested, but it is disabled in the configuration.")
      if (length(all_streams) == 0L) stop("Cannot run postprocessing without at least one postprocess configuration.")
      if (is.null(postprocess_streams)) postprocess_streams <- all_streams # run all streams if no specifics were requested
    }

    nm <- steps
    steps <- rep(TRUE, length(steps))
    names(steps) <- nm

    # scfg$log_level <- "INFO" # how much detail to park in logs
    scfg$debug <- debug # pass forward debug flag from arguments
    scfg$force <- force # pass forward force flag from arguments
  } else {
    steps <- c()
    cat("\nPlease select which steps to run:\n")
    steps["bids_conversion"] <- ifelse(isTRUE(scfg$bids_conversion$enable) && !is.null(scfg$compute_environment$heudiconv_container), prompt_input(instruct = "Run BIDS conversion?", type = "flag"), FALSE)
    steps["mriqc"] <- ifelse(isTRUE(scfg$mriqc$enable) && !is.null(scfg$compute_environment$mriqc_container), prompt_input(instruct = "Run MRIQC?", type = "flag"), FALSE)
    steps["fmriprep"] <- ifelse(isTRUE(scfg$fmriprep$enable) && !is.null(scfg$compute_environment$fmriprep_container), prompt_input(instruct = "Run fmriprep?", type = "flag"), FALSE)
    steps["aroma"] <- ifelse(isTRUE(scfg$aroma$enable) && !is.null(scfg$compute_environment$aroma_container), prompt_input(instruct = "Run ICA-AROMA?", type = "flag"), FALSE)

    steps["postprocess"] <- ifelse(isTRUE(scfg$postprocess$enable) && length(all_streams) > 0L,
      prompt_input(instruct = "Run postprocessing?", type = "flag"), FALSE
    )

    if (isTRUE(steps["postprocess"])) {
      if (length(all_streams) == 1L) {
        postprocess_streams <- all_streams # if we have only one stream, run it
      } else {
        postprocess_streams <- select.list(
          all_streams,
          multiple = TRUE,
          title = "Which postprocessing streams should be run? Press ENTER to select all."
        )
        if (length(postprocess_streams) == 0L) postprocess_streams <- all_streams # if user presses enter, run all
      }
    }

    # check whether to run in debug mode
    scfg$debug <- prompt_input(instruct = "Run pipeline in debug mode? This will echo commands to logs, but not run them.", type = "flag")
    scfg$force <- prompt_input(instruct = "Force (re-run) each processing step, even if it appears to be complete?", type = "flag")

    # not currently used and would need to propagate the choice down to sbatch scripts through and environment variable (log_message)
    # scfg$log_level <- prompt_input(
    #   instruct = "What level of detail would you like in logs? Options are INFO, DEBUG, ERROR.",
    #   type = "character", among=c("INFO", "ERROR", "DEBUG")
    # )
  }

  # look for subject directories in the DICOM directory
  # empty default data.frame for dicom directories
  subject_dicom_dirs <- data.frame(
    sub_id = character(), ses_id = character(),
    dicom_sub_dir = character(), dicom_ses_dir = character(), stringsAsFactors = FALSE
  )

  if (isTRUE(steps["bids_conversion"])) {
    subject_dicom_dirs <- get_subject_dirs(scfg$metadata$dicom_directory, sub_regex = scfg$bids_conversion$sub_regex, ses_regex = scfg$bids_conversion$ses_regex, full.names = TRUE)

    if (nrow(subject_dicom_dirs) == 0L) {
      warning(glue("Cannot find any valid subject folders inside the DICOM directory: {scfg$metadata$dicom_directory}"))
    } else {
      # add DICOM prefix
      names(subject_dicom_dirs) <- sub("(sub|ses)_dir", "dicom_\\1_dir", names(subject_dicom_dirs))
    }
  }

  # look for all existing subject BIDS directories
  subject_bids_dirs <- get_subject_dirs(scfg$metadata$bids_directory, sub_regex = "^sub-.+", ses_regex = "^ses-.+", sub_id_match = "sub-(.*)", ses_id_match = "ses-(.*)", full.names = TRUE)
  names(subject_bids_dirs) <- sub("(sub|ses)_dir", "bids_\\1_dir", names(subject_bids_dirs))

  subject_dirs <- merge(subject_dicom_dirs, subject_bids_dirs, by = c("sub_id", "ses_id"), all = TRUE)

  if (!is.null(subject_filter)) {
    if (is.data.frame(subject_filter)) {
      checkmate::assert_names(names(subject_filter), must.include = "sub_id")
      by_cols <- intersect(c("sub_id", "ses_id"), names(subject_filter))
      subject_dirs <- merge(subject_dirs, subject_filter[, by_cols, drop = FALSE], by = by_cols)
    } else {
      subject_dirs <- subject_dirs[subject_dirs$sub_id %in% subject_filter, , drop = FALSE]
    }

    if (nrow(subject_dirs) == 0L) {
      stop("No subject directories match the provided subject_filter")
    }

    msg_df <- unique(subject_dirs[, c("sub_id", "ses_id")])
    msg_lines <- apply(msg_df, 1, function(rr) {
      if (!is.na(rr["ses_id"])) {
        glue("  sub-{rr['sub_id']} ses-{rr['ses_id']}")
      } else {
        glue("  sub-{rr['sub_id']}")
      }
    })
    cat("Processing the following subjects:\n", paste(msg_lines, collapse = "\n"), "\n")
  }

  if (nrow(subject_dirs) == 0L) {
    stop(glue("Cannot find any valid subject folders in bids directory: {scfg$metadata$bids_directory}"))
  } else {
    # split data.frame by subject (some steps are subject-level, some are session-level)
    subject_dirs <- split(subject_dirs, subject_dirs$sub_id)

    for (ss in seq_along(subject_dirs)) {
      process_subject(scfg, subject_dirs[[ss]], steps, postprocess_streams = postprocess_streams)
    }
  }
}
