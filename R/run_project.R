
#' Run the processing pipeline
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @param steps Character vector of pipeline steps to execute (or `"all"` to run all steps).
#'   Options are c("flywheel_sync", "bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess", "extract_rois").
#'   If `NULL`, the user will be prompted for which steps to run.
#' @param debug A logical value indicating whether to run in debug mode (verbose output for debugging, no true processing).
#' @param force A logical value indicating whether to force the execution of all steps, regardless of their current status.
#' @param subject_filter Optional character vector or data.frame specifying which
#'   subjects (and optionally sessions) to process. When `NULL` and run
#'   interactively, the user will be prompted to enter space-separated subject
#'   IDs (press ENTER to process all subjects). When a data.frame is provided, it
#'   must contain a `sub_id` column and may include a `ses_id` column to filter
#'   on specific subject/session combinations.
#' @param postprocess_streams Optional character vector specifying which postprocessing streams should be run. If
#'   `"postprocess"`` is included in `steps`, then this setting lets the user choose streams. If NULL, all postprocess
#'   streams will be run.
#' @param extract_streams Optional character vector specifying which ROI extraction streams should be run. If
#'   `"extract_rois"`` is included in `steps`, then this setting lets the user choose streams. If NULL, all extraction
#'   streams will be run.
#' 
#' @return A logical value indicating whether the processing pipeline was successfully run.
#' @export
#' @examples
#'   \dontrun{
#'     # Assuming you have a valid project configuration list named `study_config`
#'     run_project(study_config, prompt = TRUE, force = FALSE)
#'   }
#' @importFrom glue glue
#' @importFrom checkmate assert_list assert_flag
#' @importFrom lgr get_logger_glue
run_project <- function(scfg, steps = NULL, subject_filter = NULL, postprocess_streams = NULL, 
  extract_streams = NULL, debug = FALSE, force = FALSE) {

  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_character(steps, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_character(subject_filter, any.missing = FALSE, null.ok = TRUE),
    checkmate::check_data_frame(subject_filter, null.ok = TRUE)
  )
  checkmate::assert_character(postprocess_streams, null.ok = TRUE)
  checkmate::assert_character(extract_streams, null.ok = TRUE)
  
  checkmate::assert_flag(debug)
  checkmate::assert_flag(force)
  
  if (is.null(scfg$metadata$project_name)) stop("Cannot run a nameless project. Have you run setup_project() yet?")
  if (is.null(scfg$metadata$project_directory)) stop("Cannot run a project lacking a project directory. Have you run setup_project() yet?")

  all_pp_streams <- get_postprocess_stream_names(scfg) # vector of potential postprocessing streams
  all_ex_streams <- get_extract_stream_names(scfg) # vector of potential extraction streams

  checkmate::assert_subset(postprocess_streams, choices = all_pp_streams, empty.ok = TRUE)
  checkmate::assert_subset(extract_streams, choices = all_ex_streams, empty.ok = TRUE)
  
  scfg <- setup_project_directories(scfg)

  cat(glue("
    \nRunning processing pipeline for: {scfg$metadata$project_name}
      Project directory:   {pretty_arg(scfg$metadata$project_directory)}
      DICOM directory:     {pretty_arg(scfg$metadata$dicom_directory)}
      BIDS directory:      {pretty_arg(scfg$metadata$bids_directory)}
      fmriprep directory:  {pretty_arg(scfg$metadata$fmriprep_directory)}
      Postprocess directory: {pretty_arg(scfg$metadata$postproc_directory)}\n
      "))

  # by passing steps, user is asking for unattended execution
  prompt <- is.null(steps)

  if (isFALSE(prompt)) {   
    if ("flywheel_sync" %in% steps) {
      if (!isTRUE(scfg$flywheel_sync$enable)) stop("flywheel_sync was requested, but it is disabled in the configuration.")
      if (is.null(scfg$flywheel_sync$source_url)) stop("Cannot run flywheel_sync without a source_url.")
      if (is.null(scfg$metadata$flywheel_sync_directory)) stop("Cannot run flywheel_sync without a flywheel_sync_directory.")
      if (!checkmate::test_file_exists(scfg$compute_environment$flywheel)) stop("Cannot run flywheel_sync without a valid location of the fw command.")
    }

    if ("bids_conversion" %in% steps) {
      if (!isTRUE(scfg$bids_conversion$enable)) stop("bids_conversion was requested, but it is disabled in the configuration.")
      if (is.null(scfg$bids_conversion$sub_regex)) stop("Cannot run BIDS conversion without a subject regex.")
      if (is.null(scfg$bids_conversion$ses_regex)) stop("Cannot run BIDS conversion without a session regex.")
    }

    if ("mriqc" %in% steps && !isTRUE(scfg$mriqc$enable)) stop("mriqc was requested, but it is disabled in the configuration.")

    if ("fmriprep" %in% steps && !isTRUE(scfg$fmriprep$enable)) stop("fmriprep was requested, but it is disabled in the configuration.")

    if ("aroma" %in% steps && !isTRUE(scfg$aroma$enable)) stop("aroma was requested in steps, but it is disabled in your configuration. Use edit_project to fix this.")
    
    if ("postprocess" %in% steps) {
      if (!isTRUE(scfg$postprocess$enable)) stop("postprocess was requested, but it is disabled in the configuration.")
      if (length(all_pp_streams) == 0L) stop("Cannot run postprocessing without at least one postprocess configuration.")
      if (is.null(postprocess_streams)) postprocess_streams <- all_pp_streams # run all streams if no specifics were requested
    }

    if ("extract_rois" %in% steps) {
      if (!isTRUE(scfg$extract_rois$enable)) stop("extract_rois was requested, but it is disabled in the configuration.")
      if (length(all_ex_streams) == 0L) stop("Cannot run extraction without at least one extract_rois configuration.")
      if (is.null(extract_streams)) extract_streams <- all_ex_streams # run all streams if no specifics were requested
    }

    # convert steps to logicals to match downstream expectations (e.g., in process_subject)
    user_steps <- steps # copy user character vector to populate logical vector
    steps <- rep(FALSE, 7)
    names(steps) <- c("flywheel_sync", "bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess", "extract_rois")
    for (s in user_steps) steps[s] <- TRUE
    
    # scfg$log_level <- "INFO" # how much detail to park in logs
    scfg$debug <- debug # pass forward debug flag from arguments
    scfg$force <- force # pass forward force flag from arguments
  } else {
    ids <- prompt_input(
      instruct = "Enter subject IDs to process, separated by spaces. Press enter to process all subjects.",
      type = "character", split = " ", required = FALSE
    )
    if (!is.na(ids[1])) subject_filter <- ids

    steps <- c()
    cat("\nPlease select which steps to run:\n")
    steps["flywheel_sync"] <- ifelse(isTRUE(scfg$flywheel_sync$enable), prompt_input(instruct = "Run Flywheel sync?", type = "flag"), FALSE)
    steps["bids_conversion"] <- ifelse(isTRUE(scfg$bids_conversion$enable), prompt_input(instruct = "Run BIDS conversion?", type = "flag"), FALSE)
    steps["mriqc"] <- ifelse(isTRUE(scfg$mriqc$enable), prompt_input(instruct = "Run MRIQC?", type = "flag"), FALSE)
    steps["fmriprep"] <- ifelse(isTRUE(scfg$fmriprep$enable), prompt_input(instruct = "Run fmriprep?", type = "flag"), FALSE)
    steps["aroma"] <- ifelse(isTRUE(scfg$aroma$enable), prompt_input(instruct = "Run ICA-AROMA?", type = "flag"), FALSE)

    steps["postprocess"] <- ifelse(isTRUE(scfg$postprocess$enable) && length(all_pp_streams) > 0L,
      prompt_input(instruct = "Run postprocessing?", type = "flag"), FALSE
    )

    if (isTRUE(steps["postprocess"])) {
      if (length(all_pp_streams) == 1L) {
        postprocess_streams <- all_pp_streams # if we have only one stream, run it
      } else {
        postprocess_streams <- select_list_safe(all_pp_streams, multiple = TRUE,
          title = "Which postprocessing streams should be run? Press ENTER to select all."
        )
        if (length(postprocess_streams) == 0L) postprocess_streams <- all_pp_streams # if user presses enter, run all
      }
    }

    steps["extract_rois"] <- ifelse(isTRUE(scfg$extract_rois$enable) && length(all_ex_streams) > 0L,
      prompt_input(instruct = "Run ROI extraction?", type = "flag"), FALSE
    )

    if (isTRUE(steps["extract_rois"])) {
      if (length(all_ex_streams) == 1L) {
        extract_streams <- all_ex_streams # if we have only one stream, run it
      } else {
        extract_streams <- select_list_safe(all_ex_streams, multiple = TRUE,
          title = "Which extraction streams should be run? Press ENTER to select all."
        )
        if (length(extract_streams) == 0L) extract_streams <- all_ex_streams # if user presses enter, run all
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

  if (isTRUE(scfg$force)) {
    if (!is.null(scfg$bids_conversion)) scfg$bids_conversion$overwrite <- TRUE
  }
  
  if (!any(steps)) stop("No processing steps were requested in run_project.")

  # check that required containers are present for any requested step
  if (steps["bids_conversion"] && !validate_exists(scfg$compute_environment$heudiconv_container)) {
    stop("Cannot run BIDS conversion without a heudiconv container.")
  }

  if (steps["mriqc"] && !validate_exists(scfg$compute_environment$mriqc_container)) {
    stop("Cannot run MRIQC without a valid MRIQC container.")
  }

  if (steps["fmriprep"] && !validate_exists(scfg$compute_environment$fmriprep_container)) {
    stop("Cannot run fmriprep without a valid fmriprep container.")
  }

  if (steps["aroma"] && !validate_exists(scfg$compute_environment$aroma_container)) {
    stop("Cannot run AROMA without a valid AROMA container.")
  }

  if (steps["postprocess"] && !validate_exists(scfg$compute_environment$fsl_container)) {
    stop("Cannot run postprocessing without a valid FSL container.")
  }

  flywheel_id <- NULL
  if (isTRUE(steps["flywheel_sync"])) flywheel_id <- submit_flywheel_sync(scfg)

  # If only sync was requested, don't enter subject-level processing
  if (!any(steps[names(steps) != "flywheel_sync"])) return(invisible(TRUE))

  # Submit fsaverage setup early (used by fmriprep) to avoid race conditions
  fsaverage_id <- NULL
  if (isTRUE(steps["fmriprep"])) fsaverage_id <- submit_fsaverage_setup(scfg)

  # If flywheel sync is requested, defer subject scheduling to a dependent controller job
  # This ensures that downstream steps see all data synched from flywheel (e.g., new subjects)
  if (isTRUE(steps["flywheel_sync"])) {
    snapshot <- list(
      scfg = scfg,
      steps = steps,
      subject_filter = subject_filter,
      postprocess_streams = postprocess_streams,
      extract_streams = extract_streams,
      parent_ids = fsaverage_id
    )
    snap_file <- file.path(scfg$metadata$log_directory, "run_project_snapshot.rds")
    dir.create(dirname(snap_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(snapshot, snap_file)

    # Lightweight controller job to schedule subjects after flywheel completes
    scfg$submit_subjects <- list(nhours = 0.5, memgb = 4, ncores = 1)
    sched_args <- get_job_sched_args(
      scfg, job_name = "submit_subjects",
      stdout_log = glue::glue("{scfg$metadata$log_directory}/submit_subjects_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out"),
      stderr_log = glue::glue("{scfg$metadata$log_directory}/submit_subjects_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err")
    )
    sched_script <- get_job_script(scfg, "submit_subjects", subject_suffix = FALSE)
    env_variables <- c(
      pkg_dir = find.package(package = "BrainGnomes"),
      R_HOME = R.home(),
      snapshot_rds = snap_file
    )
    cluster_job_submit(
      sched_script,
      scheduler = scfg$compute_environment$scheduler,
      sched_args = sched_args,
      env_variables = env_variables,
      wait_jobs = flywheel_id,
      echo = FALSE
    )
    return(invisible(TRUE))
  }

  # No flywheel sync: schedule subjects immediately
  submit_subjects(
    scfg = scfg, steps = steps, subject_filter = subject_filter,
    postprocess_streams = postprocess_streams, extract_streams = extract_streams, parent_ids = fsaverage_id
  )
  return(invisible(TRUE))
}

#' Schedule subject-level processing
#' @param scfg A bg_project_cfg object
#' @param steps Named logical vector of steps
#' @param subject_filter Optional subject/session filter (character or data.frame)
#' @param postprocess_streams Optional character vector of postprocess streams
#' @param extract_streams Optional character vector of extraction streams
#' @param parent_ids Optional character vector of job IDs to depend on
#' @details 
#'   This function is not meant to be called by users! Instead, it is called internally
#'   after flywheel sync completes.
#' @keywords internal
submit_subjects <- function(scfg, steps, subject_filter = NULL, postprocess_streams = NULL,
  extract_streams = NULL, parent_ids = NULL) {

  # look for subject directories in the DICOM directory
  subject_dicom_dirs <- data.frame(
    sub_id = character(), ses_id = character(),
    dicom_sub_dir = character(), dicom_ses_dir = character(), stringsAsFactors = FALSE
  )

  if (isTRUE(steps["bids_conversion"])) {
    subject_dicom_dirs <- get_subject_dirs(
      scfg$metadata$dicom_directory,
      sub_regex = scfg$bids_conversion$sub_regex,
      sub_id_match = scfg$bids_conversion$sub_id_match,
      ses_regex = scfg$bids_conversion$ses_regex,
      ses_id_match = scfg$bids_conversion$ses_id_match,
      full.names = TRUE
    )

    if (nrow(subject_dicom_dirs) == 0L) {
      warning(glue("Cannot find any valid subject folders inside the DICOM directory: {scfg$metadata$dicom_directory}"))
    } else {
      names(subject_dicom_dirs) <- sub("(sub|ses)_dir", "dicom_\\1_dir", names(subject_dicom_dirs))
    }
  }

  # look for all existing subject BIDS directories
  subject_bids_dirs <- get_subject_dirs(
    scfg$metadata$bids_directory,
    sub_regex = "^sub-.+", ses_regex = "^ses-.+",
    sub_id_match = "sub-(.*)", ses_id_match = "ses-(.*)", full.names = TRUE
  )
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

    if (nrow(subject_dirs) == 0L) stop("No subject directories match the provided subject_filter")

    msg_df <- unique(subject_dirs[, c("sub_id", "ses_id")])
    msg_lines <- apply(msg_df, 1, function(rr) {
      if (!is.na(rr["ses_id"])) glue("  sub-{rr['sub_id']} ses-{rr['ses_id']}") else glue("  sub-{rr['sub_id']}")
    })
    cat("Processing the following subjects:\n", paste(msg_lines, collapse = "\n"), "\n")
  }

  if (nrow(subject_dirs) == 0L) {
    stop(glue("Cannot find any valid subject folders in bids directory: {scfg$metadata$bids_directory}"))
  }

  # split data.frame by subject (some steps are subject-level, some are session-level)
  subject_dirs <- split(subject_dirs, subject_dirs$sub_id)

  for (ss in seq_along(subject_dirs)) {
    process_subject(
      scfg, subject_dirs[[ss]], steps,
      postprocess_streams = postprocess_streams, extract_streams = extract_streams,
      parent_ids = parent_ids
    )
  }

  invisible(TRUE)
}

#' submit Flywheel sync job -- superordinate to subjects
#' @param scfg A bg_project_cfg object
#' @param lg a lgr object
#' @keywords internal
#' @noRd
#' @importFrom checkmate test_true
submit_flywheel_sync <- function(scfg, lg = NULL) {
  checkmate::assert_list(scfg)

  if (is.null(lg)) {
    lg <- lgr::get_logger_glue("flywheel_sync")
    if (!"flywheel" %in% names(lg$appenders)) {
      lg$add_appender(
        lgr::AppenderFile$new(file.path(scfg$metadata$log_directory, "flywheel_sync_log.txt")),
        name = "flywheel"
      )
    }
  }

  sched_args <- get_job_sched_args(scfg, job_name = "flywheel_sync",
    stdout_log = glue::glue("{scfg$metadata$log_directory}/flywheel_sync_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out"),
    stderr_log = glue::glue("{scfg$metadata$log_directory}/flywheel_sync_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err")
  )

  audit_str <- if (test_true(scfg$flywheel_sync$save_audit_logs)) {
    glue("--save-audit-logs {scfg$metadata$log_directory}/flywheel_sync_audit_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.csv")
  } else {
    NULL
  }

  cli_options <- set_cli_options(scfg$flywheel_sync$cli_options, c(
    "--include dicom", "-y",
    glue("--tmp-path '{scfg$metadata$flywheel_temp_directory}'"),
    audit_str
  ), collapse = TRUE)

  cmd <- glue::glue("{scfg$compute_environment$flywheel} sync {cli_options} {scfg$flywheel_sync$source_url} {scfg$metadata$flywheel_sync_directory}")

  job_id <- cluster_job_submit(cmd, scheduler = scfg$compute_environment$scheduler, sched_args = sched_args)

  lg$info("Scheduled flywheel_sync job: {truncate_str(attr(job_id, 'cmd'))}")
  lg$debug("Full command: {attr(job_id, 'cmd')}")

  return(job_id)
}

# helper for avoiding race condition in setting up fsaverage folder in data_fmriprep
# avoid race condition in setting up fsaverage folder: https://github.com/nipreps/fmriprep/issues/3492
submit_fsaverage_setup <- function(scfg) {
  checkmate::assert_directory_exists(scfg$metadata$fmriprep_directory)
  checkmate::assert_file_exists(scfg$compute_environment$fmriprep_container)

  # get resource allocation request
  scfg$fsaverage <- list(nhours = 0.15, memgb = 8, ncores = 1) # fake top-level job to let get_job_sched_args work
  sched_args <- c(
    get_job_sched_args(scfg, "fsaverage"),
    glue::glue("--output={scfg$metadata$log_directory}/cp_fsaverage_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out")
  )

  # copy fsaverage from fmriprep's instance of freesurfer to the output destination
  cmd <- glue::glue("singularity exec --cleanenv --containall -B '{scfg$metadata$fmriprep_directory}' '{scfg$compute_environment$fmriprep_container}' \\
    rsync --mkpath -a /opt/freesurfer/subjects/fsaverage '{scfg$metadata$fmriprep_directory}/sourcedata/freesurfer'")

  job_id <- cluster_job_submit(cmd, scheduler = scfg$compute_environment$scheduler, sched_args = sched_args)

  return(job_id)
}
