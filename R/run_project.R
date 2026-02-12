
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
#' @param log_level Character string controlling log verbosity. One of
#'   `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, or `FATAL`.
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
  extract_streams = NULL, debug = FALSE, force = FALSE,
  log_level = c("INFO", "DEBUG", "WARN", "ERROR", "TRACE", "FATAL")) {

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
  valid_log_levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  if (length(log_level) > 1L) log_level <- log_level[1L]
  log_level <- toupper(log_level)
  log_level <- match.arg(log_level, valid_log_levels)
  
  if (is.null(scfg$metadata$project_name)) stop("Cannot run a nameless project. Have you run setup_project() yet?")
  if (is.null(scfg$metadata$project_directory)) stop("Cannot run a project lacking a project directory. Have you run setup_project() yet?")

  all_pp_streams <- get_postprocess_stream_names(scfg) # vector of potential postprocessing streams
  all_ex_streams <- get_extract_stream_names(scfg) # vector of potential extraction streams

  checkmate::assert_subset(postprocess_streams, choices = all_pp_streams, empty.ok = TRUE)
  checkmate::assert_subset(extract_streams, choices = all_ex_streams, empty.ok = TRUE)

  # Shared permission-check cache: setup_project_directories primes it with
  # verified-writable dirs; downstream preflight checks get instant hits.
  permission_check_cache <- new.env(parent = emptyenv())
  scfg <- setup_project_directories(scfg, check_cache = permission_check_cache)

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
    
    scfg$debug <- debug # pass forward debug flag from arguments
    scfg$force <- force # pass forward force flag from arguments
    scfg$log_level <- log_level
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
    scfg$log_level <- prompt_input(
      instruct = "Select log level (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)",
      type = "character", among = valid_log_levels, default = log_level
    )
  }

  if (isTRUE(scfg$force)) {
    if (!is.null(scfg$bids_conversion)) scfg$bids_conversion$overwrite <- TRUE
  }
  if (is.null(scfg$log_level)) scfg$log_level <- log_level
  scfg$log_level <- toupper(scfg$log_level)
  options(BrainGnomes.log_level = scfg$log_level)
  try(lgr::get_logger_glue("BrainGnomes")$set_threshold(scfg$log_level), silent = TRUE)
  
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

  # generate sequence ID for job tracking
  sequence_id <- uuid::UUIDgenerate()
  scfg <- ensure_aroma_output_space(scfg, require_aroma = isTRUE(steps["aroma"]))

  flywheel_id <- NULL
  if (isTRUE(steps["flywheel_sync"])) flywheel_id <- submit_flywheel_sync(scfg, sequence_id = sequence_id)

  # If only sync was requested, don't enter subject-level processing
  if (!any(steps[names(steps) != "flywheel_sync"])) return(invisible(TRUE))

  # Submit fsaverage setup early (used by fmriprep) to avoid race conditions
  fsaverage_id <- NULL
  if (isTRUE(steps["fmriprep"])) fsaverage_id <- submit_fsaverage_setup(scfg, sequence_id)

  # Prefetch TemplateFlow templates when needed so downstream runs can disable networking
  # This avoids socket errors in Python multiprocessing: https://github.com/nipreps/mriqc/issues/1170
  prefetch_id <- NULL
  if (any(steps[c("mriqc", "fmriprep", "aroma")])) prefetch_id <- submit_prefetch_templates(scfg, steps = steps, sequence_id = sequence_id)

  parent_ids <- c(fsaverage_id, prefetch_id)

  # If flywheel sync is requested, defer subject scheduling to a dependent controller job
  # This ensures that downstream steps see all data synched from flywheel (e.g., new subjects)
  if (isTRUE(steps["flywheel_sync"])) {
    snapshot <- list(
      scfg = scfg,
      steps = steps,
      subject_filter = subject_filter,
      postprocess_streams = postprocess_streams,
      extract_streams = extract_streams,
      parent_ids = parent_ids
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
      snapshot_rds = snap_file,
      log_level = scfg$log_level
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
    scfg = scfg, steps = steps, subject_filter = subject_filter, postprocess_streams = postprocess_streams, 
    extract_streams = extract_streams, parent_ids = parent_ids, sequence_id = sequence_id,
    permission_check_cache = permission_check_cache
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
#' @param sequence_id An identifying ID for a set of jobs in a sequence used for job tracking
#' @param permission_check_cache Optional environment for memoizing write-permission checks.
#'   When supplied (e.g. pre-primed by \code{setup_project_directories}), shared paths
#'   already verified writable are skipped.
#' @details 
#'   This function is not meant to be called by users! Instead, it is called internally
#'   after flywheel sync completes.
#' @keywords internal
submit_subjects <- function(scfg, steps, subject_filter = NULL, postprocess_streams = NULL,
  extract_streams = NULL, parent_ids = NULL, sequence_id = NULL,
  permission_check_cache = NULL) {

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
  if (is.null(permission_check_cache)) permission_check_cache <- new.env(parent = emptyenv())

  for (ss in seq_along(subject_dirs)) {
    process_subject(
      scfg, subject_dirs[[ss]], steps,
      postprocess_streams = postprocess_streams, extract_streams = extract_streams,
      parent_ids = parent_ids, sequence_id = sequence_id,
      permission_check_cache = permission_check_cache
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
submit_flywheel_sync <- function(scfg, lg = NULL, sequence_id = NULL) {
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

  sched_script <- get_job_script(scfg, "flywheel_sync", subject_suffix = FALSE)
  
  env_variables <- c(
    pkg_dir = find.package(package = "BrainGnomes"),
    R_HOME = R.home(),
    upd_job_status_path = system.file("upd_job_status.R", package = "BrainGnomes"),
    flywheel_cmd = scfg$compute_environment$flywheel,
    flywheel_cli_options = cli_options,
    flywheel_source_url = scfg$flywheel_sync$source_url,
    flywheel_sync_directory = scfg$metadata$flywheel_sync_directory
  )

  tracking_args <- list(
    job_name = "flywheel_sync",
    sequence_id = sequence_id,
    n_nodes = 1,
    n_cpus = scfg[["flywheel_sync"]]$ncores,
    wall_time = hours_to_dhms(scfg[["flywheel_sync"]]$nhours),
    mem_total = scfg[["flywheel_sync"]]$memgb,
    scheduler = scfg$compute_environment$scheduler,
    scheduler_options = sched_args
  )

  # preflight permission checks for project-level paths
  pf_issues <- c(
    check_write_target(scfg$metadata$log_directory, "log directory"),
    check_write_target(scfg$metadata$flywheel_sync_directory, "flywheel sync directory"),
    check_write_target(scfg$metadata$flywheel_temp_directory, "flywheel temp directory")
  )
  if (length(pf_issues) > 0L) {
    stop("Preflight permission check failed for flywheel_sync:\n",
         paste(paste0("  - ", pf_issues), collapse = "\n"), call. = FALSE)
  }

  job_id <- cluster_job_submit(sched_script, scheduler = scfg$compute_environment$scheduler, 
                               sched_args = sched_args, env_variables = env_variables,
                               tracking_sqlite_db = scfg$metadata$sqlite_db,
                               tracking_args = tracking_args)

  log_submission_command(lg, job_id, "flywheel_sync job")

  return(job_id)
}

# helper for avoiding race condition in setting up fsaverage folder in data_fmriprep
# avoid race condition in setting up fsaverage folder: https://github.com/nipreps/fmriprep/issues/3492
submit_fsaverage_setup <- function(scfg, sequence_id = NULL) {
  checkmate::assert_directory_exists(scfg$metadata$fmriprep_directory)
  checkmate::assert_file_exists(scfg$compute_environment$fmriprep_container)

  env_variables <- c(
    debug_pipeline = scfg$debug,
    pkg_dir = find.package(package = "BrainGnomes"), # location of installed R package
    R_HOME = R.home(), # populate location of R installation so that it can be used by any child R jobs
    stdout_log = glue("{scfg$metadata$log_directory}/cp_fsaverage_setup_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out"),
    stderr_log = glue("{scfg$metadata$log_directory}/cp_fsaverage_setup_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err"),
    upd_job_status_path = system.file("upd_job_status.R", package = "BrainGnomes"),
    add_parent_path = system.file("add_parent.R", package = "BrainGnomes"),
    loc_mrproc_root = scfg$metadata$fmriprep_directory,
    fmriprep_container = scfg$compute_environment$fmriprep_container
  )
  
  # get resource allocation request & scheduler arguments
  scfg$fsaverage <- list(nhours = 0.15, memgb = 8, ncores = 1) # fake top-level job to let get_job_sched_args work
  sched_args <- c(get_job_sched_args(scfg, "fsaverage"))
  sched_args <- set_cli_options( # setup files for stdout and stderr, job name
    sched_args,
    c(
      glue("--job-name=fsaverage_setup"),
      glue("--output={env_variables['stdout_log']}"),
      glue("--error={env_variables['stderr_log']}")
    )
  )
  ext <- ifelse(scfg$compute_environment$scheduler == "torque", "pbs", "sbatch")
  sched_script <- system.file(glue("hpc_scripts/fsaverage_setup.{ext}"), package = "BrainGnomes")
  
  # tracking info
  tracking_args <- list(
    job_name = "fsaverage_setup",
    sequence_id = sequence_id,
    n_nodes = 1,
    n_cpus = scfg[["fsaverage"]]$ncores,
    wall_time = hours_to_dhms(scfg[["fsaverage"]]$nhours),
    mem_total = scfg[["fsaverage"]]$memgb,
    scheduler = scfg$compute_environment$scheduler,
    scheduler_options = sched_args
  )
  tracking_sqlite_db <- scfg$metadata$sqlite_db

  # preflight permission checks for project-level paths
  pf_issues <- c(
    check_write_target(scfg$metadata$log_directory, "log directory"),
    check_write_target(scfg$metadata$fmriprep_directory, "fmriprep directory"),
    check_write_target(tracking_sqlite_db, "job tracking SQLite database")
  )
  if (length(pf_issues) > 0L) {
    stop("Preflight permission check failed for fsaverage_setup:\n",
         paste(paste0("  - ", pf_issues), collapse = "\n"), call. = FALSE)
  }

  job_id <- cluster_job_submit(sched_script,
                               scheduler = scfg$compute_environment$scheduler,
                               sched_args = sched_args, env_variables = env_variables,
                               echo = FALSE, tracking_sqlite_db = tracking_sqlite_db, 
                               tracking_args = tracking_args
  )

  return(job_id)
}

# helper for handling the problem of multi
submit_prefetch_templates <- function(scfg, steps, sequence_id = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_logical(steps, any.missing = FALSE)

  # run the python script for fetching within the fmriprep container to ensure templateflow presence and alignment
  container_path <- scfg$compute_environment$fmriprep_container
  if (!checkmate::test_file_exists(container_path)) {
    warning("Skipping TemplateFlow prefetch because the fMRIPrep container is missing.")
    return(NULL)
  }

  tf_home <- scfg$metadata$templateflow_home
  if (!checkmate::test_string(tf_home) || !nzchar(tf_home)) {
    tf_home <- file.path(Sys.getenv("HOME"), ".cache", "templateflow")
  }

  tf_home <- normalizePath(tf_home, mustWork = FALSE)
  if (!dir.exists(tf_home)) dir.create(tf_home, showWarnings = FALSE, recursive = TRUE)

  spaces <- scfg$fmriprep$output_spaces
  if (isTRUE(steps["aroma"]) && (is.null(spaces) || !grepl("MNI152NLin6Asym:res-2", spaces, fixed = TRUE))) {
    spaces <- trimws(paste(spaces, "MNI152NLin6Asym:res-2"))
  }

  # make sure that at least fmriprep's default space is included
  if (is.null(spaces) || !nzchar(trimws(spaces))) spaces <- "MNI152NLin2009cAsym"

  # pull out non-template output spaces
  spaces_vec <- unique(strsplit(trimws(spaces), "\\s+")[[1]])
  skip_spaces <- c("anat", "fsnative", "fsaverage", "fsaverage5", "fsaverage6", "T1w", "T2w", "func")
  fetch_spaces <- setdiff(spaces_vec, skip_spaces)
  if (length(fetch_spaces) == 0L) return(NULL)

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  if (!checkmate::test_file_exists(script_path)) {
    warning("Cannot locate TemplateFlow prefetch helper script; skipping prefetch step.")
    return(NULL)
  }

  # default resource allocation requirements
  scfg$prefetch_templates <- list(nhours = 0.5, memgb = 16, ncores = 1)
  
  log_stamp <- format(Sys.time(), "%d%b%Y_%H.%M.%S")
  stdout_log <- glue::glue("{scfg$metadata$log_directory}/prefetch_templates_jobid-%j_{log_stamp}.out")
  stderr_log <- sub("\\.out$", ".err", stdout_log)
  sched_args <- get_job_sched_args(scfg, "prefetch_templates", stdout_log = stdout_log, stderr_log = stderr_log)
  sched_script <- get_job_script(scfg, "prefetch_templates", subject_suffix = FALSE)

  # run TemplateFlow prefetch inside fmriprep container
  spaces_arg <- paste(fetch_spaces, collapse = " ")
  log_file <- file.path(scfg$metadata$log_directory, "prefetch_templates_log.txt")
  env_variables <- c(
    pkg_dir = find.package(package = "BrainGnomes"),
    R_HOME = R.home(),
    debug_pipeline = scfg$debug,
    log_file = log_file,
    stdout_log = stdout_log,
    stderr_log = stderr_log,
    upd_job_status_path = system.file("upd_job_status.R", package = "BrainGnomes"),
    prefetch_container = container_path,
    prefetch_script = script_path,
    prefetch_spaces = spaces_arg,
    templateflow_home = tf_home,
    log_level = scfg$log_level
  )

  tracking_args <- list(
    job_name = "prefetch_templates",
    sequence_id = sequence_id,
    n_nodes = 1,
    n_cpus = scfg[["prefetch_templates"]]$ncores,
    wall_time = hours_to_dhms(scfg[["prefetch_templates"]]$nhours),
    mem_total = scfg[["prefetch_templates"]]$memgb,
    scheduler = scfg$compute_environment$scheduler,
    scheduler_options = sched_args
  )
  
  # preflight permission checks for project-level paths
  pf_issues <- c(
    check_write_target(scfg$metadata$log_directory, "log directory"),
    check_write_target(tf_home, "templateflow_home directory")
  )
  if (length(pf_issues) > 0L) {
    stop("Preflight permission check failed for prefetch_templates:\n",
         paste(paste0("  - ", pf_issues), collapse = "\n"), call. = FALSE)
  }

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args,
    env_variables = env_variables,
    echo = FALSE,
    tracking_sqlite_db = scfg$metadata$sqlite_db,
    tracking_args = tracking_args
  )

  return(job_id)
}

ensure_aroma_output_space <- function(scfg, require_aroma = isTRUE(scfg$aroma$enable), verbose = TRUE) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  if (!isTRUE(require_aroma)) return(scfg)

  if (is.null(scfg$fmriprep$auto_added_aroma_space)) scfg$fmriprep$auto_added_aroma_space <- FALSE

  spaces <- scfg$fmriprep$output_spaces
  has_required_space <- !is.null(spaces) && grepl("MNI152NLin6Asym:res-2", spaces, fixed = TRUE)
  if (has_required_space) return(scfg)

  addition <- "MNI152NLin6Asym:res-2"
  if (is.null(spaces) || !nzchar(trimws(spaces))) {
    scfg$fmriprep$output_spaces <- addition
  } else {
    scfg$fmriprep$output_spaces <- trimws(paste(spaces, addition))
  }

  if (isTRUE(verbose)) {
    message("Adding MNI152NLin6Asym:res-2 to output spaces for fmriprep to allow AROMA to run.")
  }

  scfg$fmriprep$auto_added_aroma_space <- TRUE

  return(scfg)
}
