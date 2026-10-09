
# Format one resolved configuration value for concise dry-run output.
dry_run_value <- function(x, default = "<not configured>") {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) return(default)
  if (is.logical(x)) return(tolower(as.character(x)))
  paste(as.character(x), collapse = ", ")
}

# Resolve the same default postprocessing order used by postprocess_subject().
dry_run_postprocess_sequence <- function(cfg) {
  mask_file <- cfg$apply_mask$mask_file
  has_usable_mask <- isTRUE(cfg$apply_mask$enable) &&
    checkmate::test_string(mask_file) && !is.na(mask_file) &&
    (identical(mask_file, "template") || file.exists(mask_file))

  if (isTRUE(cfg$force_processing_order) && length(cfg$processing_steps) > 0L) {
    sequence <- tolower(cfg$processing_steps)
    sequence <- sub("^spatial_smoothing$", "spatial_smooth", sequence)
    sequence <- sub("^temporal_filtering$", "temporal_filter", sequence)
    sequence <- sub("^confound_regress$", "confound_regression", sequence)
    sequence <- sub("^intensity_normalization$", "intensity_normalize", sequence)
    if (!has_usable_mask) sequence <- sequence[sequence != "apply_mask"]
    return(sequence)
  }

  sequence <- character()
  if (has_usable_mask) sequence <- c(sequence, "apply_mask")
  if (isTRUE(cfg$spatial_smooth$enable)) sequence <- c(sequence, "spatial_smooth")
  if (isTRUE(cfg$intensity_normalize$enable)) sequence <- c(sequence, "intensity_normalize")
  if (isTRUE(cfg$apply_aroma$enable)) sequence <- c(sequence, "apply_aroma")
  if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$interpolate)) sequence <- c(sequence, "scrub_interpolate")
  if (isTRUE(cfg$temporal_filter$enable)) sequence <- c(sequence, "temporal_filter")
  if (isTRUE(cfg$confound_regression$enable)) sequence <- c(sequence, "confound_regression")
  if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$apply)) sequence <- c(sequence, "scrub_timepoints")
  sequence
}

print_postprocess_dry_run_plan <- function(scfg, streams) {
  cat("Resolved postprocess plan:\n")
  for (stream in streams) {
    cfg <- scfg$postprocess[[stream]]
    sequence <- dry_run_postprocess_sequence(cfg)
    cat("  - ", stream, "\n", sep = "")
    cat("      input query: ", dry_run_value(cfg$input_regex, "desc:preproc suffix:bold"), "\n", sep = "")
    cat("      output description: ", dry_run_value(cfg$bids_desc), "\n", sep = "")
    cat("      processing order: ", dry_run_value(sequence, "<no processing steps enabled>"), "\n", sep = "")
    cat("      output root: ", dry_run_value(scfg$metadata$postproc_directory), "\n", sep = "")
    cat("      overwrite: ", dry_run_value(cfg$overwrite, "false"), "\n", sep = "")
  }
  invisible(NULL)
}

print_extract_dry_run_plan <- function(scfg, streams) {
  cat("Resolved extraction plan:\n")
  for (stream in streams) {
    cfg <- scfg$extract_rois[[stream]]
    methods <- cfg$correlation$method
    if (is.null(methods)) methods <- cfg$cor_method
    input_streams <- cfg$input_streams
    sources <- vapply(input_streams, function(input_stream) {
      input_cfg <- scfg$postprocess[[input_stream]]
      input_regex <- dry_run_value(input_cfg$input_regex, "desc:preproc suffix:bold")
      bids_desc <- dry_run_value(input_cfg$bids_desc)
      paste0(input_stream, " [", input_regex, " -> desc:", bids_desc, "]")
    }, character(1))

    cat("  - ", stream, "\n", sep = "")
    cat("      inputs: ", dry_run_value(sources), "\n", sep = "")
    cat("      atlases: ", dry_run_value(cfg$atlases), "\n", sep = "")
    cat("      mask: ", dry_run_value(cfg$mask_file, "<none; BOLD-valid voxels only>"), "\n", sep = "")
    cat("      ROI reduction: ", dry_run_value(cfg$roi_reduce, "mean"), "\n", sep = "")
    cat("      correlation methods: ", dry_run_value(methods), "\n", sep = "")
    cat("      minimum voxels per ROI: ", dry_run_value(cfg$min_vox_per_roi, "5"), "\n", sep = "")
    cat("      save time series: ", dry_run_value(cfg$save_ts, "true"), "\n", sep = "")
    cat("      save ROI diagnostics: ", dry_run_value(cfg$save_diagnostics, "false"), "\n", sep = "")
    cat("      Fisher r-to-z: ", dry_run_value(cfg$rtoz, "false"), "\n", sep = "")
    cat("      output root: ", dry_run_value(scfg$metadata$rois_directory), "\n", sep = "")
    cat("      overwrite: ", dry_run_value(cfg$overwrite, "false"), "\n", sep = "")
  }
  invisible(NULL)
}

#' Run the project workflow
#'
#' This remains the standard execution path after [setup_project()]. It resolves
#' the same stages, streams, subject/session scope, and force setting exposed by
#' [plan_project()] before submission; calling `plan_project()` first is optional.
#' Plans, dry runs, and submission apply the same selected-stage configuration
#' checks before creating directories, writing provenance, or submitting jobs.
#' When stages are selected interactively, only omitted arguments are prompted
#' for. Explicit choices, including `dry_run = TRUE`, are always retained.
#'
#' @param scfg A `bg_project_cfg` object, YAML configuration file, or project
#'   directory containing `project_config.yaml`. Defaults to the current working
#'   directory.
#' @param steps Character vector of project stages to execute. Supported stages
#'   are `"flywheel_sync"`, `"bids_conversion"`, `"mriqc"`, `"fmriprep"`,
#'   `"aroma"`, `"postprocess"`, and `"extract_rois"`. Use `"all"` to run all
#'   enabled stages. If `NULL`, the user will be prompted for which stages to run.
#'   BIDS validation is configured with the project but submitted separately
#'   through [run_bids_validation()]; it is not a `run_project()` stage.
#' @param debug Logical. Enable verbose worker debugging and suppress imaging
#'   commands. Debug mode still submits scheduler jobs and writes tracking/log
#'   records; use `dry_run = TRUE` for a submission-free preview.
#' @param force A logical value indicating whether to force the execution of all steps, regardless of their current status.
#' @param dry_run A logical value indicating whether to perform a dry run. Dry
#'   runs validate settings and report subject/session scope plus resolved
#'   postprocessing and extraction stream settings without submitting any jobs.
#' @param subject_filter Optional character vector or data.frame specifying which
#'   subjects (and optionally sessions) to process. When omitted and run
#'   interactively, the user will be prompted to enter space-separated subject
#'   IDs (press ENTER to process all subjects). Explicit `NULL` selects all
#'   subjects without prompting. When a data.frame is provided, it
#'   must contain a `sub_id` column and may include a `ses_id` column to filter
#'   on specific subject/session combinations.
#' @param postprocess_streams Optional character vector specifying which
#'   postprocessing streams should run. When `"postprocess"` is included in
#'   `steps`, `NULL` selects every configured postprocessing stream.
#' @param extract_streams Optional character vector specifying which ROI
#'   extraction streams should run. When `"extract_rois"` is included in
#'   `steps`, `NULL` selects every configured extraction stream.
#' @param log_level Character string controlling log verbosity. One of
#'   `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, or `FATAL`.
#' 
#' @return For submitted work, an invisible `bg_project_run` object containing
#'   the run UUID, scheduler job IDs known at submission time, and the path to
#'   the complete run provenance record. Dry runs invisibly return a
#'   `bg_project_plan` with concrete work units, paths, resources, and current
#'   completion-marker decisions after printing the preview. No jobs are submitted.
#' @details Before submission, BrainGnomes reports when it is checking project
#'   folders, finding matching subjects, and saving the run record. The first
#'   use of a large container in a project may take longer because BrainGnomes
#'   reads the complete file once to identify the exact copy used. During large
#'   submissions, periodic messages report progress through the subject list.
#' @export
#' @examples
#'   \dontrun{
#'     # Assuming you have a valid project configuration list named `study_config`
#'     run_project(study_config, steps = "fmriprep", force = FALSE)
#'   }
#' @seealso [inspect_project()] to monitor current progress;
#'   [get_run_provenance()] to read the recorded configuration, execution
#'   context, and artifact fingerprints; [plan_project()] for optional
#'   inspection or persistence of the resolved request; [run_bids_validation()]
#'   to submit the BIDS validation configured with the project;
#'   [diagnose_project()] and [retry_project_run()] for optional recovery after
#'   a submitted run fails.
#' @importFrom glue glue
#' @importFrom checkmate assert_list assert_flag
#' @importFrom lgr get_logger_glue
run_project <- function(scfg = getwd(), steps = NULL, subject_filter = NULL, postprocess_streams = NULL,
  extract_streams = NULL, debug = FALSE, force = FALSE, dry_run = FALSE,
  log_level = c("INFO", "DEBUG", "WARN", "ERROR", "TRACE", "FATAL")) {

  # Capture omission before normalizing arguments: explicit values, including
  # FALSE and NULL, must never be replaced by answers in the guided workflow.
  prompt_subjects <- missing(subject_filter)
  prompt_postprocess <- missing(postprocess_streams)
  prompt_extract <- missing(extract_streams)
  prompt_debug <- missing(debug)
  prompt_force <- missing(force)
  prompt_dry_run <- missing(dry_run)
  prompt_log_level <- missing(log_level)

  scfg <- project_config_from_input(scfg)
  checkmate::assert_class(scfg, "bg_project_cfg")
  provenance_context <- attr(scfg, "provenance_context", exact = TRUE)
  checkmate::assert_character(steps, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_character(subject_filter, any.missing = FALSE, null.ok = TRUE),
    checkmate::check_data_frame(subject_filter, null.ok = TRUE)
  )
  checkmate::assert_character(postprocess_streams, null.ok = TRUE)
  checkmate::assert_character(extract_streams, null.ok = TRUE)
  
  checkmate::assert_flag(debug)
  checkmate::assert_flag(force)
  checkmate::assert_flag(dry_run)
  valid_log_levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  if (length(log_level) > 1L) log_level <- log_level[1L]
  log_level <- toupper(log_level)
  log_level <- match.arg(log_level, valid_log_levels)
  
  if (is.null(scfg$metadata$project_name)) stop("Cannot run a nameless project. Have you run setup_project() yet?")
  if (is.null(scfg$metadata$project_directory)) stop("Cannot run a project lacking a project directory. Have you run setup_project() yet?")

  all_pp_streams <- get_postprocess_stream_names(scfg) # vector of potential postprocessing streams
  all_ex_streams <- get_extract_stream_names(scfg) # vector of potential extraction streams

  # Shared permission-check cache: setup_project_directories primes it with
  # verified-writable dirs; downstream preflight checks get instant hits.
  permission_check_cache <- new.env(parent = emptyenv())

  cat(glue("
    \nRunning project workflow for: {scfg$metadata$project_name}
      Project directory:   {pretty_arg(scfg$metadata$project_directory)}
      DICOM directory:     {pretty_arg(scfg$metadata$dicom_directory)}
      BIDS directory:      {pretty_arg(scfg$metadata$bids_directory)}
      fmriprep directory:  {pretty_arg(scfg$metadata$fmriprep_directory)}
      Postprocess directory: {pretty_arg(scfg$metadata$postproc_directory)}\n
      "))

  # by passing steps, user is asking for unattended execution
  prompt <- is.null(steps)

  if (isFALSE(prompt)) {
    selection <- resolve_project_selection(
      scfg, steps, postprocess_streams, extract_streams, force
    )
    postprocess_streams <- selection$postprocess_streams
    extract_streams <- selection$extract_streams

    # Downstream scheduling uses the same resolved stage flags exposed by plans.
    steps <- selection$step_flags
    
    scfg$debug <- debug # pass forward debug flag from arguments
    scfg$force <- force # pass forward force flag from arguments
    scfg$dry_run <- dry_run
    scfg$log_level <- log_level
  } else {
    if (prompt_subjects) {
      ids <- prompt_input(
        instruct = "Enter subject IDs to process, separated by spaces. Press enter to process all subjects.",
        type = "character", split = " ", required = FALSE
      )
      if (!is.na(ids[1])) subject_filter <- ids
    }

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

    if (isTRUE(steps["postprocess"]) && prompt_postprocess) {
      if (length(all_pp_streams) == 1L) {
        postprocess_streams <- all_pp_streams # if we have only one stream, run it
      } else {
        postprocess_streams <- select_list_safe(all_pp_streams, multiple = TRUE,
          title = "Which postprocessing streams should be run? Select streams, or 0 to cancel."
        )
        if (!length(postprocess_streams) || anyNA(postprocess_streams) || any(!nzchar(postprocess_streams))) {
          stop("Postprocessing stream selection cancelled; no jobs submitted.", call. = FALSE)
        }
      }
    }

    steps["extract_rois"] <- ifelse(isTRUE(scfg$extract_rois$enable) && length(all_ex_streams) > 0L,
      prompt_input(instruct = "Run ROI extraction?", type = "flag"), FALSE
    )

    if (isTRUE(steps["extract_rois"]) && prompt_extract) {
      if (length(all_ex_streams) == 1L) {
        extract_streams <- all_ex_streams # if we have only one stream, run it
      } else {
        extract_streams <- select_list_safe(all_ex_streams, multiple = TRUE,
          title = "Which extraction streams should be run? Select streams, or 0 to cancel."
        )
        if (!length(extract_streams) || anyNA(extract_streams) || any(!nzchar(extract_streams))) {
          stop("ROI-extraction stream selection cancelled; no jobs submitted.", call. = FALSE)
        }
      }
    }

    # check whether to run in debug mode
    scfg$debug <- if (!prompt_debug) debug else prompt_input(instruct = "Run the project workflow in debug mode? This will echo commands to logs, but not run them.", type = "flag")
    scfg$force <- if (!prompt_force) force else prompt_input(instruct = "Force (re-run) each processing step, even if it appears to be complete?", type = "flag")
    scfg$dry_run <- if (!prompt_dry_run) dry_run else prompt_input(
      instruct = "Run as dry run? This validates configuration and reports planned jobs without submitting them.",
      type = "flag", default = FALSE
    )
    scfg$log_level <- if (!prompt_log_level) log_level else prompt_input(
      instruct = "Select log level (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)",
      type = "character", among = valid_log_levels, default = log_level
    )
  }

  if (is.null(scfg$dry_run)) scfg$dry_run <- dry_run
  if (is.null(scfg$log_level)) scfg$log_level <- log_level
  scfg$log_level <- toupper(scfg$log_level)
  options(BrainGnomes.log_level = scfg$log_level)
  try(lgr::get_logger_glue("BrainGnomes")$set_threshold(scfg$log_level), silent = TRUE)
  
  if (!any(steps)) stop("No processing steps were requested in run_project.")

  validation <- validate_project_config(scfg, quiet = TRUE, steps = names(steps)[steps],
    postprocess_streams = postprocess_streams, extract_streams = extract_streams)
  require_valid_project_selection(validation)

  if (isTRUE(scfg$force)) {
    if (!is.null(scfg$bids_conversion)) scfg$bids_conversion$overwrite <- TRUE
  }

  # Validation is read-only and precedes all submission-side writes, including
  # folder creation/permission repair. Guided runs use their final selections.
  if (!isTRUE(scfg$dry_run)) {
    cli::cli_alert_info("Checking the project folders needed for this run...")
    scfg <- setup_project_directories(scfg, check_cache = permission_check_cache)
  }

  cli::cli_alert_info("Finding the subjects and sessions that match this run...")
  execution <- resolve_project_execution(
    scfg,
    steps = names(steps)[steps],
    subject_filter = subject_filter,
    postprocess_streams = postprocess_streams,
    extract_streams = extract_streams,
    force = isTRUE(scfg$force)
  )
  steps <- execution$step_flags
  postprocess_streams <- execution$postprocess_streams
  extract_streams <- execution$extract_streams

  if (isTRUE(execution$scope_deferred)) {
    cli::cli_alert_info(
      "Subjects and sessions will be found after Flywheel synchronization finishes."
    )
  } else {
    n_subjects <- length(unique(execution$subjects$sub_id))
    n_sessions <- sum(!is.na(execution$subjects$ses_id))
    if (n_sessions > 0L) {
      cli::cli_alert_success(
        "Found {n_subjects} matching subject{?s} across {n_sessions} session{?s}."
      )
    } else {
      cli::cli_alert_success("Found {n_subjects} matching subject{?s}.")
    }
  }

  if (isTRUE(scfg$dry_run)) {
    plan <- project_plan_from_execution(scfg, execution, validation[c("valid", "issues", "messages")])
    dry_requested <- names(steps)[steps]
    cat("\nDry run enabled. No jobs will be submitted.\n")
    cat("Requested steps:", paste(dry_requested, collapse = ", "), "\n")
    if (length(postprocess_streams) > 0L) {
      cat("Postprocess streams:", paste(postprocess_streams, collapse = ", "), "\n")
      print_postprocess_dry_run_plan(scfg, postprocess_streams)
    }
    if (length(extract_streams) > 0L) {
      cat("Extraction streams:", paste(extract_streams, collapse = ", "), "\n")
      print_extract_dry_run_plan(scfg, extract_streams)
    }
    if (isTRUE(steps["flywheel_sync"])) {
      cat("Would submit: flywheel_sync\n")
      if (any(steps[names(steps) != "flywheel_sync"])) {
        cat("Would submit after flywheel_sync: submit_subjects controller\n")
      }
    }

    if (isTRUE(execution$scope_deferred)) {
      cat("Subject/session discovery will occur after Flywheel synchronization.\n")
    } else if (any(steps[names(steps) != "flywheel_sync"])) {
      submit_subjects(
        scfg = scfg, steps = steps, subject_filter = subject_filter,
        resolved_subjects = execution$subjects,
        postprocess_streams = postprocess_streams, extract_streams = extract_streams,
        parent_ids = NULL, sequence_id = NULL,
        permission_check_cache = permission_check_cache,
        dry_run = TRUE
      )
    }
    print(plan)
    return(invisible(plan))
  }

  # generate sequence ID for job tracking
  sequence_id <- uuid::UUIDgenerate()
  scfg <- ensure_aroma_output_space(scfg, require_aroma = isTRUE(steps["aroma"]))
  if (!is.null(provenance_context)) {
    attr(scfg, "provenance_context") <- provenance_context
  }
  cli::cli_alert_info(
    "Saving a record of the settings, subjects, and software used for this run..."
  )
  provenance_file <- record_run_provenance(
    scfg = scfg,
    run_id = sequence_id,
    execution = execution,
    debug = isTRUE(scfg$debug),
    log_level = scfg$log_level
  )
  cli::cli_alert_success("Saved the run record. Starting job submission.")

  flywheel_id <- NULL
  if (isTRUE(steps["flywheel_sync"])) flywheel_id <- submit_flywheel_sync(scfg, sequence_id = sequence_id)

  # If only sync was requested, don't enter subject-level processing
  if (!any(steps[names(steps) != "flywheel_sync"])) {
    return(invisible(new_project_run(
      scfg, sequence_id, submitted_ids = flywheel_id,
      provenance_file = provenance_file
    )))
  }

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
      parent_ids = parent_ids,
      sequence_id = sequence_id
    )
    run_dir <- file.path(scfg$metadata$log_directory, "runs", sequence_id)
    snap_file <- file.path(run_dir, "run_project_snapshot.rds")
    dir.create(run_dir, showWarnings = FALSE, recursive = TRUE)
    assert_provenance_metadata(snapshot, "run_project_snapshot")
    saveRDS(snapshot, snap_file)

    # Lightweight controller job to schedule subjects after flywheel completes
    scfg$submit_subjects <- list(nhours = 0.5, memgb = 4, ncores = 1)
    stdout_log <- glue::glue("{scfg$metadata$log_directory}/submit_subjects_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out")
    stderr_log <- glue::glue("{scfg$metadata$log_directory}/submit_subjects_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err")
    sched_args <- get_job_sched_args(
      scfg, job_name = "submit_subjects",
      stdout_log = stdout_log,
      stderr_log = stderr_log
    )
    sched_script <- get_job_script(scfg, "submit_subjects", subject_suffix = FALSE)
    env_variables <- c(
      pkg_dir = find.package(package = "BrainGnomes"),
      R_HOME = R.home(),
      snapshot_rds = snap_file,
      stdout_log = stdout_log,
      stderr_log = stderr_log,
      upd_job_status_path = system.file("upd_job_status.R", package = "BrainGnomes"),
      log_level = scfg$log_level
    )
    controller_tracking_args <- list(
      job_name = "submit_subjects",
      sequence_id = sequence_id,
      n_nodes = 1,
      n_cpus = scfg$submit_subjects$ncores,
      wall_time = hours_to_dhms(scfg$submit_subjects$nhours),
      mem_total = scfg$submit_subjects$memgb,
      scheduler = scfg$compute_environment$scheduler,
      scheduler_options = sched_args
    )
    controller_tracking_args <- enrich_job_tracking_args(
      scfg, controller_tracking_args,
      stage = "subject_submission", job_role = "controller",
      stdout_log = stdout_log, stderr_log = stderr_log
    )
    controller_id <- cluster_job_submit(
      sched_script,
      scheduler = scfg$compute_environment$scheduler,
      sched_args = sched_args,
      env_variables = env_variables,
      wait_jobs = flywheel_id,
      echo = FALSE,
      tracking_sqlite_db = scfg$metadata$sqlite_db,
      tracking_args = controller_tracking_args
    )
    return(invisible(new_project_run(
      scfg, sequence_id,
      submitted_ids = c(flywheel_id, fsaverage_id, prefetch_id, controller_id),
      deferred = TRUE,
      provenance_file = provenance_file
    )))
  }

  # No flywheel sync: schedule subjects immediately
  submit_subjects(
    scfg = scfg, steps = steps, subject_filter = subject_filter,
    resolved_subjects = execution$subjects,
    postprocess_streams = postprocess_streams,
    extract_streams = extract_streams, parent_ids = parent_ids, sequence_id = sequence_id,
    permission_check_cache = permission_check_cache
  )
  return(invisible(new_project_run(
    scfg, sequence_id,
    submitted_ids = c(fsaverage_id, prefetch_id),
    provenance_file = provenance_file
  )))
}

#' Schedule subject-level processing
#' @param scfg A bg_project_cfg object
#' @param steps Named logical vector of steps
#' @param subject_filter Optional subject/session filter (character or data.frame)
#' @param resolved_subjects Optional subject/session table already resolved by
#'   `resolve_project_execution()`. Deferred Flywheel controllers omit it so
#'   discovery occurs after synchronization.
#' @param postprocess_streams Optional character vector of postprocess streams
#' @param extract_streams Optional character vector of extraction streams
#' @param parent_ids Optional character vector of job IDs to depend on
#' @param sequence_id An identifying ID for a set of jobs in a sequence used for job tracking
#' @param permission_check_cache Optional environment for memoizing write-permission checks.
#'   When supplied (e.g. pre-primed by \code{setup_project_directories}), shared paths
#'   already verified writable are skipped.
#' @param dry_run Logical. If \code{TRUE}, report planned subject/session scope without submitting
#'   any subject-level jobs.
#' @details 
#'   This function is not meant to be called by users! Instead, it is called internally
#'   after flywheel sync completes.
#' @keywords internal
submit_subjects <- function(scfg, steps, subject_filter = NULL,
  resolved_subjects = NULL, postprocess_streams = NULL,
  extract_streams = NULL, parent_ids = NULL, sequence_id = NULL,
  permission_check_cache = NULL, dry_run = FALSE) {

  checkmate::assert_flag(dry_run)
  checkmate::assert_data_frame(resolved_subjects, null.ok = TRUE)

  subject_dirs <- if (is.null(resolved_subjects)) {
    discover_project_subjects(
      scfg,
      steps = names(steps)[steps],
      subject_filter = subject_filter,
      allow_empty = FALSE
    )
  } else {
    resolved_subjects
  }
  work_units <- normalize_retry_work_units(attr(scfg, "retry_work_units", exact = TRUE))
  subject_dirs <- retry_subject_scope(subject_dirs, work_units)
  if (nrow(subject_dirs) == 0L) {
    stop("No subject/session inputs match the requested run.", call. = FALSE)
  }

  preview_limit <- 20L
  if (!is.null(subject_filter)) {
    msg_df <- unique(subject_dirs[, c("sub_id", "ses_id")])
    msg_lines <- apply(msg_df, 1, function(rr) {
      if (!is.na(rr["ses_id"])) glue("  sub-{rr['sub_id']} ses-{rr['ses_id']}") else glue("  sub-{rr['sub_id']}")
    })
    preview <- utils::head(msg_lines, preview_limit)
    cat("Processing the following requested subjects:\n",
        paste(preview, collapse = "\n"), "\n")
    if (length(msg_lines) > preview_limit) {
      cat(glue("  ... and {length(msg_lines) - preview_limit} more matching subject/session entries.\n"))
    }
  }

  if (isTRUE(dry_run)) {
    if (!is.null(work_units)) {
      cat("Exact retry work units:\n")
      print(utils::head(work_units, preview_limit), row.names = FALSE)
      if (nrow(work_units) > preview_limit) cat("Full retry selection is retained in the returned plan.\n")
    }
    msg_df <- unique(subject_dirs[, c("sub_id", "ses_id"), drop = FALSE])
    msg_lines <- apply(msg_df, 1, function(rr) {
      if (!is.na(rr["ses_id"])) glue("  sub-{rr['sub_id']} ses-{rr['ses_id']}")
      else glue("  sub-{rr['sub_id']}")
    })
    cat("Dry run subject/session plan:\n", paste(utils::head(msg_lines, preview_limit), collapse = "\n"), "\n")
    if (length(msg_lines) > preview_limit) {
      cat("  ... ", length(msg_lines) - preview_limit, " more subject/session rows in the returned plan.\n", sep = "")
    }
    return(invisible(msg_df))
  }

  # split data.frame by subject (some steps are subject-level, some are session-level)
  subject_dirs <- split(subject_dirs, subject_dirs$sub_id)
  if (is.null(permission_check_cache)) permission_check_cache <- new.env(parent = emptyenv())

  n_subjects <- length(subject_dirs)
  progress_every <- max(1L, ceiling(n_subjects / 20L))
  progress_points <- unique(c(
    1L, seq.int(progress_every, n_subjects, by = progress_every), n_subjects
  ))
  cli::cli_alert_info(
    "Checking and submitting jobs for {n_subjects} subject{?s}."
  )

  for (ss in seq_along(subject_dirs)) {
    if (ss %in% progress_points) {
      cli::cli_alert_info(
        "Preparing jobs for subject {ss} of {n_subjects}: sub-{subject_dirs[[ss]]$sub_id[[1L]]}."
      )
    }
    subject_steps <- steps
    subject_postprocess <- postprocess_streams
    subject_extract <- extract_streams
    if (!is.null(work_units)) {
      units <- work_units[!is.na(work_units$sub_id) &
        work_units$sub_id == subject_dirs[[ss]]$sub_id[[1L]], , drop = FALSE]
      subject_steps[] <- names(subject_steps) %in% units$stage
      subject_postprocess <- unique(units$stream[units$stage == "postprocess"])
      subject_extract <- unique(units$stream[units$stage == "extract_rois"])
    }
    process_subject(
      scfg, subject_dirs[[ss]], subject_steps,
      postprocess_streams = subject_postprocess, extract_streams = subject_extract,
      parent_ids = parent_ids, sequence_id = sequence_id,
      permission_check_cache = permission_check_cache
    )
  }

  cli::cli_alert_success(
    "Finished checking and submitting jobs for {n_subjects} subject{?s}."
  )

  invisible(do.call(rbind, subject_dirs))
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

  stdout_log <- glue::glue("{scfg$metadata$log_directory}/flywheel_sync_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out")
  stderr_log <- glue::glue("{scfg$metadata$log_directory}/flywheel_sync_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err")
  sched_args <- get_job_sched_args(scfg, job_name = "flywheel_sync",
    stdout_log = stdout_log,
    stderr_log = stderr_log
  )

  audit_str <- if (test_true(scfg$flywheel_sync$save_audit_logs)) {
    glue("--save-audit-logs {scfg$metadata$log_directory}/flywheel_sync_audit_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.csv")
  } else {
    NULL
  }

  cli_options <- set_cli_options(scfg$flywheel_sync$cli_options, c(
    "--include dicom", "-y",
    glue("--tmp-path {scfg$metadata$flywheel_temp_directory}"),
    audit_str
  ), collapse = TRUE)

  sched_script <- get_job_script(scfg, "flywheel_sync", subject_suffix = FALSE)
  log_file <- if ("flywheel" %in% names(lg$appenders)) lg$appenders$flywheel$destination else NULL
  
  env_variables <- c(
    debug_pipeline = scfg$debug,
    pkg_dir = find.package(package = "BrainGnomes"),
    R_HOME = R.home(),
    log_file = log_file,
    stdout_log = stdout_log,
    stderr_log = stderr_log,
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
  tracking_args <- enrich_job_tracking_args(
    scfg, tracking_args,
    stage = "flywheel_sync", job_role = "project",
    stdout_log = stdout_log, stderr_log = stderr_log
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

  stdout_log <- glue("{scfg$metadata$log_directory}/cp_fsaverage_setup_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out")
  stderr_log <- glue("{scfg$metadata$log_directory}/cp_fsaverage_setup_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err")
  env_variables <- c(
    debug_pipeline = scfg$debug,
    pkg_dir = find.package(package = "BrainGnomes"), # location of installed R package
    R_HOME = R.home(), # populate location of R installation so that it can be used by any child R jobs
    stdout_log = stdout_log,
    stderr_log = stderr_log,
    upd_job_status_path = system.file("upd_job_status.R", package = "BrainGnomes"),
    add_parent_path = system.file("add_parent.R", package = "BrainGnomes"),
    loc_mrproc_root = scfg$metadata$fmriprep_directory,
    fmriprep_container = scfg$compute_environment$fmriprep_container
  )
  
  # get resource allocation request & scheduler arguments
  scfg$fsaverage <- list(nhours = 0.15, memgb = 8, ncores = 1) # fake top-level job to let get_job_sched_args work
  sched_args <- c(get_job_sched_args(
    scfg, "fsaverage",
    jobid_str = "fsaverage_setup",
    stdout_log = stdout_log,
    stderr_log = stderr_log
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
  tracking_args <- enrich_job_tracking_args(
    scfg, tracking_args,
    stage = "fsaverage_setup", job_role = "project",
    stdout_log = stdout_log, stderr_log = stderr_log
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

  log_submission_command(NULL, job_id, "fsaverage_setup job")

  return(job_id)
}

# normalize and sort TemplateFlow spaces for stable comparisons
normalize_prefetch_spaces <- function(spaces) {
  if (is.null(spaces) || length(spaces) == 0L) return(character(0))
  spaces <- trimws(as.character(spaces))
  spaces <- spaces[nzchar(spaces)]
  sort(unique(spaces))
}

fmriprep_cli_requests_cifti_defaults <- function(cli_options) {
  cli_options <- validate_char(cli_options)
  if (!checkmate::test_string(cli_options) || !nzchar(trimws(cli_options))) return(FALSE)

  parsed <- tryCatch(args_to_df(cli_options), error = function(e) NULL)
  if (is.null(parsed) || nrow(parsed) == 0L) return(FALSE)

  cifti_rows <- parsed[parsed$lhs == "cifti-output", , drop = FALSE]
  if (nrow(cifti_rows) == 0L) return(FALSE)

  rhs <- tolower(trimws(ifelse(is.na(cifti_rows$rhs), "", cifti_rows$rhs)))
  any(!rhs %in% c("", "0", "false", "off", "null", "none"))
}

prefetch_state_cache_hash <- function(templateflow_home) {
  normalized <- normalizePath(templateflow_home, winslash = "/", mustWork = FALSE)
  tmp <- tempfile("prefetch_state_hash_")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(normalized, tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

get_prefetch_state_file <- function(log_directory, templateflow_home) {
  if (!checkmate::test_string(log_directory) || !nzchar(log_directory)) {
    stop("log_directory must be a non-empty string when resolving prefetch state file.", call. = FALSE)
  }
  if (!checkmate::test_string(templateflow_home) || !nzchar(templateflow_home)) {
    stop("templateflow_home must be a non-empty string when resolving prefetch state file.", call. = FALSE)
  }

  log_directory <- normalizePath(log_directory, winslash = "/", mustWork = FALSE)
  cache_hash <- prefetch_state_cache_hash(templateflow_home)
  file.path(log_directory, sprintf(".braingnomes_prefetch_state_%s.dcf", cache_hash))
}

get_legacy_prefetch_state_file <- function(templateflow_home) {
  file.path(templateflow_home, ".braingnomes_prefetch_state.dcf")
}

read_prefetch_state <- function(state_file) {
  if (!checkmate::test_file_exists(state_file)) return(NULL)

  state <- tryCatch({
    lines <- readLines(state_file, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0L) NULL else {

      out <- list()
      for (line in lines) {
        split_idx <- regexpr(":", line, fixed = TRUE)[1]
        if (split_idx <= 0L) next
        key <- trimws(substr(line, 1L, split_idx - 1L))
        val <- trimws(substr(line, split_idx + 1L, nchar(line)))
        if (!nzchar(key)) next
        out[[key]] <- val
      }
      out
    }
  }, error = function(e) NULL)

  if (is.null(state) || length(state) == 0L) return(NULL)

  if (!is.null(state$spaces) && checkmate::test_string(state$spaces)) {
    state$spaces <- normalize_prefetch_spaces(strsplit(trimws(state$spaces), "\\s+")[[1]])
  } else {
    state$spaces <- character(0)
  }

  state
}

prefetch_templateflow_cache_initialized <- function(templateflow_home) {
  if (!checkmate::test_directory_exists(templateflow_home)) return(FALSE)
  entries <- list.files(templateflow_home, all.files = FALSE, no.. = TRUE, full.names = FALSE)
  any(grepl("^tpl-", entries))
}

copy_prefetch_state_file <- function(from, to) {
  if (!checkmate::test_file_exists(from)) {
    stop(glue::glue("Cannot copy prefetch state; source file does not exist: {from}"), call. = FALSE)
  }

  target_dir <- dirname(to)
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(to, ".tmp.", Sys.getpid())

  copied <- isTRUE(file.copy(from, tmp, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE))
  if (!copied || !file.exists(tmp)) {
    stop(glue::glue("Failed to stage copied prefetch state file: {tmp}"), call. = FALSE)
  }

  renamed <- isTRUE(file.rename(tmp, to))
  if (!renamed) {
    copied_final <- isTRUE(file.copy(tmp, to, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE))
    suppressWarnings(unlink(tmp))
    if (!copied_final || !file.exists(to)) {
      stop(glue::glue("Failed to finalize copied prefetch state file: {to}"), call. = FALSE)
    }
  }

  invisible(to)
}

migrate_prefetch_state_file <- function(state_file, legacy_state_file, templateflow_home) {
  if (!checkmate::test_file_exists(legacy_state_file)) return(invisible(NULL))

  if (!checkmate::test_file_exists(state_file)) {
    copy_prefetch_state_file(legacy_state_file, state_file)
    message(glue::glue(
      "Migrated legacy TemplateFlow prefetch state from {legacy_state_file} to {state_file}."
    ))
  } else {
    message(glue::glue(
      "Found legacy TemplateFlow prefetch state at {legacy_state_file}; using logs-based state file at {state_file}."
    ))
  }

  removed <- suppressWarnings(unlink(legacy_state_file))
  if (!checkmate::test_file_exists(legacy_state_file) || identical(removed, 0L)) {
    message(glue::glue(
      "Removed legacy TemplateFlow prefetch state file from templateflow_home: {legacy_state_file}"
    ))
    return(invisible(NULL))
  }

  cache_initialized <- prefetch_templateflow_cache_initialized(templateflow_home)
  base_msg <- glue::glue(
    "Unable to remove legacy TemplateFlow prefetch state file at {legacy_state_file}. "
  )
  guidance <- "Remove it manually to avoid TemplateFlow standard-space resolution failures."

  if (!cache_initialized) {
    stop(
      paste0(
        base_msg,
        "TemplateFlow cache appears uninitialized (no tpl-* directories). ",
        guidance
      ),
      call. = FALSE
    )
  }

  warning(
    paste0(
      base_msg,
      "Continuing because TemplateFlow cache appears initialized (tpl-* directories detected). ",
      guidance
    ),
    call. = FALSE
  )
}

find_container_runtime <- function() {
  runtimes <- Sys.which(c("singularity", "apptainer"))
  available <- unname(runtimes[nzchar(runtimes)])
  if (length(available) == 0L) return(NULL)
  available[[1L]]
}

run_prefetch_query_plan_command <- function(runtime, cmd_args, env) {
  tryCatch(
    suppressWarnings(system2(runtime, cmd_args, stdout = TRUE, stderr = TRUE, env = env)),
    error = function(e) structure(character(0), status = 1L)
  )
}

resolve_prefetch_query_plan <- function(container_path, script_path, requested_spaces, templateflow_home,
                                        include_cifti_defaults = FALSE) {
  if (!checkmate::test_file_exists(container_path) || !checkmate::test_file_exists(script_path)) {
    return(NULL)
  }

  runtime <- find_container_runtime()
  if (!checkmate::test_string(runtime)) return(NULL)

  summary_dir <- tempfile("prefetch_plan_")
  dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(summary_dir, recursive = TRUE, force = TRUE), add = TRUE)

  summary_file <- file.path(summary_dir, "prefetch_summary.json")
  script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
  bind_dirs <- unique(c(
    script_dir,
    normalizePath(summary_dir, winslash = "/", mustWork = TRUE)
  ))
  bind_args <- unlist(lapply(bind_dirs, function(path) c("-B", paste0(path, ":", path))), use.names = FALSE)

  cmd_args <- c(
    "exec", "--cleanenv", "--containall",
    bind_args,
    normalizePath(container_path, winslash = "/", mustWork = TRUE),
    "python",
    normalizePath(script_path, winslash = "/", mustWork = TRUE),
    "--output-spaces", paste(requested_spaces, collapse = " "),
    "--plan-only",
    "--summary-json", summary_file
  )
  if (isTRUE(include_cifti_defaults)) {
    cmd_args <- c(cmd_args, "--include-cifti-defaults")
  }

  env <- c(
    TEMPLATEFLOW_HOME = normalizePath(templateflow_home, winslash = "/", mustWork = FALSE),
    APPTAINERENV_TEMPLATEFLOW_HOME = normalizePath(templateflow_home, winslash = "/", mustWork = FALSE)
  )

  out <- run_prefetch_query_plan_command(runtime, cmd_args, env)
  status <- attr(out, "status")
  if (!is.null(status) && !identical(status, 0L)) return(NULL)
  if (!checkmate::test_file_exists(summary_file)) return(NULL)

  plan <- tryCatch(
    jsonlite::fromJSON(summary_file, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(plan)) return(NULL)

  list(
    query_signature = if (!is.null(plan$query_signature)) plan$query_signature else NULL,
    query_count = if (!is.null(plan$query_count)) plan$query_count else NULL,
    queries = if (!is.null(plan$queries)) plan$queries else list()
  )
}

prefetch_state_covers_request <- function(state, requested_spaces, requested_query_signature, templateflow_home) {
  if (is.null(state)) return(FALSE)

  status <- if (!is.null(state$status)) toupper(trimws(as.character(state$status))) else ""
  if (!identical(status, "COMPLETED")) return(FALSE)

  state_tf_home <- if (!is.null(state$templateflow_home)) {
    normalizePath(as.character(state$templateflow_home), winslash = "/", mustWork = FALSE)
  } else {
    ""
  }
  current_tf_home <- normalizePath(templateflow_home, winslash = "/", mustWork = FALSE)
  if (!identical(state_tf_home, current_tf_home)) return(FALSE)

  if (!all(requested_spaces %in% state$spaces)) return(FALSE)

  state_query_signature <- if (!is.null(state$query_signature)) trimws(as.character(state$query_signature)) else ""
  if (!checkmate::test_string(requested_query_signature) || !nzchar(requested_query_signature)) return(FALSE)
  identical(state_query_signature, requested_query_signature)
}

prefetch_manifest_verified <- function(sqlite_db, templateflow_home, job_id = NULL, query_signature = NULL) {
  if (!checkmate::test_string(sqlite_db) || !checkmate::test_file_exists(sqlite_db)) {
    return(FALSE)
  }
  if (!checkmate::test_directory_exists(templateflow_home)) {
    return(FALSE)
  }

  con <- NULL
  on.exit(try(if (!is.null(con)) DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  record <- tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
    cols <- DBI::dbGetQuery(con, "PRAGMA table_info(job_tracking)")
    if (!"output_manifest" %in% cols$name) return(NULL)

    if (checkmate::test_string(job_id)) {
      DBI::dbGetQuery(
        con,
        "SELECT status, output_manifest
         FROM job_tracking
         WHERE job_name = ? AND job_id = ?
         ORDER BY time_submitted DESC
         LIMIT 1",
        params = list("prefetch_templates", as.character(job_id))
      )
    } else {
      DBI::dbGetQuery(
        con,
        "SELECT status, output_manifest
         FROM job_tracking
         WHERE job_name = ?
         ORDER BY time_submitted DESC
         LIMIT 1",
        params = list("prefetch_templates")
      )
    }
  }, error = function(e) NULL)

  if (is.null(record) || nrow(record) == 0L) return(FALSE)
  if (!identical(record$status[1], "COMPLETED")) return(FALSE)

  manifest_json <- record$output_manifest[1]
  if (is.null(manifest_json) || is.na(manifest_json) || !nzchar(manifest_json)) {
    return(FALSE)
  }

  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_json, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(manifest)) return(FALSE)
  if (checkmate::test_string(query_signature)) {
    manifest_signature <- if (!is.null(manifest$query_signature)) manifest$query_signature else NULL
    if (!identical(manifest_signature, query_signature)) return(FALSE)
  }

  verification <- verify_output_manifest(templateflow_home, manifest_json, check_mtime = FALSE)
  isTRUE(verification$verified)
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
  prefetch_state_file <- get_prefetch_state_file(scfg$metadata$log_directory, tf_home)
  legacy_prefetch_state_file <- get_legacy_prefetch_state_file(tf_home)

  spaces <- scfg$fmriprep$output_spaces
  if (isTRUE(steps["aroma"]) && (is.null(spaces) || !grepl("MNI152NLin6Asym:res-2", spaces, fixed = TRUE))) {
    spaces <- trimws(paste(spaces, "MNI152NLin6Asym:res-2"))
  }

  # make sure that at least fmriprep's default space is included
  if (is.null(spaces) || !nzchar(trimws(spaces))) spaces <- "MNI152NLin2009cAsym"

  # pull out non-template output spaces
  spaces_vec <- unique(strsplit(trimws(spaces), "\\s+")[[1]])
  skip_spaces <- c("anat", "fsnative", "fsaverage", "fsaverage5", "fsaverage6", "T1w", "T2w", "func")
  fetch_spaces <- normalize_prefetch_spaces(setdiff(spaces_vec, skip_spaces))
  if (length(fetch_spaces) == 0L) return(NULL)
  include_cifti_defaults <- fmriprep_cli_requests_cifti_defaults(scfg$fmriprep$cli_options)

  script_path <- system.file("prefetch_templateflow.py", package = "BrainGnomes")
  if (!checkmate::test_file_exists(script_path)) {
    warning("Cannot locate TemplateFlow prefetch helper script; skipping prefetch step.")
    return(NULL)
  }

  # preflight permission checks for project-level paths
  pf_issues <- c(
    check_write_target(scfg$metadata$log_directory, "log directory"),
    check_write_target(tf_home, "templateflow_home directory"),
    check_write_target(prefetch_state_file, "prefetch state file")
  )
  if (length(pf_issues) > 0L) {
    stop("Preflight permission check failed for prefetch_templates:\n",
         paste(paste0("  - ", pf_issues), collapse = "\n"), call. = FALSE)
  }

  migrate_prefetch_state_file(
    state_file = prefetch_state_file,
    legacy_state_file = legacy_prefetch_state_file,
    templateflow_home = tf_home
  )
  prefetch_state <- read_prefetch_state(prefetch_state_file)
  prefetch_plan <- resolve_prefetch_query_plan(
    container_path = container_path,
    script_path = script_path,
    requested_spaces = fetch_spaces,
    templateflow_home = tf_home,
    include_cifti_defaults = include_cifti_defaults
  )
  current_query_signature <- if (!is.null(prefetch_plan$query_signature)) {
    as.character(prefetch_plan$query_signature)
  } else {
    NULL
  }

  state_covers_request <- prefetch_state_covers_request(
    prefetch_state,
    fetch_spaces,
    current_query_signature,
    tf_home
  )
  if (state_covers_request) {
    state_job_id <- if (!is.null(prefetch_state$scheduler_job_id) && checkmate::test_string(prefetch_state$scheduler_job_id)) {
      prefetch_state$scheduler_job_id
    } else {
      NULL
    }
    manifest_ok <- prefetch_manifest_verified(
      sqlite_db = scfg$metadata$sqlite_db,
      templateflow_home = tf_home,
      job_id = state_job_id,
      query_signature = current_query_signature
    )

    if (manifest_ok) {
      message(glue::glue(
        "Skipping TemplateFlow prefetch: prior successful prefetch covers requested spaces and manifest verification passed in {tf_home}."
      ))
      return(NULL)
    }

    message(glue::glue(
      "Re-running TemplateFlow prefetch because prior manifest verification failed or files are missing in {tf_home}."
    ))
  } else if (!is.null(prefetch_state) && identical(
    toupper(trimws(as.character(if (!is.null(prefetch_state$status)) prefetch_state$status else ""))),
    "COMPLETED"
  )) {
    message(glue::glue(
      "Re-running TemplateFlow prefetch because cached state in {tf_home} does not match the current query set."
    ))
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
    prefetch_include_cifti_defaults = if (isTRUE(include_cifti_defaults)) "TRUE" else "FALSE",
    templateflow_home = tf_home,
    prefetch_state_file = prefetch_state_file,
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
  tracking_args <- enrich_job_tracking_args(
    scfg, tracking_args,
    stage = "prefetch_templates", job_role = "project",
    stdout_log = stdout_log, stderr_log = stderr_log
  )
  
  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args,
    env_variables = env_variables,
    echo = FALSE,
    tracking_sqlite_db = scfg$metadata$sqlite_db,
    tracking_args = tracking_args
  )

  log_submission_command(NULL, job_id, "prefetch_templates job")

  return(job_id)
}

ensure_aroma_output_space <- function(scfg, require_aroma = isTRUE(scfg$aroma$enable), verbose = TRUE) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  if (!isTRUE(require_aroma)) return(scfg)

  if (is.null(scfg$fmriprep$auto_added_aroma_space)) scfg$fmriprep$auto_added_aroma_space <- FALSE

  spaces <- validate_char(scfg$fmriprep$output_spaces, empty_value = NULL)
  has_required_space <- !is.null(spaces) && grepl("MNI152NLin6Asym:res-2", spaces, fixed = TRUE)
  if (has_required_space) {
    scfg$fmriprep$output_spaces <- spaces # persist normalized value
    return(scfg)
  }

  addition <- "MNI152NLin6Asym:res-2"
  default_space <- "MNI152NLin2009cAsym"
  blank <- is.null(spaces) || !nzchar(trimws(spaces))

  scfg$fmriprep$output_spaces <- if (blank) {
    paste(default_space, addition)
  } else {
    trimws(paste(spaces, addition))
  }

  if (isTRUE(verbose)) {
    if (blank) {
      message("No fmriprep output spaces specified. Using default MNI152NLin2009cAsym and adding MNI152NLin6Asym:res-2 so AROMA can run.")
    } else {
      message("Adding MNI152NLin6Asym:res-2 to output spaces for fmriprep to allow AROMA to run.")
    }
  }

  scfg$fmriprep$auto_added_aroma_space <- TRUE

  return(scfg)
}
