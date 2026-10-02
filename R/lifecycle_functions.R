# Shared lifecycle APIs used by the R interface and installed command-line tool.

supported_project_steps <- function() {
  c(
    "flywheel_sync", "bids_conversion", "mriqc", "fmriprep",
    "aroma", "postprocess", "extract_rois"
  )
}

project_config_from_input <- function(input = getwd()) {
  if (is.null(input)) input <- getwd()
  if (inherits(input, "bg_project_cfg")) return(input)
  if (checkmate::test_string(input)) {
    if (checkmate::test_directory_exists(input)) {
      config_file <- file.path(input, "project_config.yaml")
      if (!checkmate::test_file_exists(config_file)) {
        project_dir <- normalizePath(
          input, winslash = "/", mustWork = FALSE
        )
        stop(
          "No project_config.yaml found in project directory: ",
          project_dir,
          ". Supply a project configuration object, YAML file, or project directory.",
          call. = FALSE
        )
      }
    }
    return(load_project(input, validate = FALSE))
  }
  stop("input must be a bg_project_cfg object, YAML file, or project directory", call. = FALSE)
}

empty_issue_df <- function() {
  data.frame(
    severity = character(), code = character(), field = character(),
    message = character(), stringsAsFactors = FALSE
  )
}

#' Validate a BrainGnomes project configuration without changing it
#'
#' This optional inspection entry point is useful for scripts, continuous
#' integration, and configuration review. It is not required before
#' [run_project()], which retains its selected-stage checks. Unlike the historical
#' repair path in `validate_project()`, this function never opens the setup wizard
#' and never writes the configuration.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param quiet Suppress the printed validation summary.
#' @return A `bg_project_validation` object containing `valid`, `issues`,
#'   `messages`, and the parsed `config`.
#' @export
validate_project_config <- function(input = getwd(), quiet = FALSE) {
  checkmate::assert_flag(quiet)

  config_error <- NULL
  scfg <- tryCatch(
    project_config_from_input(input),
    error = function(e) {
      config_error <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(scfg)) {
    result <- structure(list(
      valid = FALSE,
      issues = data.frame(
        severity = "error", code = "config_unreadable", field = NA_character_,
        message = config_error, stringsAsFactors = FALSE
      ),
      messages = config_error,
      config = NULL
    ), class = "bg_project_validation")
    if (!quiet) print(result)
    return(result)
  }

  validation_error <- NULL
  messages <- utils::capture.output(
    valid <- tryCatch(
      validate_project(scfg, quiet = TRUE, correct_problems = FALSE),
      error = function(e) {
        validation_error <<- conditionMessage(e)
        FALSE
      }
    ),
    type = "message"
  )

  gaps <- unique(attr(valid, "gaps"))
  gaps <- gaps[!is.na(gaps) & nzchar(gaps)]
  issues <- empty_issue_df()
  if (length(gaps) > 0L) {
    issues <- data.frame(
      severity = rep("error", length(gaps)),
      code = rep("missing_or_invalid", length(gaps)),
      field = gaps,
      message = paste0("Missing or invalid configuration field: ", gaps),
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(validation_error)) {
    issues <- rbind(issues, data.frame(
      severity = "error", code = "validation_error", field = NA_character_,
      message = validation_error, stringsAsFactors = FALSE
    ))
  }

  result <- structure(list(
    valid = isTRUE(valid) && nrow(issues) == 0L,
    issues = issues,
    messages = unique(messages[nzchar(messages)]),
    config = scfg
  ), class = "bg_project_validation")
  if (!quiet) print(result)
  result
}

#' @export
print.bg_project_validation <- function(x, ...) {
  if (isTRUE(x$valid)) {
    cli::cli_alert_success("Project configuration is valid.")
  } else {
    cli::cli_alert_danger("Project configuration is invalid ({nrow(x$issues)} issue{?s}).")
    if (nrow(x$issues) > 0L) print(x$issues, row.names = FALSE)
  }
  invisible(x)
}

doctor_check_df <- function() {
  data.frame(
    category = character(), check = character(), status = character(),
    detail = character(), remedy = character(), stringsAsFactors = FALSE
  )
}

#' Run non-mutating project and runtime preflight checks
#'
#' This optional comprehensive preflight is useful on a new cluster, after the
#' submission environment changes, or before an expensive run. `doctor_project()`
#' checks configuration, scheduler commands, container runtime, enabled-stage
#' files, project storage, and the job-tracking database. It is not required
#' before [run_project()] and does not submit work, create directories, or modify
#' the configuration.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param steps Optional stages to check. By default all enabled stages are used.
#' @param deep Also initialize Python and check optional postprocessing modules.
#' @param quiet Suppress the printed report.
#' @return A `bg_project_doctor` object with an `ok` flag and a checks data frame.
#' @export
doctor_project <- function(input = getwd(), steps = NULL, deep = FALSE, quiet = FALSE) {
  checkmate::assert_character(steps, null.ok = TRUE)
  checkmate::assert_flag(deep)
  checkmate::assert_flag(quiet)

  validation <- validate_project_config(input, quiet = TRUE)
  scfg <- validation$config
  checks <- doctor_check_df()
  add_check <- function(category, check, status, detail, remedy = "") {
    checks[nrow(checks) + 1L, ] <<- list(category, check, status, detail, remedy)
  }

  add_check(
    "configuration", "project_config", if (validation$valid) "pass" else "fail",
    if (validation$valid) "Configuration schema and enabled-stage requirements are valid." else
      paste(nrow(validation$issues), "configuration issue(s) found."),
    if (validation$valid) "" else "Run `BrainGnomes config edit <project>` and validate again."
  )

  if (is.null(scfg)) {
    result <- structure(list(ok = FALSE, checks = checks, validation = validation),
      class = "bg_project_doctor")
    if (!quiet) print(result)
    return(result)
  }

  requested_error <- NULL
  if (is.null(steps)) {
    requested <- supported_project_steps()
    requested <- requested[vapply(
      requested, function(step) isTRUE(scfg[[step]]$enable), logical(1)
    )]
    if (length(requested) == 0L) {
      add_check(
        "configuration", "enabled_steps", "warn",
        "No processing stages are enabled; stage-specific runtime checks were skipped.",
        "Enable and configure at least one stage before planning a run."
      )
    }
  } else {
    requested <- tryCatch(
      resolve_project_steps(scfg, steps),
      error = function(e) {
        requested_error <<- conditionMessage(e)
        character()
      }
    )
  }
  if (!is.null(requested_error)) {
    add_check(
      "configuration", "requested_steps", "fail", requested_error,
      "Enable the requested stages or choose from the stages enabled in the project configuration."
    )
  }
  scheduler <- scfg$compute_environment$scheduler
  scheduler_commands <- switch(
    scheduler,
    slurm = c("sbatch", "squeue", "sacct", "scancel"),
    torque = c("qsub", "qstat", "qselect", "qdel"),
    sh = character(), local = character(), character()
  )
  if (length(scheduler_commands) == 0L && !scheduler %in% c("sh", "local")) {
    add_check("scheduler", "scheduler", "fail", paste0("Unsupported scheduler: ", scheduler),
      "Set compute_environment/scheduler to slurm or torque.")
  }
  for (command in scheduler_commands) {
    command_path <- Sys.which(command)
    add_check(
      "scheduler", command, if (nzchar(command_path)) "pass" else "fail",
      if (nzchar(command_path)) unname(command_path) else paste(command, "was not found on PATH."),
      if (nzchar(command_path)) "" else paste("Load or install the scheduler client providing", command, "on the submission host.")
    )
  }

  container_steps <- intersect(requested, c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess"))
  if (length(container_steps) > 0L) {
    singularity <- Sys.which("singularity")
    apptainer <- Sys.which("apptainer")
    runtime_status <- if (nzchar(singularity)) "pass" else if (nzchar(apptainer)) "warn" else "fail"
    runtime_detail <- if (nzchar(singularity)) unname(singularity) else if (nzchar(apptainer)) {
      paste0(unname(apptainer), " is available, but BrainGnomes workers invoke `singularity`.")
    } else "Neither singularity nor apptainer was found on PATH."
    add_check(
      "container", "runtime", runtime_status, runtime_detail,
      if (runtime_status == "pass") "" else "Provide a `singularity` compatibility command on compute nodes."
    )
  }

  path_checks <- list(
    project_directory = scfg$metadata$project_directory,
    log_directory = scfg$metadata$log_directory,
    scratch_directory = scfg$metadata$scratch_directory,
    bids_directory = scfg$metadata$bids_directory
  )
  for (label in names(path_checks)) {
    path <- path_checks[[label]]
    exists <- checkmate::test_directory_exists(path)
    writable <- exists && file.access(path, 2L) == 0L
    status <- if (!exists) "fail" else if (!writable) "fail" else "pass"
    detail <- if (!exists) {
      paste0(value_or_default(path, "<unset>"), " does not exist.")
    } else if (!writable) {
      paste0(path, " is not writable.")
    } else normalizePath(path, winslash = "/", mustWork = TRUE)
    add_check("storage", label, status, detail,
      if (status == "pass") "" else "Create the directory or correct ownership/permissions before submission.")
  }

  stage_files <- list(
    flywheel_sync = c(flywheel = scfg$compute_environment$flywheel),
    bids_conversion = c(
      heudiconv_container = scfg$compute_environment$heudiconv_container,
      heuristic_file = scfg$bids_conversion$heuristic_file
    ),
    mriqc = c(mriqc_container = scfg$compute_environment$mriqc_container),
    fmriprep = c(
      fmriprep_container = scfg$compute_environment$fmriprep_container,
      fs_license_file = scfg$fmriprep$fs_license_file
    ),
    aroma = c(aroma_container = scfg$compute_environment$aroma_container),
    postprocess = c(fsl_container = scfg$compute_environment$fsl_container)
  )
  for (stage in intersect(names(stage_files), requested)) {
    for (label in names(stage_files[[stage]])) {
      path <- unname(stage_files[[stage]][label])
      readable <- checkmate::test_file_exists(path) && file.access(path, 4L) == 0L
      add_check(
        stage, label, if (readable) "pass" else "fail",
        if (readable) {
          normalizePath(path, winslash = "/", mustWork = TRUE)
        } else {
          paste0(value_or_default(path, "<unset>"), " is missing or unreadable.")
        },
        if (readable) "" else paste("Configure a readable", label, "for", stage, ".")
      )
    }
  }

  sqlite_db <- scfg$metadata$sqlite_db
  if (checkmate::test_string(sqlite_db)) {
    db_parent <- dirname(sqlite_db)
    parent_ok <- dir.exists(db_parent) && file.access(db_parent, 2L) == 0L
    db_ok <- if (file.exists(sqlite_db)) {
      tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        DBI::dbGetQuery(con, "PRAGMA quick_check")[[1L]][1L] == "ok"
      }, error = function(e) FALSE)
    } else parent_ok
    add_check(
      "tracking", "sqlite", if (db_ok) "pass" else "fail",
      if (file.exists(sqlite_db)) sqlite_db else paste(sqlite_db, "will be created when jobs are submitted."),
      if (db_ok) "" else "Correct the database path, parent permissions, or SQLite integrity problem."
    )
  } else {
    add_check("tracking", "sqlite", "fail", "metadata/sqlite_db is not configured.",
      "Set metadata/sqlite_db to a writable project path.")
  }

  if (deep && "postprocess" %in% requested) {
    for (module in c("nibabel", "nilearn", "templateflow")) {
      available <- tryCatch(reticulate::py_module_available(module), error = function(e) FALSE)
      add_check(
        "python", module, if (available) "pass" else "warn",
        if (available) paste(module, "is importable.") else paste(module, "is not importable in the active reticulate Python."),
        if (available) "" else paste("Install", module, "when using template-dependent postprocessing.")
      )
    }
  }

  result <- structure(list(
    ok = !any(checks$status == "fail"), checks = checks,
    validation = validation, checked_steps = requested
  ), class = "bg_project_doctor")
  if (!quiet) print(result)
  result
}

#' @export
print.bg_project_doctor <- function(x, ...) {
  print(x$checks, row.names = FALSE)
  counts <- table(factor(x$checks$status, levels = c("pass", "warn", "fail")))
  if (isTRUE(x$ok)) {
    cli::cli_alert_success("Preflight passed: {counts[['pass']]} passed, {counts[['warn']]} warning{?s}.")
  } else {
    cli::cli_alert_danger("Preflight failed: {counts[['fail']]} failed, {counts[['warn']]} warning{?s}.")
  }
  invisible(x)
}

value_or_default <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[1L]) || !nzchar(as.character(x[1L]))) y else x
}

#' Write a project configuration without interactive prompts
#'
#' @param input A `bg_project_cfg` object.
#' @param file Destination YAML path. Defaults to `project_config.yaml` beneath
#'   the configured project directory.
#' @param overwrite Replace an existing file.
#' @return The configuration, invisibly, with its `yaml_file` attribute set.
#' @export
write_project_config <- function(input, file = NULL, overwrite = FALSE) {
  scfg <- project_config_from_input(input)
  checkmate::assert_flag(overwrite)
  if (is.null(file)) file <- file.path(scfg$metadata$project_directory, "project_config.yaml")
  checkmate::assert_string(file)
  file <- path.expand(file)
  if (file.exists(file) && !overwrite) stop("Configuration file already exists: ", file, call. = FALSE)
  if (!dir.exists(dirname(file))) stop("Configuration directory does not exist: ", dirname(file), call. = FALSE)
  temp <- tempfile("project-config-", tmpdir = dirname(file), fileext = ".yaml")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  payload <- as.list(scfg)
  payload$schema_version <- value_or_default(payload$schema_version, 1L)
  yaml::write_yaml(payload, temp)
  if (!file.rename(temp, file)) stop("Failed to atomically write configuration: ", file, call. = FALSE)
  attr(scfg, "yaml_file") <- normalizePath(file, winslash = "/", mustWork = TRUE)
  invisible(scfg)
}

resolve_project_steps <- function(scfg, steps = NULL) {
  supported <- supported_project_steps()
  if (is.null(steps)) {
    steps <- supported[vapply(supported, function(step) isTRUE(scfg[[step]]$enable), logical(1))]
  }
  steps <- unique(tolower(trimws(as.character(steps))))
  steps <- steps[!is.na(steps) & nzchar(steps)]
  if ("all" %in% steps) {
    steps <- supported[vapply(supported, function(step) isTRUE(scfg[[step]]$enable), logical(1))]
  }
  unknown <- setdiff(steps, supported)
  if (length(unknown) > 0L) {
    stop("Unknown processing step(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  disabled <- steps[!vapply(steps, function(step) isTRUE(scfg[[step]]$enable), logical(1))]
  if (length(disabled) > 0L) {
    stop("Requested step(s) are disabled: ", paste(disabled, collapse = ", "), call. = FALSE)
  }
  if (length(steps) == 0L) stop("No enabled processing steps were requested.", call. = FALSE)
  steps
}

discover_project_subjects <- function(scfg, steps, subject_filter = NULL, allow_empty = FALSE) {
  requested_subjects <- if (is.data.frame(subject_filter)) {
    as.character(subject_filter$sub_id)
  } else if (!is.null(subject_filter)) {
    as.character(subject_filter)
  } else {
    NULL
  }
  dicom <- data.frame(
    sub_id = character(), ses_id = character(), dicom_sub_dir = character(),
    dicom_ses_dir = character(), stringsAsFactors = FALSE
  )
  if ("bids_conversion" %in% steps && dir.exists(scfg$metadata$dicom_directory)) {
    dicom <- get_subject_dirs(
      scfg$metadata$dicom_directory,
      sub_regex = scfg$bids_conversion$sub_regex,
      sub_id_match = scfg$bids_conversion$sub_id_match,
      ses_regex = scfg$bids_conversion$ses_regex,
      ses_id_match = scfg$bids_conversion$ses_id_match,
      full.names = TRUE,
      subject_filter = requested_subjects
    )
    names(dicom) <- sub("(sub|ses)_dir", "dicom_\\1_dir", names(dicom))
  }

  bids <- data.frame(
    sub_id = character(), ses_id = character(), bids_sub_dir = character(),
    bids_ses_dir = character(), stringsAsFactors = FALSE
  )
  if (dir.exists(scfg$metadata$bids_directory)) {
    bids <- get_subject_dirs(
      scfg$metadata$bids_directory,
      sub_regex = "^sub-.+", ses_regex = "^ses-.+",
      sub_id_match = "sub-(.*)", ses_id_match = "ses-(.*)", full.names = TRUE,
      subject_filter = requested_subjects
    )
    names(bids) <- sub("(sub|ses)_dir", "bids_\\1_dir", names(bids))
  }

  subjects <- merge(dicom, bids, by = c("sub_id", "ses_id"), all = TRUE)
  if (!is.null(subject_filter)) {
    if (is.data.frame(subject_filter)) {
      checkmate::assert_names(names(subject_filter), must.include = "sub_id")
      by_cols <- intersect(c("sub_id", "ses_id"), names(subject_filter))
      subjects <- merge(subjects, subject_filter[, by_cols, drop = FALSE], by = by_cols)
    } else {
      subjects <- subjects[subjects$sub_id %in% as.character(subject_filter), , drop = FALSE]
    }
  }
  if (nrow(subjects) == 0L && !allow_empty) {
    stop("No subject/session inputs match the requested run.", call. = FALSE)
  }
  subjects
}

resolve_project_selection <- function(scfg, steps, postprocess_streams = NULL,
                                      extract_streams = NULL, force = FALSE) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_character(steps, null.ok = TRUE)
  checkmate::assert_character(postprocess_streams, null.ok = TRUE)
  checkmate::assert_character(extract_streams, null.ok = TRUE)
  checkmate::assert_flag(force)

  resolved_steps <- resolve_project_steps(scfg, steps)
  available_postprocess <- get_postprocess_stream_names(scfg)
  available_extract <- get_extract_stream_names(scfg)
  checkmate::assert_subset(
    postprocess_streams, available_postprocess, empty.ok = TRUE
  )
  checkmate::assert_subset(
    extract_streams, available_extract, empty.ok = TRUE
  )

  if ("postprocess" %in% resolved_steps) {
    if (length(available_postprocess) == 0L) {
      stop(
        "Cannot run postprocessing without at least one postprocess configuration.",
        call. = FALSE
      )
    }
    if (is.null(postprocess_streams)) {
      postprocess_streams <- available_postprocess
    }
  } else {
    postprocess_streams <- character()
  }

  if ("extract_rois" %in% resolved_steps) {
    if (length(available_extract) == 0L) {
      stop(
        "Cannot run extraction without at least one extract_rois configuration.",
        call. = FALSE
      )
    }
    if (is.null(extract_streams)) extract_streams <- available_extract
  } else {
    extract_streams <- character()
  }

  step_flags <- stats::setNames(
    supported_project_steps() %in% resolved_steps,
    supported_project_steps()
  )
  structure(list(
    steps = resolved_steps,
    step_flags = step_flags,
    postprocess_streams = postprocess_streams,
    extract_streams = extract_streams,
    force = force
  ), class = "bg_project_selection")
}

resolve_project_execution <- function(scfg, steps, subject_filter = NULL,
                                      postprocess_streams = NULL,
                                      extract_streams = NULL, force = FALSE) {
  checkmate::assert(
    checkmate::check_character(
      subject_filter, any.missing = FALSE, null.ok = TRUE
    ),
    checkmate::check_data_frame(subject_filter, null.ok = TRUE)
  )
  selection <- resolve_project_selection(
    scfg, steps, postprocess_streams, extract_streams, force
  )
  work_units <- normalize_retry_work_units(attr(scfg, "retry_work_units", exact = TRUE))
  if (!is.null(work_units) &&
      (!setequal(selection$steps, work_units$stage) ||
       !setequal(selection$postprocess_streams, work_units$stream[work_units$stage == "postprocess"]) ||
       !setequal(selection$extract_streams, work_units$stream[work_units$stage == "extract_rois"]))) {
    stop("Run selection does not match the exact retry scope.", call. = FALSE)
  }
  subject_steps <- setdiff(selection$steps, "flywheel_sync")
  scope_deferred <-
    "flywheel_sync" %in% selection$steps && length(subject_steps) > 0L
  subjects <- if (length(subject_steps) > 0L) {
    discover_project_subjects(
      scfg, selection$steps, subject_filter,
      allow_empty = scope_deferred
    )
  } else {
    data.frame(
      sub_id = character(), ses_id = character(),
      dicom_sub_dir = character(), dicom_ses_dir = character(),
      bids_sub_dir = character(), bids_ses_dir = character(),
      stringsAsFactors = FALSE
    )
  }

  subjects <- retry_subject_scope(subjects, work_units, allow_missing = scope_deferred)
  structure(c(unclass(selection), list(
    work_units = work_units,
    subject_filter = subject_filter,
    subjects = subjects,
    scope_deferred = scope_deferred,
    scope_status = if (scope_deferred) "deferred" else "resolved",
    deferred_reasons = if (scope_deferred) "flywheel_sync" else character()
  )), class = "bg_project_execution")
}

stage_resource <- function(scfg, stage, stream = NA_character_) {
  cfg <- if (stage == "fsaverage_setup") {
    list(ncores = 1, memgb = 8, nhours = 0.15)
  } else if (stage == "prefetch_templates") {
    list(ncores = 1, memgb = 16, nhours = 0.5)
  } else if (stage == "postprocess" && !is.na(stream)) {
    scfg$postprocess[[stream]]
  } else if (stage == "extract_rois" && !is.na(stream)) {
    scfg$extract_rois[[stream]]
  } else scfg[[stage]]
  c(
    ncores = as.numeric(value_or_default(cfg$ncores, NA_real_)),
    memgb = as.numeric(value_or_default(cfg$memgb, NA_real_)),
    nhours = as.numeric(value_or_default(cfg$nhours, NA_real_))
  )
}

build_project_jobs <- function(scfg, execution) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_class(execution, "bg_project_execution")
  resolved_steps <- execution$steps
  postprocess_streams <- execution$postprocess_streams
  extract_streams <- execution$extract_streams
  subjects <- execution$subjects
  n_subjects <- length(unique(subjects$sub_id))
  n_sessions <- nrow(subjects)
  scope_count <- function(stage, stream = NA_character_) {
    if (!is.null(execution$work_units)) {
      units <- execution$work_units
      rows <- units$stage == stage
      if (!is.na(stream)) rows <- rows & !is.na(units$stream) & units$stream == stream
      return(sum(rows))
    }
    if (execution$scope_deferred && stage != "flywheel_sync") {
      return(NA_integer_)
    }
    if (stage %in% c("mriqc", "fmriprep", "aroma")) n_subjects else n_sessions
  }

  jobs <- data.frame(
    stage = character(), stream = character(), scope = character(),
    n_jobs = integer(), depends_on = character(), ncores = numeric(),
    memgb = numeric(), nhours = numeric(), stringsAsFactors = FALSE
  )
  add_job <- function(stage, stream = NA_character_, scope, n_jobs,
                      depends_on = "") {
    resource <- stage_resource(scfg, stage, stream)
    jobs[nrow(jobs) + 1L, ] <<- list(
      stage, stream, scope, as.integer(n_jobs), depends_on,
      resource[["ncores"]], resource[["memgb"]], resource[["nhours"]]
    )
  }
  if ("flywheel_sync" %in% resolved_steps) {
    add_job("flywheel_sync", scope = "project", n_jobs = 1L)
  }
  if ("fmriprep" %in% resolved_steps) {
    add_job("fsaverage_setup", scope = "project", n_jobs = 1L)
  }
  if (length(intersect(
    resolved_steps, c("mriqc", "fmriprep", "aroma")
  )) > 0L) {
    add_job("prefetch_templates", scope = "project", n_jobs = 1L)
  }
  for (stage in intersect(
    c("bids_conversion", "mriqc", "fmriprep", "aroma"), resolved_steps
  )) {
    dependency_candidates <- switch(stage,
      bids_conversion = if ("flywheel_sync" %in% resolved_steps) {
        "flywheel_sync"
      } else "",
      mriqc = "bids_conversion",
      fmriprep = "bids_conversion",
      aroma = "fmriprep"
    )
    dependency <- paste(
      intersect(dependency_candidates, resolved_steps), collapse = ","
    )
    if (stage %in% c("mriqc", "fmriprep", "aroma")) {
      dependency <- paste(
        c(dependency, "prefetch_templates")[
          nzchar(c(dependency, "prefetch_templates"))
        ],
        collapse = ","
      )
    }
    if (stage == "fmriprep") {
      dependency <- paste(
        c(dependency, "fsaverage_setup")[
          nzchar(c(dependency, "fsaverage_setup"))
        ],
        collapse = ","
      )
    }
    add_job(
      stage,
      scope = if (stage == "bids_conversion") "session" else "subject",
      n_jobs = scope_count(stage),
      depends_on = dependency
    )
  }
  if ("postprocess" %in% resolved_steps) {
    for (stream in postprocess_streams) {
      deps <- intersect(c("fmriprep", "aroma"), resolved_steps)
      add_job(
        "postprocess", stream, "session", scope_count("postprocess", stream),
        paste(deps, collapse = ",")
      )
    }
  }
  if ("extract_rois" %in% resolved_steps) {
    for (stream in extract_streams) {
      deps <- intersect("postprocess", resolved_steps)
      add_job(
        "extract_rois", stream, "session", scope_count("extract_rois", stream),
        paste(deps, collapse = ",")
      )
    }
  }
  jobs
}

#' Inspect or persist the resolved project execution model
#'
#' `plan_project()` is optional inspection and automation tooling. It records a
#' request: the stages, streams, known subject/session scope, resources,
#' dependencies, and implicit setup work resolved at planning time. It is not a
#' pre-rendered scheduler job list. When Flywheel synchronization can add data,
#' the plan reports deferred scope and the run records the realized subjects
#' after synchronization. Each actual scheduler submission is sealed separately
#' in a job manifest. [run_project()] resolves the same request model internally,
#' so creating or submitting a plan is not required for a direct run.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param steps Pipeline stages or `"all"`.
#' @param subject_filter Optional subject IDs or a data frame with `sub_id` and
#'   optionally `ses_id`.
#' @param postprocess_streams Optional postprocessing streams.
#' @param extract_streams Optional ROI-extraction streams.
#' @param force Include work whose completion markers would otherwise skip it.
#' @param allow_invalid Build the plan despite configuration validation errors.
#' @param quiet Suppress the printed plan.
#' @return A serializable `bg_project_plan` object. Its `preview$work` table
#'   lists concrete subject/session/stage/stream units, input and output roots,
#'   log locations, dependencies, resources, and current completion-marker
#'   decisions. Console output is bounded; the table retains the full selection.
#'   Deferred scope stays unknown until sync. Counts describe requested work
#'   units, not runtime or cost estimates or an exact scheduler job count.
#' @seealso [run_project()] for the standard direct execution path.
#' @export
plan_project <- function(input = getwd(), steps = "all", subject_filter = NULL,
                         postprocess_streams = NULL, extract_streams = NULL,
                         force = FALSE, allow_invalid = FALSE, quiet = FALSE) {
  checkmate::assert_flag(force)
  checkmate::assert_flag(allow_invalid)
  checkmate::assert_flag(quiet)
  validation <- validate_project_config(input, quiet = TRUE)
  if (!validation$valid && !allow_invalid) {
    stop("Project configuration is invalid. Run validate_project_config() or `BrainGnomes config validate` for details.", call. = FALSE)
  }
  scfg <- validation$config
  execution <- resolve_project_execution(
    scfg, steps, subject_filter, postprocess_streams, extract_streams, force
  )
  result <- project_plan_from_execution(scfg, execution, validation[c("valid", "issues", "messages")])
  if (!quiet) print(result)
  result
}

#' Build a reusable preview from an already resolved request
#' @param scfg Project configuration used to resolve the request.
#' @param execution Resolved stage, stream, and subject/session selections.
#' @param validation Optional configuration validation report; NULL for direct dry runs.
#' @return A bg_project_plan without performing discovery again or writing files.
#' @noRd
project_plan_from_execution <- function(scfg, execution, validation = NULL) {
  jobs <- build_project_jobs(scfg, execution)
  structure(list(
    # Older packages must reject exact retry plans rather than ignore their scope.
    schema_version = if (is.null(execution$work_units)) "brain-gnomes-plan-v1" else "brain-gnomes-plan-v2",
    plan_id = uuid::UUIDgenerate(),
    created_at = as.character(Sys.time()),
    config_file = attr(scfg, "yaml_file"),
    config = scfg,
    provenance_context = attr(scfg, "provenance_context", exact = TRUE),
    validation = validation,
    request = list(
      steps = execution$steps, subject_filter = execution$subject_filter,
      postprocess_streams = execution$postprocess_streams, extract_streams = execution$extract_streams,
      force = execution$force, work_units = execution$work_units
    ),
    subjects = execution$subjects,
    scope_deferred = execution$scope_deferred,
    scope_status = execution$scope_status,
    deferred_reasons = execution$deferred_reasons,
    jobs = jobs,
    preview = build_project_preview(scfg, execution, jobs)
  ), class = "bg_project_plan")
}

#' @export
print.bg_project_plan <- function(x, ...) {
  cli::cli_h2("BrainGnomes execution plan {.val {x$plan_id}}")
  cli::cli_text("Steps: {paste(x$request$steps, collapse = ', ')}")
  if (isTRUE(x$scope_deferred)) {
    cli::cli_alert_info(
      "Subject discovery is deferred until {paste(x$deferred_reasons, collapse = ', ')} completes."
    )
  } else {
    cli::cli_text("Scope: {length(unique(x$subjects$sub_id))} subject{?s}, {nrow(x$subjects)} subject/session row{?s}.")
  }
  print(x$jobs, row.names = FALSE)
  if (!is.null(x$request$work_units)) {
    cli::cli_text("Exact retry scope (setup dependencies are listed above):")
    print(utils::head(x$request$work_units, 20L), row.names = FALSE)
    if (nrow(x$request$work_units) > 20L) {
      cli::cli_text("Showing 20 rows; inspect plan$request$work_units for the full retry selection.")
    }
  }
  if (!is.null(x$preview)) print_project_preview(x$preview)
  invisible(x)
}

#' Save an execution plan to YAML
#' @param plan A `bg_project_plan` object.
#' @param file Destination YAML path.
#' @param overwrite Replace an existing file.
#' @return The normalized output path, invisibly.
#' @export
write_project_plan <- function(plan, file, overwrite = FALSE) {
  checkmate::assert_class(plan, "bg_project_plan")
  checkmate::assert_string(file)
  checkmate::assert_flag(overwrite)
  if (file.exists(file) && !overwrite) stop("Plan file already exists: ", file, call. = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("project-plan-", tmpdir = dirname(file), fileext = ".yaml")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  payload <- unclass(plan)
  payload$config <- as.list(plan$config)
  yaml::write_yaml(payload, temp)
  if (!file.rename(temp, file)) stop("Failed to atomically write plan file: ", file, call. = FALSE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

#' Read a saved execution plan
#' @details Ordinary plans use schema v1; exact retry plans use v2 so older
#'   BrainGnomes versions cannot silently ignore their work-unit restrictions.
#' @param file YAML plan path.
#' @return A `bg_project_plan` object.
#' @export
read_project_plan <- function(file) {
  checkmate::assert_file_exists(file)
  source_file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  plan <- yaml::read_yaml(file)
  if (!checkmate::test_choice(plan$schema_version, c("brain-gnomes-plan-v1", "brain-gnomes-plan-v2"))) {
    stop("Unsupported project plan schema: ", value_or_default(plan$schema_version, "<missing>"), call. = FALSE)
  }
  class(plan$config) <- unique(c("bg_project_cfg", class(plan$config)))
  plan$subjects <- as.data.frame(plan$subjects, stringsAsFactors = FALSE)
  plan$jobs <- as.data.frame(plan$jobs, stringsAsFactors = FALSE)
  if (!is.null(plan$preview$work)) {
    plan$preview$work <- as.data.frame(plan$preview$work, stringsAsFactors = FALSE)
  }
  plan$request$work_units <- normalize_retry_work_units(plan$request$work_units)
  if (identical(plan$schema_version, "brain-gnomes-plan-v2") && is.null(plan$request$work_units)) {
    stop("An exact retry plan must include work_units.", call. = FALSE)
  }
  if (is.null(plan$scope_status)) {
    plan$scope_status <- if (isTRUE(plan$scope_deferred)) "deferred" else "resolved"
  }
  if (is.null(plan$deferred_reasons)) {
    plan$deferred_reasons <- if (isTRUE(plan$scope_deferred)) {
      "flywheel_sync"
    } else {
      character()
    }
  }
  class(plan) <- "bg_project_plan"
  attr(plan, "source_file") <- source_file
  plan
}

#' Submit a saved or in-memory request plan
#'
#' The saved configuration, requested stages and streams, and resolved subject
#' scope are reused. Deferred scope is discovered after Flywheel synchronization.
#' Retry plans also retain the exact work units and source-run provenance.
#' The plan itself is not the final scheduler contract: BrainGnomes writes an
#' immutable manifest immediately before each job is submitted and a runtime
#' receipt when that job starts.
#' @param plan A `bg_project_plan` object or YAML plan path.
#' @param debug Enable debug submission mode.
#' @param log_level Pipeline log threshold.
#' @return A `bg_project_run` object returned by [run_project()].
#' @export
submit_project_plan <- function(plan, debug = FALSE, log_level = "INFO") {
  if (checkmate::test_string(plan)) plan <- read_project_plan(plan)
  checkmate::assert_class(plan, "bg_project_plan")
  request <- plan$request
  if (identical(plan$schema_version, "brain-gnomes-plan-v2") && is.null(request$work_units)) {
    stop("An exact retry plan must include work_units.", call. = FALSE)
  }
  planned_subjects <- request$subject_filter
  if (is.list(planned_subjects) && !is.data.frame(planned_subjects)) {
    planned_subjects <- if ("sub_id" %in% names(planned_subjects)) {
      as.data.frame(planned_subjects, stringsAsFactors = FALSE)
    } else {
      unlist(planned_subjects, use.names = FALSE)
    }
  }
  if (!isTRUE(plan$scope_deferred) && nrow(plan$subjects) > 0L) {
    subject_columns <- intersect(c("sub_id", "ses_id"), names(plan$subjects))
    planned_subjects <- plan$subjects[, subject_columns, drop = FALSE]
  }
  scfg <- plan$config
  attr(scfg, "retry_work_units") <- normalize_retry_work_units(request$work_units)
  context <- plan$provenance_context
  if (is.null(context)) context <- list()
  attr(scfg, "provenance_context") <- utils::modifyList(context, list(
    interface = if (checkmate::test_string(attr(plan, "source_file"))) {
      "saved_plan"
    } else {
      "in_memory_plan"
    },
    plan_id = plan$plan_id,
    plan_created_at = plan$created_at,
    plan_file = attr(plan, "source_file", exact = TRUE)
  ))
  run_project(
    scfg,
    steps = unlist(request$steps, use.names = FALSE),
    subject_filter = planned_subjects,
    postprocess_streams = unlist(request$postprocess_streams, use.names = FALSE),
    extract_streams = unlist(request$extract_streams, use.names = FALSE),
    force = isTRUE(request$force), debug = debug, log_level = log_level
  )
}

new_project_run <- function(scfg, run_id, submitted_ids = NULL, deferred = FALSE,
                            provenance_file = NULL) {
  tracked_ids <- character()
  sqlite_db <- scfg$metadata$sqlite_db
  if (checkmate::test_file_exists(sqlite_db) && sqlite_table_exists(sqlite_db, "job_tracking")) {
    tracked <- tryCatch(
      get_tracked_job_status(sequence_id = run_id, sqlite_db = sqlite_db),
      error = function(e) NULL
    )
    if (is.data.frame(tracked) && nrow(tracked) > 0L) {
      tracked_ids <- as.character(tracked$job_id)
    }
  }
  run <- structure(list(
    run_id = run_id,
    job_ids = unique(c(as.character(submitted_ids), tracked_ids)),
    submitted_at = as.character(Sys.time()),
    deferred_subject_submission = isTRUE(deferred),
    project_directory = scfg$metadata$project_directory,
    sqlite_db = sqlite_db,
    provenance_file = provenance_file
  ), class = "bg_project_run")
  tryCatch(
    update_run_provenance_submission(
      scfg, run_id, submitted_ids = run$job_ids, deferred = deferred
    ),
    error = function(e) warning(
      "Run was submitted, but its provenance record could not be finalized: ",
      conditionMessage(e), call. = FALSE
    )
  )
  run
}

#' @export
print.bg_project_run <- function(x, ...) {
  cli::cli_alert_success("Submitted BrainGnomes run {.val {x$run_id}}.")
  cli::cli_text("Tracked scheduler jobs: {length(x$job_ids)}")
  if (isTRUE(x$deferred_subject_submission)) {
    cli::cli_alert_info("Subject discovery and submission are deferred until Flywheel synchronization completes.")
  }
  if (checkmate::test_string(x$provenance_file)) {
    cli::cli_text("Provenance: {.file {x$provenance_file}}")
  }
  invisible(x)
}

resolve_run_id <- function(scfg, run_id = "latest") {
  if (!identical(run_id, "latest")) return(as.character(run_id))
  provenance_id <- latest_run_provenance_id(scfg)
  if (checkmate::test_string(provenance_id)) return(provenance_id)
  sqlite_db <- scfg$metadata$sqlite_db
  if (!checkmate::test_file_exists(sqlite_db) || !sqlite_table_exists(sqlite_db, "job_tracking")) {
    stop("No job-tracking database is available for this project.", call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  latest <- DBI::dbGetQuery(con,
    "SELECT sequence_id FROM job_tracking WHERE sequence_id IS NOT NULL ORDER BY datetime(time_submitted) DESC, id DESC LIMIT 1")
  if (nrow(latest) == 0L) stop("No tracked runs were found.", call. = FALSE)
  latest$sequence_id[[1L]]
}

#' List submitted runs for a project
#'
#' `get_project_runs()` is superseded by `inspect_project()$runs`. It remains
#' available as a compatibility wrapper for code that needs only the run table.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @return A data frame with one row per run, including its ID, submission and
#'   end times, number of tracked jobs, and overall status.
#' @examples
#' \dontrun{
#' runs <- get_project_runs(scfg)
#' run_id <- runs$run_id[[1L]]
#' }
#' @seealso [inspect_project()] for project and run-level progress.
#' @export
get_project_runs <- function(input = getwd()) {
  .Deprecated("inspect_project", package = "BrainGnomes")
  .get_project_runs_data(input)
}

#' Inspect the jobs submitted for one project run
#'
#' `get_run_jobs()` is superseded by `inspect_project(input, run_id)$jobs`. It
#' remains available as a compatibility wrapper and returns a data-frame
#' subclass with compact printing.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param run_id Run ID returned by [run_project()] or listed in
#'   `inspect_project(input)$runs`. Use `"latest"` for the most recently
#'   recorded run.
#' @return The tracking rows for the run.
#' @examples
#' \dontrun{
#' jobs <- get_run_jobs(scfg, run$run_id)
#' jobs[, c("job_id", "job_name", "status")]
#' }
#' @seealso [inspect_project()] for summarized progress, [diagnose_project()] for a failure-focused summary, and
#'   [find_run_logs()] for log-file locations.
#' @export
get_run_jobs <- function(input = getwd(), run_id = "latest") {
  .Deprecated("inspect_project", package = "BrainGnomes")
  .get_run_jobs_data(input, run_id)
}

#' Find output and error logs for one run
#'
#' Matches tracked job IDs to scheduler output (`.out`) and error (`.err`) files
#' beneath the configured log directory. It returns file locations without
#' opening or changing the logs.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param run_id Run ID returned by [run_project()] or listed in
#'   `inspect_project(input)$runs`. Use `"latest"` for the most recently
#'   recorded run.
#' @param failed_only If `TRUE`, include only failed, cancelled, or downstream
#'   jobs that could not run because an earlier job failed.
#' @return A data frame mapping jobs to stdout/stderr log files.
#' @examples
#' \dontrun{
#' failed_logs <- find_run_logs(scfg, run$run_id, failed_only = TRUE)
#' failed_logs[, c("job_name", "status", "type", "path")]
#' }
#' @seealso [diagnose_project()] for a run summary that includes these logs.
#' @export
find_run_logs <- function(input = getwd(), run_id = "latest", failed_only = FALSE) {
  checkmate::assert_flag(failed_only)
  scfg <- project_config_from_input(input)
  jobs <- .get_run_jobs_data(scfg, run_id)
  if (failed_only) jobs <- jobs[jobs$status %in% c("FAILED", "FAILED_BY_EXT", "CANCELLED"), , drop = FALSE]
  find_logs_for_jobs(scfg, jobs)
}

find_logs_for_jobs <- function(scfg, jobs) {
  files <- if (dir.exists(scfg$metadata$log_directory)) {
    list.files(scfg$metadata$log_directory, recursive = TRUE, full.names = TRUE, pattern = "\\.(out|err)$")
  } else character()
  rows <- lapply(seq_len(nrow(jobs)), function(i) {
    job_id <- as.character(jobs$job_id[[i]])
    matches <- files[grepl(job_id, basename(files), fixed = TRUE)]
    if (length(matches) == 0L) return(NULL)
    data.frame(
      run_id = jobs$sequence_id[[i]], job_id = job_id,
      job_name = jobs$job_name[[i]], status = jobs$status[[i]],
      type = ifelse(grepl("\\.err$", matches), "stderr", "stdout"),
      path = matches, stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(data.frame(
    run_id = character(), job_id = character(), job_name = character(),
    status = character(), type = character(), path = character(), stringsAsFactors = FALSE
  ))
  do.call(rbind, rows)
}

build_project_diagnosis <- function(input, run_id = NULL,
                                     subject_id = NULL, job_id = NULL) {
  subject_id <- normalize_inspection_subject_id(subject_id)
  job_id <- diagnosis_job_id(job_id)
  if (inherits(input, "bg_project_inspection") && is.null(subject_id)) {
    subject_id <- normalize_inspection_subject_id(input$subject_id)
  }
  inspection <- if (inherits(input, "bg_project_inspection")) {
    if (!is.null(run_id)) {
      stop("run_id cannot be supplied when input is already a project inspection.", call. = FALSE)
    }
    input
  } else {
    inspect_project(input, run_id = run_id, subject_id = subject_id)
  }
  scfg <- if (inherits(input, "bg_project_inspection")) {
    structure(list(
      metadata = list(
        project_directory = input$project_directory,
        log_directory = input$log_directory
      )
    ), class = "bg_project_cfg")
  } else {
    project_config_from_input(input)
  }

  jobs <- inspection$jobs
  if (!is.null(job_id)) {
    keep <- !is.na(jobs$job_id) & as.character(jobs$job_id) == job_id
    if (!any(keep)) {
      run_text <- if (is.null(run_id)) "" else paste0(" in run ", inspection$run_id)
      stop("No tracked job has ID ", job_id, run_text, ".", call. = FALSE)
    }
    jobs <- jobs[keep, , drop = FALSE]
  }
  if (!is.null(subject_id)) {
    keep <- !is.na(jobs$sub_id) & jobs$sub_id == subject_id
    if (!any(keep)) {
      focus_text <- if (is.null(job_id)) "" else paste0(" for job ", job_id)
      stop("No tracked jobs were found for sub-", subject_id, focus_text, ".", call. = FALSE)
    }
    jobs <- jobs[keep, , drop = FALSE]
  }

  current <- if (!is.null(job_id)) {
    rep(TRUE, nrow(jobs))
  } else if (identical(inspection$scope, "project")) {
    !is.na(jobs$is_current_attempt) & jobs$is_current_attempt
  } else {
    rep(TRUE, nrow(jobs))
  }
  failures <- as.data.frame(
    jobs[
      current & jobs$lifecycle_status %in% diagnosis_problem_statuses,
      , drop = FALSE
    ]
  )
  log_jobs <- if (is.null(job_id)) failures else jobs
  logs <- find_logs_for_jobs(scfg, log_jobs)
  structure(list(
    scope = inspection$scope,
    run_id = inspection$run_id,
    focus = list(subject_id = subject_id, job_id = job_id),
    inspection = inspection,
    jobs = jobs,
    failures = failures,
    logs = logs
  ), class = "bg_project_diagnosis")
}

#' Diagnose failed project work
#'
#' Reports unresolved failed, cancelled, and blocked work and locates matching
#' logs. In an interactive R session, the default opens the guided dependency
#' and log browser. In a non-interactive session, the default returns a
#' structured diagnosis of the current project state assembled by
#' [inspect_project()] across runs. Select a run for a historical post-mortem or
#' set `interactive` explicitly when behavior must not depend on the session.
#'
#' @param input A project configuration object, YAML file, project directory, or
#'   a `bg_project_inspection` object returned by [inspect_project()]. Defaults
#'   to the current working directory.
#' @param run_id Optional run ID. Use `NULL` for current project failures across
#'   runs, `"latest"` for the newest run, or an explicit historical run ID.
#' @param subject_id Optional subject identifier, with or without the `sub-`
#'   prefix. Restricts both structured and interactive diagnosis to that
#'   subject while preserving the selected run scope.
#' @param job_id Optional scheduler job identifier. Opens or returns that exact
#'   tracked job; this can select a historical job when `run_id` is `NULL`.
#' @param interactive If `TRUE`, open the guided interactive browser. If
#'   `FALSE`, return a structured diagnosis. The default is
#'   `base::interactive()`, so console users get the guided browser while
#'   scripts, tests, and reports get structured output. Interactive mode retains
#'   the behavior formerly provided by [diagnose_pipeline()].
#' @return When `interactive = FALSE`, a `bg_project_diagnosis` object containing
#'   the underlying `inspection`, its `jobs`, unresolved `failures`, and matching
#'   `logs` for the current project state or selected run. Interactive mode
#'   returns the selected result from the guided browser, usually invisibly.
#' @examples
#' \dontrun{
#' diagnose_project(scfg) # guided browser in an interactive R session
#'
#' diagnosis <- diagnose_project(scfg, subject_id = "014", interactive = FALSE)
#' diagnosis$failures
#' diagnosis$logs
#'
#' diagnose_project(scfg, job_id = "66273010")
#'
#' old_run <- diagnose_project(scfg, run$run_id, interactive = FALSE)
#' }
#' @seealso [inspect_project()] for routine progress monitoring and
#'   [retry_project_run()] to preview a new run after correcting a failure.
#' @export
diagnose_project <- function(input = getwd(), run_id = NULL,
                             interactive = base::interactive(),
                             subject_id = NULL, job_id = NULL) {
  checkmate::assert_flag(interactive)
  if (interactive) {
    if (inherits(input, "bg_project_inspection")) {
      stop("Interactive diagnosis requires a project configuration, YAML file, or directory.", call. = FALSE)
    }
    return(run_interactive_diagnosis(
      project_config_from_input(input), run_id = run_id,
      subject_id = subject_id, job_id = job_id
    ))
  }
  build_project_diagnosis(
    input, run_id = run_id, subject_id = subject_id, job_id = job_id
  )
}

#' @export
print.bg_project_diagnosis <- function(x, ...) {
  focus <- x$focus
  if (is.null(focus)) focus <- list(subject_id = NULL, job_id = NULL)
  if (!is.null(focus$job_id)) {
    cli::cli_h2("Diagnosis for job {.val {focus$job_id}}")
  } else if (!is.null(focus$subject_id)) {
    cli::cli_h2("Diagnosis for sub-{focus$subject_id}")
  } else if (identical(x$scope, "project")) {
    cli::cli_h2("Current project diagnosis")
  } else {
    cli::cli_h2("Run diagnosis {.val {x$run_id}}")
  }
  current <- if (!is.null(focus$job_id)) {
    rep(TRUE, nrow(x$jobs))
  } else if (identical(x$scope, "project")) {
    !is.na(x$jobs$is_current_attempt) & x$jobs$is_current_attempt
  } else {
    rep(TRUE, nrow(x$jobs))
  }
  counts <- as.data.frame(table(x$jobs$lifecycle_status[current]), stringsAsFactors = FALSE)
  names(counts) <- c("status", "n_jobs")
  print(counts, row.names = FALSE)
  if (nrow(x$failures) > 0L) {
    cli::cli_alert_danger("{nrow(x$failures)} failed, blocked, or cancelled job{?s}.")
    print(x$failures[, intersect(c("job_id", "job_name", "status", "time_ended"), names(x$failures)), drop = FALSE], row.names = FALSE)
  } else {
    cli::cli_alert_success("No failed jobs were found.")
  }
  invisible(x)
}

#' Cancel queued or running jobs from one run
#'
#' Cancellation affects only tracked jobs that are still queued or running. It
#' does not delete project data, logs, or completed outputs. Calling this
#' function with `dry_run = FALSE` immediately sends cancellation requests to
#' the configured scheduler, so preview the commands first.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param run_id Run ID returned by [run_project()] or listed in
#'   `inspect_project(input)$runs`. An explicit ID is recommended for
#'   cancellation.
#' @param dry_run If `TRUE`, report the commands without contacting the
#'   scheduler. If `FALSE`, request cancellation immediately.
#' @return A data frame describing each cancellation attempt.
#' @examples
#' \dontrun{
#' cancel_project_run(scfg, run$run_id, dry_run = TRUE)
#' cancel_project_run(scfg, run$run_id, dry_run = FALSE)
#' }
#' @seealso [inspect_project()] to inspect current job states before
#'   cancellation.
#' @export
cancel_project_run <- function(input = getwd(), run_id = "latest", dry_run = TRUE) {
  checkmate::assert_flag(dry_run)
  scfg <- project_config_from_input(input)
  resolved <- resolve_run_id(scfg, run_id)
  jobs <- .get_run_jobs_data(scfg, resolved)
  jobs <- jobs[jobs$status %in% c("QUEUED", "STARTED"), , drop = FALSE]
  command <- switch(scfg$compute_environment$scheduler,
    slurm = "scancel", torque = "qdel",
    stop("Cancellation is supported only for slurm and torque projects.", call. = FALSE)
  )
  if (nrow(jobs) == 0L) return(data.frame(
    run_id = character(), job_id = character(), command = character(),
    status = character(), stringsAsFactors = FALSE
  ))
  rows <- lapply(seq_len(nrow(jobs)), function(i) {
    job_id <- as.character(jobs$job_id[[i]])
    exit <- 0L
    if (!dry_run) {
      # Native subprocess output bypasses R's output sinks. Capture it before
      # forwarding diagnostics so cancellation cannot corrupt CLI JSON stdout.
      output <- suppressWarnings(system2(command, job_id, stdout = TRUE, stderr = TRUE))
      if (length(output)) writeLines(output, stderr())
      exit <- value_or_default(attr(output, "status"), 0L)
    }
    status <- if (dry_run) "would_cancel" else if (identical(as.integer(exit), 0L)) "cancelled" else "failed"
    if (!dry_run && status == "cancelled") {
      update_tracked_job_status(scfg$metadata$sqlite_db, job_id, "CANCELLED")
    }
    data.frame(run_id = resolved, job_id = job_id,
      command = paste(command, job_id), status = status, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Recover exact retry identities from structured and legacy tracking records
#' @param jobs Source run's tracked jobs, including parent records.
#' @param include_blocked Include jobs blocked by failed dependencies.
#' @return Run selection and deduplicated work units, excluding setup helpers.
#' @noRd
retry_request_from_jobs <- function(jobs, include_blocked = FALSE) {
  statuses <- c("FAILED", "CANCELLED", if (include_blocked) "FAILED_BY_EXT")
  # Structured identity is authoritative. Legacy names and ancestor records
  # fill missing identity only; arrays and sentinels collapse to their work unit.
  jobs <- .annotate_tracked_jobs(jobs)
  failed <- jobs[jobs$status %in% statuses, , drop = FALSE]
  failed <- failed[failed$stage %in% supported_project_steps(), , drop = FALSE]
  units <- if (nrow(failed)) normalize_retry_work_units(failed) else
    data.frame(stage = character(), stream = character(), sub_id = character(), ses_id = character())
  list(
    steps = unique(units$stage),
    subject_filter = unique(units$sub_id[!is.na(units$sub_id)]),
    postprocess_streams = unique(units$stream[units$stage == "postprocess"]),
    extract_streams = unique(units$stream[units$stage == "extract_rois"]),
    work_units = units,
    jobs = failed
  )
}

#' Create a new run for failed work
#'
#' A retry does not resume scheduler jobs in place and does not change the
#' original run. It creates a new [run_project()] submission containing the
#' exact failed or cancelled subject/session/stage/stream combinations found in
#' the source run. Array tasks and sentinels resolve to their owning work unit;
#' the whole unit is retried, not individual files within it. Selected work is
#' rerun even if old completion markers would normally skip it. Successful
#' combinations are not added, and the new provenance record identifies the
#' source run even when a retry plan is saved and submitted later.
#'
#' Required project setup jobs are listed separately in the plan. A setup-only
#' failure cannot determine the intended subject scope: include blocked jobs or
#' use [run_project()] with an explicit selection. Ambiguous legacy records or
#' missing inputs cause an error instead of broadening the retry scope.
#'
#' Preview with `dry_run = TRUE` before submitting. The preview returns a plan
#' and contacts no scheduler. With `dry_run = FALSE`, submission begins
#' immediately and the function returns the new run handle.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param run_id Source run ID returned by [run_project()] or listed in
#'   `inspect_project(input)$runs`. An explicit ID is recommended for retry.
#' @param include_blocked Also include downstream jobs marked `FAILED_BY_EXT`.
#'   These jobs did not fail themselves; they could not run because an earlier
#'   required job failed. They are excluded by default.
#' @param dry_run If `TRUE`, return a plan without submitting jobs. If `FALSE`,
#'   submit the new run immediately.
#' @return A `bg_project_plan` for a dry run or `bg_project_run` after submission.
#' @examples
#' \dontrun{
#' # Inspect and correct the failure before retrying.
#' diagnosis <- diagnose_project(
#'   scfg, run$run_id, interactive = FALSE
#' )
#'
#' # Preview only; no jobs are submitted.
#' retry_plan <- retry_project_run(scfg, run$run_id, dry_run = TRUE)
#'
#' # Submit a separate new run after reviewing the plan.
#' retry_run <- retry_project_run(scfg, run$run_id, dry_run = FALSE)
#' }
#' @seealso [diagnose_project()] to inspect the source failure and
#'   [get_run_provenance()] to compare the original and retry runs.
#' @export
retry_project_run <- function(input = getwd(), run_id = "latest", include_blocked = FALSE,
                              dry_run = TRUE) {
  checkmate::assert_flag(include_blocked)
  checkmate::assert_flag(dry_run)
  scfg <- project_config_from_input(input)
  source_run_id <- resolve_run_id(scfg, run_id)
  jobs <- .get_run_jobs_data(scfg, source_run_id)
  request <- retry_request_from_jobs(jobs, include_blocked)
  if (length(request$steps) == 0L) {
    stop("No retryable failed jobs were found in this run. If only setup/controller jobs failed, use include_blocked = TRUE to recover their blocked work, or run_project() with an explicit selection.", call. = FALSE)
  }
  attr(scfg, "retry_work_units") <- request$work_units
  attr(scfg, "provenance_context") <- list(
    interface = "retry",
    parent_run_id = source_run_id,
    include_blocked = include_blocked
  )
  if (dry_run) {
    return(plan_project(
      scfg, steps = request$steps,
      subject_filter = if (length(request$subject_filter)) request$subject_filter else NULL,
      postprocess_streams = if (length(request$postprocess_streams)) request$postprocess_streams else NULL,
      extract_streams = if (length(request$extract_streams)) request$extract_streams else NULL,
      force = TRUE
    ))
  }
  run_project(
    scfg, steps = request$steps,
    subject_filter = if (length(request$subject_filter)) request$subject_filter else NULL,
    postprocess_streams = if (length(request$postprocess_streams)) request$postprocess_streams else NULL,
    extract_streams = if (length(request$extract_streams)) request$extract_streams else NULL,
    force = TRUE
  )
}
