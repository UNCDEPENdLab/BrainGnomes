# Internal helpers and S3 methods for project inspection. The public entry
# point is inspect_project(); the data-oriented getters in
# lifecycle_functions.R are retained as deprecated compatibility wrappers.

.empty_tracked_jobs <- function() {
  data.frame(
    id = integer(), parent_id = integer(), child_level = integer(),
    job_id = character(), job_name = character(), sequence_id = character(),
    batch_directory = character(), batch_file = character(),
    compute_file = character(), code_file = character(), n_nodes = integer(),
    n_cpus = integer(), wall_time = character(), mem_per_cpu = character(),
    mem_total = character(), scheduler = character(),
    scheduler_options = character(), time_submitted = character(),
    time_started = character(), time_ended = character(), status = character(),
    output_manifest = character(), contract_id = character(),
    stage = character(), stream = character(), sub_id = character(),
    ses_id = character(), job_role = character(), unit_key = character(),
    attempt = integer(), job_manifest_path = character(),
    job_manifest_checksum = character(), runtime_receipt_path = character(),
    runtime_receipt_checksum = character(), runtime_host = character(),
    contract_status = character(), scheduler_terminal_state = character(),
    exit_code = integer(), failure_category = character(),
    stdout_log = character(), stderr_log = character(),
    stringsAsFactors = FALSE
  )
}

.read_project_tracking_jobs <- function(scfg, run_id = NULL) {
  db <- scfg$metadata$sqlite_db
  if (!checkmate::test_file_exists(db) || !sqlite_table_exists(db, "job_tracking")) {
    return(.empty_tracked_jobs())
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db, synchronous = NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  RSQLite::sqliteSetBusyHandler(con, 10000L)
  query <- "SELECT * FROM job_tracking WHERE sequence_id IS NOT NULL"
  params <- NULL
  if (!is.null(run_id)) {
    query <- paste(query, "AND sequence_id = ?")
    params <- list(as.character(run_id))
  }
  jobs <- if (is.null(params)) {
    DBI::dbGetQuery(con, query)
  } else {
    DBI::dbGetQuery(con, query, params = params)
  }
  if ("job_obj" %in% names(jobs)) jobs$job_obj <- NULL
  jobs
}

.extract_job_token <- function(x, token) {
  pattern <- paste0("(?:^|_)", token, "-([^_]+)")
  vapply(x, function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_character_)
    match <- regexec(pattern, value, perl = TRUE)
    pieces <- regmatches(value, match)[[1L]]
    if (length(pieces) < 2L) NA_character_ else pieces[[2L]]
  }, character(1))
}

.job_stage <- function(job_name) {
  vapply(job_name, function(name) {
    if (is.na(name) || !nzchar(name)) return("other")
    if (grepl("^flywheel_sync", name)) "flywheel_sync"
    else if (grepl("^fsaverage_setup", name)) "fsaverage_setup"
    else if (grepl("^prefetch_templates", name)) "prefetch_templates"
    else if (grepl("^submit_subjects", name)) "subject_submission"
    else if (grepl("^bids_validation", name)) "bids_validation"
    else if (grepl("^bids_conversion", name)) "bids_conversion"
    else if (grepl("^mriqc", name)) "mriqc"
    else if (grepl("^fmriprep", name)) "fmriprep"
    else if (grepl("^aroma", name)) "aroma"
    else if (grepl("^postprocess_", name)) "postprocess"
    else if (grepl("^extract_rois_", name)) "extract_rois"
    else "other"
  }, character(1))
}

.job_stream <- function(job_name, stage) {
  vapply(seq_along(job_name), function(i) {
    name <- job_name[[i]]
    if (is.na(name) || !stage[[i]] %in% c("postprocess", "extract_rois")) {
      return(NA_character_)
    }
    value <- sub(paste0("^", stage[[i]], "_"), "", name)
    value <- sub("_sub-.*$", "", value)
    value <- sub("_(sentinel|array)$", "", value)
    if (!nzchar(value)) NA_character_ else value
  }, character(1))
}

.job_role <- function(job_name, sub_id) {
  vapply(seq_along(job_name), function(i) {
    name <- job_name[[i]]
    if (!is.na(name) && grepl("_sentinel$", name)) "sentinel"
    else if (!is.na(name) && grepl("_array$", name)) "array"
    else if (!is.na(name) && grepl("^submit_subjects", name)) "controller"
    else if (!is.na(sub_id[[i]])) "subject"
    else "project"
  }, character(1))
}

.lifecycle_status <- function(status) {
  status <- toupper(trimws(as.character(status)))
  status[is.na(status) | !nzchar(status)] <- "UNKNOWN"
  status[status == "STARTED"] <- "RUNNING"
  status[status == "FAILED_BY_EXT"] <- "BLOCKED"
  known <- c("COMPLETED", "RUNNING", "QUEUED", "FAILED", "BLOCKED", "CANCELLED")
  status[!status %in% known] <- "UNKNOWN"
  status
}

.annotate_tracked_jobs <- function(jobs) {
  if (!is.data.frame(jobs)) jobs <- .empty_tracked_jobs()
  needed <- names(.empty_tracked_jobs())
  empty <- .empty_tracked_jobs()
  for (name in setdiff(needed, names(jobs))) {
    prototype <- empty[[name]]
    jobs[[name]] <- if (is.integer(prototype)) {
      rep(NA_integer_, nrow(jobs))
    } else if (is.numeric(prototype)) {
      rep(NA_real_, nrow(jobs))
    } else {
      rep(NA_character_, nrow(jobs))
    }
  }

  if (nrow(jobs) == 0L) {
    jobs$stage <- character()
    jobs$stream <- character()
    jobs$sub_id <- character()
    jobs$ses_id <- character()
    jobs$job_role <- character()
    jobs$lifecycle_status <- character()
    jobs$unit_key <- character()
    jobs$is_current_attempt <- logical()
    return(jobs)
  }

  names_ <- as.character(jobs$job_name)
  fill_missing <- function(value, fallback) {
    missing <- is.na(value) | !nzchar(as.character(value))
    value[missing] <- fallback[missing]
    value
  }
  jobs$stage <- fill_missing(as.character(jobs$stage), .job_stage(names_))
  jobs$stream <- fill_missing(
    as.character(jobs$stream), .job_stream(names_, jobs$stage)
  )
  jobs$sub_id <- fill_missing(
    as.character(jobs$sub_id), .extract_job_token(names_, "sub")
  )
  jobs$ses_id <- fill_missing(
    as.character(jobs$ses_id), .extract_job_token(names_, "ses")
  )
  session_stages <- c("bids_conversion", "postprocess", "extract_rois")
  jobs$ses_id[!jobs$stage %in% session_stages] <- NA_character_
  jobs$job_role <- fill_missing(
    as.character(jobs$job_role), .job_role(names_, jobs$sub_id)
  )
  jobs$lifecycle_status <- .lifecycle_status(jobs$status)

  # Array and sentinel records use concise scheduler names. Attribute them to
  # the nearest subject-bearing ancestor rather than guessing from stream name.
  id_match <- match(as.character(jobs$parent_id), as.character(jobs$id))
  for (iteration in seq_len(nrow(jobs))) {
    changed <- FALSE
    for (i in which(!is.na(id_match))) {
      parent <- id_match[[i]]
      if (is.na(jobs$sub_id[[i]]) && !is.na(jobs$sub_id[[parent]])) {
        jobs$sub_id[[i]] <- jobs$sub_id[[parent]]
        changed <- TRUE
      }
      if (is.na(jobs$ses_id[[i]]) && !is.na(jobs$ses_id[[parent]])) {
        jobs$ses_id[[i]] <- jobs$ses_id[[parent]]
        changed <- TRUE
      }
    }
    if (!changed) break
  }

  subject_key <- !is.na(jobs$sub_id)
  ses <- ifelse(is.na(jobs$ses_id), "", jobs$ses_id)
  stream <- ifelse(is.na(jobs$stream), "", jobs$stream)
  project_name <- ifelse(is.na(names_) | !nzchar(names_), paste0("job-", jobs$job_id), names_)
  derived_unit_key <- ifelse(
    subject_key,
    paste("subject", jobs$sub_id, ses, jobs$stage, stream, sep = "::"),
    paste("project", jobs$stage, stream, project_name, sep = "::")
  )
  jobs$unit_key <- fill_missing(as.character(jobs$unit_key), derived_unit_key)
  jobs$is_current_attempt <- FALSE
  jobs
}

.status_counts <- function(status) {
  status <- .lifecycle_status(status)
  c(
    n_completed = sum(status == "COMPLETED"),
    n_running = sum(status == "RUNNING"),
    n_queued = sum(status == "QUEUED"),
    n_failed = sum(status == "FAILED"),
    n_blocked = sum(status == "BLOCKED"),
    n_cancelled = sum(status == "CANCELLED"),
    n_unknown = sum(status == "UNKNOWN")
  )
}

.aggregate_status <- function(status) {
  counts <- .status_counts(status)
  if (counts[["n_failed"]] > 0L) "FAILED"
  else if (counts[["n_blocked"]] > 0L) "BLOCKED"
  else if (counts[["n_running"]] > 0L) "RUNNING"
  else if (counts[["n_queued"]] > 0L) "QUEUED"
  else if (counts[["n_cancelled"]] > 0L) "CANCELLED"
  else if (length(status) > 0L && counts[["n_completed"]] == length(status)) "COMPLETED"
  else "UNKNOWN"
}

.first_value <- function(x) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(x) == 0L) NA_character_ else as.character(x[[1L]])
}

.time_extreme <- function(x, which = c("min", "max")) {
  which <- match.arg(which)
  x <- as.character(x)
  keep <- !is.na(x) & nzchar(x)
  if (!any(keep)) return(NA_character_)
  values <- x[keep]
  timestamps <- sub("T", " ", sub("Z$", "", values), fixed = TRUE)
  parsed <- suppressWarnings(as.numeric(as.POSIXct(timestamps, tz = "UTC")))
  if (all(is.na(parsed))) return(if (which == "min") min(values) else max(values))
  index <- if (which == "min") which.min(replace(parsed, is.na(parsed), Inf)) else which.max(replace(parsed, is.na(parsed), -Inf))
  values[[index]]
}

.empty_attempts <- function() {
  data.frame(
    run_id = character(), unit_key = character(), sub_id = character(),
    ses_id = character(), stage = character(), stream = character(),
    unit_name = character(), status = character(), n_jobs = integer(),
    n_completed = integer(), n_running = integer(), n_queued = integer(),
    n_failed = integer(), n_blocked = integer(), n_cancelled = integer(),
    n_unknown = integer(), submitted = character(), last_update = character(),
    is_current = logical(), stringsAsFactors = FALSE
  )
}

.aggregate_job_attempts <- function(jobs) {
  if (nrow(jobs) == 0L) return(.empty_attempts())
  attempt_key <- paste(jobs$sequence_id, jobs$unit_key, sep = "\r")
  groups <- split(seq_len(nrow(jobs)), factor(attempt_key, levels = unique(attempt_key)))
  rows <- lapply(groups, function(index) {
    group <- jobs[index, , drop = FALSE]
    counts <- .status_counts(group$lifecycle_status)
    updates <- c(group$time_submitted, group$time_started, group$time_ended)
    data.frame(
      run_id = as.character(group$sequence_id[[1L]]),
      unit_key = group$unit_key[[1L]],
      sub_id = .first_value(group$sub_id),
      ses_id = .first_value(group$ses_id),
      stage = group$stage[[1L]],
      stream = .first_value(group$stream),
      unit_name = if (!is.na(.first_value(group$sub_id))) {
        stream_name <- .first_value(group$stream)
        paste(c(group$stage[[1L]], if (!is.na(stream_name)) stream_name), collapse = ":")
      } else .first_value(group$job_name),
      status = .aggregate_status(group$lifecycle_status),
      n_jobs = nrow(group),
      n_completed = unname(counts[["n_completed"]]),
      n_running = unname(counts[["n_running"]]),
      n_queued = unname(counts[["n_queued"]]),
      n_failed = unname(counts[["n_failed"]]),
      n_blocked = unname(counts[["n_blocked"]]),
      n_cancelled = unname(counts[["n_cancelled"]]),
      n_unknown = unname(counts[["n_unknown"]]),
      submitted = .time_extreme(group$time_submitted, "min"),
      last_update = .time_extreme(updates, "max"),
      is_current = FALSE,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.empty_runs <- function() {
  data.frame(
    run_id = character(), submitted = character(), ended = character(),
    n_jobs = integer(), status = character(), n_completed = integer(),
    n_running = integer(), n_queued = integer(), n_failed = integer(),
    n_blocked = integer(), n_cancelled = integer(), n_unknown = integer(),
    stringsAsFactors = FALSE
  )
}

.project_runs_from_jobs <- function(jobs) {
  if (nrow(jobs) == 0L) return(.empty_runs())
  valid <- !is.na(jobs$sequence_id) & nzchar(as.character(jobs$sequence_id))
  jobs <- jobs[valid, , drop = FALSE]
  if (nrow(jobs) == 0L) return(.empty_runs())
  groups <- split(seq_len(nrow(jobs)), factor(jobs$sequence_id, levels = unique(jobs$sequence_id)))
  rows <- lapply(groups, function(index) {
    group <- jobs[index, , drop = FALSE]
    counts <- .status_counts(group$lifecycle_status)
    run_status <- if (counts[["n_failed"]] > 0L || counts[["n_blocked"]] > 0L) "FAILED"
    else if (counts[["n_cancelled"]] > 0L) "CANCELLED"
    else if (counts[["n_running"]] > 0L) "RUNNING"
    else if (counts[["n_queued"]] > 0L) "QUEUED"
    else if (counts[["n_completed"]] == nrow(group)) "COMPLETED"
    else "UNKNOWN"
    data.frame(
      run_id = as.character(group$sequence_id[[1L]]),
      submitted = .time_extreme(group$time_submitted, "min"),
      ended = .time_extreme(group$time_ended, "max"),
      .latest_id = max(as.numeric(group$id), na.rm = TRUE),
      n_jobs = nrow(group), status = run_status,
      n_completed = unname(counts[["n_completed"]]),
      n_running = unname(counts[["n_running"]]),
      n_queued = unname(counts[["n_queued"]]),
      n_failed = unname(counts[["n_failed"]]),
      n_blocked = unname(counts[["n_blocked"]]),
      n_cancelled = unname(counts[["n_cancelled"]]),
      n_unknown = unname(counts[["n_unknown"]]),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  timestamps <- sub("T", " ", sub("Z$", "", result$submitted), fixed = TRUE)
  parsed <- suppressWarnings(as.numeric(as.POSIXct(timestamps, tz = "UTC")))
  result <- result[
    order(parsed, result$.latest_id, decreasing = TRUE, na.last = TRUE),
    , drop = FALSE
  ]
  result$.latest_id <- NULL
  rownames(result) <- NULL
  result
}

.mark_current_attempts <- function(attempts, runs, run_scope = FALSE) {
  if (nrow(attempts) == 0L) return(attempts)
  if (run_scope) {
    attempts$is_current <- TRUE
    return(attempts)
  }
  run_rank <- match(attempts$run_id, runs$run_id)
  groups <- split(seq_len(nrow(attempts)), factor(attempts$unit_key, levels = unique(attempts$unit_key)))
  for (index in groups) {
    ranks <- run_rank[index]
    chosen <- index[[which.min(replace(ranks, is.na(ranks), Inf))]]
    attempts$is_current[[chosen]] <- TRUE
  }
  attempts
}

.empty_unit_summary <- function(group_cols) {
  result <- setNames(replicate(length(group_cols), character(), simplify = FALSE), group_cols)
  result$status <- character()
  result$n_units <- integer()
  result$n_completed <- integer()
  result$n_running <- integer()
  result$n_queued <- integer()
  result$n_failed <- integer()
  result$n_blocked <- integer()
  result$n_cancelled <- integer()
  result$n_unknown <- integer()
  result$pct_completed <- numeric()
  result$n_jobs <- integer()
  as.data.frame(result, stringsAsFactors = FALSE)
}

.aggregate_units <- function(attempts, group_cols) {
  if (nrow(attempts) == 0L) return(.empty_unit_summary(group_cols))
  key_parts <- lapply(attempts[group_cols], function(x) ifelse(is.na(x), "<NA>", as.character(x)))
  key <- do.call(paste, c(key_parts, sep = "\r"))
  groups <- split(seq_len(nrow(attempts)), factor(key, levels = unique(key)))
  rows <- lapply(groups, function(index) {
    group <- attempts[index, , drop = FALSE]
    values <- lapply(group_cols, function(name) group[[name]][[1L]])
    names(values) <- group_cols
    counts <- .status_counts(group$status)
    values$status <- .aggregate_status(group$status)
    values$n_units <- nrow(group)
    for (name in names(counts)) values[[name]] <- unname(counts[[name]])
    values$pct_completed <- round(100 * counts[["n_completed"]] / nrow(group), 1)
    values$n_jobs <- sum(group$n_jobs)
    as.data.frame(values, stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.inspection_overview <- function(current, jobs, runs, scope, run_id) {
  counts <- .status_counts(current$status)
  n_units <- nrow(current)
  active <- counts[["n_running"]] + counts[["n_queued"]]
  problems <- counts[["n_failed"]] + counts[["n_blocked"]]
  overall <- if (n_units == 0L) "EMPTY"
  else if (active > 0L && problems > 0L) "IN_PROGRESS_WITH_FAILURES"
  else if (active > 0L) "IN_PROGRESS"
  else if (problems > 0L) "FAILED"
  else if (counts[["n_cancelled"]] > 0L) "CANCELLED"
  else if (counts[["n_completed"]] == n_units) "COMPLETED"
  else "INCOMPLETE"
  current_jobs <- jobs[jobs$is_current_attempt, , drop = FALSE]
  job_counts <- .status_counts(current_jobs$lifecycle_status)
  subjects <- unique(current$sub_id[!is.na(current$sub_id)])
  data.frame(
    scope = scope,
    run_id = if (is.null(run_id) || is.na(run_id)) NA_character_ else as.character(run_id),
    overall_status = overall,
    n_runs = if (scope == "run" && n_units > 0L) 1L else nrow(runs),
    n_subjects = length(subjects), n_units = n_units,
    n_completed = unname(counts[["n_completed"]]),
    n_running = unname(counts[["n_running"]]),
    n_queued = unname(counts[["n_queued"]]),
    n_failed = unname(counts[["n_failed"]]),
    n_blocked = unname(counts[["n_blocked"]]),
    n_cancelled = unname(counts[["n_cancelled"]]),
    n_unknown = unname(counts[["n_unknown"]]),
    pct_completed = if (n_units == 0L) NA_real_ else round(100 * counts[["n_completed"]] / n_units, 1),
    n_jobs = nrow(current_jobs),
    n_jobs_all_attempts = nrow(jobs),
    n_jobs_running = unname(job_counts[["n_running"]]),
    n_jobs_queued = unname(job_counts[["n_queued"]]),
    n_jobs_failed = unname(job_counts[["n_failed"]]),
    n_jobs_blocked = unname(job_counts[["n_blocked"]]),
    stringsAsFactors = FALSE
  )
}

.build_project_inspection <- function(scfg, jobs, scope = "project", run_id = NULL,
                                      subject_id = NULL, refresh = FALSE,
                                      all_runs = NULL) {
  retrieved_at <- Sys.time()
  submissions <- inspect_job_submissions(scfg$metadata$sqlite_db)
  if (nrow(submissions) && identical(scope, "run")) {
    submissions <- submissions[!is.na(submissions$run_id) & submissions$run_id == run_id, , drop = FALSE]
  }
  if (nrow(submissions) && !is.null(subject_id)) {
    submissions <- submissions[!is.na(submissions$sub_id) & submissions$sub_id == subject_id, , drop = FALSE]
  }
  jobs <- .annotate_tracked_jobs(jobs)
  if (is.null(all_runs)) all_runs <- .project_runs_from_jobs(jobs)
  all_runs <- project_runs_with_submissions(all_runs, submissions)
  attempts <- .aggregate_job_attempts(jobs)
  attempts <- .mark_current_attempts(attempts, all_runs, run_scope = identical(scope, "run"))
  current <- attempts[attempts$is_current, , drop = FALSE]
  if (nrow(jobs) > 0L && nrow(current) > 0L) {
    current_keys <- paste(current$run_id, current$unit_key, sep = "\r")
    jobs$is_current_attempt <- paste(jobs$sequence_id, jobs$unit_key, sep = "\r") %in% current_keys
  }

  subject_stages <- current[!is.na(current$sub_id), , drop = FALSE]
  keep <- c(
    "sub_id", "ses_id", "stage", "stream", "status", "n_jobs",
    "n_completed", "n_running", "n_queued", "n_failed", "n_blocked",
    "n_cancelled", "n_unknown", "run_id", "submitted", "last_update"
  )
  subject_stages <- subject_stages[, keep, drop = FALSE]
  stages <- .aggregate_units(current, c("stage", "stream"))
  subjects <- .aggregate_units(
    current[!is.na(current$sub_id), , drop = FALSE], c("sub_id", "ses_id")
  )
  overview <- .inspection_overview(current, jobs, all_runs, scope, run_id)
  overview$n_unconfirmed_submissions <- if (nrow(submissions)) {
    sum(submissions$submission_state %in% c("SUBMITTING", "UNKNOWN"))
  } else 0L
  # A confirmed pre-launch rejection has no scheduler-bound row. Include the
  # latest result for its unit/role, but not superseded rejection history.
  overview$n_rejected_submissions <- 0L
  if (nrow(submissions)) {
    identities <- paste(submissions$unit_key, submissions$job_role, sep = "\r")
    latest_submissions <- submissions[!duplicated(identities, fromLast = TRUE), , drop = FALSE]
    overview$n_rejected_submissions <- sum(latest_submissions$submission_state == "REJECTED")
  }
  if (overview$n_rejected_submissions > 0L) overview$overall_status <- "SUBMISSION_FAILED"
  if (overview$n_unconfirmed_submissions > 0L) overview$overall_status <- "UNCONFIRMED_SUBMISSIONS"
  active_health <- build_active_job_health(
    scfg, jobs[jobs$is_current_attempt, , drop = FALSE],
    retrieved_at = retrieved_at, refresh = refresh
  )
  overview$n_active_attention <- sum(active_health$active$health %in% c(
    "STALE_DATABASE", "DATABASE_LAG", "SUSPENDED", "OVERDUE", "UNCONFIRMED"
  ))
  overview$n_active_overdue <- sum(active_health$active$overdue)
  overview$n_active_stale <- sum(active_health$active$stale)
  runs <- if (identical(scope, "run") && !is.null(run_id) && !is.na(run_id)) {
    all_runs[all_runs$run_id == run_id, , drop = FALSE]
  } else {
    all_runs
  }
  project_directory <- scfg$metadata$project_directory
  if (!checkmate::test_string(project_directory)) project_directory <- dirname(scfg$metadata$sqlite_db)
  project_name <- scfg$metadata$project_name
  if (!checkmate::test_string(project_name)) project_name <- basename(project_directory)
  if (!checkmate::test_string(project_name) || !nzchar(project_name)) project_name <- "unnamed project"
  log_directory <- scfg$metadata$log_directory
  if (!checkmate::test_string(log_directory)) {
    log_directory <- file.path(project_directory, "logs")
  }

  class(jobs) <- unique(c("bg_run_jobs", class(jobs)))
  attr(jobs, "scope") <- scope
  attr(jobs, "run_id") <- run_id
  attr(jobs, "project") <- project_name
  attr(jobs, "subject_id") <- subject_id

  structure(list(
    project = project_name,
    project_directory = project_directory,
    log_directory = log_directory,
    scope = scope,
    run_id = run_id,
    subject_id = subject_id,
    scheduler_refreshed = isTRUE(refresh),
    retrieved_at = retrieved_at,
    overview = overview,
    active = active_health$active,
    reconciliation = active_health$reconciliation,
    stages = stages,
    subjects = subjects,
    subject_stages = subject_stages,
    runs = runs,
    attempts = attempts,
    submissions = submissions,
    jobs = jobs
  ), class = "bg_project_inspection")
}

#' Inspect current project progress
#'
#' Provides a compact, non-interactive view of tracked work from submission
#' through queueing, execution, completion, failure, or cancellation. By
#' default, the current state is integrated across runs by retaining the most
#' recent attempt for each project or subject-level work unit. Set `run_id` to
#' inspect only one submission.
#'
#' @param input A project configuration object, YAML file, or project directory.
#'   Defaults to the current working directory.
#' @param run_id Optional run ID. Use `"latest"` for the most recently recorded
#'   run, an explicit run ID for an older submission, or `NULL` (the default)
#'   for current project status across all runs.
#' @param subject_id Optional subject identifier, with or without the `sub-`
#'   prefix. When supplied, every returned resolution is restricted to that
#'   subject, including its run summaries.
#' @param refresh If `TRUE`, query the configured scheduler for jobs recorded as
#'   queued or running. The query is read-only: database values are retained and
#'   any differences are returned in `reconciliation`. The default, `FALSE`,
#'   relies only on the tracking database.
#' @return A `bg_project_inspection` object. Its `overview`, `stages`,
#'   `active`, `reconciliation`, `subjects`, `subject_stages`, `runs`,
#'   `attempts`, `submissions`, and `jobs` elements are data frames suitable for programmatic
#'   queries.
#' @details The `overview` table contains one row for the selected scope.
#'   `stages` and `subjects` aggregate its current work units;
#'   `active` reports current queued and running jobs with elapsed time,
#'   requested wall time, and health flags. When `refresh = TRUE`,
#'   `reconciliation` compares those database states with the scheduler without
#'   modifying either source.
#'   `submissions` exposes UUID attempts registered before scheduler submission,
#'   including uncertain outcomes without a scheduler ID. Those outcomes remain
#'   visible and prevent a project from being reported as completed.
#'   `subject_stages` retains the stage and stream detail; `runs` summarizes
#'   submissions; and `attempts` retains both current and superseded logical
#'   attempts. `jobs` contains the underlying tracking rows and marks the rows
#'   contributing to current project status with `is_current_attempt`. Printing
#'   the object or its `jobs` component deliberately omits long scheduler,
#'   path, and manifest fields, but those columns remain available for ordinary
#'   data-frame access. Subject-wide stages use `NA` for `ses_id`; stages that
#'   run separately by session retain their session identifier.
#' @examples
#' \dontrun{
#' status <- inspect_project(scfg)
#' status
#' summary(status, by = "subject")
#' subject <- inspect_project(scfg, subject_id = "014")
#' subject$active
#'
#' latest <- inspect_project(scfg, run_id = "latest", refresh = TRUE)
#' latest$reconciliation
#' subset(latest$jobs, lifecycle_status == "FAILED")
#' }
#' @seealso [diagnose_project()] for failure and log investigation.
#' @export
inspect_project <- function(input = getwd(), run_id = NULL,
                            subject_id = NULL, refresh = FALSE) {
  subject_id <- normalize_inspection_subject_id(subject_id)
  checkmate::assert_flag(refresh)
  scfg <- project_config_from_input(input)
  all_jobs <- .read_project_tracking_jobs(scfg)
  all_jobs <- .annotate_tracked_jobs(all_jobs)
  all_runs <- .project_runs_from_jobs(all_jobs)
  submissions <- inspect_job_submissions(scfg$metadata$sqlite_db)
  all_runs <- project_runs_with_submissions(all_runs, submissions)

  scope <- "project"
  resolved <- NULL
  jobs <- all_jobs
  if (!is.null(run_id)) {
    scope <- "run"
    if (identical(run_id, "latest")) {
      if (nrow(all_runs) == 0L) {
        resolved <- NA_character_
        jobs <- all_jobs[FALSE, , drop = FALSE]
      } else {
        resolved <- all_runs$run_id[[1L]]
        jobs <- all_jobs[all_jobs$sequence_id == resolved, , drop = FALSE]
      }
    } else {
      resolved <- as.character(run_id)
      if (!resolved %in% all_runs$run_id) {
        stop("No tracked project run has ID: ", resolved, call. = FALSE)
      }
      jobs <- all_jobs[all_jobs$sequence_id == resolved, , drop = FALSE]
    }
  }
  if (!is.null(subject_id)) {
    jobs <- jobs[!is.na(jobs$sub_id) & jobs$sub_id == subject_id, , drop = FALSE]
    matching_submissions <- FALSE
    if (nrow(submissions)) {
      eligible <- !is.na(submissions$sub_id) & submissions$sub_id == subject_id
      if (!is.null(resolved)) eligible <- eligible & !is.na(submissions$run_id) & submissions$run_id == resolved
      matching_submissions <- any(eligible)
    }
    if (nrow(jobs) == 0L && !matching_submissions) {
      run_text <- if (is.null(run_id)) "" else paste0(" in the selected run")
      stop(
        "No tracked jobs were found for sub-", subject_id, run_text, ".",
        call. = FALSE
      )
    }
  }
  scoped_runs <- .project_runs_from_jobs(jobs)

  .build_project_inspection(
    scfg, jobs, scope = scope, run_id = resolved,
    subject_id = subject_id, refresh = refresh, all_runs = scoped_runs
  )
}

#' @export
summary.bg_project_inspection <- function(object,
                                          by = c("run", "stage", "subject", "subject_stage", "runs", "attempt", "job", "active", "reconciliation"),
                                          ...) {
  by <- match.arg(by)
  switch(by,
    run = object$overview,
    stage = object$stages,
    subject = object$subjects,
    subject_stage = object$subject_stages,
    runs = object$runs,
    attempt = object$attempts,
    job = object$jobs,
    active = object$active,
    reconciliation = object$reconciliation
  )
}

.status_display_table <- function(x, include_id = FALSE) {
  cols <- c(if (include_id) c("sub_id", "ses_id") else c("stage", "stream"),
            "status", "n_completed", "n_running", "n_queued", "n_failed",
            "n_blocked", "n_units")
  x[, intersect(cols, names(x)), drop = FALSE]
}

#' @export
print.bg_project_inspection <- function(x, ..., max_subjects = 12L,
                                        max_active = 8L) {
  checkmate::assert_integerish(max_subjects, len = 1L, lower = 0)
  checkmate::assert_integerish(max_active, len = 1L, lower = 0)
  overview <- x$overview[1L, , drop = FALSE]
  title <- if (identical(x$scope, "run") && !is.na(x$run_id)) {
    paste0("BrainGnomes run ", x$run_id)
  } else {
    paste0("BrainGnomes project: ", x$project)
  }
  cli::cli_h2(title)
  if (!is.null(x$subject_id)) {
    cli::cli_text("Subject: {.strong sub-{x$subject_id}}")
  }
  cli::cli_text("Status: {.strong {overview$overall_status}}")
  cli::cli_text(
    "Scope: {overview$n_runs} run{?s} | {overview$n_subjects} subject{?s}"
  )
  cli::cli_text(
    "Work units: {overview$n_units} total | {overview$n_completed} completed | {overview$n_running} running | {overview$n_queued} queued | {overview$n_failed} failed | {overview$n_blocked} blocked"
  )
  cli::cli_text(
    "Tracked jobs: {overview$n_jobs} total | {overview$n_jobs_running} running | {overview$n_jobs_queued} queued"
  )
  if (overview$n_unconfirmed_submissions > 0L) {
    cli::cli_alert_warning(
      "Unconfirmed submissions: {overview$n_unconfirmed_submissions}. Inspect x$submissions before resubmitting."
    )
  }
  if (overview$n_rejected_submissions > 0L) {
    cli::cli_alert_warning("Rejected submissions: {overview$n_rejected_submissions}. Inspect x$submissions for details.")
  }

  if (nrow(x$active) > 0L && max_active > 0L) {
    shown_active <- utils::head(x$active, max_active)
    cli::cli_h3("Oldest or noteworthy active jobs")
    print(active_status_display_table(shown_active), row.names = FALSE)
    if (nrow(x$active) > nrow(shown_active)) {
      cli::cli_alert_info(
        "{nrow(x$active) - nrow(shown_active)} additional active job{?s} omitted."
      )
    }
    if (overview$n_active_attention > 0L) {
      cli::cli_alert_warning(
        "{overview$n_active_attention} active job{?s} need scheduler or timing attention."
      )
    }
  }

  if (nrow(x$stages) > 0L) {
    cli::cli_h3("Stage and stream progress")
    print(.status_display_table(x$stages), row.names = FALSE)
  }

  attention <- x$subjects[x$subjects$status != "COMPLETED", , drop = FALSE]
  if (nrow(attention) > 0L && max_subjects > 0L) {
    priority <- match(attention$status, c("FAILED", "BLOCKED", "RUNNING", "QUEUED", "CANCELLED", "UNKNOWN"))
    attention <- attention[order(priority, attention$sub_id, attention$ses_id, na.last = TRUE), , drop = FALSE]
    shown <- utils::head(attention, max_subjects)
    cli::cli_h3("Subjects needing attention or still active")
    print(.status_display_table(shown, include_id = TRUE), row.names = FALSE)
    if (nrow(attention) > nrow(shown)) {
      cli::cli_alert_info("{nrow(attention) - nrow(shown)} additional subject row{?s} omitted from this display.")
    }
  } else if (nrow(x$subjects) > 0L && overview$n_unconfirmed_submissions == 0L && overview$n_rejected_submissions == 0L) {
    cli::cli_alert_success("All currently tracked subject work is complete.")
  } else if (overview$n_units == 0L && overview$n_unconfirmed_submissions == 0L) {
    cli::cli_alert_info("No tracked work was found for this scope.")
  }
  cli::cli_alert_info(
    "Use x$active, summary(x, by = \"subject\"), or x$subject_stages for complete structured views."
  )
  invisible(x)
}

.get_project_runs_data <- function(input) {
  scfg <- project_config_from_input(input)
  jobs <- .annotate_tracked_jobs(.read_project_tracking_jobs(scfg))
  project_runs_with_submissions(.project_runs_from_jobs(jobs),
                                inspect_job_submissions(scfg$metadata$sqlite_db))
}

.get_run_jobs_data <- function(input, run_id = "latest") {
  scfg <- project_config_from_input(input)
  inspection <- inspect_project(scfg, run_id = run_id)
  if (is.na(inspection$run_id)) {
    stop("No tracked runs were found.", call. = FALSE)
  }
  jobs <- inspection$jobs
  class(jobs) <- unique(c("bg_run_jobs", class(jobs)))
  attr(jobs, "run_id") <- inspection$run_id
  attr(jobs, "project") <- inspection$project
  jobs
}

#' @export
print.bg_run_jobs <- function(x, ..., max = 20L) {
  checkmate::assert_integerish(max, len = 1L, lower = 0)
  scope <- attr(x, "scope", exact = TRUE)
  run_id <- attr(x, "run_id", exact = TRUE)
  project <- attr(x, "project", exact = TRUE)
  subject_id <- attr(x, "subject_id", exact = TRUE)
  project_scope <- identical(scope, "project")
  shown_jobs <- if (project_scope && "is_current_attempt" %in% names(x)) {
    as.data.frame(x[x$is_current_attempt, , drop = FALSE])
  } else {
    as.data.frame(x)
  }
  if (project_scope) {
    cli::cli_h2("Current tracked jobs: {project}")
  } else {
    cli::cli_h2("Tracked jobs for run {.val {run_id}}")
  }
  if (!is.null(subject_id)) cli::cli_text("Subject: {.strong sub-{subject_id}}")
  counts <- .status_counts(shown_jobs$lifecycle_status)
  cli::cli_text(
    "{nrow(shown_jobs)} total | {counts[['n_completed']]} completed | {counts[['n_running']]} running | {counts[['n_queued']]} queued | {counts[['n_failed']]} failed | {counts[['n_blocked']]} blocked"
  )
  if (project_scope && nrow(x) > nrow(shown_jobs)) {
    cli::cli_text("Historical job rows retained but not printed: {nrow(x) - nrow(shown_jobs)}")
  }
  if (nrow(shown_jobs) > 0L && max > 0L) {
    cols <- intersect(
      c("job_id", "sub_id", "ses_id", "stage", "stream", "job_role", "lifecycle_status"),
      names(shown_jobs)
    )
    print(utils::head(shown_jobs[, cols, drop = FALSE], max), row.names = FALSE)
    if (nrow(shown_jobs) > max) {
      cli::cli_alert_info("{nrow(shown_jobs) - max} additional job{?s} omitted.")
    }
  }
  if (project_scope) {
    cli::cli_alert_info("Use as.data.frame(x) to print or export every raw tracking column.")
  } else {
    cli::cli_alert_info("Use inspect_project(..., run_id = {.val {run_id}}) for summarized views.")
  }
  invisible(x)
}
