#' Open a tracking database without silently creating a worker-side database
#' @param sqlite_db Existing tracking database path.
#' @return An SQLite connection with a bounded busy timeout.
#' @noRd
job_submission_connect <- function(sqlite_db) {
  if (!is.character(sqlite_db) || length(sqlite_db) != 1L ||
      is.na(sqlite_db) || !file.exists(sqlite_db)) {
    stop("Submission tracking database is missing: ", sqlite_db, call. = FALSE)
  }
  # Use durable rollback-journal commits. Never enable cross-host WAL on a
  # shared filesystem, and do not inherit RSQLite's synchronous=off default.
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db, synchronous = "full")
  RSQLite::sqliteSetBusyHandler(con, 10000L)
  con
}

#' Parse a unique scheduler acknowledgement without mistaking diagnostics for IDs
#' @param lines Scheduler stdout lines or a local PID.
#' @param scheduler Normalized submission executable.
#' @return One canonical job ID, or NULL when acceptance is unconfirmed.
#' @noRd
parse_job_submission_id <- function(lines, scheduler) {
  lines <- trimws(as.character(lines))
  if (scheduler == "sbatch") {
    lines <- sub("^Submitted batch job[[:space:]]+", "", lines)
    lines <- sub(";[A-Za-z0-9_.-]+$", "", lines)
    candidates <- lines[grepl("^[0-9]+$", lines)]
  } else if (scheduler == "qsub") {
    candidates <- lines[grepl("^[0-9]+(\\[\\])?([.][A-Za-z0-9_-]+)*$", lines)]
  } else {
    candidates <- lines[grepl("^[0-9]+$", lines)]
  }
  candidates <- unique(candidates)
  if (length(candidates) == 1L) candidates[[1L]] else NULL
}

#' Serialize an attempt transition and its tracking-row changes
#' @param sqlite_db Existing tracking database path.
#' @param action Function accepting the transaction connection.
#' @return The action result, after commit.
#' @noRd
job_submission_transaction <- function(sqlite_db, action) {
  con <- job_submission_connect(sqlite_db)
  committed <- FALSE
  on.exit({
    if (!committed) try(DBI::dbRollback(con), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  result <- action(con)
  DBI::dbCommit(con)
  committed <- TRUE
  result
}

#' Add the independent pre-submission attempt registry
#' @param sqlite_db Existing tracking database path.
#' @return NULL, invisibly; creates an additive table without changing job IDs.
#' @noRd
ensure_job_submission_schema <- function(sqlite_db) {
  con <- job_submission_connect(sqlite_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS job_submission_attempts (",
    "contract_id TEXT PRIMARY KEY, run_id TEXT, unit_key TEXT, job_role TEXT, attempt INTEGER,",
    "submission_state TEXT NOT NULL CHECK (submission_state IN",
    "('PREPARED','SUBMITTING','ACCEPTED','REJECTED','UNKNOWN')),",
    "scheduler_job_id TEXT, manifest_path TEXT NOT NULL, manifest_checksum TEXT,",
    "runtime_directory TEXT, tracking_metadata TEXT NOT NULL,",
    "prepared_at TEXT NOT NULL, submitted_at TEXT, acknowledged_at TEXT,",
    "submission_exit_code INTEGER, submission_detail TEXT,",
    "bootstrap_failure_path TEXT)"
  ))
  DBI::dbExecute(con, paste(
    "CREATE INDEX IF NOT EXISTS job_submission_unit_state",
    "ON job_submission_attempts (unit_key, job_role, submission_state)"
  ))
  invisible(NULL)
}

#' Read registered attempts without migrating or writing the database
#' @param sqlite_db Tracking database path.
#' @return A data frame of pre-submission attempts, possibly empty.
#' @noRd
read_job_submission_attempts <- function(sqlite_db) {
  if (!checkmate::test_file_exists(sqlite_db)) return(data.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db, flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "job_submission_attempts")) return(data.frame())
  DBI::dbGetQuery(con, "SELECT * FROM job_submission_attempts ORDER BY prepared_at, rowid")
}

#' Expose safe submission identities and scope fields for read-only inspection
#' @param sqlite_db Tracking database path.
#' @return Registered submissions with decoded scalar work-unit metadata.
#' @noRd
inspect_job_submissions <- function(sqlite_db) {
  records <- read_job_submission_attempts(sqlite_db)
  if (!nrow(records)) return(records)
  metadata <- lapply(records$tracking_metadata, jsonlite::fromJSON)
  for (name in c("stage", "stream", "sub_id", "ses_id")) {
    records[[name]] <- vapply(metadata, function(row) {
      if (is.null(row[[name]])) NA_character_ else as.character(row[[name]])
    }, character(1))
  }
  records$tracking_metadata <- NULL
  records
}

#' Include registered runs that have no scheduler-bound jobs yet
#' @param runs Existing inspection run summary.
#' @param submissions Registered submission attempts.
#' @return Run summary ordered newest first, including unconfirmed runs.
#' @noRd
project_runs_with_submissions <- function(runs, submissions) {
  if (!nrow(submissions)) return(runs)
  valid <- !is.na(submissions$run_id) & nzchar(submissions$run_id)
  submissions <- submissions[valid, , drop = FALSE]
  for (run_id in unique(submissions$run_id)) {
    group <- submissions[submissions$run_id == run_id, , drop = FALSE]
    if (!run_id %in% runs$run_id) {
      row <- data.frame(run_id = run_id, submitted = min(group$prepared_at),
                        ended = NA_character_, n_jobs = 0L,
                        status = "UNKNOWN", n_completed = 0L, n_running = 0L,
                        n_queued = 0L, n_failed = 0L, n_blocked = 0L,
                        n_cancelled = 0L, n_unknown = 0L)
      runs <- rbind(runs, row)
    }
    if (any(group$submission_state %in% c("SUBMITTING", "UNKNOWN"))) {
      runs$status[runs$run_id == run_id] <- "UNCONFIRMED"
    }
  }
  if (!nrow(runs)) return(runs)
  timestamps <- sub("T", " ", sub("Z$", "", runs$submitted), fixed = TRUE)
  parsed <- suppressWarnings(as.numeric(as.POSIXct(timestamps, tz = "UTC")))
  runs[order(parsed, -seq_len(nrow(runs)), decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}

#' Register the immutable identity and metadata before invoking a scheduler
#' @param sqlite_db Tracking database path.
#' @param contract Allocated job contract.
#' @param tracking_args Job metadata, including the sealed manifest path.
#' @return NULL, invisibly; errors prevent scheduler submission.
#' @noRd
register_job_submission <- function(sqlite_db, contract, tracking_args) {
  if (is.null(sqlite_db)) return(invisible(NULL))
  if (!file.exists(sqlite_db)) create_tracking_db(sqlite_db)
  ensure_tracking_db_schema(sqlite_db)
  ensure_job_submission_schema(sqlite_db)
  # Restrict the prepared record to scalar tracking metadata. In particular,
  # do not put arbitrary job objects, credentials, or data in provenance JSON.
  fields <- c("job_name", "sequence_id", "batch_directory", "batch_file",
              "compute_file", "code_file", "n_nodes", "n_cpus", "wall_time",
              "mem_per_cpu", "mem_total", "scheduler", "scheduler_options",
              "stage", "stream", "sub_id", "ses_id", "job_role", "unit_key",
              "attempt", "job_manifest_path", "job_manifest_checksum",
              "stdout_log", "stderr_log", "parent_job_id", "child_level")
  metadata <- tracking_args[intersect(fields, names(tracking_args))]
  assert_provenance_metadata(metadata, "Submission attempt metadata")
  encoded <- as.character(jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null", na = "null"))
  job_submission_transaction(sqlite_db, function(con) {
    # An uncertain submission can already be running. Never duplicate it just
    # because the submitter lost the acknowledgement or died before binding.
    if (checkmate::test_string(tracking_args$unit_key) && nzchar(tracking_args$unit_key)) {
      unresolved <- DBI::dbGetQuery(con, paste(
        "SELECT contract_id FROM job_submission_attempts WHERE unit_key = ?",
        "AND COALESCE(job_role, '') = ? AND submission_state IN ('SUBMITTING','UNKNOWN')"
      ), params = list(tracking_args$unit_key,
                       if (is.null(tracking_args$job_role)) "" else tracking_args$job_role))
      if (nrow(unresolved)) {
        stop("An earlier submission for this work unit is unconfirmed: ",
             unresolved$contract_id[[1L]], ". Confirm its outcome before resubmitting.", call. = FALSE)
      }
    }
    DBI::dbExecute(con, paste(
      "INSERT INTO job_submission_attempts",
      "(contract_id, run_id, unit_key, job_role, attempt, submission_state, manifest_path,",
      "manifest_checksum, runtime_directory, tracking_metadata, prepared_at)",
      "VALUES (?, ?, ?, ?, ?, 'PREPARED', ?, ?, ?, ?, ?)"
    ), params = list(contract$contract_id,
                     if (is.null(tracking_args$sequence_id)) NA_character_ else tracking_args$sequence_id,
                     if (is.null(tracking_args$unit_key)) NA_character_ else tracking_args$unit_key,
                     if (is.null(tracking_args$job_role)) NA_character_ else tracking_args$job_role,
                     if (is.null(tracking_args$attempt)) 1L else tracking_args$attempt,
                     tracking_args$job_manifest_path, tracking_args$job_manifest_checksum,
                     if (is.null(tracking_args$runtime$directory)) NA_character_ else tracking_args$runtime$directory,
                     encoded, format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")))
  })
  invisible(NULL)
}

#' Record a submission phase without downgrading an already bound worker
#' @param sqlite_db Tracking database path, or NULL for untracked jobs.
#' @param contract_id Prepared attempt UUID.
#' @param state Submission phase.
#' @param exit_code Optional command exit code.
#' @param detail Optional non-sensitive explanation.
#' @return NULL, invisibly.
#' @noRd
set_job_submission_state <- function(sqlite_db, contract_id, state, exit_code = NA_integer_, detail = NA_character_) {
  if (is.null(sqlite_db)) return(invisible(NULL))
  job_submission_transaction(sqlite_db, function(con) {
    if (state == "SUBMITTING") {
      unresolved <- DBI::dbGetQuery(con, paste(
        "SELECT other.contract_id FROM job_submission_attempts current",
        "JOIN job_submission_attempts other ON other.unit_key = current.unit_key",
        "AND COALESCE(other.job_role, '') = COALESCE(current.job_role, '')",
        "LEFT JOIN job_tracking job ON job.contract_id = other.contract_id",
        "WHERE current.contract_id = ? AND other.contract_id != current.contract_id",
        "AND (other.submission_state IN ('SUBMITTING','UNKNOWN')",
        "OR (other.submission_state = 'ACCEPTED' AND job.status IN ('QUEUED','STARTED')))"
      ), params = list(contract_id))
      if (nrow(unresolved)) stop("An earlier submission is unconfirmed or active; refusing a duplicate launch.", call. = FALSE)
    }
    rows <- DBI::dbExecute(con, paste(
      "UPDATE job_submission_attempts SET submission_state = ?,",
      "submitted_at = CASE WHEN ? = 'SUBMITTING' THEN COALESCE(submitted_at, ?) ELSE submitted_at END,",
      "submission_exit_code = ?, submission_detail = ?",
      "WHERE contract_id = ? AND submission_state != 'ACCEPTED'"
    ), params = list(state, state, job_contract_timestamp(), exit_code, detail, contract_id))
    if (!rows && !nrow(DBI::dbGetQuery(con,
        "SELECT contract_id FROM job_submission_attempts WHERE contract_id = ?",
        params = list(contract_id)))) stop("Submission attempt is not registered.", call. = FALSE)
  })
  invisible(NULL)
}

#' Bind a worker or scheduler acknowledgement to its pre-existing attempt
#' @param sqlite_db Tracking database path.
#' @param contract_id Prepared attempt UUID, not a scheduler ID.
#' @param job_id Allocation ID, canonicalized for TORQUE arrays.
#' @return Canonical allocation ID; repeated bindings preserve worker state.
#' @noRd
bind_job_submission <- function(sqlite_db, contract_id, job_id) {
  job_id <- sub("\\[[0-9]+\\]", "[]", as.character(job_id))
  if (length(job_id) != 1L || is.na(job_id) || !nzchar(job_id)) stop("Worker job ID is missing.", call. = FALSE)
  job_submission_transaction(sqlite_db, function(con) {
    record <- DBI::dbGetQuery(con,
      "SELECT * FROM job_submission_attempts WHERE contract_id = ?", params = list(contract_id))
    if (nrow(record) != 1L) stop("Worker attempt is not registered: ", contract_id, call. = FALSE)
    if (!is.na(record$scheduler_job_id) && record$scheduler_job_id != job_id) {
      stop("Submission acknowledgement conflicts with the worker job ID.", call. = FALSE)
    }
    existing <- DBI::dbGetQuery(con, "SELECT contract_id FROM job_tracking WHERE job_id = ?",
                               params = list(job_id))
    if (nrow(existing) && (is.na(existing$contract_id[[1L]]) || existing$contract_id[[1L]] != contract_id)) {
      stop("Scheduler job ID belongs to another tracking attempt: ", job_id, call. = FALSE)
    }
    if (!nrow(existing)) {
      metadata <- jsonlite::fromJSON(record$tracking_metadata[[1L]], simplifyVector = FALSE)
      metadata$parent_id <- NULL
      if (!is.null(metadata$parent_job_id)) {
        parent <- DBI::dbGetQuery(con, "SELECT id FROM job_tracking WHERE job_id = ?",
                                 params = list(metadata$parent_job_id))
        if (nrow(parent)) metadata$parent_id <- parent$id[[1L]]
      }
      metadata$parent_job_id <- NULL
      metadata$job_id <- job_id
      metadata$contract_id <- contract_id
      metadata$status <- "QUEUED"
      metadata$time_submitted <- record$submitted_at[[1L]]
      if (is.na(metadata$time_submitted)) metadata$time_submitted <- record$prepared_at[[1L]]
      columns <- intersect(names(metadata), DBI::dbListFields(con, "job_tracking"))
      values <- lapply(metadata[columns], function(value) if (is.null(value)) NA else value)
      DBI::dbExecute(con, paste0(
        "INSERT INTO job_tracking (", paste(columns, collapse = ","), ") VALUES (",
        paste(rep("?", length(columns)), collapse = ","), ")"
      ), params = unname(values))
    }
    DBI::dbExecute(con, paste(
      "UPDATE job_submission_attempts SET scheduler_job_id = ?, submission_state = 'ACCEPTED',",
      "acknowledged_at = COALESCE(acknowledged_at, ?) WHERE contract_id = ?"
    ), params = list(job_id, format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"), contract_id))
    job_id
  })
}

#' Mark a non-array bootstrap failure without depending on BrainGnomes startup
#' @param sqlite_db Tracking database path.
#' @param contract_id Prepared UUID.
#' @param job_id Allocation ID.
#' @param exit_code Bootstrap process exit code.
#' @param receipt_path Persistent failure record.
#' @param array_task Whether failure belongs to one array task, not the allocation.
#' @return NULL, invisibly; array allocation completion remains sentinel-owned.
#' @noRd
record_job_bootstrap_failure <- function(sqlite_db, contract_id, job_id, exit_code, receipt_path, array_task = FALSE) {
  job_id <- bind_job_submission(sqlite_db, contract_id, job_id)
  job_submission_transaction(sqlite_db, function(con) {
    DBI::dbExecute(con, paste(
      "UPDATE job_submission_attempts SET bootstrap_failure_path =",
      "COALESCE(bootstrap_failure_path, ?) WHERE contract_id = ?"
    ), params = list(receipt_path, contract_id))
    if (!array_task) DBI::dbExecute(con, paste(
      "UPDATE job_tracking SET status = 'FAILED', time_ended = ?, exit_code = ?,",
      "failure_category = 'BOOTSTRAP' WHERE job_id = ? AND status IN ('QUEUED','STARTED')"
    ), params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"), exit_code, job_id))
  })
  invisible(NULL)
}
