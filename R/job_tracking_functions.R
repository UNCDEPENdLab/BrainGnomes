format_tracking_db_error <- function(sqlite_db = NULL, operation = "tracking DB operation", err = NULL) {
  db_path <- if (checkmate::test_string(sqlite_db)) {
    normalizePath(sqlite_db, winslash = "/", mustWork = FALSE)
  } else {
    "<unset>"
  }
  parent_dir <- if (checkmate::test_string(sqlite_db)) {
    normalizePath(dirname(sqlite_db), winslash = "/", mustWork = FALSE)
  } else {
    "<unset>"
  }

  db_exists <- checkmate::test_string(sqlite_db) && file.exists(sqlite_db)
  parent_exists <- checkmate::test_string(parent_dir) && dir.exists(parent_dir)
  db_write <- if (db_exists) isTRUE(file.access(sqlite_db, 2L) == 0L) else NA
  parent_write <- if (parent_exists) isTRUE(file.access(parent_dir, 2L) == 0L) else NA

  err_msg <- if (inherits(err, "condition")) conditionMessage(err) else as.character(err)
  err_msg <- if (length(err_msg) == 0L || is.na(err_msg) || !nzchar(err_msg)) "Unknown SQLite error" else err_msg

  hints <- c()
  low <- tolower(err_msg)
  if (grepl("readonly database", low, fixed = TRUE) || grepl("read-only", low, fixed = TRUE)) {
    hints <- c(hints, "Hint: SQLite is read-only. Check write permission for the DB file and its parent directory.")
  }
  if (grepl("unable to open database file", low, fixed = TRUE)) {
    hints <- c(hints, "Hint: SQLite could not be opened. Verify that the parent directory exists and is writable.")
  }
  if (grepl("database is locked", low, fixed = TRUE)) {
    hints <- c(hints, "Hint: SQLite is locked by another writer. Retry after active jobs finish or increase busy timeout.")
  }
  if (length(hints) == 0L) {
    hints <- "Hint: Verify SQLite path exists and is writable from compute nodes."
  }

  lines <- c(
    glue("Tracking database error during {operation}."),
    glue("Cause: {err_msg}"),
    glue("sqlite_db: {db_path}"),
    glue("db_exists: {db_exists}"),
    glue("db_write_access: {if (is.na(db_write)) 'NA' else db_write}"),
    glue("db_details: {describe_path_permissions(db_path)}"),
    glue("parent_dir: {parent_dir}"),
    glue("parent_exists: {parent_exists}"),
    glue("parent_write_access: {if (is.na(parent_write)) 'NA' else parent_write}"),
    glue("parent_details: {describe_path_permissions(parent_dir)}"),
    hints
  )
  paste(lines, collapse = "\n")
}

#' Internal helper function to submit a query to the tracking SQLite database
#'
#' @param str Character string SQLite query
#' @param sqlite_db Path to SQLite database used for tracking
#' @param param List of parameters/arguments to be used in query
#' @importFrom DBI dbExistsTable
#'
#' @keywords internal
submit_tracking_query = function(str, sqlite_db, param = NULL) {
  # previously called submit_sqlite()
  checkmate::assert_string(str)
  if (!checkmate::test_string(sqlite_db)) {
    stop(format_tracking_db_error(sqlite_db, operation = "submit_tracking_query", err = "sqlite_db is NULL or empty"), call. = FALSE)
  }
  
  # check if tracking table exists in sqlite_db; if not, create it
  table_exists <- tryCatch({
    con <- dbConnect(RSQLite::SQLite(), sqlite_db, synchronous = NULL) # establish connection
    on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
    sqliteSetBusyHandler(con, 10 * 1000) # busy_timeout of 10 seconds
    dbExistsTable(con, "job_tracking")
  }, error = function(e) {
    stop(format_tracking_db_error(sqlite_db, operation = "checking tracking table", err = e), call. = FALSE)
  })
  
  if (isFALSE(table_exists)) {
    tryCatch({
      create_tracking_db(sqlite_db)
    }, error = function(e) {
      stop(format_tracking_db_error(sqlite_db, operation = "create_tracking_db", err = e), call. = FALSE)
    })
  } else {
    tryCatch({
      ensure_tracking_db_schema(sqlite_db)
    }, error = function(e) {
      stop(format_tracking_db_error(sqlite_db, operation = "ensure_tracking_db_schema", err = e), call. = FALSE)
    })
  }
  
  # open sqlite connection and execute query
  tryCatch({
    submit_sqlite_query(str = str, sqlite_db = sqlite_db, param = param)
  }, error = function(e) {
    stop(format_tracking_db_error(sqlite_db, operation = "submit_tracking_query dbExecute", err = e), call. = FALSE)
  })
  
}

ensure_tracking_db_schema <- function(sqlite_db) {
  con <- tryCatch({
    dbConnect(RSQLite::SQLite(), sqlite_db, synchronous = NULL)
  }, error = function(e) {
    stop(format_tracking_db_error(sqlite_db, operation = "ensure_tracking_db_schema connect", err = e), call. = FALSE)
  })
  on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
  sqliteSetBusyHandler(con, 10 * 1000) # busy_timeout of 10 seconds
  cols <- tryCatch({
    dbGetQuery(con, "PRAGMA table_info(job_tracking)")
  }, error = function(e) {
    stop(format_tracking_db_error(sqlite_db, operation = "ensure_tracking_db_schema PRAGMA table_info", err = e), call. = FALSE)
  })
  col_names <- cols$name
  if (!"sequence_id" %in% col_names) {
    tryCatch(
      dbExecute(con, "ALTER TABLE job_tracking ADD COLUMN sequence_id VARCHAR"),
      error = function(e) {
        if (!grepl("duplicate column name", conditionMessage(e), fixed = TRUE)) {
          stop(format_tracking_db_error(sqlite_db, operation = "ALTER TABLE add sequence_id", err = e), call. = FALSE)
        }
      }
    )
  }
  if (!"child_level" %in% col_names) {
    tryCatch(
      dbExecute(con, "ALTER TABLE job_tracking ADD COLUMN child_level INTEGER DEFAULT 0"),
      error = function(e) {
        if (!grepl("duplicate column name", conditionMessage(e), fixed = TRUE)) {
          stop(format_tracking_db_error(sqlite_db, operation = "ALTER TABLE add child_level", err = e), call. = FALSE)
        }
      }
    )
  }
  if (!"output_manifest" %in% col_names) {
    tryCatch(
      dbExecute(con, "ALTER TABLE job_tracking ADD COLUMN output_manifest TEXT"),
      error = function(e) {
        if (!grepl("duplicate column name", conditionMessage(e), fixed = TRUE)) {
          stop(format_tracking_db_error(sqlite_db, operation = "ALTER TABLE add output_manifest", err = e), call. = FALSE)
        }
      }
    )
  }
  contract_columns <- c(
    contract_id = "VARCHAR",
    stage = "VARCHAR",
    stream = "VARCHAR",
    sub_id = "VARCHAR",
    ses_id = "VARCHAR",
    job_role = "VARCHAR",
    unit_key = "VARCHAR",
    attempt = "INTEGER",
    job_manifest_path = "TEXT",
    job_manifest_checksum = "VARCHAR",
    runtime_receipt_path = "TEXT",
    runtime_receipt_checksum = "VARCHAR",
    runtime_host = "VARCHAR",
    contract_status = "VARCHAR",
    scheduler_terminal_state = "VARCHAR",
    exit_code = "INTEGER",
    failure_category = "VARCHAR",
    stdout_log = "TEXT",
    stderr_log = "TEXT"
  )
  for (column in setdiff(names(contract_columns), col_names)) {
    statement <- paste(
      "ALTER TABLE job_tracking ADD COLUMN", column,
      unname(contract_columns[[column]])
    )
    tryCatch(
      dbExecute(con, statement),
      error = function(e) {
        if (!grepl("duplicate column name", conditionMessage(e), fixed = TRUE)) {
          stop(format_tracking_db_error(
            sqlite_db,
            operation = paste("ALTER TABLE add", column), err = e
          ), call. = FALSE)
        }
      }
    )
  }
  invisible(NULL)
}

#' Internal helper function to reset tracking SQLite database
#'
#' @param sqlite_db Path to SQLite database used for tracking
#'
#' @keywords internal
reset_tracking_sqlite_db = function(sqlite_db) {
  # this file has the SQL syntax to setup (and reset) the database
  # reset_sql <- "
  # SET foreign_key_checks = 0;
  # DROP TABLE IF EXISTS job_tracking;
  # SET foreign_key_checks = 1;
  # "
  reset_sql <- "DELETE FROM job_tracking" # delete all records
  submit_tracking_query(str = reset_sql, sqlite_db = sqlite_db)
}


#' Internal helper function to create the tracking SQLite database
#'
#' @param sqlite_db Path to SQLite database used for tracking
#'
#' @keywords internal
create_tracking_db = function(sqlite_db) {
  # previously called create_sqlite_db()
  job_spec_sql <- "
    CREATE TABLE job_tracking (
      id INTEGER PRIMARY KEY,
      parent_id INTEGER,
      child_level INTEGER DEFAULT 0,
      job_id VARCHAR NOT NULL UNIQUE,
      job_name VARCHAR,
      sequence_id VARCHAR,
      batch_directory VARCHAR,
      batch_file VARCHAR,
      compute_file VARCHAR,
      code_file VARCHAR,
      n_nodes INTEGER CHECK (n_nodes >= 1),
      n_cpus INTEGER CHECK (n_cpus >= 1),
      wall_time VARCHAR,
      mem_per_cpu VARCHAR,
      mem_total VARCHAR,
      scheduler VARCHAR,
      scheduler_options VARCHAR,
      job_obj BLOB,
      time_submitted INTEGER,
      time_started INTEGER,
      time_ended INTEGER,
      status VARCHAR(24),
      output_manifest TEXT,
      contract_id VARCHAR,
      stage VARCHAR,
      stream VARCHAR,
      sub_id VARCHAR,
      ses_id VARCHAR,
      job_role VARCHAR,
      unit_key VARCHAR,
      attempt INTEGER,
      job_manifest_path TEXT,
      job_manifest_checksum VARCHAR,
      runtime_receipt_path TEXT,
      runtime_receipt_checksum VARCHAR,
      runtime_host VARCHAR,
      contract_status VARCHAR,
      scheduler_terminal_state VARCHAR,
      exit_code INTEGER,
      failure_category VARCHAR,
      stdout_log TEXT,
      stderr_log TEXT,
      FOREIGN KEY (parent_id) REFERENCES job_tracking (id)
    );
    "
  # open sqlite connection
  tryCatch({
    submit_sqlite_query(str = job_spec_sql, sqlite_db = sqlite_db)
  }, error = function(e) {
    stop(format_tracking_db_error(sqlite_db, operation = "create_tracking_db schema creation", err = e), call. = FALSE)
  })
  ensure_job_submission_schema(sqlite_db)
}


#' Internal helper function to insert a job into the tracking SQLite database
#'
#' @param sqlite_db Path to SQLite database used for tracking
#' @param job_id Character string job ID
#' @param tracking_args List of tracking arguments for SQLite database
#'
#' @export
insert_tracked_job = function(sqlite_db, job_id, tracking_args = list()) {
  # previously called sqlite_insert_job()
  if (is.null(sqlite_db) || is.null(job_id)) return(invisible(NULL)) # skip out if not using DB or if job_id is NULL
  if (is.numeric(job_id)) job_id <- as.character(job_id)
  if (is.null(tracking_args$status)) tracking_args$status <- "QUEUED" # default value of first status
  if (checkmate::test_string(tracking_args$job_manifest_path) &&
      file.exists(tracking_args$job_manifest_path)) {
    manifest <- suppressWarnings(tryCatch(
      jsonlite::read_json(
        tracking_args$job_manifest_path, simplifyVector = TRUE
      ),
      error = function(e) NULL
    ))
    if (is.list(manifest)) {
      manifest_values <- list(
        contract_id = manifest$contract_id,
        sequence_id = manifest$run_id,
        stage = manifest$logical_work_unit$stage,
        stream = manifest$logical_work_unit$stream,
        sub_id = manifest$logical_work_unit$subject_id,
        ses_id = manifest$logical_work_unit$session_id,
        job_role = manifest$logical_work_unit$role,
        unit_key = manifest$logical_work_unit$unit_key,
        attempt = manifest$logical_work_unit$attempt,
        stdout_log = manifest$execution$stdout_log,
        stderr_log = manifest$execution$stderr_log
      )
      for (field in names(manifest_values)) {
        if (is.null(tracking_args[[field]]) || is.na(tracking_args[[field]])) {
          tracking_args[[field]] <- manifest_values[[field]]
        }
      }
    }
    if (!checkmate::test_string(tracking_args$contract_id)) {
      tracking_args$contract_id <- basename(dirname(tracking_args$job_manifest_path))
    }
    if (!checkmate::test_string(tracking_args$job_manifest_checksum)) {
      tracking_args$job_manifest_checksum <- unname(tools::md5sum(
        tracking_args$job_manifest_path
      ))
    }
  }

  if (checkmate::test_string(tracking_args$contract_id)) {
    attempts <- read_job_submission_attempts(sqlite_db)
    if (nrow(attempts) && tracking_args$contract_id %in% attempts$contract_id) {
      # Prepared external workers may bind themselves before the submitter's
      # legacy insertion helper runs. Never reset their STARTED/terminal state.
      bind_job_submission(sqlite_db, tracking_args$contract_id, job_id)
      if (!is.null(tracking_args$job_obj)) {
        submit_tracking_query("UPDATE job_tracking SET job_obj = ? WHERE job_id = ?",
                              sqlite_db, param = list(tracking_args$job_obj, job_id))
      }
      return(invisible(NULL))
    }
  }
  
  insert_job_sql <- "INSERT INTO job_tracking
    (job_id, job_name, sequence_id, batch_directory,
    batch_file, compute_file, code_file,
    n_nodes, n_cpus, wall_time,
    mem_per_cpu, mem_total,
    scheduler, scheduler_options, job_obj,
    time_submitted, status, contract_id, stage, stream, sub_id, ses_id,
    job_role, unit_key, attempt, job_manifest_path, job_manifest_checksum,
    runtime_receipt_path, runtime_receipt_checksum, runtime_host,
    contract_status, scheduler_terminal_state, exit_code, failure_category,
    stdout_log, stderr_log)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
      ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  
  # gather tracking parameters into a list
  param <- list(job_id, tracking_args$job_name, tracking_args$sequence_id, 
                tracking_args$batch_directory, tracking_args$batch_file, 
                tracking_args$compute_file, tracking_args$code_file, 
                tracking_args$n_nodes, tracking_args$n_cpus, tracking_args$wall_time, 
                tracking_args$mem_per_cpu, tracking_args$mem_total, tracking_args$scheduler,
                tracking_args$scheduler_options, tracking_args$job_obj, 
                as.character(Sys.time()), tracking_args$status,
                tracking_args$contract_id, tracking_args$stage,
                tracking_args$stream, tracking_args$sub_id,
                tracking_args$ses_id, tracking_args$job_role,
                tracking_args$unit_key, tracking_args$attempt,
                tracking_args$job_manifest_path,
                tracking_args$job_manifest_checksum,
                tracking_args$runtime_receipt_path,
                tracking_args$runtime_receipt_checksum,
                tracking_args$runtime_host, tracking_args$contract_status,
                tracking_args$scheduler_terminal_state,
                tracking_args$exit_code, tracking_args$failure_category,
                tracking_args$stdout_log, tracking_args$stderr_log)
  
  for (i in 1:length(param)) {
    param[[i]] <- ifelse(is.null(param[[i]]), NA, param[[i]]) # convert NULL values to NA for dbExecute
  }
  
  # order the tracking arguments to match the query; status is always 'QUEUED' when first added to the database
  submit_tracking_query(str = insert_job_sql, sqlite_db = sqlite_db, param = param)
}


#' Add parent/child id relationship to tracking database
#'
#' @param sqlite_db Path to SQLite database used for tracking
#' @param job_id Job id of job for which to add a parent
#' @param parent_job_id Job id of the parent job to job_id
#' @param child_level Level of child; currently supports two levels
#'
#' @importFrom DBI dbConnect dbExecute dbDisconnect
#' @export
add_tracked_job_parent = function(sqlite_db = NULL, job_id = NULL, parent_job_id = NULL, child_level = 1) {
  # skip out if not using DB or job_id/parent_id is NULL
  if (is.null(sqlite_db) || is.null(job_id) || is.null(parent_job_id)) return(invisible(NULL))
  if (isFALSE(child_level %in% 1:2)) child_level <- NA # if level not 1 or 2, return NA
  if (is.numeric(job_id)) job_id <- as.character(job_id)
  if (is.numeric(parent_job_id)) parent_job_id <- as.character(parent_job_id)
  
  # retrieve sequence id from parent
  sequence_id_sql <- "SELECT sequence_id FROM job_tracking WHERE job_id = ?"
  sequence_id <- tryCatch({
    # open sqlite connection and execute query
    id <- submit_sqlite_query(str = sequence_id_sql, sqlite_db = sqlite_db, 
                              param = list(parent_job_id), return_result = TRUE)
    if(nrow(id) > 0L && !is.na(id[1,1])) { id[1,1] } else { NA }
  }, error = function(e) {
    warning(format_tracking_db_error(sqlite_db, operation = "add_tracked_job_parent sequence lookup", err = e), call. = FALSE)
    NA
  })
  
  
  add_parent_sql <- "UPDATE job_tracking
    SET parent_id = (SELECT id FROM job_tracking WHERE job_id = ?), 
      sequence_id = ?,
      child_level = ?
    WHERE job_id = ?"
  tryCatch({
    # open sqlite connection and execute query
    submit_sqlite_query(str = add_parent_sql, sqlite_db = sqlite_db, 
                        param = list(parent_job_id, sequence_id, child_level, job_id))
  }, error = function(e) {
    warning(format_tracking_db_error(sqlite_db, operation = "add_tracked_job_parent parent update", err = e), call. = FALSE)
    return(NULL)
  })
  
}


#' Update Job Status in Tracking SQLite Database
#'
#' Updates the status of a specific job in a tracking database, optionally cascading failure status to downstream jobs.
#'
#' @param sqlite_db Character string. Path to the SQLite database file used for job tracking.
#' @param job_id Character string or numeric. ID of the job to update. If numeric, it will be coerced to a string.
#' @param status Character string. The job status to set. Must be one of:
#'   \code{"QUEUED"}, \code{"STARTED"}, \code{"FAILED"}, \code{"COMPLETED"}, \code{"FAILED_BY_EXT"}.
#' @param output_manifest Character string. Optional JSON manifest of output files to store when
#'   status is \code{"COMPLETED"}. See \code{\link{capture_output_manifest}}.
#' @param cascade Logical. If \code{TRUE}, and the \code{status} is a failure type (\code{"FAILED"} or \code{"FAILED_BY_EXT"}),
#'   the failure is recursively propagated to child jobs not listed in \code{exclude}.
#' @param exclude Character or numeric vector. One or more job IDs to exclude from cascading failure updates.
#' @param strict Logical. If \code{TRUE}, invalid tracking targets, failed SQLite
#'   writes, and unmatched job identifiers raise errors instead of warning or
#'   silently returning. Worker commands use this to require persisted updates.
#'
#' @details
#' The function updates both the job \code{status} and a timestamp corresponding to the status type:
#' \itemize{
#'   \item \code{"QUEUED"} -> updates \code{time_submitted}
#'   \item \code{"STARTED"} -> updates \code{time_started}
#'   \item \code{"FAILED"}, \code{"COMPLETED"}, or \code{"FAILED_BY_EXT"} -> updates \code{time_ended}
#' }
#' A `"STARTED"` update also writes a one-time runtime receipt when the
#' tracking row has an associated job manifest. The receipt records the compute
#' host and verifies that the sealed manifest and execution-driving files still
#' match their submission-time checksums. A manifest configured with the default
#' `"fail"` drift policy stops execution when verification fails.
#'
#' When \code{status} is \code{"COMPLETED"} and \code{output_manifest} is provided, the manifest
#' is stored in the \code{output_manifest} column for later verification. Completion
#' status, its timestamp, and the manifest are written atomically in one SQL update.
#'
#' If \code{cascade = TRUE}, and the status is \code{"FAILED"} or \code{"FAILED_BY_EXT"}, any dependent jobs (as determined
#' via \code{get_tracked_job_status()}) will be recursively marked as \code{"FAILED_BY_EXT"}, unless their status is already
#' \code{"FAILED"} or they are listed in \code{exclude}.
#'
#' If no tracking row matches \code{job_id}, a warning is emitted and no manifest/cascade
#' updates are attempted, preventing silent status-update failures.
#'
#' If \code{sqlite_db} or \code{job_id} is invalid or missing, the function fails
#' silently and returns \code{NULL}, unless \code{strict = TRUE}.
#'
#' @return Invisibly returns \code{NULL}. Side effect is a modification to the SQLite job tracking table.
#'
#' @importFrom glue glue
#' @importFrom DBI dbConnect dbExecute dbDisconnect
#' @export
update_tracked_job_status <- function(sqlite_db = NULL, job_id = NULL, status, 
                                      output_manifest = NULL, cascade = FALSE, exclude = NULL,
                                      strict = FALSE) {
  checkmate::assert_flag(strict)
  if (strict) {
    checkmate::assert_string(sqlite_db)
    checkmate::assert_file_exists(sqlite_db)
  }
  if (!checkmate::test_string(sqlite_db)) return(invisible(NULL))
  if (is.numeric(job_id)) job_id <- as.character(job_id)
  if (strict) checkmate::assert_string(job_id)
  if (!checkmate::test_string(job_id)) return(invisible(NULL)) # quiet failure on invalid job id
  resolved_job_id <- resolve_tracked_job_id(sqlite_db, job_id)
  if (checkmate::test_string(resolved_job_id)) job_id <- resolved_job_id

  checkmate::assert_string(status)
  status <- toupper(status)
  checkmate::assert_subset(status, c("QUEUED", "STARTED", "FAILED", "COMPLETED", "FAILED_BY_EXT", "CANCELLED"))
  if (cascade && status %in% c("QUEUED", "STARTED", "COMPLETED", "CANCELLED")) {
    cascade <- FALSE
    warning("Only status FAILED or FAILED_BY_EXT can cascade in `update_tracked_job_status`")
  }
  
  now <- as.character(Sys.time())
  time_field <- switch(status,
                       QUEUED = "time_submitted",
                       STARTED = "time_started",
                       FAILED = "time_ended",
                       COMPLETED = "time_ended",
                       FAILED_BY_EXT = "time_ended",
                       CANCELLED = "time_ended"
  )

  update_sql <- glue("UPDATE job_tracking SET STATUS = ?, {time_field} = ? WHERE job_id = ?")
  update_params <- list(status, now, job_id)
  if (status == "COMPLETED") {
    has_manifest <- is.character(output_manifest) &&
      length(output_manifest) == 1L && !is.na(output_manifest) && nzchar(output_manifest)
    manifest_value <- if (has_manifest) output_manifest else NA_character_
    # A failed manifest write must not leave a successful completion behind.
    update_sql <- glue("UPDATE job_tracking SET STATUS = ?, {time_field} = ?, output_manifest = ? WHERE job_id = ?")
    update_params <- list(status, now, manifest_value, job_id)
  }
  rows_updated <- tryCatch({
    submit_tracking_query(
      str = update_sql,
      sqlite_db = sqlite_db, 
      param = update_params
    )
  }, error = function(e) {
    message <- format_tracking_db_error(sqlite_db, operation = "update_tracked_job_status", err = e)
    if (strict) stop(message, call. = FALSE)
    warning(message, call. = FALSE)
    return(NA_integer_)
  })

  if (length(rows_updated) != 1L || !is.numeric(rows_updated) || is.na(rows_updated) || rows_updated < 1) {
    message <- glue(
      "update_tracked_job_status did not match any row for job_id '{job_id}' in {sqlite_db}. ",
      "Status remains unchanged in SQLite."
    )
    if (strict) stop(message, call. = FALSE)
    warning(message, call. = FALSE)
    return(invisible(NULL))
  }

  if (status == "STARTED") {
    record_job_runtime_receipt(
      sqlite_db = sqlite_db, job_id = job_id
    )
  }
  
  # recursive function for "cascading" failures using status "FAILED_BY_EXT"
  if (cascade) {
    if (is.numeric(exclude)) exclude <- as.character(exclude)
    
    status_tree <- get_tracked_job_status(
      job_id = job_id,
      return_children = TRUE,
      sqlite_db = sqlite_db
    ) # retrieve current job and children
    if (!is.data.frame(status_tree) || nrow(status_tree) == 0L) return(invisible(NULL))
    job_ids <- status_tree$job_id # get list of job ids
    
    for (child_job in setdiff(job_ids, c(job_id, exclude))) {
      child_idx <- which(status_tree$job_id == child_job)
      child_status <- if (length(child_idx) > 0L) status_tree$status[child_idx[1L]] else NA_character_ # check status
      if (length(child_status) != 1L || is.na(child_status) || child_status != "FAILED") {
        update_tracked_job_status(
          sqlite_db = sqlite_db,
          job_id = child_job,
          status = "FAILED_BY_EXT",
          cascade = TRUE,
          strict = strict
        )
      }
    }
  }
  
  return(invisible(NULL))
  
}

resolve_tracked_job_id <- function(sqlite_db, job_id) {
  if (!checkmate::test_string(sqlite_db) || !file.exists(sqlite_db) ||
      !checkmate::test_string(job_id)) {
    return(NULL)
  }
  exact <- suppressWarnings(tryCatch(
    submit_sqlite_query(
      "SELECT job_id FROM job_tracking WHERE job_id = ? LIMIT 1",
      sqlite_db = sqlite_db, param = list(job_id), return_result = TRUE
    ),
    error = function(e) NULL
  ))
  if (is.data.frame(exact) && nrow(exact) > 0L) {
    return(as.character(exact$job_id[[1L]]))
  }
  if (!grepl("\\[[0-9]+\\]", job_id)) return(NULL)
  candidates <- unique(c(
    sub("\\[[0-9]+\\]", "[]", job_id),
    sub("\\[[0-9]+\\]", "", job_id)
  ))
  for (candidate in candidates) {
    match <- suppressWarnings(tryCatch(
      submit_sqlite_query(
        "SELECT job_id FROM job_tracking WHERE job_id = ? LIMIT 1",
        sqlite_db = sqlite_db, param = list(candidate), return_result = TRUE
      ),
      error = function(e) NULL
    ))
    if (is.data.frame(match) && nrow(match) > 0L) {
      return(as.character(match$job_id[[1L]]))
    }
  }
  NULL
}

#' Query job status in tracking SQLite database
#' 
#' @param job_id The job id for which to retreive the status
#' @param return_children Return child jobs of this job
#' @param return_parent Return parent jobs of this job
#' @param sequence_id The sequence id to query instead of \code{job_id}
#' @param sqlite_db Character string of sqlite database
#' 
#' @return An R data.frame version of the tracking database
#' @details Status reads wait up to 10 seconds for a concurrent SQLite writer
#'   before reporting a database lock error.
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom checkmate assert_logical test_file_exists
#' @importFrom RSQLite SQLite
#' @export
get_tracked_job_status <- function(job_id = NULL, return_children = FALSE, return_parent = FALSE, 
                                   sequence_id = NULL, sqlite_db) {
  
  con <- NULL
  on.exit(try(if (!is.null(con)) dbDisconnect(con), silent = TRUE), add = TRUE)
  if (!checkmate::test_file_exists(sqlite_db)) {
    warning("Cannot find SQLite database at: ", sqlite_db)
  }
  stopifnot("Must specify `job_id` or `sequence_id`, not both" =
              is.null(job_id) || is.null(sequence_id))
  
  if (!is.null(sequence_id)) {
    
    if (is.numeric(sequence_id)) sequence_id <- as.character(sequence_id)
    if (!checkmate::test_string(sequence_id)) return(invisible(NULL))
    str <- paste0("SELECT * FROM job_tracking WHERE sequence_id = ?")
    param <- list(sequence_id)
    
  } else {
    
    if (is.numeric(job_id)) job_id <- as.character(job_id)
    if (!checkmate::test_string(job_id)) return(invisible(NULL))
    checkmate::assert_logical(return_children)
    checkmate::assert_logical(return_parent)
    
    str <- paste0("SELECT * FROM job_tracking WHERE job_id = ?", 
                  ifelse(return_children, " OR parent_id = (SELECT id FROM job_tracking WHERE job_id = ?)", ""),
                  ifelse(return_parent, " OR id = (SELECT parent_id FROM job_tracking WHERE job_id = ?)", ""))
    param <- as.list(rep(job_id, 1 + return_children + return_parent))
  }
  
  con <- tryCatch({
    dbConnect(RSQLite::SQLite(), sqlite_db, synchronous = NULL)
  }, error = function(e) {
    stop(format_tracking_db_error(sqlite_db, operation = "get_tracked_job_status connect", err = e), call. = FALSE)
  })
  df <- tryCatch({
    RSQLite::sqliteSetBusyHandler(con, 10000L)
    dbGetQuery(con, str, params = param)
  }, error = function(e) {
    stop(format_tracking_db_error(sqlite_db, operation = "get_tracked_job_status query", err = e), call. = FALSE)
  })
  
  # rehydrate job_obj back into R6 class
  if (nrow(df) > 0L) df$job_obj <- lapply(df$job_obj, function(x) if (!is.null(x)) unserialize(x))
  return(df)
}


#' Internal helper function to update tracker_args object
#'
#' @param list_to_populate The list whose argument will be populated/updated
#' @param arg_name The named list element to update
#' @param new_value The new value to update the element with
#' @param append If TRUE, appends the new value to the current value using the paste function. Default: FALSE
#'
#' @keywords internal
populate_list_arg = function(list_to_populate, arg_name, new_value = NULL, append = FALSE) {
  
  checkmate::assert_list(list_to_populate)
  if (is.null(new_value)) return(list_to_populate) # if the new_value arg is NULL just return the list as is

  if(is.null(list_to_populate[[arg_name]]) || is.na(list_to_populate[[arg_name]])) {
    list_to_populate[[arg_name]] <- new_value # if the current arg is NULL, update with new value
  } else if (append) {
    # if it's not NULL but append is TRUE, appends new value to beginning of old value
    list_to_populate[[arg_name]] <- paste(list_to_populate[[arg_name]], new_value, sep = "\n")  
  }
  return(list_to_populate)
}


#' Capture output manifest for a completed step
#'
#' Creates a JSON manifest containing output file paths, sizes, and modification
#' times. By default all files beneath `output_dir` are scanned; `files` can
#' instead identify an exact job-specific subset. The manifest can be stored in
#' the job tracking database and later used to verify that outputs remain intact.
#'
#' @param output_dir Root directory used to locate and verify output files.
#' @param recursive Scan subdirectories (default TRUE)
#' @param pattern Optional regex pattern to filter files.
#' @param files Optional character vector of exact output files to record. When
#'   supplied, only these files are included and they must exist beneath
#'   `output_dir`. This is useful when several jobs share an output directory.
#'
#' @return JSON string containing the manifest, or NULL if directory doesn't exist
#'
#' @importFrom jsonlite toJSON
#' @keywords internal
capture_output_manifest <- function(output_dir, recursive = TRUE, pattern = NULL,
                                    files = NULL) {
  if (!checkmate::test_directory_exists(output_dir)) {
    return(NULL)
  }

  norm_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  scope <- if (is.null(files)) "directory" else "explicit"
  if (is.null(files)) {
    files <- list.files(
      output_dir,
      full.names = TRUE,
      recursive = recursive,
      pattern = pattern,
      all.files = FALSE
    )
  } else {
    checkmate::assert_character(files, any.missing = FALSE)
    files <- unique(files[nzchar(files)])
    if (length(files) > 0L && any(!file.exists(files))) {
      stop(
        "Cannot capture output manifest because these files do not exist: ",
        paste(files[!file.exists(files)], collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (length(files) == 0) {
    manifest <- list(
      output_dir = norm_dir,
      scope = scope,
      captured_at = as.character(Sys.time()),
      file_count = 0L,
      total_size_bytes = 0L,
      files = list()
    )
    return(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = FALSE))
  }
  
  norm_files <- sort(unique(normalizePath(
    files,
    winslash = "/",
    mustWork = TRUE
  )))
  root_prefix <- paste0(sub("/+$", "", norm_dir), "/")
  if (any(!startsWith(norm_files, root_prefix))) {
    stop(
      "All manifest files must be located beneath output_dir: ",
      norm_dir,
      call. = FALSE
    )
  }
  info <- file.info(norm_files)
  if (any(info$isdir)) {
    stop("Manifest entries must be files, not directories.", call. = FALSE)
  }
  relative_files <- substring(norm_files, nchar(root_prefix) + 1L)

  manifest <- list(
    output_dir = norm_dir,
    scope = scope,
    captured_at = as.character(Sys.time()),
    file_count = length(norm_files),
    total_size_bytes = sum(info$size, na.rm = TRUE),
    files = lapply(seq_len(nrow(info)), function(i) {
      list(
        path = relative_files[i],
        size = info$size[i],
        mtime = as.numeric(info$mtime[i])
      )
    })
  )
  
  jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = FALSE)
}


#' Verify current directory state against stored manifest
#'
#' Compares the current contents of an output directory against a previously
#' captured manifest. This can detect missing files, size changes, or other
#' modifications that may indicate incomplete or corrupted outputs.
#'
#' @param output_dir Directory to check
#' @param manifest_json JSON string from database (as returned by capture_output_manifest)
#' @param check_mtime Also verify modification times match (default FALSE)
#'
#' @return List with:
#'   \itemize{
#'     \item \code{verified}: logical indicating if all files match, or NA if no manifest
#'     \item \code{reason}: character string describing the result
#'     \item \code{missing}: character vector of missing file paths
#'     \item \code{changed}: character vector of files with size/mtime changes
#'     \item \code{extra}: character vector of files not in original manifest
#'     \item \code{manifest_time}: timestamp when manifest was captured
#'   }
#'
#' @importFrom jsonlite fromJSON
#' @keywords internal
verify_output_manifest <- function(output_dir, manifest_json, check_mtime = FALSE) {
  # Handle missing or invalid manifest

if (is.null(manifest_json) || (length(manifest_json) == 1 && is.na(manifest_json)) || 
      (is.character(manifest_json) && nchar(manifest_json) == 0)) {
    return(list(
      verified = NA,
      reason = "no_manifest",
      missing = character(0),
      changed = character(0),
      extra = character(0),
      manifest_time = NA_character_
    ))
  }
  
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_json, simplifyVector = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(manifest)) {
    return(list(
      verified = NA,
      reason = "invalid_manifest_json",
      missing = character(0),
      changed = character(0),
      extra = character(0),
      manifest_time = NA_character_
    ))
  }
  
  # Check if directory exists
  if (!dir.exists(output_dir)) {
    return(list(
      verified = FALSE,
      reason = "directory_missing",
      missing = output_dir,
      changed = character(0),
      extra = character(0),
      manifest_time = manifest$captured_at
    ))
  }
  
  # Handle empty manifest (no files expected)
  if (length(manifest$files) == 0) {
    current_files <- list.files(output_dir, recursive = TRUE, all.files = FALSE)
    return(list(
      verified = length(current_files) == 0,
      reason = if (length(current_files) == 0) "ok" else "unexpected_files",
      missing = character(0),
      changed = character(0),
      extra = current_files,
      manifest_time = manifest$captured_at
    ))
  }
  
  # Extract expected file info from manifest
  expected_files <- vapply(manifest$files, function(x) x$path, character(1))
  expected_sizes <- vapply(manifest$files, function(x) as.numeric(x$size), numeric(1))
  expected_mtimes <- vapply(manifest$files, function(x) as.numeric(x$mtime), numeric(1))
  
  missing <- character(0)
  changed <- character(0)
  
  # Check each expected file
  for (i in seq_along(expected_files)) {
    full_path <- file.path(output_dir, expected_files[i])
    
    if (!file.exists(full_path)) {
      missing <- c(missing, expected_files[i])
    } else {
      info <- file.info(full_path)
      if (!is.na(expected_sizes[i]) && info$size != expected_sizes[i]) {
        changed <- c(changed, expected_files[i])
      } else if (check_mtime && !is.na(expected_mtimes[i]) && 
                 abs(as.numeric(info$mtime) - expected_mtimes[i]) > 1) {
        changed <- c(changed, expected_files[i])
      }
    }
  }
  
  # Check for unexpected extra files (informational)
  current_files <- list.files(output_dir, recursive = TRUE, all.files = FALSE)
  extra <- setdiff(current_files, expected_files)
  
  verified <- length(missing) == 0 && length(changed) == 0
  
  list(
    verified = verified,
    reason = if (verified) "ok" else "files_differ",
    missing = missing,
    changed = changed,
    extra = extra,
    manifest_time = manifest$captured_at
  )
}
