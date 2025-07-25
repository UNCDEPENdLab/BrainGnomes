#' Internal helper function to submit a query to the tracking SQLite database
#'
#' @param str Character string SQLite query
#' @param sqlite_db Path to SQLite database used for tracking
#' @param param List of parameters/arguments to be used in query
#'
#' @keywords internal
submit_tracking_query = function(str, sqlite_db, param = NULL) {
  # previously called submit_sqlite()
  checkmate::assert_string(str)
  
  # check if tracking table exists in sqlite_db; if not, create it
  con <- dbConnect(RSQLite::SQLite(), sqlite_db) # establish connection
  sqliteSetBusyHandler(con, 10 * 1000) # busy_timeout of 10 seconds
  table_exists <- dbExistsTable(con, "job_tracking")
  dbDisconnect(con)
  
  if (isFALSE(table_exists)) {
    create_tracking_db(sqlite_db)
  }
  
  # open sqlite connection and execute query
  submit_sqlite_query(str = str, sqlite_db = sqlite_db, param = param)
  
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
      job_id VARCHAR NOT NULL UNIQUE,
      job_name VARCHAR,
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
      FOREIGN KEY (parent_id) REFERENCES job_tracking (id)
    );
    "
  # open sqlite connection
  submit_sqlite_query(str = job_spec_sql, sqlite_db = sqlite_db)
}


#' Internal helper funciton to insert a job into the tracking SQLite database
#'
#' @param sqlite_db Path to SQLite database used for tracking
#'
#' @keywords internal
insert_tracked_job = function(sqlite_db, job_id, tracking_args = list()) {
  # previously called sqlite_insert_job()
  if (is.null(sqlite_db) || is.null(job_id)) return(invisible(NULL)) # skip out if not using DB or if job_id is NULL
  if (is.numeric(job_id)) job_id <- as.character(job_id)
  if (is.null(tracking_args$status)) tracking_args$status <- "QUEUED" # default value of first status

  sql <- "INSERT INTO job_tracking
    (job_id, job_name, batch_directory,
    batch_file, compute_file, code_file,
    n_nodes, n_cpus, wall_time,
    mem_per_cpu, mem_total,
    scheduler, scheduler_options, job_obj,
    time_submitted, status)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  # gather tracking parameters into a list
  param <- list(job_id, tracking_args$job_name, tracking_args$batch_directory, 
                tracking_args$batch_file, tracking_args$compute_file, tracking_args$code_file, 
                tracking_args$n_nodes, tracking_args$n_cpus, tracking_args$wall_time, 
                tracking_args$mem_per_cpu, tracking_args$mem_total, tracking_args$scheduler,
                tracking_args$scheduler_options, tracking_args$job_obj, as.character(Sys.time()), tracking_args$status)
  
  for (i in 1:length(param)) {
    param[[i]] <- ifelse(is.null(param[[i]]), NA, param[[i]]) # convert NULL values to NA for dbExecute
  }
  
  # order the tracking arguments to match the query; status is always 'QUEUED' when first added to the database
  submit_tracking_query(str = sql, sqlite_db = sqlite_db, param = param)
}


#' Add parent/child id relationship to tracking database
#'
#' @param sqlite_db Path to SQLite database used for tracking
#' @param job_id Job id of job for which to add a parent
#' @param parent_job_id Job id of the parent job to job_id
#'
#' @importFrom DBI dbConnect dbExecute dbDisconnect
#' @export
add_tracked_job_parent = function(sqlite_db = NULL, job_id = NULL, parent_job_id = NULL) {
  # skip out if not using DB or job_id/parent_id is NULL
  if (is.null(sqlite_db) || is.null(job_id) || is.null(parent_job_id)) return(invisible(NULL)) 
  if (is.numeric(job_id)) job_id <- as.character(job_id)
  if (is.numeric(parent_job_id)) parent_job_id <- as.character(parent_job_id)

  sql <- "UPDATE job_tracking
  SET parent_id = (SELECT id FROM job_tracking WHERE job_id = ?)
  WHERE job_id = ?"
  
  tryCatch({
    # open sqlite connection and execute query
    submit_sqlite_query(str = sql, sqlite_db = sqlite_db, param = list(parent_job_id, job_id))
  }, error = function(e) { print(e); return(NULL)})
}


#' Update Job Status in Tracking SQLite Database
#'
#' Updates the status of a specific job in a tracking database, optionally cascading failure status to downstream jobs.
#'
#' @param sqlite_db Character string. Path to the SQLite database file used for job tracking.
#' @param job_id Character string or numeric. ID of the job to update. If numeric, it will be coerced to a string.
#' @param status Character string. The job status to set. Must be one of:
#'   \code{"QUEUED"}, \code{"STARTED"}, \code{"FAILED"}, \code{"COMPLETED"}, \code{"FAILED_BY_EXT"}.
#' @param cascade Logical. If \code{TRUE}, and the \code{status} is a failure type (\code{"FAILED"} or \code{"FAILED_BY_EXT"}),
#'   the failure is recursively propagated to child jobs not listed in \code{exclude}.
#' @param exclude Character or numeric vector. One or more job IDs to exclude from cascading failure updates.
#'
#' @details
#' The function updates both the job \code{status} and a timestamp corresponding to the status type:
#' \itemize{
#'   \item \code{"QUEUED"} → updates \code{time_submitted}
#'   \item \code{"STARTED"} → updates \code{time_started}
#'   \item \code{"FAILED"}, \code{"COMPLETED"}, or \code{"FAILED_BY_EXT"} → updates \code{time_ended}
#' }
#'
#' If \code{cascade = TRUE}, and the status is \code{"FAILED"} or \code{"FAILED_BY_EXT"}, any dependent jobs (as determined
#' via \code{get_tracked_job_status()}) will be recursively marked as \code{"FAILED_BY_EXT"}, unless their status is already
#' \code{"FAILED"} or they are listed in \code{exclude}.
#'
#' If \code{sqlite_db} or \code{job_id} is invalid or missing, the function fails silently and returns \code{NULL}.
#'
#' @return Invisibly returns \code{NULL}. Side effect is a modification to the SQLite job tracking table.
#'
#' @importFrom glue glue
#' @importFrom DBI dbConnect dbExecute dbDisconnect
#' @export
update_tracked_job_status <- function(sqlite_db = NULL, job_id = NULL, status, cascade = FALSE, exclude = NULL) {
  
  if (!checkmate::test_string(sqlite_db)) return(invisible(NULL))
  if (is.numeric(job_id)) job_id <- as.character(job_id)
  if (!checkmate::test_string(job_id)) return(invisible(NULL)) # quiet failure on invalid job id

  checkmate::assert_string(status)
  status <- toupper(status)
  checkmate::assert_subset(status, c("QUEUED", "STARTED", "FAILED", "COMPLETED", "FAILED_BY_EXT"))
  if (cascade & status %in% c("QUEUED", "STARTED", "COMPLETED")) {
    cascade <- FALSE
    warning("Only status FAILED or FAILED_BY_EXT can cascade in `update_tracked_job_status`")
  }
  
  now <- as.character(Sys.time())
  time_field <- switch(status,
                       QUEUED = "time_submitted",
                       STARTED = "time_started",
                       FAILED = "time_ended",
                       COMPLETED = "time_ended",
                       FAILED_BY_EXT = "time_ended"
  )
  
  tryCatch({
    submit_sqlite_query(str = glue("UPDATE job_tracking SET STATUS = ?, {time_field} = ? WHERE job_id = ?"),
                        sqlite_db = sqlite_db, param = list(status, now, job_id))
  }, error = function(e) { print(e); return(NULL)})
  
  # recursive function for "cascading" failures using status "FAILED_BY_EXT"
  if (cascade) {
    if (is.numeric(exclude)) exclude <- as.character(exclude)
    
    status_tree <- get_tracked_job_status(job_id, return_children = TRUE, sqlite_db = sqlite_db) # retreive current job and children
    job_ids <- status_tree$job_id # get list of job ids
    
    for (child_job in setdiff(job_ids, c(job_id, exclude))) {
      child_status <- with(status_tree, status[which(job_ids == child_job)]) # check status
      if (child_status != "FAILED") update_tracked_job_status(child_job, sqlite_db = sqlite_db, status = "FAILED_BY_EXT", cascade = TRUE)
    }
  }
  
  return(invisible(NULL))
  
}

#' Query job status in tracking SQLite database
#' 
#' @param job_id The job id for which to retreive the status
#' @param sqlite_db Character string of sqlite database
#' @param return_children Return child jobs of this job
#' @param return_parent Return parent jobs of this job
#' 
#' @return An R data.frame version of the tracking database
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom checkmate assert_logical test_file_exists
#' @importFrom RSQLite SQLite
#' @export
get_tracked_job_status <- function(job_id = NULL, return_children = FALSE, return_parent = FALSE, sqlite_db) {
  
  on.exit(try(dbDisconnect(con)))

  if (is.numeric(job_id)) job_id <- as.character(job_id)
  if (!checkmate::test_string(job_id)) return(invisible(NULL))
  checkmate::assert_logical(return_children)
  checkmate::assert_logical(return_parent)
  if (!checkmate::test_file_exists(sqlite_db)) {
    warning("Cannot find SQLite database at: ", sqlite_db)
  }
  
  con <- dbConnect(RSQLite::SQLite(), sqlite_db)
  
  
  str <- paste0("SELECT * FROM job_tracking WHERE job_id = ?", 
                ifelse(return_children, " OR parent_id = (SELECT id FROM job_tracking WHERE job_id = ?)", ""),
                ifelse(return_parent, " OR id = (SELECT parent_id FROM job_tracking WHERE job_id = ?)", ""))
  df <- dbGetQuery(con, str, param = as.list(rep(job_id, 1 + return_children + return_parent)))
  
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
