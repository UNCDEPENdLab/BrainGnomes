#' helper function to insert a keyed data.frame into the sqlite storage database
#' 
#' @param gpa A \code{glm_pipeline_arguments} object used to lookup location of SQLite database for this insert
#' @param id the id of the subject to whom these data belong
#' @param session the session of these data
#' @param run_number the run_number of these data
#' @param data A \code{data.frame} containing the data to be inserted into the sqlite db
#' @param table A character string of the table name to be modified
#' @param delete_extant Whether to delete any existing records for this id + session + run_number combination
#' @param append Whether to append records to the table (passed through to dbWriteTable)
#' @param overwrite Whether to overwrite the existing table (passed through to dbWriteTable)
#' @param immediate Whether to open unique connection, commit transaction, then close the connection.
#'   This should be useful for SQLite concurrency issues in a parallel compute environment, but at present
#'   we are still getting errors even with the immediate approach.
#'
#' @return a TRUE/FALSE indicating whether the record was successfully inserted
#' @importFrom checkmate assert_integerish test_null assert_data_frame assert_string
#' @importFrom DBI dbDataType dbConnect dbDisconnect dbIsValid dbCommit dbRollback dbBegin
#' @importFrom glue glue_sql
insert_df_sqlite <- function(gpa = NULL, id = NULL, session = NULL, run_number = NULL, data = NULL,
                             table = NULL, delete_extant = TRUE, append = TRUE, overwrite = FALSE, immediate=FALSE) {
  checkmate::assert_class(gpa, "glm_pipeline_arguments")
  if (checkmate::test_null(id)) {
    stop("insert_df_sqlite requires a specific id for keying data")
  }
  checkmate::assert_integerish(session, lower = 1, null.ok = TRUE)
  if (is.null(session)) session <- 1
  checkmate::assert_integerish(run_number, lower = 1, null.ok = TRUE)
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_string(table, null.ok = FALSE)
  checkmate::assert_logical(delete_extant, null.ok = FALSE, len=1L)
  checkmate::assert_logical(append, null.ok = FALSE, len = 1L)
  checkmate::assert_logical(overwrite, null.ok = FALSE, len = 1L)
  
  # open connection if needed
  if (isTRUE(immediate)) {
    # cf. https://blog.r-hub.io/2021/03/13/rsqlite-parallel/
    con <- DBI::dbConnect(RSQLite::SQLite(), gpa$output_locations$sqlite_db)
    DBI::dbExecute(con, "PRAGMA busy_timeout = 10000") # retry write operations several times (10 s)
    on.exit(try(DBI::dbDisconnect(con)))
  } else if (is.null(gpa$sqlite_con) || !DBI::dbIsValid(gpa$sqlite_con)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), gpa$output_locations$sqlite_db)
    on.exit(try(DBI::dbDisconnect(con)))
  } else {
    con <- gpa$sqlite_con # recycle connection
  }
  
  # handle columns in data that are not in table
  has_table <- DBI::dbExistsTable(con, table)
  if (isTRUE(has_table)) {
    table_names <- DBI::dbListFields(con, table)
    uniq_df <- setdiff(names(data), table_names)
    if (length(uniq_df) > 0L) {
      DBI::dbBegin(con) # begin transaction
      alter_failed <- FALSE
      for (nn in uniq_df) {
        dtype <- DBI::dbDataType(con, data[[nn]])
        query <- glue::glue_sql("ALTER TABLE {table} ADD COLUMN {nn} {dtype};", .con = con)
        q_result <- tryCatch(DBI::dbExecute(con, query), error = function(e) {
          message("Error with query: ", query)
          message(as.character(e))
          DBI::dbRollback(con)
          return(FALSE)
        })
        if (isFALSE(q_result)) {
          alter_failed <- TRUE
          break # end loop
        }
      }
      if (!alter_failed) DBI::dbCommit(con) # commit transaction
    }
  }
  
  
  # treat the delete and append as a single transaction so that if either fails, the table is unchanged
  DBI::dbBegin(con)
  transaction_failed <- FALSE
  
  # delete any existing record
  if (isTRUE(delete_extant) && isTRUE(has_table)) {
    query <- glue::glue_sql(
      "DELETE FROM {`table`}",
      "WHERE id = {id} AND session = {session}",
      ifelse(is.null(run_number), "", "AND run_number = {run_number}"),
      .con = con, .sep = " "
    )
    q_result <- tryCatch(DBI::dbExecute(con, query), error = function(e) {
      message("Problem with query: ", query)
      message(as.character(e))
      DBI::dbRollback(con)
      return(FALSE)
    })
    if (isFALSE(q_result)) { transaction_failed <- TRUE }
  }
  
  # add record -- include keying fields for lookup
  data$id <- id
  data$session <- session
  if (!is.null(run_number)) data$run_number <- run_number
  
  q_result <- tryCatch(
    DBI::dbWriteTable(conn = con, name = table, value = data, append = append, overwrite = overwrite),
    error = function(e) {
      print(as.character(e))
      return(FALSE)
    }
  )
  
  if (isFALSE(q_result)) {
    transaction_failed <- TRUE
  }
  
  # commit delete and insert if no errors in subcomponents
  if (isFALSE(transaction_failed)) { DBI::dbCommit(con) }
  
  return(invisible(NULL))
}


#' helper function to lookup a keyed data.frame from the sqlite storage database
#'
#' @param gpa A \code{glm_pipeline_arguments} object used to lookup location of SQLite database for this analysis
#' @param db_file An optional string specifying the SQLite database from which to read
#' @param id the id of the subject to whom these data belong
#' @param session the session of these data
#' @param run_number the run_number of these data
#' @param table A character string of the table name from which to read
#' @param drop_keys whether to drop identifying metatdata columns from data before returning the object
#' @param quiet a logical indicating whether to issue a warning if the table is not found
#'
#' @return a data.frame containing the requested data. Will return NULL if not found
#' @importFrom checkmate assert_integerish test_null assert_data_frame assert_string
#' @importFrom glue glue_sql
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbExistsTable
#' @keywords internal
read_df_sqlite <- function(gpa = NULL, db_file=NULL, id = NULL, session = NULL, run_number = NULL, table = NULL, drop_keys=TRUE, quiet=TRUE) {
  checkmate::assert_class(gpa, "glm_pipeline_arguments", null.ok = TRUE)
  if (is.null(gpa)) {
    checkmate::assert_string(db_file)
    checkmate::assert_file_exists(db_file)
    extant_con <- NULL
  } else {
    db_file <- gpa$output_locations$sqlite_db
    extant_con <- gpa$sqlite_con
  }
  if (checkmate::test_null(id)) stop("read_df_sqlite requires a specific id for lookup")
  checkmate::assert_integerish(session, lower = 1, null.ok = TRUE)
  if (is.null(session)) session <- 1
  checkmate::assert_integerish(run_number, lower = 1, null.ok = TRUE)
  checkmate::assert_string(table, null.ok = FALSE)
  checkmate::assert_flag(drop_keys)
  checkmate::assert_flag(quiet)
  
  # open connection if needed
  if (is.null(extant_con) || !DBI::dbIsValid(extant_con)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
    on.exit(try(DBI::dbDisconnect(con)), add = TRUE)
  } else {
    con <- extant_con # recycle connection
  }
  
  # if table does not exist, then query is invalid (just return NULL)
  if (!DBI::dbExistsTable(con, table)) {
    if (!quiet) warning(sprintf("Cannot find SQLite table %s in file %s.", table, db_file))
    return(NULL)
  }
  
  # lookup any existing record
  query <- glue::glue_sql(
    "SELECT * FROM {`table`}",
    "WHERE id = {id} AND session = {session}",
    ifelse(is.null(run_number), "", "AND run_number = {run_number}"),
    .con = con, .sep = " "
  )
  
  data <- tryCatch(DBI::dbGetQuery(con, query), error = function(e) {
    if (!quiet) warning("Failed to obtain records for query: ", query)
    return(data.frame())
  })
  
  if (nrow(data) > 0L && drop_keys) {
    drop_cols <- c("id", "session")
    if (!is.null(run_number)) drop_cols <- c(drop_cols, "run_number")
    drop_cols <- intersect(drop_cols, names(data))  # only drop if present
    data <- data[ , !(names(data) %in% drop_cols), drop = FALSE]
  }
  
  # return NULL in case of zero matches
  if (nrow(data) == 0L) data <- NULL
  
  return(data)
}


#' helper function to establish sqlite connection and submit query
#' 
#' @param sqlite_db Character path to SQLite database
#' @param str Character query statement to execute
#' @param param Optional list of parameters to pass to statement
#' @param busy_timeout Time (in s) after which to retry write operations; default is 10 s
#' @param return_result Logical. If TRUE submits DBI::dbGetQuery instead of DBI::dbExecute;
#'                        Only use if expecting something in return for your query
#' 
#' @importFrom DBI dbConnect dbGetQuery dbExecute dbDisconnect
#' @importFrom RSQLite sqliteSetBusyHandler
#' 
#' @keywords internal
submit_sqlite_query <- function(str = NULL, sqlite_db = NULL, param = NULL, 
                                busy_timeout = 10, return_result = FALSE) {
  checkmate::assert_logical(return_result)
  if(is.null(str) | is.null(sqlite_db)) return(invisible(NULL))

  con <- dbConnect(RSQLite::SQLite(), sqlite_db) # establish connection
  sqliteSetBusyHandler(con, busy_timeout * 1000) # busy_timeout arg in seconds * 1000 ms
  
  if (isTRUE(return_result)) {
    res <- dbGetQuery(con, str, param = param) # execute query and return result
  } else {
    res <- dbExecute(con, str, param = param) # execute query
  }
  
  dbDisconnect(con) # disconnect
  
  return(invisible(res))
}

#' helper function get all unique sequence_ids for a given sqlite database file
#'
#' @param db a sqlite database file path
#'
#' @keywords internal
get_sequence_ids <- function(db) {
  query <- "SELECT DISTINCT sequence_id FROM job_tracking WHERE sequence_id IS NOT NULL;"
  res <- submit_sqlite_query(
    str = query,
    sqlite_db = db,
    return_result = TRUE
  )
  if (is.null(res)) {
    return(character(0))
  } else {
    return(res$sequence_id)
  }
}

#' helper function to check if a table exists in a SQLite database
#'
#' @param sqlite_db Path to SQLite database file
#' @param table_name Name of the table to check for
#'
#' @return Logical indicating whether the table exists
#' @keywords internal
sqlite_table_exists <- function(sqlite_db, table_name) {
  # Check if database file exists
  if (!file.exists(sqlite_db)) {
    return(FALSE)
  }
  
  # Connect to database
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Check if table exists
  exists <- DBI::dbExistsTable(con, table_name)
  
  return(exists)
}

#' helper for converting tracking data.frame into a multi-level data.tree hierarchy
#'
#' @param tracking_df A data.frame returned by get_tracked_job_status()
#' @return A named list of data.tree Node objects (one per sequence_id)
#' @importFrom data.tree Node
#' @export
tracking_df_to_tree <- function(tracking_df) {
  if (!requireNamespace("data.tree", quietly = TRUE)) {
    stop("Package 'data.tree' is required for tracking_df_to_tree()")
  }

  tracking_df$id <- as.character(tracking_df$id)
  tracking_df$parent_id <- as.character(tracking_df$parent_id)
  tracking_df$sequence_id <- as.character(tracking_df$sequence_id)

  seq_split <- split(tracking_df, tracking_df$sequence_id)
  trees <- list()

  for (seq_id in names(seq_split)) {
    df <- seq_split[[seq_id]]

    # Create root node
    root <- data.tree::Node$new(paste0("Sequence_", seq_id))

    # Create a lookup: id -> Node
    node_lookup <- list()
    node_lookup[["root"]] <- root

    # First, create all nodes without attaching
    for (i in seq_len(nrow(df))) {
      job <- df[i, ]
      node <- data.tree::Node$new(job$job_name)
      node$job_id <- job$job_id
      node$scheduler <- job$scheduler
      node$wall_time <- job$wall_time
      node$status <- job$status
      node$time_submitted <- job$time_submitted
      node$time_started <- job$time_started
      node$time_ended <- job$time_ended
      node$compute_file <- job$compute_file

      node_lookup[[job$id]] <- node
    }

    # Attach nodes to their parents
    for (i in seq_len(nrow(df))) {
      job <- df[i, ]
      node <- node_lookup[[job$id]]

      if (is.na(job$parent_id) || job$parent_id == "") {
        # Attach to root if no parent
        root$AddChildNode(node)
      } else {
        parent_node <- node_lookup[[job$parent_id]]
        if (is.null(parent_node)) {
          # fallback: attach to root if parent not found
          root$AddChildNode(node)
        } else {
          parent_node$AddChildNode(node)
        }
      }
    }

    trees[[seq_id]] <- root
  }

  return(trees)
}


#' helper for retrieving a human-readable title for a pipeline step
#'
#' @param node A data.tree Node object representing a job
#' @return A string summarizing the job
#' @export
get_step_title <- function(node) {
  if (is.null(node)) return(NA_character_)
  
  # Use job_name if available, otherwise node name
  job_name <- if (!is.null(node$job_name)) node$job_name else node$name
  job_id <- if (!is.null(node$job_id)) node$job_id else NA
  
  # Return as "job_name (job job_id)" if job_id exists
  if (!is.na(job_id)) {
    paste0(job_name, " (job ", job_id, ")")
  } else {
    job_name
  }
}
