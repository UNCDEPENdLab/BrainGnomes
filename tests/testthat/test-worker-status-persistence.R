test_that("SQLite query connections close after rejected writes", {
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db), add = TRUE)
  connection <- NULL
  query <- submit_sqlite_query
  environment(query) <- list2env(list(dbConnect = function(...) {
    connection <<- DBI::dbConnect(...)
    connection
  }), parent = environment(query))
  expect_error(query("UPDATE absent_table SET status = 'FAILED'", db), "no such table")
  expect_false(DBI::dbIsValid(connection))
  expect_no_error(query("CREATE TABLE example (status TEXT)", db))
  expect_false(DBI::dbIsValid(connection))
})

test_that("strict tracking updates reject invalid or unmatched targets", {
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db), add = TRUE)
  create_tracking_db(db)
  expect_error(update_tracked_job_status(db, "absent", "FAILED", strict = TRUE),
               "did not match any row")
  expect_error(update_tracked_job_status(NULL, "1", "STARTED", strict = TRUE))
  expect_error(update_tracked_job_status(db, NULL, "STARTED", strict = TRUE))
  expect_error(update_tracked_job_status(paste0(db, "-absent"), "1", "STARTED", strict = TRUE))
  expect_false(file.exists(paste0(db, "-absent")))
})

test_that("strict tracking errors preserve the underlying SQLite failure", {
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db), add = TRUE)
  create_tracking_db(db)
  insert_tracked_job(db, "1")
  local_mocked_bindings(submit_tracking_query = function(...) stop("database is locked"),
                        .package = "BrainGnomes")
  expect_error(update_tracked_job_status(db, "1", "COMPLETED", strict = TRUE),
               "database is locked")
  expect_identical(get_tracked_job_status("1", sqlite_db = db)$status, "QUEUED")
})

test_that("completion and manifest writes are atomic when SQLite rejects the manifest", {
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db), add = TRUE)
  create_tracking_db(db)
  insert_tracked_job(db, "1")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TRIGGER reject_manifest BEFORE UPDATE OF output_manifest ON job_tracking",
    "WHEN NEW.output_manifest = 'reject' BEGIN SELECT RAISE(ABORT, 'manifest rejected'); END"
  ))
  expect_error(update_tracked_job_status(db, "1", "COMPLETED",
                                        output_manifest = "reject", strict = TRUE),
               "manifest rejected")
  row <- get_tracked_job_status("1", sqlite_db = db)
  expect_identical(row$status, "QUEUED")
  expect_true(is.na(row$time_ended))
  expect_true(is.na(row$output_manifest))
  update_tracked_job_status(db, "1", "COMPLETED", output_manifest = "{}", strict = TRUE)
  row <- get_tracked_job_status("1", sqlite_db = db)
  expect_identical(row$status, "COMPLETED")
  expect_identical(row$output_manifest, "{}")
  expect_false(is.na(row$time_ended))
})

test_that("worker CLI exits unsuccessfully when SQLite has no matching tracking row", {
  db <- tempfile("worker's tracking db ", fileext = ".sqlite")
  on.exit(unlink(db), add = TRUE)
  create_tracking_db(db)
  script <- system.file("upd_job_status.R", package = "BrainGnomes")
  worker <- new.env(parent = globalenv())
  # Source the actual command in-process; intercept quit so it cannot exit tests.
  worker$commandArgs <- function(...) c("--sqlite_db", db, "--job_id", "absent", "--status", "FAILED")
  worker$quit <- function(save, status) stop(paste("worker exit", status))
  capture.output(
    expect_error(sys.source(script, envir = worker), "worker exit 1"), type = "message"
  )
  worker$commandArgs <- function(...) c("--sqlite_db", db, "--job_id", "1", "--status", "COMPLETED")
  insert_tracked_job(db, "1")
  expect_no_error(sys.source(script, envir = worker))
  expect_identical(get_tracked_job_status("1", sqlite_db = db)$status, "COMPLETED")
  worker$commandArgs <- function(...) c("--sqlite_db", "NULL", "--job_id", "1", "--status", "STARTED")
  expect_error(sys.source(script, envir = worker), "worker exit 0")
  worker$commandArgs <- function(...) c("--sqlite_db", "", "--job_id", "1", "--status", "STARTED")
  expect_error(sys.source(script, envir = worker), "worker exit 0")
  worker$commandArgs <- function(...) c("--sqlite_db", db, "--job_id", "NULL", "--status", "FAILED")
  capture.output(expect_error(sys.source(script, envir = worker), "worker exit 1"), type = "message")
})
