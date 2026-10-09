for (reader in c("status", "inspection", "submissions", "table lookup")) {
  test_that(paste(reader, "reads wait for a concurrent worker to commit"), {
    skip_on_os("windows")
    root <- tempfile("tracking-contention-")
    dir.create(root)
    withr::defer(unlink(root, recursive = TRUE))
    db <- file.path(root, "tracking.sqlite")
    ready <- file.path(root, "writer-ready")
    reading <- file.path(root, "reader-started")
    create_tracking_db(db)
    insert_tracked_job(db, "1", list(sequence_id = "run-1"))
    BrainGnomes:::ensure_job_submission_schema(db)

    # Hold an exclusive worker write until the parent starts its status read.
    # Release after a short delay so a reader with no busy handler fails, while
    # a correctly bounded reader observes the committed status.
    hold_write <- function() {
      con <- DBI::dbConnect(RSQLite::SQLite(), db)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(con, "BEGIN EXCLUSIVE")
      DBI::dbExecute(con, "UPDATE job_tracking SET status = 'STARTED' WHERE job_id = '1'")
      file.create(ready)
      deadline <- Sys.time() + 5
      while (!file.exists(reading) && Sys.time() < deadline) Sys.sleep(0.01)
      if (!file.exists(reading)) stop("Parent did not start its status read")
      Sys.sleep(0.2)
      DBI::dbExecute(con, "COMMIT")
      TRUE
    }
    writer <- parallel::mcparallel(hold_write(), silent = TRUE)
    collected <- FALSE
    withr::defer({
      if (!collected && is.null(parallel::mccollect(writer, wait = FALSE))) {
        tools::pskill(writer$pid)
        parallel::mccollect(writer)
      }
    })
    deadline <- Sys.time() + 5
    while (!file.exists(ready) && Sys.time() < deadline) Sys.sleep(0.01)
    expect_true(file.exists(ready))
    file.create(reading)
    if (reader == "status") {
      expect_identical(get_tracked_job_status("1", sqlite_db = db)$status, "STARTED")
    } else if (reader == "inspection") {
      cfg <- list(metadata = list(sqlite_db = db))
      expect_identical(BrainGnomes:::.read_project_tracking_jobs(cfg)$status, "STARTED")
    } else if (reader == "submissions") {
      expect_equal(nrow(BrainGnomes:::read_job_submission_attempts(db)), 0L)
    } else {
      expect_true(BrainGnomes:::sqlite_table_exists(db, "job_tracking"))
    }
    result <- parallel::mccollect(writer)
    collected <- TRUE
    expect_true(result[[1L]])
  })
}
