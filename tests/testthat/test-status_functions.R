test_that("get_project_status reports completion", {
  root <- tempfile("status-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  log_dir <- file.path(root, "logs"); dir.create(log_dir)
  bids_dir <- file.path(root, "bids"); dir.create(bids_dir)
  fmriprep_dir <- tempfile("fmriprep_"); dir.create(fmriprep_dir)
  postproc_dir <- file.path(root, "postproc"); dir.create(postproc_dir)
  mriqc_dir <- file.path(root, "mriqc"); dir.create(mriqc_dir)


  sub <- "01"; ses_a <- "A"; ses_b <- "B"
  dir.create(file.path(log_dir, paste0("sub-", sub)))
  dir.create(file.path(bids_dir, paste0("sub-", sub), paste0("ses-", ses_a)), recursive = TRUE)
  dir.create(file.path(bids_dir, paste0("sub-", sub), paste0("ses-", ses_b)), recursive = TRUE)
  dir.create(file.path(fmriprep_dir, paste0("sub-", sub)), recursive = TRUE)
  dir.create(file.path(fmriprep_dir, paste0("sub-", sub), paste0("ses-", ses_a)), recursive = TRUE)
  dir.create(file.path(postproc_dir, paste0("sub-", sub)), recursive = TRUE)
  dir.create(file.path(postproc_dir, paste0("sub-", sub), paste0("ses-", ses_a)), recursive = TRUE)

  cat("2024-05-04 10:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".bids_conversion_sub-", sub, "_ses-", ses_a, "_complete")))
  cat("2024-05-04 11:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".fmriprep_sub-", sub, "_complete")))
  cat("2024-05-04 12:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".postprocess_stream1_sub-", sub, "_ses-", ses_a, "_complete")))

  scfg <- list(
    metadata = list(log_directory = log_dir, bids_directory = bids_dir, fmriprep_directory = fmriprep_dir, mriqc_directory = mriqc_dir, postproc_directory = postproc_dir),
    bids_conversion = list(enable = TRUE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = TRUE, stream1 = list())
  )
  class(scfg) <- "bg_project_cfg"

  res <- get_project_status(scfg)
  expect_equal(nrow(res), 2)
  expect_true(res$bids_conversion_complete[res$ses_id == ses_a])
  expect_false(res$bids_conversion_complete[res$ses_id == ses_b])
  expect_true(all(res$fmriprep_complete))
  expect_true(res$stream1_complete[res$ses_id == ses_a])
  expect_false(res$stream1_complete[res$ses_id == ses_b])

  sm <- summary(res)
  expect_equal(sm$n_complete[sm$step == "bids_conversion_complete"], 1)
})

test_that("is_step_complete prefers newer complete over stale fail", {
  root <- tempfile("status-logic-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs"); dir.create(log_dir)
  postproc_dir <- file.path(root, "postproc"); dir.create(postproc_dir)

  sub <- "01"
  dir.create(file.path(log_dir, paste0("sub-", sub)))
  dir.create(file.path(postproc_dir, paste0("sub-", sub)), recursive = TRUE)

  complete_file <- file.path(log_dir, paste0("sub-", sub), paste0(".postprocess_stream1_sub-", sub, "_complete"))
  fail_file <- file.path(log_dir, paste0("sub-", sub), paste0(".postprocess_stream1_sub-", sub, "_fail"))
  cat("2025-01-02 00:00:00", file = complete_file)
  cat("2025-01-01 00:00:00", file = fail_file)
  Sys.setFileTime(complete_file, as.POSIXct("2025-01-02 00:00:00", tz = "UTC"))
  Sys.setFileTime(fail_file, as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))

  scfg <- list(
    metadata = list(log_directory = log_dir, postproc_directory = postproc_dir),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = TRUE, stream1 = list())
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "postprocess", pp_stream = "stream1")
  expect_true(res$complete)

  Sys.setFileTime(fail_file, as.POSIXct("2025-01-03 00:00:00", tz = "UTC"))
  res_new_fail <- is_step_complete(scfg, sub_id = sub, step_name = "postprocess", pp_stream = "stream1")
  expect_false(res_new_fail$complete)
})

test_that("is_step_complete requires out_dir when manifest is missing or invalid", {
  root <- tempfile("status-db-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job1",
    tracking_args = list(job_name = job_name)
  )

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  update_tracked_job_status(sqlite_db = sqlite_db, job_id = "job1", status = "COMPLETED")

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)

  res_missing_manifest <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_true(res_missing_manifest$complete)
  expect_equal(res_missing_manifest$verification_source, "db_status_dir_exists")

  unlink(out_dir, recursive = TRUE, force = TRUE)
  res_missing_manifest_no_dir <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_false(res_missing_manifest_no_dir$complete)
  expect_equal(res_missing_manifest_no_dir$verification_source, "db_status_dir_missing")

  dir.create(out_dir, recursive = TRUE)
  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job1",
    status = "COMPLETED",
    output_manifest = "{not valid json"
  )

  res_invalid_manifest <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_true(res_invalid_manifest$complete)
  expect_true(is.na(res_invalid_manifest$manifest_verified))
  expect_equal(res_invalid_manifest$verification_source, "db_status_dir_exists")
})

test_that("is_step_complete handles old schema without output_manifest column", {
  root <- tempfile("status-db-old-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE job_tracking (
      id INTEGER PRIMARY KEY,
      job_id VARCHAR NOT NULL UNIQUE,
      job_name VARCHAR,
      time_submitted INTEGER,
      time_ended INTEGER,
      status VARCHAR(24)
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO job_tracking (job_id, job_name, time_submitted, time_ended, status)
    VALUES ('job_old', 'fmriprep_sub-01', 1, 2, 'COMPLETED')
  ")

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  out_dir <- file.path(fmriprep_dir, "sub-01")
  dir.create(out_dir, recursive = TRUE)

  res_old_schema <- is_step_complete(scfg, sub_id = "01", step_name = "fmriprep")
  expect_true(res_old_schema$complete)
  expect_true(is.na(res_old_schema$manifest_verified))
  expect_equal(res_old_schema$verification_source, "db_status_dir_exists")

  unlink(out_dir, recursive = TRUE, force = TRUE)
  res_old_schema_no_dir <- is_step_complete(scfg, sub_id = "01", step_name = "fmriprep")
  expect_false(res_old_schema_no_dir$complete)
  expect_equal(res_old_schema_no_dir$verification_source, "db_status_dir_missing")
})

test_that("is_step_complete uses manifest verification for DB COMPLETED", {
  root <- tempfile("status-db-manifest-ok-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_manifest_ok",
    tracking_args = list(job_name = job_name)
  )

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  writeLines("data", file.path(out_dir, "output.txt"))
  manifest_json <- capture_output_manifest(out_dir)

  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job_manifest_ok",
    status = "COMPLETED",
    output_manifest = manifest_json
  )

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_true(res$complete)
  expect_true(isTRUE(res$manifest_verified))
  expect_equal(res$verification_source, "db_manifest_verified")
})

test_that("is_step_complete fails when manifest verification fails", {
  root <- tempfile("status-db-manifest-fail-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_manifest_fail",
    tracking_args = list(job_name = job_name)
  )

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  file_path <- file.path(out_dir, "output.txt")
  writeLines("data", file_path)
  manifest_json <- capture_output_manifest(out_dir)

  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job_manifest_fail",
    status = "COMPLETED",
    output_manifest = manifest_json
  )

  unlink(file_path)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_false(res$complete)
  expect_false(isTRUE(res$manifest_verified))
  expect_equal(res$verification_source, "db_manifest_failed")
})

test_that("is_step_complete respects FAILED DB status", {
  root <- tempfile("status-db-failed-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_failed",
    tracking_args = list(job_name = job_name)
  )

  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job_failed",
    status = "FAILED"
  )

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  sub_log_dir <- file.path(log_dir, paste0("sub-", sub))
  dir.create(sub_log_dir, recursive = TRUE)
  complete_file <- file.path(sub_log_dir, paste0(".fmriprep_sub-", sub, "_complete"))
  writeLines("done", complete_file)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_false(res$complete)
  expect_equal(res$verification_source, "db_status_failed")
})

test_that("is_step_complete falls back to complete file for STARTED jobs", {
  root <- tempfile("status-db-started-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_started",
    tracking_args = list(job_name = job_name)
  )

  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job_started",
    status = "STARTED"
  )

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  sub_log_dir <- file.path(log_dir, paste0("sub-", sub))
  dir.create(sub_log_dir, recursive = TRUE)
  complete_file <- file.path(sub_log_dir, paste0(".fmriprep_sub-", sub, "_complete"))
  writeLines("done", complete_file)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_true(res$complete)
  expect_equal(res$verification_source, "complete_file")
})
