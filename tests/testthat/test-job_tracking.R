# Tests for job tracking functions including manifest capture and verification

test_that("create_tracking_db creates table with output_manifest column", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  create_tracking_db(db_file)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(job_tracking)")
  expect_true("output_manifest" %in% cols$name)
  expect_equal(cols$type[cols$name == "output_manifest"], "TEXT")
})

test_that("ensure_tracking_db_schema adds output_manifest to existing DB", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  # Create old-style schema without output_manifest
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  DBI::dbExecute(con, "
    CREATE TABLE job_tracking (
      id INTEGER PRIMARY KEY,
      job_id VARCHAR NOT NULL UNIQUE,
      status VARCHAR(24)
    )
  ")
  DBI::dbDisconnect(con)
  
  # Run migration
  ensure_tracking_db_schema(db_file)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(job_tracking)")
  expect_true("output_manifest" %in% cols$name)
})

test_that("insert_tracked_job works correctly", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  create_tracking_db(db_file)
  
  insert_tracked_job(
    sqlite_db = db_file,
    job_id = "12345",
    tracking_args = list(
      job_name = "test_job",
      sequence_id = "seq001"
    )
  )
  
  result <- get_tracked_job_status(job_id = "12345", sqlite_db = db_file)
  expect_equal(nrow(result), 1)
  expect_equal(result$job_id, "12345")
  expect_equal(result$job_name, "test_job")
  expect_equal(result$status, "QUEUED")
})

test_that("update_tracked_job_status updates status correctly", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  create_tracking_db(db_file)
  insert_tracked_job(sqlite_db = db_file, job_id = "12345")
  
  update_tracked_job_status(sqlite_db = db_file, job_id = "12345", status = "STARTED")
  result <- get_tracked_job_status(job_id = "12345", sqlite_db = db_file)
  expect_equal(result$status, "STARTED")
  expect_false(is.na(result$time_started))
  
  update_tracked_job_status(sqlite_db = db_file, job_id = "12345", status = "COMPLETED")
  result <- get_tracked_job_status(job_id = "12345", sqlite_db = db_file)
  expect_equal(result$status, "COMPLETED")
  expect_false(is.na(result$time_ended))
})

test_that("update_tracked_job_status warns when job_id is not found", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)

  create_tracking_db(db_file)

  expect_warning(
    update_tracked_job_status(sqlite_db = db_file, job_id = "missing_job", status = "FAILED"),
    "did not match any row"
  )

  res <- get_tracked_job_status(job_id = "missing_job", sqlite_db = db_file)
  expect_equal(nrow(res), 0)
})

test_that("capture_output_manifest creates valid JSON", {
  out_dir <- tempfile("manifest_test_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  # Create some test files
  writeLines("test content 1", file.path(out_dir, "file1.txt"))
  writeLines("test content two", file.path(out_dir, "file2.nii.gz"))
  sub_dir <- file.path(out_dir, "subdir")
  dir.create(sub_dir)
  writeLines("nested file", file.path(sub_dir, "nested.tsv"))
  
  manifest_json <- capture_output_manifest(out_dir)
  
  expect_type(manifest_json, "character")
  expect_true(nchar(manifest_json) > 0)
  
  # Parse and validate structure
  manifest <- jsonlite::fromJSON(manifest_json, simplifyVector = FALSE)
  expect_equal(manifest$file_count, 3)
  expect_true(manifest$total_size_bytes > 0)
  expect_length(manifest$files, 3)
  
  # Check that paths are relative
  file_paths <- vapply(manifest$files, `[[`, character(1), "path")
  expect_true(all(!grepl("^/", file_paths)))  # No absolute paths
  expect_true("file1.txt" %in% file_paths)
  expect_true("subdir/nested.tsv" %in% file_paths)
})

test_that("capture_output_manifest returns NULL for non-existent directory", {
  result <- capture_output_manifest("/nonexistent/path/12345")
  expect_null(result)
})

test_that("capture_output_manifest handles empty directory", {
  out_dir <- tempfile("empty_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  manifest_json <- capture_output_manifest(out_dir)
  manifest <- jsonlite::fromJSON(manifest_json, simplifyVector = FALSE)
  
  expect_equal(manifest$file_count, 0)
  expect_equal(manifest$total_size_bytes, 0)
  expect_length(manifest$files, 0)
})

test_that("verify_output_manifest returns verified=TRUE for matching files", {
  out_dir <- tempfile("verify_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  # Create files and capture manifest
  writeLines("content a", file.path(out_dir, "a.txt"))
  writeLines("content b", file.path(out_dir, "b.txt"))
  manifest_json <- capture_output_manifest(out_dir)
  
  # Verify against same state
  result <- verify_output_manifest(out_dir, manifest_json)
  
  expect_true(result$verified)
  expect_equal(result$reason, "ok")
  expect_length(result$missing, 0)
  expect_length(result$changed, 0)
})

test_that("verify_output_manifest detects missing files", {
  out_dir <- tempfile("verify_missing_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  # Create files and capture manifest
  writeLines("content a", file.path(out_dir, "a.txt"))
  writeLines("content b", file.path(out_dir, "b.txt"))
  manifest_json <- capture_output_manifest(out_dir)
  
  # Delete one file

  unlink(file.path(out_dir, "b.txt"))
  
  result <- verify_output_manifest(out_dir, manifest_json)
  
  expect_false(result$verified)
  expect_equal(result$reason, "files_differ")
  expect_true("b.txt" %in% result$missing)
})

test_that("verify_output_manifest detects changed file sizes", {
  out_dir <- tempfile("verify_changed_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  # Create file and capture manifest
  writeLines("short", file.path(out_dir, "data.txt"))
  manifest_json <- capture_output_manifest(out_dir)
  
  # Modify file to different size
  writeLines("much longer content here", file.path(out_dir, "data.txt"))
  
  result <- verify_output_manifest(out_dir, manifest_json)
  
  expect_false(result$verified)
  expect_equal(result$reason, "files_differ")
  expect_true("data.txt" %in% result$changed)
})

test_that("verify_output_manifest detects extra files", {
  out_dir <- tempfile("verify_extra_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  # Create file and capture manifest
  writeLines("original", file.path(out_dir, "original.txt"))
  manifest_json <- capture_output_manifest(out_dir)
  
  # Add new file
  writeLines("new file", file.path(out_dir, "extra.txt"))
  
  result <- verify_output_manifest(out_dir, manifest_json)
  
  # Extra files don't cause verification failure, but are reported
  expect_true(result$verified)
  expect_true("extra.txt" %in% result$extra)
})

test_that("verify_output_manifest handles NULL/NA/empty manifest", {
  out_dir <- tempfile("verify_null_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  result_null <- verify_output_manifest(out_dir, NULL)
  expect_true(is.na(result_null$verified))
  expect_equal(result_null$reason, "no_manifest")
  
  result_na <- verify_output_manifest(out_dir, NA)
  expect_true(is.na(result_na$verified))
  expect_equal(result_na$reason, "no_manifest")
  
  result_empty <- verify_output_manifest(out_dir, "")
  expect_true(is.na(result_empty$verified))
  expect_equal(result_empty$reason, "no_manifest")
})

test_that("verify_output_manifest handles missing directory", {
  manifest_json <- jsonlite::toJSON(list(
    output_dir = "/some/path",
    captured_at = as.character(Sys.time()),
    file_count = 1,
    files = list(list(path = "test.txt", size = 10, mtime = 12345))
  ), auto_unbox = TRUE)
  
  result <- verify_output_manifest("/nonexistent/directory", manifest_json)
  
  expect_false(result$verified)
  expect_equal(result$reason, "directory_missing")
})

test_that("update_tracked_job_status stores manifest on COMPLETED", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  out_dir <- tempfile("out_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  # Create test output files
  writeLines("result data", file.path(out_dir, "output.nii.gz"))
  writeLines("confounds", file.path(out_dir, "confounds.tsv"))
  
  manifest_json <- capture_output_manifest(out_dir)
  
  create_tracking_db(db_file)
  insert_tracked_job(sqlite_db = db_file, job_id = "99999", tracking_args = list(job_name = "test"))
  update_tracked_job_status(sqlite_db = db_file, job_id = "99999", status = "STARTED")
  
  # Complete with manifest
  update_tracked_job_status(
    sqlite_db = db_file, 
    job_id = "99999", 
    status = "COMPLETED",
    output_manifest = manifest_json
  )
  
  # Retrieve and verify manifest was stored
  result <- get_tracked_job_status(job_id = "99999", sqlite_db = db_file)
  expect_equal(result$status, "COMPLETED")
  expect_false(is.na(result$output_manifest))
  expect_true(nchar(result$output_manifest) > 0)
  
  # Verify stored manifest is valid JSON
  stored_manifest <- jsonlite::fromJSON(result$output_manifest, simplifyVector = FALSE)
  expect_equal(stored_manifest$file_count, 2)
})

test_that("update_tracked_job_status clears manifest when missing on COMPLETED", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  out_dir <- tempfile("out_clear_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  writeLines("result data", file.path(out_dir, "output.nii.gz"))
  manifest_json <- capture_output_manifest(out_dir)
  
  create_tracking_db(db_file)
  insert_tracked_job(sqlite_db = db_file, job_id = "clear_test", tracking_args = list(job_name = "test"))
  update_tracked_job_status(
    sqlite_db = db_file, 
    job_id = "clear_test", 
    status = "COMPLETED",
    output_manifest = manifest_json
  )
  
  update_tracked_job_status(
    sqlite_db = db_file, 
    job_id = "clear_test", 
    status = "COMPLETED",
    output_manifest = NULL
  )
  
  result <- get_tracked_job_status(job_id = "clear_test", sqlite_db = db_file)
  expect_true(is.na(result$output_manifest))
})

test_that("manifest round-trip through database preserves data", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  out_dir <- tempfile("roundtrip_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  # Create test files
  writeLines("content here", file.path(out_dir, "data.txt"))
  
  # Capture manifest
  original_manifest <- capture_output_manifest(out_dir)
  
  # Store in DB
  create_tracking_db(db_file)
  insert_tracked_job(sqlite_db = db_file, job_id = "roundtrip_test")
  update_tracked_job_status(
    sqlite_db = db_file,
    job_id = "roundtrip_test",
    status = "COMPLETED",
    output_manifest = original_manifest
  )
  
  # Retrieve from DB
  result <- get_tracked_job_status(job_id = "roundtrip_test", sqlite_db = db_file)
  retrieved_manifest <- result$output_manifest
  
  # Verify against current directory state
  verification <- verify_output_manifest(out_dir, retrieved_manifest)
  expect_true(verification$verified)
  expect_equal(verification$reason, "ok")
})

test_that("job tracking cascade works for FAILED status", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  create_tracking_db(db_file)
  
  # Create parent job
  insert_tracked_job(sqlite_db = db_file, job_id = "parent_001", 
                     tracking_args = list(job_name = "parent", sequence_id = "seq1"))
  
  # Create child job
  insert_tracked_job(sqlite_db = db_file, job_id = "child_001",
                     tracking_args = list(job_name = "child", sequence_id = "seq1"))
  add_tracked_job_parent(sqlite_db = db_file, job_id = "child_001", 
                         parent_job_id = "parent_001", child_level = 1)
  
  update_tracked_job_status(sqlite_db = db_file, job_id = "parent_001", status = "STARTED")
  update_tracked_job_status(sqlite_db = db_file, job_id = "child_001", status = "QUEUED")
  
  # Fail parent with cascade
  update_tracked_job_status(sqlite_db = db_file, job_id = "parent_001", 
                            status = "FAILED", cascade = TRUE)
  
  # Child should be FAILED_BY_EXT
  child_result <- get_tracked_job_status(job_id = "child_001", sqlite_db = db_file)
  expect_equal(child_result$status, "FAILED_BY_EXT")
})

test_that("get_tracked_job_status returns children when requested", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  create_tracking_db(db_file)
  
  insert_tracked_job(sqlite_db = db_file, job_id = "parent",
                     tracking_args = list(job_name = "parent_job", sequence_id = "seq1"))
  insert_tracked_job(sqlite_db = db_file, job_id = "child1",
                     tracking_args = list(job_name = "child_job_1", sequence_id = "seq1"))
  insert_tracked_job(sqlite_db = db_file, job_id = "child2",
                     tracking_args = list(job_name = "child_job_2", sequence_id = "seq1"))
  
  add_tracked_job_parent(sqlite_db = db_file, job_id = "child1", parent_job_id = "parent")
  add_tracked_job_parent(sqlite_db = db_file, job_id = "child2", parent_job_id = "parent")
  
  result <- get_tracked_job_status(job_id = "parent", return_children = TRUE, sqlite_db = db_file)
  expect_equal(nrow(result), 3)
  expect_true(all(c("parent", "child1", "child2") %in% result$job_id))
})

test_that("get_tracked_job_status by sequence_id returns all jobs in sequence", {
  db_file <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_file), add = TRUE)
  
  create_tracking_db(db_file)
  
  insert_tracked_job(sqlite_db = db_file, job_id = "job1",
                     tracking_args = list(job_name = "job_1", sequence_id = "my_sequence"))
  insert_tracked_job(sqlite_db = db_file, job_id = "job2",
                     tracking_args = list(job_name = "job_2", sequence_id = "my_sequence"))
  insert_tracked_job(sqlite_db = db_file, job_id = "job3",
                     tracking_args = list(job_name = "job_3", sequence_id = "other_sequence"))
  
  result <- get_tracked_job_status(sequence_id = "my_sequence", sqlite_db = db_file)
  expect_equal(nrow(result), 2)
  expect_true(all(result$sequence_id == "my_sequence"))
})

test_that("format_tracking_db_error reports path diagnostics and readonly hint", {
  db_file <- tempfile(fileext = ".sqlite")
  err <- simpleError("attempt to write a readonly database")
  msg <- format_tracking_db_error(
    sqlite_db = db_file,
    operation = "insert_tracked_job",
    err = err
  )
  expect_true(grepl("insert_tracked_job", msg, fixed = TRUE))
  expect_true(grepl("sqlite_db:", msg, fixed = TRUE))
  expect_true(grepl("parent_dir:", msg, fixed = TRUE))
  expect_true(grepl("read-only", tolower(msg), fixed = TRUE))
})

test_that("submit_tracking_query surfaces informative sqlite path error", {
  bad_db <- file.path(tempdir(), "does_not_exist_parent", "tracking.sqlite")
  expect_error(
    submit_tracking_query("SELECT 1", sqlite_db = bad_db),
    regexp = "Tracking database error during checking tracking table"
  )
  expect_error(
    submit_tracking_query("SELECT 1", sqlite_db = bad_db),
    regexp = "parent_dir:"
  )
})
