make_inspection_project <- function() {
  root <- tempfile("inspection-project-")
  dir.create(root)
  log_dir <- file.path(root, "logs")
  dir.create(log_dir)
  db <- file.path(root, "tracking.sqlite")
  BrainGnomes:::create_tracking_db(db)
  cfg <- structure(list(
    metadata = list(
      project_name = "inspection",
      project_directory = root,
      log_directory = log_dir,
      sqlite_db = db
    )
  ), class = "bg_project_cfg")
  list(root = root, log_dir = log_dir, db = db, cfg = cfg)
}

add_inspection_job <- function(db, job_id, job_name, run_id, status,
                               scheduler_options = "--constraint=a-very-long-value",
                               wall_time = "08:00:00") {
  insert_tracked_job(db, job_id, list(
    job_name = job_name,
    sequence_id = run_id,
    status = status,
    scheduler = "slurm",
    wall_time = wall_time,
    scheduler_options = scheduler_options
  ))
}

test_that("inspect_project selects the latest attempt for each logical work unit", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  add_inspection_job(fixture$db, "100", "fmriprep_sub-01", "run-old", "FAILED")
  add_inspection_job(fixture$db, "101", "mriqc_sub-02", "run-old", "COMPLETED")
  add_inspection_job(fixture$db, "102", "fmriprep_sub-02_ses-A", "run-old", "COMPLETED")
  add_inspection_job(fixture$db, "200", "fmriprep_sub-01", "run-new", "COMPLETED")
  add_inspection_job(fixture$db, "201", "postprocess_rest_clean_sub-01_ses-A", "run-new", "COMPLETED")
  add_inspection_job(fixture$db, "202", "postprocess_rest_clean_sentinel", "run-new", "QUEUED")
  add_tracked_job_parent(fixture$db, "202", "201")
  add_inspection_job(fixture$db, "203", "postprocess_rest_clean_array", "run-new", "STARTED")
  add_tracked_job_parent(fixture$db, "203", "201", child_level = 2L)
  add_inspection_job(fixture$db, "204", "fmriprep_sub-02", "run-new", "FAILED")

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-01 10:00:00' WHERE sequence_id = 'run-old'")
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00' WHERE sequence_id = 'run-new'")

  status <- inspect_project(fixture$cfg)

  expect_s3_class(status, "bg_project_inspection")
  expect_s3_class(status$jobs, "bg_run_jobs")
  expect_s3_class(status$jobs, "data.frame")
  expect_equal(status$overview$overall_status, "IN_PROGRESS_WITH_FAILURES")
  expect_equal(status$overview$n_runs, 2L)
  expect_equal(status$overview$n_units, 4L)
  expect_equal(status$overview$n_completed, 2L)
  expect_equal(status$overview$n_running, 1L)
  expect_equal(status$overview$n_failed, 1L)

  fmriprep_01 <- subset(status$subject_stages, sub_id == "01" & stage == "fmriprep")
  expect_equal(fmriprep_01$status, "COMPLETED")
  expect_equal(fmriprep_01$run_id, "run-new")
  old_fmriprep <- status$attempts$is_current[
    status$attempts$run_id == "run-old" & status$attempts$stage == "fmriprep"
  ]
  expect_true(all(!old_fmriprep))

  fmriprep_02 <- subset(status$subject_stages, sub_id == "02" & stage == "fmriprep")
  expect_equal(fmriprep_02$status, "FAILED")
  expect_true(is.na(fmriprep_02$ses_id))

  postprocess <- subset(status$subject_stages, stage == "postprocess")
  expect_equal(postprocess$stream, "rest_clean")
  expect_equal(postprocess$sub_id, "01")
  expect_equal(postprocess$ses_id, "A")
  expect_equal(postprocess$status, "RUNNING")
  expect_equal(postprocess$n_jobs, 3L)

  inherited <- subset(status$jobs, job_id %in% c("202", "203"))
  expect_true(all(inherited$sub_id == "01"))
  expect_true(all(inherited$ses_id == "A"))
  expect_setequal(inherited$job_role, c("sentinel", "array"))

  job_output <- capture.output(
    job_messages <- capture.output(print(status$jobs), type = "message")
  )
  job_output <- c(job_output, job_messages)
  expect_true(any(grepl("Historical job rows retained", job_output, fixed = TRUE)))
  expect_false(any(grepl("constraint=a-very-long-value", job_output, fixed = TRUE)))
})

test_that("latest run selection breaks timestamp ties by tracking order", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "600", "mriqc_sub-01", "run-first", "COMPLETED")
  add_inspection_job(fixture$db, "601", "mriqc_sub-01", "run-second", "FAILED")

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00'")

  latest <- inspect_project(fixture$cfg, run_id = "latest")
  current <- inspect_project(fixture$cfg)
  expect_equal(latest$run_id, "run-second")
  expect_equal(current$subject_stages$status, "FAILED")
})

test_that("inspect_project supports run scope and structured resolutions", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "300", "mriqc_sub-01", "run-one", "COMPLETED")
  add_inspection_job(fixture$db, "301", "fmriprep_sub-01", "run-one", "QUEUED")

  status <- inspect_project(fixture$cfg, run_id = "latest")

  expect_equal(status$scope, "run")
  expect_equal(status$run_id, "run-one")
  expect_equal(status$overview$overall_status, "IN_PROGRESS")
  expect_s3_class(summary(status), "data.frame")
  expect_equal(summary(status, by = "stage"), status$stages)
  expect_equal(summary(status, by = "subject"), status$subjects)
  expect_equal(summary(status, by = "subject_stage"), status$subject_stages)
  expect_equal(summary(status, by = "runs"), status$runs)
  expect_equal(nrow(status$runs), 1L)
  expect_equal(summary(status, by = "attempt"), status$attempts)
  expect_equal(summary(status, by = "job"), status$jobs)
  expect_equal(summary(status, by = "active"), status$active)
  expect_equal(
    summary(status, by = "reconciliation"), status$reconciliation
  )

  output <- capture.output(
    messages <- capture.output(print(status), type = "message")
  )
  output <- c(output, messages)
  expect_true(any(grepl("Work units:", output, fixed = TRUE)))
  expect_true(any(grepl("Stage and stream progress", output, fixed = TRUE)))
  expect_false(any(grepl("scheduler_options", output, fixed = TRUE)))

  expect_error(
    inspect_project(fixture$cfg, run_id = "missing"),
    "No tracked project run"
  )
})

test_that("inspect_project applies subject focus at every resolution", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "310", "mriqc_sub-01", "run-old", "COMPLETED")
  add_inspection_job(fixture$db, "311", "fmriprep_sub-01", "run-new", "QUEUED")
  add_inspection_job(fixture$db, "312", "mriqc_sub-02", "run-new", "FAILED")

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "UPDATE job_tracking SET time_submitted = '2026-09-01 10:00:00' WHERE sequence_id = 'run-old'"
  )
  DBI::dbExecute(
    con,
    "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00' WHERE sequence_id = 'run-new'"
  )

  focused <- inspect_project(fixture$cfg, subject_id = "sub-01")
  expect_identical(focused$subject_id, "01")
  expect_true(all(focused$jobs$sub_id == "01"))
  expect_true(all(focused$subject_stages$sub_id == "01"))
  expect_identical(focused$subjects$sub_id, "01")
  expect_setequal(focused$runs$run_id, c("run-old", "run-new"))
  expect_equal(sum(focused$runs$n_jobs), 2L)
  expect_identical(focused$active$job_id, "311")

  selected <- inspect_project(
    fixture$cfg, run_id = "run-new", subject_id = "01"
  )
  expect_identical(selected$scope, "run")
  expect_identical(selected$jobs$job_id, "311")
  expect_identical(selected$runs$n_jobs, 1L)

  diagnosis <- diagnose_project(focused, interactive = FALSE)
  expect_identical(diagnosis$focus$subject_id, "01")
  expect_true(all(diagnosis$jobs$sub_id == "01"))

  output <- capture.output(
    messages <- capture.output(print(focused), type = "message")
  )
  expect_true(any(grepl("Subject: sub-01", c(output, messages), fixed = TRUE)))
  expect_error(
    inspect_project(fixture$cfg, subject_id = "missing"),
    "No tracked jobs were found for sub-missing"
  )
})

test_that("active job health reports elapsed time and requested limits", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(
    fixture$db, "320", "mriqc_sub-01", "run-one", "QUEUED",
    wall_time = "08:00:00"
  )
  add_inspection_job(
    fixture$db, "321", "fmriprep_sub-02", "run-one", "STARTED",
    wall_time = "00:30:00"
  )
  add_inspection_job(
    fixture$db, "322", "mriqc_sub-03", "run-one", "COMPLETED"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    paste0(
      "UPDATE job_tracking SET time_submitted = '2026-09-04 08:00:00' ",
      "WHERE job_id = '320'"
    )
  )
  DBI::dbExecute(
    con,
    paste0(
      "UPDATE job_tracking SET time_submitted = '2026-09-04 08:00:00', ",
      "time_started = '2026-09-04 09:00:00' WHERE job_id = '321'"
    )
  )

  inspection <- inspect_project(fixture$cfg)
  expect_setequal(inspection$active$job_id, c("320", "321"))
  expect_equal(nrow(inspection$reconciliation), 0L)
  expect_false(inspection$scheduler_refreshed)

  current_jobs <- as.data.frame(
    inspection$jobs[inspection$jobs$is_current_attempt, , drop = FALSE]
  )
  health <- build_active_job_health(
    fixture$cfg, current_jobs,
    retrieved_at = as.POSIXct("2026-09-04 10:00:00"), refresh = FALSE
  )
  queued <- health$active[health$active$job_id == "320", , drop = FALSE]
  running <- health$active[health$active$job_id == "321", , drop = FALSE]
  expect_equal(queued$queue_seconds, 7200)
  expect_true(is.na(queued$runtime_seconds))
  expect_equal(running$runtime_seconds, 3600)
  expect_equal(running$requested_wall_seconds, 1800)
  expect_true(running$overdue)
  expect_identical(running$health, "OVERDUE")
  expect_s3_class(health$active$state_since, "POSIXct")
  expect_equal(
    unname(parse_wall_time_seconds(c("08:00:00", "1-02:03:04"))),
    c(28800, 93784)
  )

  local_mocked_bindings(
    scheduler_job_status = function(job_ids, scheduler = "local", user = NULL) {
      data.frame(
        job_id = job_ids, scheduler = scheduler,
        scheduler_status = "MISSING", scheduler_raw_status = "MISSING",
        query_detail = NA_character_, stringsAsFactors = FALSE
      )
    },
    .package = "BrainGnomes"
  )
  refreshed_health <- build_active_job_health(
    fixture$cfg, current_jobs,
    retrieved_at = as.POSIXct("2026-09-04 10:00:00"), refresh = TRUE
  )
  refreshed_running <- refreshed_health$active[
    refreshed_health$active$job_id == "321", , drop = FALSE
  ]
  expect_identical(refreshed_running$health, "OVERDUE")
})

test_that("scheduler refresh is read-only and exposes reconciliation", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  fixture$cfg$compute_environment <- list(scheduler = "slurm")
  add_inspection_job(fixture$db, "330", "mriqc_sub-01", "run-one", "QUEUED")
  add_inspection_job(fixture$db, "331", "fmriprep_sub-02", "run-one", "STARTED")

  local_mocked_bindings(
    scheduler_job_status = function(job_ids, scheduler = "local", user = NULL) {
      observed <- c(`330` = "RUNNING", `331` = "COMPLETED")[job_ids]
      data.frame(
        job_id = job_ids, scheduler = scheduler,
        scheduler_status = unname(observed),
        scheduler_raw_status = unname(observed),
        query_detail = NA_character_, stringsAsFactors = FALSE
      )
    },
    .package = "BrainGnomes"
  )
  refreshed <- inspect_project(fixture$cfg, refresh = TRUE)
  expect_true(refreshed$scheduler_refreshed)
  expect_setequal(
    refreshed$active$health, c("DATABASE_LAG", "STALE_DATABASE")
  )
  expect_true(all(refreshed$active$stale))
  expect_equal(nrow(refreshed$reconciliation), 2L)
  expect_true(all(!refreshed$reconciliation$agrees))
  expect_equal(refreshed$overview$n_active_attention, 2L)
  expect_equal(refreshed$overview$n_active_stale, 2L)

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  database_jobs <- DBI::dbGetQuery(
    con, "SELECT job_id, status FROM job_tracking ORDER BY job_id"
  )
  expect_identical(database_jobs$status, c("QUEUED", "STARTED"))

  output <- capture.output(
    messages <- capture.output(print(refreshed), type = "message")
  )
  output <- c(output, messages)
  expect_true(any(grepl("Oldest or noteworthy active jobs", output, fixed = TRUE)))
  expect_true(any(grepl("STALE_DATABASE", output, fixed = TRUE)))

})

test_that("scheduler refresh reports unsupported schedulers without failing", {
  unavailable <- scheduler_job_status(c("1", "2"), "unsupported")
  expect_true(all(unavailable$scheduler_status == "UNAVAILABLE"))
  expect_true(all(grepl("Unsupported scheduler", unavailable$query_detail)))
})

test_that("diagnose_project uses current failures or a selected historical run", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "400", "fmriprep_sub-01", "run-old", "FAILED")
  add_inspection_job(fixture$db, "401", "fmriprep_sub-01", "run-new", "COMPLETED")
  add_inspection_job(fixture$db, "402", "mriqc_sub-02", "run-new", "FAILED")

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-01 10:00:00' WHERE sequence_id = 'run-old'")
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00' WHERE sequence_id = 'run-new'")

  dir.create(file.path(fixture$log_dir, "sub-02"))
  failure_log <- file.path(fixture$log_dir, "sub-02", "mriqc_jobid-402.err")
  writeLines("current failure", failure_log)

  current <- diagnose_project(fixture$cfg, interactive = FALSE)
  expect_equal(current$scope, "project")
  expect_equal(current$failures$job_id, "402")
  expect_equal(current$logs$path, failure_log)

  from_snapshot <- diagnose_project(
    inspect_project(fixture$cfg), interactive = FALSE
  )
  expect_equal(from_snapshot$failures$job_id, "402")

  historical <- diagnose_project(
    fixture$cfg, run_id = "run-old", interactive = FALSE
  )
  expect_equal(historical$scope, "run")
  expect_equal(historical$failures$job_id, "400")
})

test_that("diagnose_project defaults to the R session's interactivity", {
  expect_identical(
    formals(diagnose_project)$interactive,
    quote(base::interactive())
  )

  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  local_mocked_bindings(
    run_interactive_diagnosis = function(input, run_id = NULL,
                                         subject_id = NULL, job_id = NULL) {
      expect_identical(input, fixture$cfg)
      expect_identical(run_id, "run-selected")
      expect_null(subject_id)
      expect_null(job_id)
      "guided-browser"
    },
    .package = "BrainGnomes"
  )

  result <- diagnose_project(fixture$cfg, "run-selected", TRUE)
  expect_identical(result, "guided-browser")
})

test_that("inspection and diagnosis default to the current project directory", {
  expect_identical(formals(inspect_project)$input, quote(getwd()))
  expect_identical(formals(diagnose_project)$input, quote(getwd()))

  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(
    fixture$db, "450", "mriqc_sub-01", "run-current", "FAILED"
  )
  yaml::write_yaml(
    as.list(fixture$cfg), file.path(fixture$root, "project_config.yaml")
  )

  from_directory <- inspect_project(fixture$root)
  expect_equal(
    from_directory$jobs$job_id[
      from_directory$jobs$lifecycle_status == "FAILED"
    ],
    "450"
  )

  withr::local_dir(fixture$root)
  from_cwd <- inspect_project()
  expect_equal(
    from_cwd$jobs$job_id[from_cwd$jobs$lifecycle_status == "FAILED"],
    "450"
  )

  diagnosis <- diagnose_project(interactive = FALSE)
  expect_equal(diagnosis$failures$job_id, "450")

  local_mocked_bindings(
    run_interactive_diagnosis = function(input, run_id = NULL,
                                         subject_id = NULL, job_id = NULL) {
      expect_s3_class(input, "bg_project_cfg")
      expect_equal(input$metadata$project_directory, fixture$root)
      expect_null(run_id)
      expect_null(subject_id)
      expect_null(job_id)
      "guided-from-cwd"
    },
    .package = "BrainGnomes"
  )
  guided <- diagnose_project(interactive = TRUE)
  expect_identical(guided, "guided-from-cwd")
})

test_that("diagnose_project supports structured subject and exact job focus", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "700", "mriqc_sub-01", "run-old", "FAILED")
  add_inspection_job(fixture$db, "701", "mriqc_sub-01", "run-new", "COMPLETED")
  add_inspection_job(fixture$db, "702", "extract_rois_rest_sub-01", "run-new", "FAILED")
  add_inspection_job(fixture$db, "703", "fmriprep_sub-02", "run-new", "FAILED")

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-01 10:00:00' WHERE sequence_id = 'run-old'")
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00' WHERE sequence_id = 'run-new'")

  dir.create(file.path(fixture$log_dir, "sub-01"))
  old_log <- file.path(fixture$log_dir, "sub-01", "mriqc_jobid-700.err")
  current_log <- file.path(fixture$log_dir, "sub-01", "extract_jobid-702.err")
  writeLines("historical failure", old_log)
  writeLines("current failure", current_log)

  subject <- diagnose_project(
    fixture$cfg, subject_id = "sub-01", interactive = FALSE
  )
  expect_identical(subject$focus$subject_id, "01")
  expect_setequal(subject$jobs$job_id, c("700", "701", "702"))
  expect_identical(subject$failures$job_id, "702")
  expect_identical(subject$logs$path, current_log)
  expect_setequal(
    diagnosis_current_jobs(subject)$job_id,
    c("701", "702")
  )

  current <- diagnose_project(fixture$cfg, interactive = FALSE)
  current_groups <- diagnosis_problem_groups(diagnosis_current_jobs(current))
  expect_false("mriqc" %in% current_groups$stage)

  exact <- diagnose_project(fixture$cfg, job_id = 700, interactive = FALSE)
  expect_identical(exact$focus$job_id, "700")
  expect_identical(exact$jobs$job_id, "700")
  expect_identical(exact$failures$job_id, "700")
  expect_identical(exact$logs$path, old_log)
  expect_identical(diagnosis_current_jobs(exact)$job_id, "700")

  expect_error(
    diagnose_project(fixture$cfg, subject_id = "missing", interactive = FALSE),
    "No tracked jobs were found for sub-missing"
  )
  expect_error(
    diagnose_project(fixture$cfg, job_id = "missing", interactive = FALSE),
    "No tracked job has ID missing"
  )
})

test_that("guided diagnosis opens a singleton failure without widening scope", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "900", "mriqc_sub-01", "run-one", "FAILED")
  add_inspection_job(fixture$db, "901", "fmriprep_sub-01", "run-one", "COMPLETED")
  add_inspection_job(fixture$db, "902", "extract_rois_rest_sub-01", "run-one", "FAILED")
  add_inspection_job(fixture$db, "903", "extract_rois_rest_sub-02", "run-one", "FAILED")

  prompts <- 0L
  local_mocked_bindings(
    prompt_input = function(...) {
      prompts <<- prompts + 1L
      1L
    },
    diagnosis_job_screen = function(scfg, job, allow_back = TRUE) {
      expect_identical(scfg, fixture$cfg)
      expect_true(allow_back)
      structure(list(action = "exit", job = job), class = "bg_interactive_diagnosis")
    },
    .package = "BrainGnomes"
  )

  result <- diagnose_project(fixture$cfg, interactive = TRUE)
  expect_identical(result$job$job_id, "900")
  expect_identical(result$job$sub_id, "01")
  expect_identical(prompts, 1L)
})

test_that("guided diagnosis keeps multi-job problem groups narrowed", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "910", "mriqc_sub-01", "run-one", "FAILED")
  add_inspection_job(fixture$db, "911", "extract_rois_rest_sub-01", "run-one", "FAILED")
  add_inspection_job(fixture$db, "912", "extract_rois_rest_sub-02", "run-one", "FAILED")
  add_inspection_job(fixture$db, "913", "fmriprep_sub-03", "run-one", "COMPLETED")

  answers <- c(2L, 2L)
  prompt_index <- 0L
  local_mocked_bindings(
    prompt_input = function(...) {
      prompt_index <<- prompt_index + 1L
      answers[[prompt_index]]
    },
    diagnosis_job_screen = function(scfg, job, allow_back = TRUE) {
      structure(list(action = "exit", job = job), class = "bg_interactive_diagnosis")
    },
    .package = "BrainGnomes"
  )

  result <- diagnose_project(fixture$cfg, interactive = TRUE)
  expect_identical(result$job$job_id, "912")
  expect_identical(result$job$stage, "extract_rois")
  expect_identical(prompt_index, 2L)
})

test_that("guided subject and job shortcuts preserve their exact focus", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "920", "mriqc_sub-01", "run-one", "FAILED")
  add_inspection_job(fixture$db, "921", "extract_rois_rest_sub-01", "run-one", "FAILED")
  add_inspection_job(fixture$db, "922", "mriqc_sub-02", "run-one", "FAILED")

  answers <- 1L
  prompt_index <- 0L
  local_mocked_bindings(
    prompt_input = function(...) {
      prompt_index <<- prompt_index + 1L
      answers[[prompt_index]]
    },
    diagnosis_job_screen = function(scfg, job, allow_back = TRUE) {
      structure(list(action = "exit", job = job), class = "bg_interactive_diagnosis")
    },
    .package = "BrainGnomes"
  )

  subject <- diagnose_project(
    fixture$cfg, subject_id = "01", interactive = TRUE
  )
  expect_identical(subject$job$job_id, "920")
  expect_identical(prompt_index, 1L)

  local_mocked_bindings(
    prompt_input = function(...) stop("An exact job should not require a selection prompt."),
    diagnosis_job_screen = function(scfg, job, allow_back = TRUE) {
      expect_false(allow_back)
      structure(list(action = "exit", job = job), class = "bg_interactive_diagnosis")
    },
    .package = "BrainGnomes"
  )
  exact <- diagnose_project(fixture$cfg, job_id = "922", interactive = TRUE)
  expect_identical(exact$job$job_id, "922")
  expect_identical(exact$job$sub_id, "02")
})

test_that("guided job details expose logs and immediate dependency context", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "930", "submit_subjects_sub-01", "run-one", "COMPLETED")
  add_inspection_job(fixture$db, "931", "mriqc_sub-01", "run-one", "FAILED")
  add_inspection_job(fixture$db, "932", "postprocess_rest_sub-01", "run-one", "FAILED_BY_EXT")
  add_tracked_job_parent(fixture$db, "931", "930")
  add_tracked_job_parent(fixture$db, "932", "931")

  subject_logs <- file.path(fixture$log_dir, "sub-01")
  dir.create(subject_logs)
  stderr <- file.path(subject_logs, "mriqc_jobid-931.err")
  stdout <- file.path(subject_logs, "mriqc_jobid-931.out")
  writeLines("failure details", stderr)
  writeLines("scheduler output", stdout)

  focused <- diagnose_project(
    fixture$cfg, job_id = "931", interactive = FALSE
  )
  dependencies <- diagnosis_job_dependencies(fixture$cfg, focused$jobs)
  expect_identical(dependencies$upstream$job_id, "930")
  expect_identical(dependencies$downstream$job_id, "932")
  expect_identical(diagnosis_latest_log(focused$logs, "stderr"), stderr)
  expect_identical(diagnosis_latest_log(focused$logs, "stdout"), stdout)

  local_mocked_bindings(
    prompt_input = function(...) 6L,
    .package = "BrainGnomes"
  )
  details <- diagnosis_job_screen(
    fixture$cfg, focused$jobs, allow_back = FALSE
  )
  expect_identical(details$action, "exit")
  expect_identical(details$job$job_id, "931")
  expect_setequal(details$logs$type, c("stderr", "stdout"))
})

test_that("guided log display strips embedded terminal formatting", {
  log_file <- tempfile("diagnosis-ansi-", fileext = ".err")
  on.exit(unlink(log_file), add = TRUE)
  writeLines(paste0("\033[31m", "readable failure", "\033[0m"), log_file)
  local_mocked_bindings(
    prompt_input = function(...) 1L,
    .package = "BrainGnomes"
  )

  output <- capture.output(diagnosis_show_log(log_file))
  expect_true(any(grepl("readable failure", output, fixed = TRUE)))
  expect_false(any(grepl("\033", output, fixed = TRUE)))
})

test_that("current-directory discovery requires a project configuration", {
  empty_dir <- tempfile("not-a-project-")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE, force = TRUE), add = TRUE)
  withr::local_dir(empty_dir)

  expect_error(
    inspect_project(),
    "No project_config.yaml found in project directory"
  )
  expect_error(
    diagnose_project(interactive = FALSE),
    "No project_config.yaml found in project directory"
  )
  expect_error(
    diagnose_project(interactive = TRUE),
    "No project_config.yaml found in project directory"
  )
})

test_that("empty inspections and deprecated run getters retain stable contracts", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  empty <- inspect_project(fixture$cfg)
  expect_equal(empty$overview$overall_status, "EMPTY")
  expect_equal(nrow(empty$jobs), 0L)
  expect_equal(nrow(empty$subjects), 0L)

  add_inspection_job(fixture$db, "500", "mriqc_sub-01", "run-one", "QUEUED")
  expect_warning(runs <- get_project_runs(fixture$cfg), "deprecated")
  expect_warning(jobs <- get_run_jobs(fixture$cfg), "deprecated")
  expect_s3_class(runs, "data.frame")
  expect_s3_class(jobs, "bg_run_jobs")
  expect_s3_class(jobs, "data.frame")
  expect_equal(jobs$sub_id, "01")
  output <- capture.output(
    messages <- capture.output(print(jobs), type = "message")
  )
  output <- c(output, messages)
  expect_false(any(grepl("constraint=a-very-long-value", output, fixed = TRUE)))
})

test_that("diagnose_project is the only public diagnosis entry point", {
  expect_true("diagnose_project" %in% getNamespaceExports("BrainGnomes"))
  expect_false("diagnose_pipeline" %in% getNamespaceExports("BrainGnomes"))
  expect_false(exists("diagnose_pipeline", envir = asNamespace("BrainGnomes"), inherits = FALSE))
})
