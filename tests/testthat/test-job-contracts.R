make_contract_project <- function() {
  root <- tempfile("contract-project-")
  cfg <- setup_project(
    project_name = "contract",
    project_directory = root,
    interactive = FALSE
  )
  dir.create(file.path(cfg$metadata$bids_directory, "sub-01"), recursive = TRUE)
  container <- file.path(root, "fmriprep.sif")
  license <- file.path(root, "license.txt")
  file.create(container, license)
  cfg$fmriprep <- list(
    enable = TRUE, output_spaces = "MNI152NLin2009cAsym",
    fs_license_file = license, memgb = 8, nhours = 1, ncores = 2,
    cli_options = NULL, sched_args = NULL
  )
  cfg$compute_environment$fmriprep_container <- container
  cfg <- write_project_config(cfg, overwrite = TRUE)
  list(root = root, cfg = cfg)
}

test_that("tracked scheduler submissions seal a job manifest", {
  skip_on_os("windows") # Slurm submission and its shell-script fixture are Unix-only.

  root <- tempfile("job-contract-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)

  fake_sbatch <- file.path(root, "sbatch")
  writeLines(c("#!/bin/sh", "echo 'Submitted batch job 4242'"), fake_sbatch)
  Sys.chmod(fake_sbatch, "0755")
  script <- file.path(root, "worker.sbatch")
  writeLines(c("#!/bin/sh", "exit 0"), script)
  Sys.chmod(script, "0755")
  container <- file.path(root, "fmriprep.sif")
  custom_input <- file.path(root, "custom-input.txt")
  unclassified_file <- file.path(root, "unclassified.txt")
  mutable_files <- file.path(root, c(
    "subject.log", "job.out", "job.err", "complete.marker",
    "task-status.env", "prefetch-state.json", "outputs.json"
  ))
  file.create(c(container, custom_input, unclassified_file, mutable_files))
  withr::local_envvar(PATH = paste(root, Sys.getenv("PATH"), sep = .Platform$path.sep))

  tracking <- list(
    job_name = "fmriprep_sub-01",
    sequence_id = "run-contract",
    contract_directory = file.path(root, "runs", "run-contract", "jobs"),
    stage = "fmriprep",
    sub_id = "01",
    job_role = "subject",
    unit_key = "subject::01::::fmriprep::",
    n_nodes = 1L,
    n_cpus = 2L,
    wall_time = "01:00:00",
    mem_total = "8G",
    stdout_log = mutable_files[[2L]],
    stderr_log = mutable_files[[3L]],
    contract_artifact_env_names = c(
      "custom_input", "sqlite_db", "log_file", "stdout_log",
      "stderr_log", "complete_file", "status_file",
      "prefetch_state_file", "output_manifest_file"
    )
  )
  job_id <- cluster_job_submit(
    script, scheduler = "slurm", sched_args = "--time=01:00:00",
    env_variables = c(
      API_TOKEN = "do-not-record", input_label = "task",
      fmriprep_container = container, custom_input = custom_input,
      unclassified_file = unclassified_file,
      log_file = mutable_files[[1L]], stdout_log = mutable_files[[2L]],
      stderr_log = mutable_files[[3L]], complete_file = mutable_files[[4L]],
      status_file = mutable_files[[5L]],
      prefetch_state_file = mutable_files[[6L]],
      output_manifest_file = mutable_files[[7L]]
    ),
    echo = FALSE, tracking_sqlite_db = db, tracking_args = tracking
  )

  expect_identical(as.character(job_id), "4242")
  row <- get_tracked_job_status("4242", sqlite_db = db)
  expect_identical(row$stage, "fmriprep")
  expect_identical(row$sub_id, "01")
  expect_identical(row$job_role, "subject")
  expect_identical(row$attempt, 1L)
  expect_true(file.exists(row$job_manifest_path))
  expect_identical(
    row$job_manifest_checksum,
    unname(tools::md5sum(row$job_manifest_path))
  )

  manifest <- jsonlite::read_json(row$job_manifest_path, simplifyVector = TRUE)
  expect_identical(manifest$schema_version, "brain-gnomes-job-manifest-v1")
  expect_identical(manifest$state, "prepared_for_submission")
  expect_identical(manifest$logical_work_unit$stage, "fmriprep")
  expect_identical(manifest$scheduler$requested_resources$nodes, 1L)
  token <- manifest$execution$environment[
    manifest$execution$environment$name == "API_TOKEN", , drop = FALSE
  ]
  expect_identical(token$value, "<redacted>")
  expect_false(grepl(
    "do-not-record", manifest$execution$rendered_submission_command,
    fixed = TRUE
  ))
  artifact_roles <- manifest$artifacts$role
  expect_setequal(artifact_roles, c(
    "batch_script", "execution_payload", "environment.fmriprep_container",
    "environment.custom_input"
  ))
  expect_true(all(c(
    "sqlite_db", "log_file", "stdout_log", "stderr_log", "complete_file",
    "status_file", "prefetch_state_file", "output_manifest_file"
  ) %in% manifest$execution$environment$name))
  expect_false(any(paste0("environment.", c(
    "unclassified_file", "sqlite_db", "log_file", "stdout_log",
    "stderr_log", "complete_file", "status_file", "prefetch_state_file",
    "output_manifest_file"
  )) %in% artifact_roles))

  # Normal scheduler bookkeeping mutates every operational file after the
  # contract is sealed. None of those changes should be treated as code drift.
  invisible(lapply(
    c(unclassified_file, mutable_files),
    function(path) write("changed after submission", path, append = TRUE)
  ))
  expect_no_error(update_tracked_job_status(db, job_id, "STARTED"))
  started <- get_tracked_job_status(job_id, sqlite_db = db)
  expect_identical(started$contract_status, "VERIFIED")
})

test_that("environment artifact policy retains current immutable pipeline inputs", {
  env_names <- c(
    "snapshot_rds", "filelist_path", "heudiconv_heuristic",
    "fs_license_file", "bids_validator", "flywheel_cmd",
    "heudiconv_container", "fmriprep_container", "mriqc_container",
    "aroma_container", "prefetch_container", "prefetch_script",
    "postprocess_rscript", "postprocess_image_sched_script",
    "extract_rscript", "extract_sched_script", "insert_tracked_job_path",
    "upd_job_status_path", "add_parent_path"
  )
  env_variables <- stats::setNames(rep("placeholder", length(env_names)), env_names)

  expect_setequal(
    job_contract_environment_artifact_names(env_variables),
    env_names
  )

  mutable_names <- c(
    "sqlite_db", "project_tracking_db", "database_path", "subject_log",
    "worker_stdout_file", "job_status_marker", "prefetch_state_file",
    "runtime_receipt_path", "output_manifest_file", "BG_JOB_MANIFEST"
  )
  expect_true(all(vapply(
    mutable_names,
    is_mutable_job_contract_environment_name,
    logical(1)
  )))
  expect_false(any(vapply(
    c("upd_job_status_path", "filelist_path", "snapshot_rds"),
    is_mutable_job_contract_environment_name,
    logical(1)
  )))
})

test_that("STARTED writes an immutable verified runtime receipt", {
  root <- tempfile("runtime-receipt-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)
  script <- file.path(root, "worker.sh")
  writeLines("exit 0", script)

  manifest <- prepare_external_job_manifest(
    tracking_sqlite_db = db,
    sequence_id = "run-receipt",
    contract_directory = file.path(root, "runs", "run-receipt", "jobs"),
    script = script,
    scheduler = "slurm",
    stage = "mriqc",
    sub_id = "02",
    job_role = "subject",
    job_name = "mriqc_sub-02",
    env_variables = c(input_label = "rest")
  )
  insert_tracked_job(db, "5001", list(
    job_name = "mriqc_sub-02",
    job_manifest_path = manifest$path
  ))

  update_tracked_job_status(db, "5001", "STARTED")
  row <- get_tracked_job_status("5001", sqlite_db = db)
  expect_identical(row$contract_status, "VERIFIED")
  expect_true(file.exists(row$runtime_receipt_path))
  receipt <- jsonlite::read_json(row$runtime_receipt_path, simplifyVector = TRUE)
  expect_identical(
    receipt$schema_version, "brain-gnomes-job-runtime-receipt-v1"
  )
  expect_identical(receipt$verification_status, "verified")
  expect_true(all(receipt$artifacts$status == "matched"))

  before <- readLines(row$runtime_receipt_path, warn = FALSE)
  update_tracked_job_status(db, "5001", "STARTED")
  expect_identical(readLines(row$runtime_receipt_path, warn = FALSE), before)
})

test_that("runtime receipt detects execution-driving file drift", {
  root <- tempfile("runtime-drift-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)
  script <- file.path(root, "worker.sh")
  writeLines("first", script)

  manifest <- prepare_external_job_manifest(
    tracking_sqlite_db = db,
    sequence_id = "run-drift",
    contract_directory = file.path(root, "runs", "run-drift", "jobs"),
    script = script,
    scheduler = "slurm",
    stage = "fmriprep",
    sub_id = "03",
    job_role = "subject",
    env_variables = character()
  )
  insert_tracked_job(db, "5002", list(job_manifest_path = manifest$path))
  writeLines("changed content", script)

  expect_error(
    update_tracked_job_status(db, "5002", "STARTED"),
    "Job contract verification failed"
  )
  row <- get_tracked_job_status("5002", sqlite_db = db)
  expect_identical(row$contract_status, "DRIFT")
  expect_true(file.exists(row$runtime_receipt_path))
  receipt <- jsonlite::read_json(row$runtime_receipt_path, simplifyVector = TRUE)
  expect_identical(receipt$verification_status, "drift")
  expect_true(any(receipt$artifacts$status == "changed"))
})

test_that("missing or unreadable manifests leave a failure receipt", {
  for (damage in c("missing", "unreadable")) {
    root <- tempfile(paste0("runtime-manifest-", damage, "-"))
    dir.create(root, recursive = TRUE)
    db <- file.path(root, "tracking.sqlite")
    create_tracking_db(db)
    script <- file.path(root, "worker.sh")
    writeLines("exit 0", script)
    job_id <- if (damage == "missing") "5101" else "5102"

    manifest <- prepare_external_job_manifest(
      tracking_sqlite_db = db,
      sequence_id = paste0("run-", damage),
      contract_directory = file.path(root, "runs", damage, "jobs"),
      script = script,
      scheduler = "slurm",
      stage = "fmriprep",
      sub_id = "04",
      job_role = "subject",
      env_variables = character()
    )
    insert_tracked_job(db, job_id, list(job_manifest_path = manifest$path))
    if (damage == "missing") {
      unlink(manifest$path)
    } else {
      writeLines("not valid JSON", manifest$path)
    }

    expect_error(
      update_tracked_job_status(db, job_id, "STARTED"),
      "Job contract verification failed"
    )
    row <- get_tracked_job_status(job_id, sqlite_db = db)
    expect_identical(row$contract_status, "DRIFT")
    expect_true(file.exists(row$runtime_receipt_path))
    receipt <- jsonlite::read_json(
      row$runtime_receipt_path, simplifyVector = TRUE
    )
    expect_identical(receipt$verification_status, "drift")
    expect_identical(receipt$enforcement, "failed")
    expect_identical(receipt$manifest$status, damage)
    unlink(root, recursive = TRUE, force = TRUE)
  }
})

test_that("legacy tracking databases migrate job contract columns", {
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, paste(
    "CREATE TABLE job_tracking (",
    "id INTEGER PRIMARY KEY, job_id VARCHAR NOT NULL UNIQUE, status VARCHAR(24))"
  ))
  DBI::dbDisconnect(con)

  ensure_tracking_db_schema(db)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  columns <- DBI::dbGetQuery(con, "PRAGMA table_info(job_tracking)")$name
  expect_true(all(c(
    "contract_id", "stage", "stream", "sub_id", "ses_id", "job_role",
    "unit_key", "attempt", "job_manifest_path", "runtime_receipt_path",
    "contract_status", "exit_code", "stdout_log", "stderr_log"
  ) %in% columns))
})

test_that("structured work-unit keys remain compatible with legacy inspection", {
  expect_identical(
    tracking_unit_key(
      "postprocess", "rest_clean", "01", "A", "controller",
      "postprocess_rest_clean_sub-01_ses-A"
    ),
    "subject::01::A::postprocess::rest_clean"
  )
  expect_identical(
    tracking_unit_key(
      "postprocess", "rest_clean", "01", "A", "sentinel",
      "postprocess_rest_clean_sentinel"
    ),
    "subject::01::A::postprocess::rest_clean"
  )
  expect_identical(
    tracking_unit_key(
      "prefetch_templates", job_name = "prefetch_templates"
    ),
    "project::prefetch_templates::::prefetch_templates"
  )
})

test_that("one logical unit shares an attempt across its component jobs", {
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db), add = TRUE)
  create_tracking_db(db)
  key <- "subject::01::A::postprocess::rest_clean"

  expect_identical(next_job_contract_attempt(db, key, "run-one"), 1L)
  insert_tracked_job(db, "6001", list(
    sequence_id = "run-one", unit_key = key, attempt = 1L
  ))
  expect_identical(next_job_contract_attempt(db, key, "run-one"), 1L)
  expect_identical(next_job_contract_attempt(db, key, "run-two"), 2L)
})

test_that("deferred scope realization is recorded before downstream work", {
  fixture <- make_contract_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  execution <- resolve_project_execution(
    fixture$cfg, steps = "fmriprep", subject_filter = "01"
  )
  run_id <- "deferred-scope"
  execution$scope_deferred <- TRUE
  execution$scope_status <- "deferred"
  execution$deferred_reasons <- "flywheel_sync"
  execution$subjects <- execution$subjects[0, , drop = FALSE]
  record_run_provenance(fixture$cfg, run_id, execution)

  subjects <- discover_project_subjects(
    fixture$cfg, "fmriprep", subject_filter = "01"
  )
  realization <- record_run_scope_realization(
    fixture$cfg, run_id, subjects
  )
  expect_true(file.exists(realization))
  scope <- jsonlite::read_json(realization, simplifyVector = TRUE)
  expect_identical(scope$reason, "flywheel_sync")
  expect_identical(scope$n_subjects, 1L)

  provenance <- get_run_provenance(fixture$cfg, run_id)
  expect_identical(provenance$execution$scope_status, "resolved")
  events <- if (is.data.frame(provenance$execution$scope_events)) {
    provenance$execution$scope_events$event
  } else {
    vapply(
      provenance$execution$scope_events, `[[`, character(1), "event"
    )
  }
  expect_identical(events, c("scope_deferred", "scope_resolved"))

  expect_identical(
    record_run_scope_realization(fixture$cfg, run_id, subjects),
    realization
  )
  changed <- subjects
  changed$sub_id <- "02"
  expect_error(
    record_run_scope_realization(fixture$cfg, run_id, changed),
    "saved subject scope differs"
  )
})
