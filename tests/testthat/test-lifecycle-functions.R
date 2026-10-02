make_lifecycle_project <- function() {
  root <- tempfile("lifecycle-project-")
  cfg <- setup_project(
    project_name = "lifecycle",
    project_directory = root,
    interactive = FALSE
  )
  dir.create(file.path(cfg$metadata$bids_directory, "sub-01"), recursive = TRUE)
  container <- file.path(root, "fmriprep.sif")
  license <- file.path(root, "license.txt")
  file.create(container, license)
  cfg$fmriprep <- list(
    enable = TRUE,
    output_spaces = "MNI152NLin2009cAsym",
    fs_license_file = license,
    memgb = 8,
    nhours = 1,
    ncores = 2,
    cli_options = NULL,
    sched_args = NULL
  )
  cfg$compute_environment$fmriprep_container <- container
  cfg <- write_project_config(cfg, overwrite = TRUE)
  list(root = root, cfg = cfg, config_file = attr(cfg, "yaml_file"))
}

test_that("non-interactive setup creates a valid portable project", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_true(file.exists(fixture$config_file))
  expect_identical(fixture$cfg$schema_version, 1L)
  validation <- validate_project_config(fixture$cfg, quiet = TRUE)
  expect_true(validation$valid)
  expect_s3_class(validation, "bg_project_validation")
  expect_identical(names(validation$issues), c("severity", "code", "field", "message"))
})

test_that("setup_project is the only exported project-creation entry point", {
  exports <- getNamespaceExports("BrainGnomes")

  expect_true("setup_project" %in% exports)
  expect_false("initialize_project" %in% exports)
  expect_false(exists(
    "initialize_project",
    envir = asNamespace("BrainGnomes"),
    inherits = FALSE
  ))
})

test_that("non-interactive setup supports templates and guarded overwrite", {
  template_root <- tempfile("setup-template-")
  project_root <- tempfile("setup-from-template-")
  on.exit(unlink(template_root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(project_root, recursive = TRUE, force = TRUE), add = TRUE)
  template <- setup_project(
    project_name = "template",
    project_directory = template_root,
    interactive = FALSE
  )
  template$custom_setting <- "preserved"

  cfg <- setup_project(
    project_name = "derived",
    project_directory = project_root,
    template = template,
    interactive = FALSE
  )

  expect_identical(cfg$metadata$project_name, "derived")
  expect_identical(
    cfg$metadata$project_directory,
    normalizePath(project_root, winslash = "/", mustWork = TRUE)
  )
  expect_identical(cfg$custom_setting, "preserved")
  expect_error(
    setup_project(
      project_name = "derived",
      project_directory = project_root,
      interactive = FALSE
    ),
    "Configuration file already exists"
  )
  expect_s3_class(
    setup_project(
      project_name = "derived",
      project_directory = project_root,
      interactive = FALSE,
      overwrite = TRUE
    ),
    "bg_project_cfg"
  )
})

test_that("load_project validation never repairs or rewrites configuration", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  raw <- yaml::read_yaml(fixture$config_file)
  raw$metadata$scratch_directory <- file.path(fixture$root, "missing-scratch")
  yaml::write_yaml(raw, fixture$config_file)
  before <- readLines(fixture$config_file, warn = FALSE)

  loaded <- load_project(fixture$config_file, validate = TRUE)
  after <- readLines(fixture$config_file, warn = FALSE)
  validation <- attr(loaded, "validation")

  expect_identical(after, before)
  expect_false(validation$valid)
  expect_true("metadata/scratch_directory" %in% validation$issues$field)
})

test_that("doctor_project returns structured non-mutating preflight checks", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  report <- doctor_project(fixture$cfg, steps = "fmriprep", quiet = TRUE)
  expect_s3_class(report, "bg_project_doctor")
  expect_identical(names(report$checks), c("category", "check", "status", "detail", "remedy"))
  expect_true(any(report$checks$check == "project_config" & report$checks$status == "pass"))
  expect_true(all(report$checks$status %in% c("pass", "warn", "fail")))

  disabled <- doctor_project(fixture$cfg, steps = "mriqc", quiet = TRUE)
  expect_false(disabled$ok)
  expect_true(any(
    disabled$checks$check == "requested_steps" &
      disabled$checks$status == "fail"
  ))
})

test_that("doctor_project is the only exported R preflight entry point", {
  exports <- getNamespaceExports("BrainGnomes")

  expect_true("doctor_project" %in% exports)
  expect_false("doctor" %in% exports)
  expect_false(exists("doctor", envir = asNamespace("BrainGnomes"), inherits = FALSE))
})

test_that("plans include implicit setup jobs and round-trip through YAML", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  plan <- plan_project(fixture$cfg, steps = "fmriprep", quiet = TRUE)
  expect_s3_class(plan, "bg_project_plan")
  expect_setequal(plan$jobs$stage, c("fsaverage_setup", "prefetch_templates", "fmriprep"))
  expect_equal(plan$subjects$sub_id, "01")
  expect_match(plan$jobs$depends_on[plan$jobs$stage == "fmriprep"], "fsaverage_setup")

  plan_file <- file.path(fixture$root, "plans", "fmriprep.yaml")
  write_project_plan(plan, plan_file)
  restored <- read_project_plan(plan_file)
  expect_s3_class(restored, "bg_project_plan")
  expect_identical(restored$plan_id, plan$plan_id)
  expect_equal(restored$jobs$stage, plan$jobs$stage)
})

test_that("plans expose the same resolved execution model used by direct runs", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  execution <- BrainGnomes:::resolve_project_execution(
    fixture$cfg,
    steps = "fmriprep",
    subject_filter = "01",
    force = TRUE
  )
  plan <- plan_project(
    fixture$cfg,
    steps = "fmriprep",
    subject_filter = "01",
    force = TRUE,
    quiet = TRUE
  )

  expect_identical(plan$request$steps, execution$steps)
  expect_identical(
    plan$request$postprocess_streams,
    execution$postprocess_streams
  )
  expect_identical(plan$request$extract_streams, execution$extract_streams)
  expect_identical(plan$request$force, execution$force)
  expect_equal(plan$subjects, execution$subjects)
  expect_identical(plan$scope_deferred, execution$scope_deferred)
  expect_identical(plan$scope_status, execution$scope_status)
  expect_identical(plan$deferred_reasons, execution$deferred_reasons)
})

test_that("Flywheel plans defer scope and preserve the requested filter", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  fixture$cfg$flywheel_sync <- list(
    enable = TRUE,
    source_url = "fw://example/project"
  )

  plan <- plan_project(
    fixture$cfg,
    steps = c("flywheel_sync", "fmriprep"),
    subject_filter = "01",
    allow_invalid = TRUE,
    quiet = TRUE
  )
  expect_true(plan$scope_deferred)
  expect_identical(plan$scope_status, "deferred")
  expect_identical(plan$deferred_reasons, "flywheel_sync")
  expect_identical(plan$request$subject_filter, "01")
  expect_true(is.na(plan$jobs$n_jobs[plan$jobs$stage == "fmriprep"]))

  captured_filter <- NULL
  captured_context <- NULL
  local_mocked_bindings(
    run_project = function(scfg, ..., subject_filter = NULL) {
      captured_filter <<- subject_filter
      captured_context <<- attr(scfg, "provenance_context")
      "submitted"
    },
    .package = "BrainGnomes"
  )
  expect_identical(submit_project_plan(plan), "submitted")
  expect_identical(captured_filter, "01")
  expect_identical(captured_context$interface, "in_memory_plan")
  expect_identical(captured_context$plan_id, plan$plan_id)
})

test_that("submitting a reviewed plan preserves its subject scope", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  plan <- plan_project(fixture$cfg, steps = "fmriprep", quiet = TRUE)
  dir.create(file.path(fixture$cfg$metadata$bids_directory, "sub-02"))

  captured_filter <- NULL
  captured_context <- NULL
  local_mocked_bindings(
    run_project = function(scfg, ..., subject_filter = NULL) {
      captured_filter <<- subject_filter
      captured_context <<- attr(scfg, "provenance_context")
      "submitted"
    },
    .package = "BrainGnomes"
  )

  expect_identical(submit_project_plan(plan), "submitted")
  expect_s3_class(captured_filter, "data.frame")
  expect_identical(captured_filter$sub_id, "01")
  expect_identical(captured_context$interface, "in_memory_plan")
  expect_identical(captured_context$plan_id, plan$plan_id)
})

test_that("tracked run APIs summarize, diagnose, locate logs, and preview cancellation", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- fixture$cfg$metadata$sqlite_db
  run_id <- "run-lifecycle-1"

  insert_tracked_job(db, "81001", list(
    job_name = "fmriprep_sub-01", sequence_id = run_id,
    n_nodes = 1, n_cpus = 2, status = "FAILED", scheduler = "slurm"
  ))
  insert_tracked_job(db, "81002", list(
    job_name = "mriqc_sub-01", sequence_id = run_id,
    n_nodes = 1, n_cpus = 1, status = "QUEUED", scheduler = "slurm"
  ))
  log_file <- file.path(fixture$cfg$metadata$log_directory, "sub-01", "fmriprep_jobid-81001.err")
  dir.create(dirname(log_file), recursive = TRUE)
  writeLines("simulated failure", log_file)

  runs <- suppressWarnings(get_project_runs(fixture$cfg))
  expect_equal(runs$run_id, run_id)
  expect_equal(runs$status, "FAILED")
  jobs <- suppressWarnings(get_run_jobs(fixture$cfg, "latest"))
  expect_setequal(jobs$job_id, c("81001", "81002"))

  diagnosis <- diagnose_project(fixture$cfg, run_id, interactive = FALSE)
  expect_s3_class(diagnosis, "bg_project_diagnosis")
  expect_equal(diagnosis$failures$job_id, "81001")
  expect_equal(diagnosis$logs$path, log_file)

  cancellation <- cancel_project_run(fixture$cfg, run_id)
  expect_equal(cancellation$job_id, "81002")
  expect_equal(cancellation$status, "would_cancel")
  expect_equal(cancellation$command, "scancel 81002")
})

test_that("cancellation captures scheduler diagnostics and preserves exit status", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  insert_tracked_job(fixture$cfg$metadata$sqlite_db, "81003", list(
    job_name = "fmriprep_sub-01", sequence_id = "cancel-output",
    n_nodes = 1, n_cpus = 2, status = "QUEUED", scheduler = "slurm"
  ))
  scheduler_status <- 1L
  local_mocked_bindings(
    system2 = function(command, args, stdout, stderr) {
      expect_identical(command, "scancel")
      expect_identical(args, "81003")
      expect_true(stdout)
      expect_true(stderr)
      structure("Scheduler diagnostic", status = scheduler_status)
    },
    .package = "base"
  )
  expect_output(failed <- cancel_project_run(fixture$cfg, "cancel-output", dry_run = FALSE), NA)
  expect_identical(failed$status, "failed")
  expect_identical(inspect_project(fixture$cfg)$jobs$status, "QUEUED")
  scheduler_status <- 0L
  expect_output(cancelled <- cancel_project_run(fixture$cfg, "cancel-output", dry_run = FALSE), NA)
  expect_identical(cancelled$status, "cancelled")
  expect_identical(inspect_project(fixture$cfg)$jobs$status, "CANCELLED")
})

test_that("retry dry runs derive a force plan from failed jobs", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- fixture$cfg$metadata$sqlite_db

  insert_tracked_job(db, "82001", list(
    job_name = "fmriprep_sub-01", sequence_id = "retry-source",
    n_nodes = 1, n_cpus = 2, status = "FAILED", scheduler = "slurm"
  ))
  retry <- retry_project_run(fixture$cfg, "retry-source")
  expect_s3_class(retry, "bg_project_plan")
  expect_identical(retry$request$steps, "fmriprep")
  expect_true(retry$request$force)
  expect_identical(retry$request$subject_filter, "01")
})

test_that("retry previews preserve stream names containing underscores", {
  jobs <- data.frame(
    id = 1:3,
    parent_id = c(NA_integer_, 1L, NA_integer_),
    job_name = c(
      "postprocess_rest_clean_sub-01_ses-A",
      "postprocess_rest_clean_sentinel",
      "extract_rois_schaefer_task_sub-01_ses-A"
    ),
    status = rep("FAILED", 3L),
    stringsAsFactors = FALSE
  )

  request <- BrainGnomes:::retry_request_from_jobs(jobs)

  expect_setequal(request$steps, c("postprocess", "extract_rois"))
  expect_identical(request$postprocess_streams, "rest_clean")
  expect_identical(request$extract_streams, "schaefer_task")
  expect_identical(request$subject_filter, "01")
})

test_that("submitted retries record their source run", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- fixture$cfg$metadata$sqlite_db
  insert_tracked_job(db, "82101", list(
    job_name = "fmriprep_sub-01", sequence_id = "retry-parent",
    n_nodes = 1, n_cpus = 2, status = "FAILED", scheduler = "slurm"
  ))

  captured_context <- NULL
  local_mocked_bindings(
    run_project = function(scfg, ...) {
      captured_context <<- attr(scfg, "provenance_context")
      "submitted"
    },
    .package = "BrainGnomes"
  )
  expect_identical(
    retry_project_run(fixture$cfg, "retry-parent", dry_run = FALSE),
    "submitted"
  )
  expect_identical(captured_context$interface, "retry")
  expect_identical(captured_context$parent_run_id, "retry-parent")
})

test_that("CANCELLED is a supported terminal tracking status", {
  fixture <- make_lifecycle_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- fixture$cfg$metadata$sqlite_db
  insert_tracked_job(db, "83001", list(
    job_name = "fmriprep_sub-01", sequence_id = "cancelled-run",
    n_nodes = 1, n_cpus = 2, status = "QUEUED", scheduler = "slurm"
  ))
  update_tracked_job_status(db, "83001", "CANCELLED")
  expect_equal(get_tracked_job_status("83001", sqlite_db = db)$status, "CANCELLED")
})
