# Create a disposable configured project with two subjects and placeholder
# resources. All scheduler boundaries are mocked by tests that reach submission.
make_guard_project <- function() {
  root <- tempfile("workflow-guards-")
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  cfg <- setup_project(project_name = "guards", project_directory = root, interactive = FALSE)
  for (subject in c("01", "02")) {
    dir.create(file.path(cfg$metadata$bids_directory, paste0("sub-", subject)))
  }
  resources <- file.path(root, c("container.sif", "license.txt"))
  file.create(resources)
  cfg$fmriprep <- list(enable = TRUE, output_spaces = "MNI152NLin2009cAsym",
    fs_license_file = resources[2], memgb = 8, nhours = 1, ncores = 2)
  cfg$mriqc <- list(enable = TRUE, memgb = 8, nhours = 1, ncores = 2)
  cfg$compute_environment$fmriprep_container <- resources[1]
  cfg$compute_environment$mriqc_container <- resources[1]
  write_project_config(cfg, overwrite = TRUE)
}

test_that("explicit run choices survive guided R and CLI stage selection", {
  cfg <- make_guard_project()
  cfg$mriqc$enable <- FALSE
  cfg <- write_project_config(cfg, overwrite = TRUE)
  prompts <- character()
  local_mocked_bindings(
    prompt_input = function(instruct, ...) {
      prompts <<- c(prompts, instruct)
      if (identical(instruct, "Run fmriprep?")) return(TRUE)
      stop("An explicit argument was prompted for: ", instruct)
    },
    record_run_provenance = function(...) stop("A dry run entered live execution"),
    cluster_job_submit = function(...) stop("Unexpected scheduler submission"),
    .package = "BrainGnomes"
  )
  expect_s3_class(run_project(cfg, subject_filter = "01", debug = FALSE,
    force = FALSE, dry_run = TRUE, log_level = "INFO"), "bg_project_plan")
  expect_s3_class(run_project_cli(cfg$metadata$project_directory, list(
    subject_filter = "01", debug = FALSE, force = FALSE, dry_run = TRUE, log_level = "INFO")), "bg_project_plan")
  expect_identical(prompts, rep("Run fmriprep?", 2L))
})

test_that("retry plans preserve subject-stage pairs through YAML and submission", {
  cfg <- make_guard_project()
  for (i in 1:2) insert_tracked_job(cfg$metadata$sqlite_db, paste0("901", i), list(
    job_name = paste0("custom-job-", i), sequence_id = "mixed-failures",
    stage = c("fmriprep", "mriqc")[i], sub_id = c("01", "02")[i],
    status = "FAILED", n_nodes = 1, n_cpus = 2, scheduler = "slurm"
  ))
  plan <- retry_project_run(cfg, "mixed-failures")
  expect_equal(plan$jobs$n_jobs[plan$jobs$stage %in% c("fmriprep", "mriqc")], c(1L, 1L))
  expect_equal(plan$request$work_units$sub_id, c("01", "02"))
  file <- file.path(cfg$metadata$project_directory, "retry.yaml")
  write_project_plan(plan, file)
  saved <- read_project_plan(file)
  expect_identical(saved$schema_version, "brain-gnomes-plan-v2")
  expect_identical(saved$request$work_units, plan$request$work_units)
  damaged <- saved
  damaged$request$work_units <- NULL
  expect_error(submit_project_plan(damaged), "must include work_units")
  write_project_plan(damaged, file, overwrite = TRUE)
  expect_error(read_project_plan(file), "must include work_units")
  submitted <- list()
  seen_context <- NULL
  local_mocked_bindings(
    record_run_provenance = function(scfg, ..., execution) {
      seen_context <<- attr(scfg, "provenance_context")
      expect_identical(execution$work_units, plan$request$work_units)
      NULL
    },
    submit_fsaverage_setup = function(...) "setup1",
    submit_prefetch_templates = function(...) "setup2",
    process_subject = function(scfg, sub_cfg, steps, ...) {
      submitted[[sub_cfg$sub_id[1]]] <<- names(steps)[steps]
    },
    new_project_run = function(...) "submitted",
    cluster_job_submit = function(...) stop("Unexpected real submission"),
    .package = "BrainGnomes"
  )
  expect_identical(submit_project_plan(saved), "submitted")
  expect_identical(submitted, list("01" = "fmriprep", "02" = "mriqc"))
  expect_identical(seen_context$parent_run_id, "mixed-failures")
  expect_identical(seen_context$interface, "saved_plan")
  submitted <- list()
  expect_identical(retry_project_run(cfg, "mixed-failures", dry_run = FALSE), "submitted")
  expect_identical(submitted, list("01" = "fmriprep", "02" = "mriqc"))
})

test_that("retry gates exact sessions and streams before touching completed work", {
  cfg <- make_guard_project()
  cfg$force <- TRUE
  cfg$debug <- FALSE
  cfg$log_level <- "INFO"
  cfg$metadata$sqlite_db <- NULL
  stream <- list(memgb = 1, nhours = 1, ncores = 1)
  cfg$extract_rois <- list(enable = TRUE, alpha = stream, beta = stream)
  for (session in c("A", "B")) {
    dir.create(file.path(cfg$metadata$bids_directory, "sub-01", paste0("ses-", session)))
  }
  units <- data.frame(stage = "extract_rois", stream = c("alpha", "beta"),
    sub_id = "01", ses_id = c("A", "B"))
  attr(cfg, "retry_work_units") <- units
  sub_cfg <- discover_project_subjects(cfg, "extract_rois", "01")
  log_dir <- file.path(cfg$metadata$log_directory, "sub-01")
  dir.create(log_dir)
  untouched <- file.path(log_dir, c(".extract_rois_beta_sub-01_ses-A_complete",
    ".extract_rois_alpha_sub-01_ses-B_complete"))
  file.create(untouched)
  # Loggers are process-global; earlier tests may have removed their temp roots.
  lgr::get_logger(c("sub", "01"))$config(NULL)
  withr::defer(try(lgr::get_logger(c("sub", "01"))$config(NULL), silent = TRUE))
  scheduled <- character()
  local_mocked_bindings(
    get_job_script = function(...) "worker.sbatch",
    get_job_sched_args = function(...) character(),
    submit_extract_rois = function(scfg, sub_dir, sub_id, ses_id, env_variables,
        sched_script, sched_args, parent_ids, lg, tracking_sqlite_db, tracking_args, ex_stream) {
      scheduled <<- c(scheduled, paste(sub_id, ses_id, ex_stream, sep = "/"))
      as.character(length(scheduled))
    },
    cluster_job_submit = function(...) stop("Unexpected real submission"),
    .package = "BrainGnomes"
  )
  steps <- setNames(supported_project_steps() == "extract_rois", supported_project_steps())
  process_subject(cfg, sub_cfg, steps, extract_streams = c("alpha", "beta"))
  expect_identical(scheduled, c("01/A/alpha", "01/B/beta"))
  expect_true(all(file.exists(untouched)))
  expect_identical(retry_subject_scope(sub_cfg, units[1, ])$ses_id, "A")
  units$ses_id[1] <- "missing"
  expect_error(retry_subject_scope(sub_cfg, units), "No inputs found for retry unit")
})

test_that("retry rejects ambiguous legacy jobs and honors blocked selection", {
  expect_error(retry_request_from_jobs(data.frame(
    job_name = "postprocess_clean_sentinel", status = "FAILED")), "Cannot determine exact retry scope")
  jobs <- data.frame(job_name = c("postprocess_clean_sub-01_ses-A", "extract_rois_roi_sub-01_ses-B"),
    status = c("FAILED", "FAILED_BY_EXT"))
  expect_identical(retry_request_from_jobs(jobs)$work_units$ses_id, "A")
  expect_identical(retry_request_from_jobs(jobs, TRUE)$work_units$ses_id, c("A", "B"))
  # NA is a sessionless unit, never a wildcard for every session.
  sessionless <- data.frame(stage = "extract_rois", stream = "roi", sub_id = "01", ses_id = NA_character_)
  expect_true(retry_includes_unit(sessionless, "extract_rois", "01", NA_character_, "roi"))
  expect_false(retry_includes_unit(sessionless, "extract_rois", "01", "B", "roi"))
})

test_that("setup-only failures need blocked work or an explicit new selection", {
  cfg <- make_guard_project()
  insert_tracked_job(cfg$metadata$sqlite_db, "9021", list(
    job_name = "cp_fsaverage_setup", sequence_id = "setup-failure", stage = "fsaverage_setup",
    status = "FAILED", n_nodes = 1, n_cpus = 1, scheduler = "slurm"
  ))
  insert_tracked_job(cfg$metadata$sqlite_db, "9022", list(
    job_name = "fmriprep_sub-01", sequence_id = "setup-failure", stage = "fmriprep",
    sub_id = "01", status = "FAILED_BY_EXT", n_nodes = 1, n_cpus = 1, scheduler = "slurm"
  ))
  expect_error(retry_project_run(cfg, "setup-failure"), "include_blocked = TRUE")
  plan <- retry_project_run(cfg, "setup-failure", include_blocked = TRUE)
  expect_identical(plan$request$work_units$sub_id, "01")
  expect_identical(plan$request$work_units$stage, "fmriprep")
  expect_true("fsaverage_setup" %in% plan$jobs$stage)
})

test_that("deferred retry scope and provenance retain only requested units", {
  cfg <- make_guard_project()
  cfg$flywheel_sync$enable <- TRUE
  units <- data.frame(stage = c("flywheel_sync", "fmriprep"), stream = NA_character_,
    sub_id = c(NA_character_, "01"), ses_id = NA_character_)
  attr(cfg, "retry_work_units") <- units
  attr(cfg, "provenance_context") <- list(parent_run_id = "original")
  execution <- resolve_project_execution(cfg, c("flywheel_sync", "fmriprep"), force = TRUE)
  expect_true(execution$scope_deferred)
  record_run_provenance(cfg, "deferred-retry", execution)
  snapshot <- list(scfg = cfg, steps = execution$step_flags, sequence_id = "deferred-retry")
  # Real discovery sees both subjects; the controller must keep only sub-01.
  resolved <- realize_deferred_subjects(snapshot)
  expect_identical(resolved$sub_id, "01")
  provenance <- get_run_provenance(cfg, "deferred-retry")
  expect_equal(normalize_retry_work_units(provenance$request$work_units), units)
  expect_identical(provenance$invocation$parent_run_id, "original")
  expect_identical(provenance$execution$subjects$sub_id, "01")
})

test_that("template creation isolates state and preserves external dependencies", {
  cfg <- make_guard_project()
  root <- cfg$metadata$project_directory
  external <- tempfile("shared-resources-")
  dir.create(external)
  withr::defer(unlink(external, recursive = TRUE))
  cfg$metadata$dicom_directory <- external
  cfg$metadata$templateflow_home <- external
  cfg$metadata$log_directory <- external
  cfg$metadata$scratch_directory <- external
  cfg$metadata$postproc_directory <- file.path(root, "custom-output")
  cfg$bids_validation$outfile <- file.path(external, "validation.html")
  cfg$extract_rois$atlas <- list(atlases = file.path(root, "atlas.nii.gz"))
  template <- write_project_config(cfg, overwrite = TRUE)
  before <- readLines(attr(template, "yaml_file"))
  for (input in list(template, attr(template, "yaml_file"), root)) {
    destination <- tempfile("cloned-project-")
    withr::defer(unlink(destination, recursive = TRUE))
    clone <- setup_project(project_name = "new", project_directory = destination,
      template = input, interactive = FALSE)
    destination <- clone$metadata$project_directory
    expect_path_identical(clone$metadata$postproc_directory, file.path(destination, "custom-output"))
    expect_path_identical(clone$metadata$log_directory, file.path(destination, "logs"))
    expect_path_identical(clone$metadata$scratch_directory, file.path(destination, "scratch"))
    expect_path_identical(clone$metadata$sqlite_db, file.path(destination, "new.sqlite"), mustWork = FALSE)
    expect_path_identical(clone$metadata$rois_directory, file.path(destination, "data_rois"))
    expect_path_identical(clone$metadata$bids_directory, file.path(destination, "data_bids"))
    expect_path_identical(clone$metadata$dicom_directory, external)
    expect_path_identical(clone$metadata$templateflow_home, external)
    expect_identical(clone$compute_environment, template$compute_environment)
    expect_identical(clone$extract_rois$atlas, template$extract_rois$atlas)
    expect_identical(clone$bids_validation$outfile, "validation.html")
    expect_identical(clone$fmriprep, template$fmriprep)
    expect_identical(readLines(attr(template, "yaml_file")), before)
  }
  destination <- tempfile("shared-project-")
  withr::defer(unlink(destination, recursive = TRUE))
  shared <- setup_project(project_name = "shared", project_directory = destination,
    template = template, interactive = FALSE, reuse_template_paths = TRUE)
  expect_identical(shared$metadata$sqlite_db, template$metadata$sqlite_db)
  expect_identical(shared$metadata$postproc_directory, template$metadata$postproc_directory)
})

test_that("templates distinguish external inputs from producing-stage destinations", {
  cfg <- make_guard_project()
  external <- tempfile("external-inputs-")
  dir.create(external)
  withr::defer(unlink(external, recursive = TRUE))
  cfg$metadata$bids_directory <- external
  cfg$metadata$fmriprep_directory <- external
  cfg$fmriprep$enable <- FALSE
  root <- tempfile("input-template-")
  withr::defer(unlink(root, recursive = TRUE))
  clone <- setup_project(project_name = "inputs", project_directory = root,
    template = cfg, interactive = FALSE)
  expect_path_identical(clone$metadata$bids_directory, external)
  expect_path_identical(clone$metadata$fmriprep_directory, external)
  cfg$aroma$enable <- TRUE
  cfg$flywheel_sync$enable <- TRUE
  cfg$metadata$flywheel_sync_directory <- external
  cfg$metadata$dicom_directory <- file.path(external, "incoming", "dicoms")
  root2 <- tempfile("output-template-")
  withr::defer(unlink(root2, recursive = TRUE))
  clone <- setup_project(project_name = "outputs", project_directory = root2,
    template = cfg, interactive = FALSE)
  root2 <- clone$metadata$project_directory
  expect_path_identical(clone$metadata$fmriprep_directory, file.path(root2, "data_fmriprep"))
  expect_path_identical(clone$metadata$bids_directory, file.path(root2, "data_bids"))
  expect_false(dir.exists(cfg$metadata$dicom_directory))
  expect_path_identical(clone$metadata$dicom_directory,
    file.path(clone$metadata$flywheel_sync_directory, "incoming", "dicoms"))
})
