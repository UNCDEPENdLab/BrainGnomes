# Opt-in smoke test against a maintainer-supplied real project. Nothing is copied
# into the repository, no configuration is saved, and no processing is launched.
# This tests real discovery/preview prerequisites, not container execution or
# imaging correctness. Run separately from CI, for example:
# BG_SETUP_SMOKE_PROJECT=/path/to/project_config.yaml \
# BG_SETUP_SMOKE_STEPS=fmriprep BG_SETUP_SMOKE_SUBJECT=01 \
# Rscript -e 'devtools::test(filter = "^setup-preview-smoke$")'
# STEPS accepts a comma-separated list (default: all enabled stages). SUBJECT
# is optional; it limits discovery to one subject when working with large data.
test_that("a supplied real project validates and previews without changing project state", {
  input <- Sys.getenv("BG_SETUP_SMOKE_PROJECT", "")
  skip_if(!nzchar(input), "Set BG_SETUP_SMOKE_PROJECT to opt into a read-only real-project preview smoke test.")
  steps <- trimws(strsplit(Sys.getenv("BG_SETUP_SMOKE_STEPS", "all"), ",", fixed = TRUE)[[1L]])
  subject <- Sys.getenv("BG_SETUP_SMOKE_SUBJECT", "")
  if (!nzchar(subject)) subject <- NULL
  cfg <- load_project(input, validate = FALSE)
  config_file <- attr(cfg, "yaml_file")
  # Monitor configured output locations and the tracking database without
  # traversing/hashing large imaging datasets or reading patient metadata.
  watched <- unique(c(config_file, unlist(cfg$metadata[intersect(names(cfg$metadata), c(
    "sqlite_db", "log_directory", "scratch_directory", "bids_directory",
    "fmriprep_directory", "postproc_directory", "rois_directory", "mriqc_directory",
    "flywheel_sync_directory", "flywheel_temp_directory", "templateflow_home"
  ))], use.names = FALSE)))
  watched <- watched[!is.na(watched) & nzchar(watched)]
  before <- file.info(watched)[, c("size", "isdir", "mode", "mtime")]
  config_hash <- tools::md5sum(config_file)
  local_mocked_bindings(
    prompt_input = function(...) stop("Smoke preview must not prompt"),
    save_project_config = function(...) stop("Smoke preview attempted to save configuration"),
    setup_project_directories = function(...) stop("Smoke preview attempted directory creation"),
    record_run_provenance = function(...) stop("Smoke preview attempted provenance writes"),
    submit_flywheel_sync = function(...) stop("Smoke preview attempted Flywheel sync"),
    submit_fsaverage_setup = function(...) stop("Smoke preview attempted FreeSurfer setup"),
    submit_prefetch_templates = function(...) stop("Smoke preview attempted template downloads"),
    cluster_job_submit = function(...) stop("Smoke preview attempted scheduler submission"),
    .package = "BrainGnomes"
  )
  report <- validate_project_config(cfg, steps = steps, quiet = TRUE)
  expect_true(report$valid, info = paste(report$issues$message, collapse = "\n"))
  plan <- plan_project(cfg, steps = steps, subject_filter = subject, quiet = TRUE)
  # Avoid printing real subject identifiers and paths in the test reporter.
  invisible(capture.output(preview <- run_project(cfg, steps = steps,
    subject_filter = subject, dry_run = TRUE)))
  expect_true(plan$validation$valid)
  expect_identical(plan$validation, preview$validation)
  expect_identical(plan$preview, preview$preview)
  expect_true(isTRUE(plan$scope_deferred) || nrow(plan$subjects) > 0L)
  expect_identical(tools::md5sum(config_file), config_hash)
  expect_identical(file.info(watched)[, c("size", "isdir", "mode", "mtime")], before)
})
