test_that("collect_submit_permission_issues caches repeated project-level checks", {
  root <- tempfile("perm_cache_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      scratch_directory = file.path(root, "scratch"),
      templateflow_home = file.path(root, "templateflow"),
      postproc_directory = file.path(root, "postproc")
    )
  )

  n_calls <- 0L
  local_mocked_bindings(
    check_write_target = function(path, label) {
      n_calls <<- n_calls + 1L
      NULL
    },
    .package = "BrainGnomes"
  )

  check_cache <- new.env(parent = emptyenv())
  collect_submit_permission_issues(
    scfg = scfg, step_name = "postprocess", sub_id = "01",
    stdout_log = file.path(root, "logs", "sub-01", "step.out"),
    stderr_log = file.path(root, "logs", "sub-01", "step.err"),
    sqlite_db = file.path(root, "track.sqlite"),
    check_cache = check_cache
  )
  collect_submit_permission_issues(
    scfg = scfg, step_name = "postprocess", sub_id = "02",
    stdout_log = file.path(root, "logs", "sub-02", "step.out"),
    stderr_log = file.path(root, "logs", "sub-02", "step.err"),
    sqlite_db = file.path(root, "track.sqlite"),
    check_cache = check_cache
  )

  # With path-only cache keys:
  #  Call 1 (sub-01): logs/sub-01 (1), track.sqlite (2), scratch (3), postproc/sub-01 (4)
  #    (stderr dirname = stdout dirname → cache hit, no new call)
  #  Call 2 (sub-02): logs/sub-02 (5), postproc/sub-02 (6)
  #    (track.sqlite, scratch → cache hits; stderr dirname → cache hit of logs/sub-02)
  expect_equal(n_calls, 6L)
})

test_that("submit_subjects reuses one permission-check cache across subjects", {
  root <- tempfile("perm_submit_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  bids_dir <- file.path(root, "bids")
  dir.create(file.path(bids_dir, "sub-01"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(bids_dir, "sub-02"), recursive = TRUE, showWarnings = FALSE)

  scfg <- list(metadata = list(bids_directory = bids_dir))
  class(scfg) <- "bg_project_cfg"

  steps <- c(
    flywheel_sync = FALSE, bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE,
    aroma = FALSE, postprocess = FALSE, extract_rois = FALSE
  )

  caches <- list()
  local_mocked_bindings(
    process_subject = function(scfg, sub_cfg, steps, postprocess_streams = NULL, extract_streams = NULL,
                               parent_ids = NULL, sequence_id = NULL, permission_check_cache = NULL) {
      caches[[length(caches) + 1L]] <<- permission_check_cache
      TRUE
    },
    .package = "BrainGnomes"
  )

  submit_subjects(scfg = scfg, steps = steps)

  expect_equal(length(caches), 2L)
  expect_true(is.environment(caches[[1L]]))
  expect_identical(caches[[1L]], caches[[2L]])
})
