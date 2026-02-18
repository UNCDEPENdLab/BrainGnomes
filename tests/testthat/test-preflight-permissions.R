# Tests for describe_path_permissions, check_write_target, and project-level
# preflight permission checks in submit_flywheel_sync, submit_fsaverage_setup,
# and submit_prefetch_templates.

# -- describe_path_permissions -------------------------------------------------

test_that("describe_path_permissions returns fallback for non-string input", {
  expect_equal(describe_path_permissions(NULL), "<unset>")
  expect_equal(describe_path_permissions(NA), "<unset>")
  expect_equal(describe_path_permissions(123), "<unset>")
  expect_equal(describe_path_permissions(NULL, fallback_label = "<missing path>"), "<missing path>")
})

test_that("describe_path_permissions returns formatted string for existing path", {
  tmp <- tempfile("dpp_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  result <- describe_path_permissions(tmp)
  expect_true(grepl(tmp, result, fixed = TRUE))
  expect_true(grepl("mode=", result, fixed = TRUE))
  expect_true(grepl("uid=", result, fixed = TRUE))
  expect_true(grepl("gid=", result, fixed = TRUE))
})

test_that("describe_path_permissions handles non-existent path gracefully", {
  result <- describe_path_permissions("/no/such/path/xyzzy_1234567890")
  expect_true(grepl("mode=unknown", result, fixed = TRUE))
})

# -- check_write_target --------------------------------------------------------

test_that("check_write_target returns NULL for writable directory", {
  tmp <- tempfile("cwt_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  expect_null(check_write_target(tmp, "test dir"))
})

test_that("check_write_target returns NULL when parent is writable and target doesn't exist", {
  tmp <- tempfile("cwt_parent_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  target <- file.path(tmp, "new_subdir", "deep")
  expect_null(check_write_target(target, "nested target"))
})

test_that("check_write_target returns NULL for non-string input", {
  expect_null(check_write_target(NULL, "null path"))
  expect_null(check_write_target(NA, "na path"))
})

test_that("check_write_target reports issue for unwritable directory", {
  skip_on_os("windows")
  tmp <- tempfile("cwt_noperm_")
  dir.create(tmp)
  on.exit({
    Sys.chmod(tmp, "755")
    unlink(tmp, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # Remove write permission

  Sys.chmod(tmp, "555")
  result <- check_write_target(tmp, "readonly dir")
  expect_true(is.character(result))
  expect_true(grepl("readonly dir", result, fixed = TRUE))
  expect_true(grepl("not writable", result, fixed = TRUE))
})

test_that("check_write_target reports issue when ancestor is unwritable", {
  skip_on_os("windows")
  tmp <- tempfile("cwt_ancestor_")
  dir.create(tmp)
  on.exit({
    Sys.chmod(tmp, "755")
    unlink(tmp, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  Sys.chmod(tmp, "555")
  target <- file.path(tmp, "subdir", "file.db")
  result <- check_write_target(target, "deep file")
  expect_true(is.character(result))
  expect_true(grepl("deep file", result, fixed = TRUE))
})

# -- project-level preflight in submit_flywheel_sync --------------------------

test_that("submit_flywheel_sync stops on unwritable log directory", {
  root <- tempfile("fw_pf_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  log_dir <- file.path(root, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      flywheel_sync_directory = file.path(root, "fw_sync"),
      flywheel_temp_directory = file.path(root, "fw_tmp")
    ),
    flywheel_sync = list(
      source_url = "fw://test/project",
      save_audit_logs = FALSE,
      nhours = 1, memgb = 4, ncores = 1,
      cli_options = NULL
    ),
    compute_environment = list(
      flywheel = "/usr/bin/fw",
      scheduler = "sh"
    )
  )

  # Mock get_job_sched_args to avoid needing full scfg, and check_write_target to simulate failure
  local_mocked_bindings(
    get_job_sched_args = function(...) "--time=01:00:00",
    check_write_target = function(path, label) {
      if (grepl("logs", path)) return(glue::glue("{label} is not writable: {path}"))
      NULL
    },
    .package = "BrainGnomes"
  )

  expect_error(
    submit_flywheel_sync(scfg),
    regexp = "Preflight permission check failed for flywheel_sync"
  )
})

test_that("submit_flywheel_sync proceeds when all paths are writable", {
  root <- tempfile("fw_ok_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  log_dir <- file.path(root, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      flywheel_sync_directory = file.path(root, "fw_sync"),
      flywheel_temp_directory = file.path(root, "fw_tmp")
    ),
    flywheel_sync = list(
      source_url = "fw://test/project",
      save_audit_logs = FALSE,
      nhours = 1, memgb = 4, ncores = 1,
      cli_options = NULL
    ),
    compute_environment = list(
      flywheel = "/usr/bin/fw",
      scheduler = "sh"
    )
  )

  submitted <- FALSE
  captured_env <- NULL
  local_mocked_bindings(
    get_job_sched_args = function(...) "--time=01:00:00",
    check_write_target = function(path, label) NULL,
    cluster_job_submit = function(...) {
      captured_env <<- list(...)[["env_variables"]]
      submitted <<- TRUE
      jid <- "12345"
      attr(jid, "cmd") <- "fw sync ..."
      jid
    },
    to_log = function(...) invisible(NULL),
    .package = "BrainGnomes"
  )

  result <- submit_flywheel_sync(scfg)
  expect_true(submitted)
  expect_true("stdout_log" %in% names(captured_env))
  expect_true("stderr_log" %in% names(captured_env))
  expect_match(unname(captured_env["stdout_log"]), "flywheel_sync_jobid-%j_")
  expect_match(unname(captured_env["stderr_log"]), "flywheel_sync_jobid-%j_")
  expect_equal(result, {
    jid <- "12345"
    attr(jid, "cmd") <- "fw sync ..."
    jid
  })
})

# -- project-level preflight in submit_fsaverage_setup -------------------------

test_that("submit_fsaverage_setup stops on unwritable fmriprep directory", {
  root <- tempfile("fsa_pf_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(fmriprep_dir)
  container <- file.path(root, "fmriprep.sif")
  file.create(container)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = file.path(root, "logs"),
      fmriprep_directory = fmriprep_dir,
      sqlite_db = file.path(root, "track.sqlite")
    ),
    compute_environment = list(
      fmriprep_container = container,
      scheduler = "slurm"
    ),
    debug = FALSE
  )
  class(scfg) <- "bg_project_cfg"

  local_mocked_bindings(
    check_write_target = function(path, label) {
      if (grepl("fmriprep", path)) return(glue::glue("{label} not writable"))
      NULL
    },
    .package = "BrainGnomes"
  )

  expect_error(
    submit_fsaverage_setup(scfg),
    regexp = "Preflight permission check failed for fsaverage_setup"
  )
})

test_that("submit_fsaverage_setup proceeds when all paths are writable", {
  root <- tempfile("fsa_ok_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(fmriprep_dir)
  container <- file.path(root, "fmriprep.sif")
  file.create(container)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = file.path(root, "logs"),
      fmriprep_directory = fmriprep_dir,
      sqlite_db = file.path(root, "track.sqlite")
    ),
    compute_environment = list(
      fmriprep_container = container,
      scheduler = "slurm"
    ),
    debug = FALSE
  )
  class(scfg) <- "bg_project_cfg"

  submitted <- FALSE
  captured_env <- NULL
  captured_sched_call <- NULL
  local_mocked_bindings(
    check_write_target = function(path, label) NULL,
    get_job_sched_args = function(...) {
      captured_sched_call <<- list(...)
      "--time=00:10:00"
    },
    hours_to_dhms = function(h) "0-00:09:00",
    cluster_job_submit = function(...) {
      captured_env <<- list(...)[["env_variables"]]
      submitted <<- TRUE
      "99999"
    },
    .package = "BrainGnomes"
  )

  result <- submit_fsaverage_setup(scfg, sequence_id = "test-seq")
  expect_true(submitted)
  expect_true("stdout_log" %in% names(captured_env))
  expect_true("stderr_log" %in% names(captured_env))
  expect_equal(captured_sched_call$jobid_str, "fsaverage_setup")
  expect_equal(unname(captured_sched_call$stdout_log), unname(captured_env["stdout_log"]))
  expect_equal(unname(captured_sched_call$stderr_log), unname(captured_env["stderr_log"]))
  expect_equal(result, "99999")
})

# -- project-level preflight in submit_prefetch_templates ----------------------

test_that("submit_prefetch_templates stops on unwritable templateflow_home", {
  root <- tempfile("pft_pf_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  container <- file.path(root, "fmriprep.sif")
  file.create(container)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = file.path(root, "logs"),
      templateflow_home = file.path(root, "templateflow")
    ),
    compute_environment = list(
      fmriprep_container = container,
      scheduler = "slurm"
    ),
    fmriprep = list(output_spaces = "MNI152NLin2009cAsym"),
    debug = FALSE,
    log_level = "INFO"
  )
  class(scfg) <- "bg_project_cfg"

  steps <- c(mriqc = FALSE, fmriprep = TRUE, aroma = FALSE)

  local_mocked_bindings(
    get_job_sched_args = function(...) "--time=00:30:00",
    get_job_script = function(...) "/path/to/script.sbatch",
    check_write_target = function(path, label) {
      if (grepl("templateflow", path, ignore.case = TRUE)) {
        return(glue::glue("{label} not writable"))
      }
      NULL
    },
    .package = "BrainGnomes"
  )

  expect_error(
    submit_prefetch_templates(scfg, steps = steps),
    regexp = "Preflight permission check failed for prefetch_templates"
  )
})

test_that("submit_prefetch_templates proceeds when all paths are writable", {
  root <- tempfile("pft_ok_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  container <- file.path(root, "fmriprep.sif")
  file.create(container)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = file.path(root, "logs"),
      templateflow_home = tf_home
    ),
    compute_environment = list(
      fmriprep_container = container,
      scheduler = "slurm"
    ),
    fmriprep = list(output_spaces = "MNI152NLin2009cAsym"),
    debug = FALSE,
    log_level = "INFO"
  )
  class(scfg) <- "bg_project_cfg"

  steps <- c(mriqc = FALSE, fmriprep = TRUE, aroma = FALSE)

  submitted <- FALSE
  local_mocked_bindings(
    get_job_sched_args = function(...) "--time=00:30:00",
    get_job_script = function(...) "/path/to/script.sbatch",
    check_write_target = function(path, label) NULL,
    cluster_job_submit = function(...) {
      submitted <<- TRUE
      "77777"
    },
    .package = "BrainGnomes"
  )

  result <- submit_prefetch_templates(scfg, steps = steps)
  expect_true(submitted)
  expect_equal(result, "77777")
})

test_that("submit_prefetch_templates skips when cached state covers requested spaces", {
  root <- tempfile("pft_cached_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  container <- file.path(root, "fmriprep.sif")
  file.create(container)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home)
  state_file <- file.path(tf_home, ".braingnomes_prefetch_state.dcf")
  writeLines(c(
    "status: COMPLETED",
    paste0("templateflow_home: ", normalizePath(tf_home, winslash = "/", mustWork = FALSE)),
    "spaces: MNI152NLin2009cAsym MNI152NLin6Asym:res-2"
  ), state_file)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = file.path(root, "logs"),
      templateflow_home = tf_home
    ),
    compute_environment = list(
      fmriprep_container = container,
      scheduler = "slurm"
    ),
    fmriprep = list(output_spaces = "MNI152NLin2009cAsym"),
    debug = FALSE,
    log_level = "INFO"
  )
  class(scfg) <- "bg_project_cfg"

  steps <- c(mriqc = FALSE, fmriprep = TRUE, aroma = FALSE)

  submitted <- FALSE
  local_mocked_bindings(
    get_job_sched_args = function(...) "--time=00:30:00",
    get_job_script = function(...) "/path/to/script.sbatch",
    check_write_target = function(path, label) NULL,
    prefetch_manifest_verified = function(...) TRUE,
    cluster_job_submit = function(...) {
      submitted <<- TRUE
      "88888"
    },
    .package = "BrainGnomes"
  )

  result <- submit_prefetch_templates(scfg, steps = steps)
  expect_false(submitted)
  expect_null(result)
})

test_that("submit_prefetch_templates resubmits when state covers spaces but manifest verification fails", {
  root <- tempfile("pft_reprefetch_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  container <- file.path(root, "fmriprep.sif")
  file.create(container)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home)
  state_file <- file.path(tf_home, ".braingnomes_prefetch_state.dcf")
  writeLines(c(
    "status: COMPLETED",
    paste0("templateflow_home: ", normalizePath(tf_home, winslash = "/", mustWork = FALSE)),
    "spaces: MNI152NLin2009cAsym"
  ), state_file)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = file.path(root, "logs"),
      templateflow_home = tf_home,
      sqlite_db = file.path(root, "tracking.sqlite")
    ),
    compute_environment = list(
      fmriprep_container = container,
      scheduler = "slurm"
    ),
    fmriprep = list(output_spaces = "MNI152NLin2009cAsym"),
    debug = FALSE,
    log_level = "INFO"
  )
  class(scfg) <- "bg_project_cfg"

  steps <- c(mriqc = FALSE, fmriprep = TRUE, aroma = FALSE)

  submitted <- FALSE
  local_mocked_bindings(
    get_job_sched_args = function(...) "--time=00:30:00",
    get_job_script = function(...) "/path/to/script.sbatch",
    check_write_target = function(path, label) NULL,
    prefetch_manifest_verified = function(...) FALSE,
    cluster_job_submit = function(...) {
      submitted <<- TRUE
      "99990"
    },
    .package = "BrainGnomes"
  )

  result <- submit_prefetch_templates(scfg, steps = steps)
  expect_true(submitted)
  expect_equal(result, "99990")
})

test_that("submit_prefetch_templates resubmits when a new space is requested", {
  root <- tempfile("pft_newspace_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  container <- file.path(root, "fmriprep.sif")
  file.create(container)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home)
  state_file <- file.path(tf_home, ".braingnomes_prefetch_state.dcf")
  writeLines(c(
    "status: COMPLETED",
    paste0("templateflow_home: ", normalizePath(tf_home, winslash = "/", mustWork = FALSE)),
    "spaces: MNI152NLin2009cAsym"
  ), state_file)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      log_directory = file.path(root, "logs"),
      templateflow_home = tf_home
    ),
    compute_environment = list(
      fmriprep_container = container,
      scheduler = "slurm"
    ),
    fmriprep = list(output_spaces = "MNI152NLin2009cAsym MNI152NLin6Asym:res-2"),
    debug = FALSE,
    log_level = "INFO"
  )
  class(scfg) <- "bg_project_cfg"

  steps <- c(mriqc = FALSE, fmriprep = TRUE, aroma = FALSE)

  submitted <- FALSE
  captured_env <- NULL
  local_mocked_bindings(
    get_job_sched_args = function(...) "--time=00:30:00",
    get_job_script = function(...) "/path/to/script.sbatch",
    check_write_target = function(path, label) NULL,
    cluster_job_submit = function(...) {
      args <- list(...)
      submitted <<- TRUE
      captured_env <<- args$env_variables
      "99991"
    },
    .package = "BrainGnomes"
  )

  result <- submit_prefetch_templates(scfg, steps = steps)
  expect_true(submitted)
  expect_equal(result, "99991")
  expect_true("prefetch_state_file" %in% names(captured_env))
  expect_equal(
    normalizePath(captured_env[["prefetch_state_file"]], winslash = "/", mustWork = FALSE),
    normalizePath(state_file, winslash = "/", mustWork = FALSE)
  )
})

test_that("prefetch_manifest_verified returns TRUE when manifest matches templateflow contents", {
  root <- tempfile("pft_manifest_ok_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home)
  tf_file <- file.path(tf_home, "tpl-MNI152NLin2009cAsym", "template.nii.gz")
  dir.create(dirname(tf_file), recursive = TRUE, showWarnings = FALSE)
  writeLines("dummy", tf_file)

  sqlite_db <- file.path(root, "tracking.sqlite")
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job-1",
    tracking_args = list(job_name = "prefetch_templates", status = "QUEUED")
  )
  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job-1",
    status = "COMPLETED",
    output_manifest = capture_output_manifest(tf_home)
  )
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_true(prefetch_manifest_verified(sqlite_db, tf_home, job_id = "job-1"))
})

test_that("prefetch_manifest_verified returns FALSE when manifest files are missing", {
  root <- tempfile("pft_manifest_missing_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  tf_home <- file.path(root, "templateflow")
  dir.create(tf_home)
  tf_file <- file.path(tf_home, "tpl-MNI152NLin2009cAsym", "template.nii.gz")
  dir.create(dirname(tf_file), recursive = TRUE, showWarnings = FALSE)
  writeLines("dummy", tf_file)

  sqlite_db <- file.path(root, "tracking.sqlite")
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job-2",
    tracking_args = list(job_name = "prefetch_templates", status = "QUEUED")
  )
  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job-2",
    status = "COMPLETED",
    output_manifest = capture_output_manifest(tf_home)
  )
  unlink(tf_file, force = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_false(prefetch_manifest_verified(sqlite_db, tf_home, job_id = "job-2"))
})

# -- check_write_target_cached: path-only keying ------------------------------

test_that("check_write_target_cached uses path-only keys (label-agnostic)", {
  tmp <- tempfile("cwt_cache_key_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  n_real <- 0L
  local_mocked_bindings(
    check_write_target = function(path, label) {
      n_real <<- n_real + 1L
      NULL
    },
    .package = "BrainGnomes"
  )

  cache <- new.env(parent = emptyenv())

  # First call with label A
 check_write_target_cached(tmp, "label A", cache)
  expect_equal(n_real, 1L)

  # Second call with label B — same path, should hit cache
  check_write_target_cached(tmp, "label B", cache)
  expect_equal(n_real, 1L)  # no additional call
})

test_that("check_write_target_cached does not cache failures", {
  cache <- new.env(parent = emptyenv())

  n_real <- 0L
  local_mocked_bindings(
    check_write_target = function(path, label) {
      n_real <<- n_real + 1L
      glue::glue("{label} not writable")
    },
    .package = "BrainGnomes"
  )

  result1 <- check_write_target_cached("/fake/path", "first label", cache)
  expect_true(is.character(result1))
  expect_equal(n_real, 1L)

  # Same path again — should call check_write_target again (not cached)
  result2 <- check_write_target_cached("/fake/path", "second label", cache)
  expect_equal(n_real, 2L)
  expect_true(grepl("second label", result2))
})

# -- setup_project_directories primes the cache --------------------------------

test_that("setup_project_directories primes check_cache with writable dirs", {
  root <- tempfile("spd_prime_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  scratch_dir <- file.path(root, "scratch")
  log_dir <- file.path(root, "logs")

  scfg <- list(
    metadata = list(
      project_directory = root,
      bids_directory = file.path(root, "bids"),
      log_directory = log_dir,
      rois_directory = file.path(root, "rois"),
      postproc_directory = file.path(root, "postproc"),
      fmriprep_directory = file.path(root, "fmriprep"),
      mriqc_directory = file.path(root, "mriqc"),
      scratch_directory = scratch_dir,
      templateflow_home = file.path(root, "templateflow"),
      flywheel_temp_directory = NA,
      flywheel_sync_directory = NA
    ),
    flywheel_sync = list(flywheel_temp_directory = NULL)
  )
  class(scfg) <- "bg_project_cfg"

  cache <- new.env(parent = emptyenv())
  setup_project_directories(scfg, check_cache = cache)

  # All created dirs should be primed in the cache
  scratch_key <- normalizePath(scratch_dir, winslash = "/", mustWork = FALSE)
  log_key <- normalizePath(log_dir, winslash = "/", mustWork = FALSE)

  expect_true(exists(scratch_key, envir = cache, inherits = FALSE))
  expect_true(exists(log_key, envir = cache, inherits = FALSE))

  # Using check_write_target_cached on a primed path should skip the real check
  n_real <- 0L
  local_mocked_bindings(
    check_write_target = function(path, label) {
      n_real <<- n_real + 1L
      NULL
    },
    .package = "BrainGnomes"
  )

  result <- check_write_target_cached(scratch_dir, "scratch directory", cache)
  expect_null(result)
  expect_equal(n_real, 0L)  # cache hit, no real check needed
})

# -- setup_project_directories writability checks ------------------------------

test_that("setup_project_directories warns and remediates unwritable directories", {
  skip_on_os("windows")   # Sys.chmod() is a no-op on Windows
  tmp <- tempdir(TRUE)
  base <- file.path(tmp, paste0("spd_write_", Sys.getpid()))
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base, recursive = TRUE, force = TRUE), add = TRUE)

  proj_dir <- file.path(base, "proj")
  log_dir  <- file.path(base, "logs")
  dir.create(proj_dir, showWarnings = FALSE)
  dir.create(log_dir, showWarnings = FALSE)

  # Make log_dir read-only to trigger the writability remediation path

  Sys.chmod(log_dir, "0555")

  scfg <- list(
    metadata = list(
      project_directory = proj_dir,
      bids_directory = NA,
      log_directory = log_dir,
      rois_directory = NA,
      postproc_directory = NA,
      fmriprep_directory = NA,
      mriqc_directory = NA,
      scratch_directory = NA,
      templateflow_home = NA,
      flywheel_temp_directory = NA,
      flywheel_sync_directory = NA
    ),
    flywheel_sync = list(flywheel_temp_directory = NULL)
  )
  class(scfg) <- "bg_project_cfg"

  cache <- new.env(parent = emptyenv())

  # Should warn about unwritable directory and attempt remediation
  expect_warning(
    setup_project_directories(scfg, check_cache = cache),
    "not user-writable"
  )

  # proj_dir should be in cache (it was writable)
  proj_key <- normalizePath(proj_dir, winslash = "/", mustWork = FALSE)
  expect_true(exists(proj_key, envir = cache))
})

# -- process_subject log-directory guard ---------------------------------------

test_that("process_subject skips subject when log directory is not writable", {
  skip_on_os("windows")   # Sys.chmod() is a no-op on Windows
  tmp <- tempdir(TRUE)
  base <- file.path(tmp, paste0("logguard_", Sys.getpid()))
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(base, "logs")
  dir.create(log_dir, showWarnings = FALSE)
  # Make the log directory unwritable so that sub-X dir cannot be created
  Sys.chmod(log_dir, "0555")

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      postproc_directory = file.path(base, "postproc"),
      scratch_directory = file.path(base, "scratch"),
      templateflow_home = file.path(base, "templateflow"),
      project_directory = base
    )
  )
  class(scfg) <- "bg_project_cfg"

  sub_cfg <- data.frame(
    sub_id = "001",
    ses_id = NA_character_,
    dicom_sub_dir = NA_character_,
    dicom_ses_dir = NA_character_,
    bids_sub_dir = file.path(base, "bids", "sub-001"),
    bids_ses_dir = NA_character_,
    stringsAsFactors = FALSE
  )
  sub_cfg$steps <- list(list())

  cache <- new.env(parent = emptyenv())
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE,
             aroma = FALSE, postprocess = FALSE, extract_rois = FALSE)

  # process_subject should warn about unwritable log dir and return TRUE (skip)
  expect_warning(
    result <- process_subject(scfg, sub_cfg, steps = steps, permission_check_cache = cache),
    "not writable"
  )
  expect_true(result)
})

test_that("process_subject proceeds when log directory is writable", {
  tmp <- tempdir(TRUE)
  base <- file.path(tmp, paste0("logguard_ok_", Sys.getpid()))
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(base, "logs")
  dir.create(log_dir, showWarnings = FALSE)

  postproc_dir <- file.path(base, "postproc")
  dir.create(postproc_dir, showWarnings = FALSE)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      postproc_directory = postproc_dir,
      scratch_directory = file.path(base, "scratch"),
      templateflow_home = file.path(base, "templateflow"),
      project_directory = base
    )
  )
  class(scfg) <- "bg_project_cfg"

  sub_cfg <- data.frame(
    sub_id = "002",
    ses_id = NA_character_,
    dicom_sub_dir = NA_character_,
    dicom_ses_dir = NA_character_,
    bids_sub_dir = file.path(base, "bids", "sub-002"),
    bids_ses_dir = NA_character_,
    stringsAsFactors = FALSE
  )
  sub_cfg$steps <- list(list())

  cache <- new.env(parent = emptyenv())
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE,
             aroma = FALSE, postprocess = FALSE, extract_rois = FALSE)

  # Should NOT warn about log dir. process_subject will proceed into the
  # step loop, which is empty, so it should just return (possibly with a
  # different warning/message but NOT a log-directory warning).
  result <- tryCatch(
    withCallingHandlers(
      process_subject(scfg, sub_cfg, steps = steps, permission_check_cache = cache),
      warning = function(w) {
        if (grepl("not writable", conditionMessage(w))) {
          stop("Unexpected log-dir warning: ", conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  # If we got here without the "not writable" warning, the guard passed.
  # The call may error later due to missing step configs, which is fine;
  # we just need to verify the log-dir guard didn't fire.
  expect_false(inherits(result, "error") && grepl("not writable", result$message))
})
