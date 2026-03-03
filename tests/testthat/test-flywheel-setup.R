test_that("normalize_flag_value coerces common YAML boolean strings", {
  expect_true(isTRUE(normalize_flag_value("yes")))
  expect_true(isTRUE(normalize_flag_value("TRUE")))
  expect_false(isTRUE(normalize_flag_value("no")))
  expect_identical(normalize_flag_value("maybe"), "maybe")
})

test_that("validate_project accepts quoted Flywheel boolean settings", {
  root <- tempfile("validate_fw_flags_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- c("project", "logs", "scratch", "templateflow", "fw_sync", "fw_tmp")
  dir_paths <- file.path(root, dirs)
  vapply(dir_paths, dir.create, recursive = TRUE, showWarnings = FALSE, FUN.VALUE = logical(1))

  fw_path <- file.path(root, "fw")
  writeLines(c("#!/bin/sh", "exit 0"), fw_path)
  Sys.chmod(fw_path, mode = "0755")

  scfg <- structure(list(
    metadata = list(
      project_name = "quoted_fw_flags",
      project_directory = file.path(root, "project"),
      log_directory = file.path(root, "logs"),
      scratch_directory = file.path(root, "scratch"),
      templateflow_home = file.path(root, "templateflow"),
      flywheel_sync_directory = file.path(root, "fw_sync"),
      flywheel_temp_directory = file.path(root, "fw_tmp")
    ),
    flywheel_sync = list(
      enable = "yes",
      memgb = 16,
      nhours = 4,
      ncores = 1,
      cli_options = "",
      sched_args = "",
      source_url = "fw://server/group/project",
      save_audit_logs = "no"
    ),
    bids_conversion = list(enable = FALSE),
    bids_validation = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE),
    extract_rois = list(enable = FALSE),
    compute_environment = list(
      scheduler = "slurm",
      flywheel = fw_path
    )
  ), class = "bg_project_cfg")

  expect_true(isTRUE(validate_project(scfg, correct_problems = FALSE)))
})

test_that("setup_compute_environment prompts for Flywheel path when CLI is absent", {
  root <- tempfile("setup_fw_cli_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  fw_path <- file.path(root, "fw")
  writeLines(c("#!/bin/sh", "exit 0"), fw_path)
  Sys.chmod(fw_path, mode = "0755")

  scfg <- structure(list(
    compute_environment = list(flywheel = NULL),
    flywheel_sync = list(enable = TRUE),
    bids_conversion = list(enable = FALSE),
    bids_validation = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  ), class = "bg_project_cfg")

  prompts <- character()

  local_mocked_bindings(
    discover_cli_path = function(command) "",
    prompt_input = function(instruct = NULL, ...) {
      prompts <<- c(prompts, instruct)
      fw_path
    },
    .package = "BrainGnomes"
  )

  result <- setup_compute_environment(scfg, fields = "compute_environment/flywheel")

  expect_identical(
    normalizePath(result$compute_environment$flywheel, winslash = "/", mustWork = TRUE),
    normalizePath(fw_path, winslash = "/", mustWork = TRUE)
  )
  expect_true(any(grepl("not found on your PATH", prompts, fixed = TRUE)))
})
