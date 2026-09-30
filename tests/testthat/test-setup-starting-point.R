test_that("starting points configure upstream stages without choosing analysis methods", {
  expected <- list(
    flywheel = c(flywheel_sync = TRUE, bids_conversion = TRUE),
    dicom = c(flywheel_sync = FALSE, bids_conversion = TRUE),
    bids = c(flywheel_sync = FALSE, bids_conversion = FALSE),
    fmriprep = c(flywheel_sync = FALSE, bids_conversion = FALSE, fmriprep = FALSE)
  )
  for (point in names(expected)) {
    cfg <- apply_project_starting_point(list(), point)
    expect_identical(cfg$metadata$starting_point, point)
    for (stage in names(expected[[point]])) {
      expect_identical(cfg[[stage]]$enable, unname(expected[[point]][[stage]]))
    }
    expect_null(cfg$postprocess$enable)
    expect_null(cfg$extract_rois$enable)
  }
  cfg <- apply_project_starting_point(list(), "fmriprep")
  expect_false(cfg$mriqc$enable)
  expect_false(cfg$bids_validation$enable)
  expect_null(cfg$aroma$enable)
})

test_that("headless skeletons reopen optional decisions but configured projects retain them", {
  cfg <- list()
  for (stage in c(supported_project_steps(), "bids_validation")) cfg[[stage]] <- list(enable = FALSE)
  expect_true(project_needs_starting_point(cfg))
  out <- apply_project_starting_point(cfg, "bids")
  expect_null(out$fmriprep$enable)
  expect_null(out$postprocess$enable)
  expect_false(project_needs_starting_point(out))
  cfg$postprocess <- list(enable = TRUE, clean = list(input_regex = "task:rest"))
  expect_false(project_needs_starting_point(cfg))
  out <- apply_project_starting_point(cfg, "fmriprep")
  expect_identical(out$postprocess, cfg$postprocess)
  expect_identical(out$mriqc, cfg$mriqc)
  expect_false(out$fmriprep$enable)
  expect_identical(apply_project_starting_point(cfg, "custom")$postprocess, cfg$postprocess)
})

test_that("starting-point menus support review, reselection, and cancellation", {
  answers <- list(1L, FALSE, 3L, TRUE)
  prompts <- character()
  local_mocked_bindings(prompt_input = function(prompt, ...) {
    prompts <<- c(prompts, prompt)
    answer <- answers[[1]]
    answers <<- answers[-1]
    answer
  }, .package = "BrainGnomes")
  output <- capture.output(point <- choose_project_starting_point(list()), type = "message")
  expect_identical(point, "bids")
  expect_length(prompts, 4L)
  expect_true(any(grepl("Flywheel sync -> local DICOMs", output, fixed = TRUE)))
  expect_match(gsub("[[:space:]]+", " ", paste(output, collapse = " ")),
    "Sync and DICOM conversion are skipped", fixed = TRUE)
  answers <- list(0L)
  expect_error(choose_project_starting_point(list()), "Setup cancelled")
})

test_that("existing input roots must exist and are never created by setup", {
  root <- tempfile("starting-input-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  missing <- file.path(root, "missing")
  answers <- c(missing, root)
  local_mocked_bindings(prompt_input = function(...) {
    answer <- answers[[1]]
    answers <<- answers[-1]
    answer
  }, .package = "BrainGnomes")
  expect_path_identical(prompt_setup_input_directory("Input root"), root)
  expect_false(dir.exists(missing))
  expect_length(answers, 0L)
})

test_that("Flywheel can start before DICOMs exist and keeps the expected subject root", {
  root <- tempfile("flywheel-starting-")
  cfg <- list(metadata = list(project_directory = root))
  prompts <- character()
  local_mocked_bindings(
    prompt_directory = function(prompt, default, ...) {
      prompts <<- c(prompts, prompt)
      if (grepl("subject folders", prompt)) file.path(default, "project") else default
    },
    prompt_setup_input_directory = function(...) stop("Flywheel data do not exist yet"),
    .package = "BrainGnomes"
  )
  out <- setup_starting_point_paths(cfg, "flywheel")
  expect_identical(out$metadata$flywheel_sync_directory, file.path(root, "data_dicoms"))
  expect_identical(out$metadata$dicom_directory, file.path(root, "data_dicoms", "project"))
  expect_identical(out$metadata$bids_directory, file.path(root, "data_bids"))
  expect_length(prompts, 3L)
  expect_false(dir.exists(root))
})

test_that("guided setup starts with data choice and does not repeat decided enable questions", {
  root <- tempfile("guided-start-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  cfg <- setup_project(project_name = "guided", project_directory = root, interactive = FALSE)
  # Turn the empty BIDS/derivative destinations into distinct external inputs.
  bids <- file.path(root, "existing-bids")
  derivatives <- file.path(root, "existing-fmriprep")
  dir.create(bids)
  dir.create(derivatives)
  prompts <- character()
  local_mocked_bindings(
    prompt_input = function(prompt = "", instruct = NULL, ...) {
      prompts <<- c(prompts, prompt)
      if (prompt == "Starting point (0 to cancel)") return(4L)
      if (prompt == "Continue with this starting point?") return(TRUE)
      if (prompt == "Where is the existing BIDS dataset?") return(bids)
      if (prompt == "Where are the existing fMRIPrep outputs?") return(derivatives)
      if (prompt %in% c("Run ICA-AROMA?", "Enable postprocessing?", "Perform ROI extraction?")) return(FALSE)
      stop("Unexpected question: ", prompt)
    },
    setup_compute_environment = function(scfg, fields = NULL) {
      if (!is.null(fields)) stop("Unexpected stage resource prompt: ", fields)
      scfg
    },
    save_project_config = function(scfg) write_project_config(scfg, overwrite = TRUE),
    cluster_job_submit = function(...) stop("Setup must not submit jobs"),
    .package = "BrainGnomes"
  )
  out <- setup_project(cfg)
  expect_identical(prompts[1], "Starting point (0 to cancel)")
  expect_false(out$fmriprep$enable)
  expect_false(out$bids_conversion$enable)
  expect_false(out$flywheel_sync$enable)
  expect_path_identical(out$metadata$bids_directory, bids)
  expect_path_identical(out$metadata$fmriprep_directory, derivatives)
  expect_identical(load_project(root, validate = FALSE)$metadata$starting_point, "fmriprep")
})

test_that("Flywheel-guided setup produces deferred conversion scope without local DICOMs", {
  root <- tempfile("flywheel-guided-")
  cfg <- setup_project(project_name = "flywheel", project_directory = root, interactive = FALSE)
  withr::defer(unlink(root, recursive = TRUE))
  resource <- file.path(root, "resource")
  file.create(resource)
  cfg$compute_environment$flywheel <- resource
  cfg$compute_environment$heudiconv_container <- resource
  cfg$metadata$flywheel_sync_directory <- file.path(root, "future-dicoms")
  cfg$metadata$flywheel_temp_directory <- file.path(root, "transfer-temp")
  cfg$flywheel_sync <- list(enable = FALSE, source_url = "fw://example/group/project/", save_audit_logs = TRUE)
  cfg$bids_conversion <- list(enable = FALSE, sub_regex = "^sub-.+", sub_id_match = "sub-(.*)",
    ses_regex = NA_character_, ses_id_match = NA_character_, heuristic_file = resource,
    overwrite = FALSE, clear_cache = FALSE)
  asked <- character()
  local_mocked_bindings(
    prompt_input = function(prompt, ...) {
      asked <<- c(asked, prompt)
      if (prompt == "Continue with this starting point?") return(TRUE)
      stop("Unexpected question: ", prompt)
    },
    prompt_directory = function(default, ...) default,
    setup_job = function(scfg, job_name, defaults, ...) {
      scfg[[job_name]] <- utils::modifyList(defaults, scfg[[job_name]])
      scfg
    },
    save_project_config = function(scfg) write_project_config(scfg, overwrite = TRUE),
    cluster_job_submit = function(...) stop("Setup must not submit jobs"),
    .package = "BrainGnomes"
  )
  out <- setup_project(cfg, starting_point = "flywheel")
  expect_identical(asked, "Continue with this starting point?")
  expect_true(out$flywheel_sync$enable)
  expect_true(out$bids_conversion$enable)
  expect_false(dir.exists(out$metadata$dicom_directory))
  expect_identical(out$metadata$dicom_directory, out$metadata$flywheel_sync_directory)
  execution <- resolve_project_execution(out, "all")
  expect_true(execution$scope_deferred)
  expect_equal(nrow(execution$subjects), 0L)
})

test_that("inspection from setup never edits configuration or asks for resources", {
  root <- tempfile("inspect-start-")
  cfg <- setup_project(project_name = "inspect", project_directory = root, interactive = FALSE)
  withr::defer(unlink(root, recursive = TRUE))
  file <- attr(cfg, "yaml_file")
  before <- readLines(file)
  for (input in list(cfg, root, file)) {
    out <- setup_project(input, starting_point = "existing")
    expect_identical(out$metadata, cfg$metadata)
    expect_identical(readLines(file), before)
  }
  local_mocked_bindings(
    prompt_input = function(prompt, ...) {
      if (prompt == "Starting point (0 to cancel)") return(5L)
      if (prompt == "Existing project directory or configuration YAML") return(root)
      stop("Unexpected question: ", prompt)
    },
    setup_project_metadata = function(...) stop("Inspection entered setup"),
    save_project_config = function(...) stop("Inspection attempted to save"),
    .package = "BrainGnomes"
  )
  expect_identical(setup_project()$metadata, cfg$metadata)
  expect_false(file.exists(cfg$metadata$sqlite_db))
})

test_that("starting-point choices never interfere with headless or targeted setup", {
  expect_error(setup_project(interactive = FALSE, starting_point = "bids"), "full guided setup")
  expect_error(setup_project(fields = "metadata/project_name", starting_point = "bids"), "full guided setup")
  expect_error(setup_project(starting_point = "unknown"), "starting_point")
  cfg <- structure(list(metadata = list(starting_point = "bids")), class = "bg_project_cfg")
  local_mocked_bindings(
    choose_project_starting_point = function(...) stop("Should not reopen the starting-point menu"),
    setup_project_metadata = function(scfg, ...) scfg,
    setup_flywheel_sync = function(scfg, ...) scfg,
    setup_bids_conversion = function(scfg, ...) scfg,
    setup_fmriprep = function(scfg, ...) scfg,
    setup_mriqc = function(scfg, ...) scfg,
    setup_aroma = function(scfg, ...) scfg,
    setup_postprocess_streams = function(scfg, ...) scfg,
    setup_extract_streams = function(scfg, ...) scfg,
    setup_bids_validation = function(scfg, ...) scfg,
    setup_compute_environment = function(scfg, ...) scfg,
    save_project_config = function(scfg) scfg,
    .package = "BrainGnomes"
  )
  expect_identical(setup_project(cfg)$metadata, cfg$metadata)
  cfg$metadata$starting_point <- NULL
  expect_identical(setup_project(cfg, fields = "metadata/project_name")$metadata, cfg$metadata)
})
