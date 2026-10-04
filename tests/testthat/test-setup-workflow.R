# These integration tests keep setup, saving/loading, validation, discovery, and
# previews real. Only user answers and external execution boundaries are mocked.
# Resource files are placeholders: no imaging software or scheduler is invoked.

# Create isolated input trees and software placeholders for one setup route.
# Returns paths; the caller owns cleanup of the temporary root.
make_setup_workflow_inputs <- function(point) {
  root <- tempfile("setup-flow-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  paths <- list(root = root, project = file.path(root, "project with spaces"),
    dicom = file.path(root, "inputs", "dicoms"), bids = file.path(root, "inputs", "bids"),
    derivatives = file.path(root, "inputs", "fmriprep"),
    templates = file.path(root, "templates"), scratch = file.path(root, "scratch"),
    container = file.path(root, "container.sif"), license = file.path(root, "license.txt"),
    heuristic = file.path(root, "heuristic.py"), atlas = file.path(root, "atlas.nii.gz"))
  for (path in paths[c("project", "templates", "scratch")]) dir.create(path)
  for (path in paths[c("container", "license", "heuristic", "atlas")]) {
    writeLines("Test placeholder; never execute or process this file.", path)
  }
  # DICOM names intentionally differ from BIDS names to exercise ID extraction.
  for (subject in c("01", "02")) for (session in c("1", "2")) {
    if (point == "dicom") {
      path <- file.path(paths$dicom, paste0("participant-P", subject), paste0("visit-V", session))
      dir.create(path, recursive = TRUE)
      writeLines("Synthetic DICOM placeholder", file.path(path, "image.dcm"))
    }
    if (!point %in% c("flywheel", "dicom")) {
      path <- file.path(paths$bids, paste0("sub-", subject), paste0("ses-", session))
      dir.create(path, recursive = TRUE)
      if (point == "fmriprep") {
        dir.create(file.path(paths$derivatives, paste0("sub-", subject), paste0("ses-", session)),
          recursive = TRUE)
      }
    }
  }
  paths
}

# Script user decisions while recording questions. Unknown required questions
# fail immediately, and bounded menu queues prevent accidental interactive loops.
# Returns an environment with input functions and a prompt transcript.
setup_workflow_answers <- function(paths, point) {
  answers <- new.env(parent = emptyenv())
  answers$prompts <- character()
  answers$menus <- list(
    "Modify output spaces:" = c(1L, 3L), "Add space type:" = 1L,
    "Modify postprocessing streams:" = c(1L, 5L),
    "Modify extraction streams:" = c(1L, 5L)
  )
  values <- list(
    "Starting point (0 to cancel)" = match(point, c("flywheel", "dicom", "bids", "fmriprep", "existing", "custom")),
    "Continue with this starting point?" = TRUE,
    "Proceed with this directory?" = TRUE,
    "Overwrite existing project_config.yaml?" = TRUE,
    "Templateflow directory:" = paths$templates,
    "Where is your work directory?" = paths$scratch,
    "Scheduler (slurm/torque):" = "slurm",
    "Where should Flywheel download the DICOMs?" = paths$dicom,
    "Which directory will contain the DICOM subject folders after sync?" = file.path(paths$dicom, "downloaded project"),
    "Where are the existing DICOM subject folders?" = paths$dicom,
    "Where is the existing BIDS dataset?" = paths$bids,
    "Where is your BIDS directory?" = paths$bids,
    "Where are the existing fMRIPrep outputs?" = paths$derivatives,
    "Run Flywheel sync?" = FALSE, "Run BIDS conversion?" = FALSE,
    "Do you want to include fMRIPrep as part of your preprocessing pipeline?" = point != "fmriprep",
    "Run MRIQC?" = point == "custom", "Run ICA-AROMA?" = FALSE,
    "Enable postprocessing?" = point == "fmriprep", "Perform ROI extraction?" = point == "fmriprep",
    "Location of Flywheel CLI:" = paths$container,
    "Location of fmriprep container:" = paths$container,
    "Location of heudiconv container:" = paths$container,
    "Location of mriqc container:" = paths$container,
    "Location of FSL container:" = paths$container,
    "Flywheel project URL:" = "fw://example/group/project",
    "What is the regex pattern for the subject IDs?" = "^participant-P[0-9]+$",
    "What is the regex pattern for extracting the subject ID from the folder name?" = "participant-P([0-9]+)",
    "What is the regex pattern for the session IDs?" = "^visit-V[0-9]+$",
    "What is the regex pattern for extracting the session ID from the folder name?" = "visit-V([0-9]+)",
    "Heudiconv heuristic file:" = paths$heuristic,
    "What is the location of your FreeSurfer license file?" = paths$license,
    "Name for this postprocess configuration" = "clean",
    "Enter the BIDS description ('desc') for the fully postprocessed file" = "clean",
    "Repetition time (in seconds) of the scan sequence" = 2,
    "Apply spatial smoothing?" = TRUE, "Spatial smoothing FWHM (mm)" = 6,
    "Name for this ROI extraction configuration" = "roi",
    "Atlas NIfTI file(s) (separate using commas for multiple):" = paths$atlas
  )
  # Supply no-denoising choices explicitly rather than implicitly accepting all
  # flags. Tests must notice a newly introduced scientific/setup decision.
  for (prompt in c("Enable BIDS validation?", "Apply brain mask?", "Apply AROMA denoising?",
    "Do you want to apply temporal filtering to each fMRI run?", "Apply intensity normalization?",
    "Generate confound file?", "Enable scrubbing?", "Apply confound regression?",
    "Filter motion parameters before computing FD?", "Do you want to specify the postprocessing sequence?",
    "Perform (Fisher) r-to-z transformation on correlations?")) values[[prompt]] <- FALSE
  answers$prompt <- function(prompt = NULL, instruct = NULL, default = NULL, required = TRUE, ...) {
    key <- trimws(as.character(if (is.null(prompt)) instruct else prompt))
    answers$prompts <- c(answers$prompts, key)
    if (length(answers$prompts) > 200L) stop("Unexpected prompt loop")
    if (key %in% names(values)) return(values[[key]])
    if (!is.null(default)) return(default)
    if (!required) return(NA_character_)
    stop("Unscripted setup question: ", key)
  }
  answers$menu <- function(choices, title, ...) {
    queue <- answers$menus[[title]]
    if (!length(queue)) stop("Unscripted setup menu: ", title)
    answers$menus[[title]] <- queue[-1L]
    queue[1L]
  }
  answers$select <- function(choices, title, ...) {
    result <- switch(title, "Choose a template" = "MNI152NLin2009cAsym",
      "Select postprocess stream(s) to use" = "clean", "ROI reduction method" = "mean",
      "Correlation method(s)" = "none", stop("Unscripted selection: ", title))
    stopifnot(result %in% choices)
    result
  }
  answers
}

# Snapshot all paths and file contents in a small synthetic fixture so previews
# cannot silently create directories or mutate configuration, inputs, or logs.
setup_workflow_snapshot <- function(root) {
  paths <- sort(list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = TRUE))
  files <- paths[!dir.exists(file.path(root, paths))]
  list(paths = paths, hashes = unname(tools::md5sum(file.path(root, files))))
}

# Supply portable ownership-query results for prompt_directory(). Any other
# shell command is unexpected: setup and previews must not execute software.
setup_workflow_system2 <- function(command, ...) {
  if (command %in% c("stat", "id")) return("setup-test-user")
  stop("Setup/preview attempted an external command: ", command)
}

# Exercise a complete starting-point route and assert its saved configuration,
# concrete scope, selected streams, and read-only preview behavior.
check_setup_workflow <- function(point, headless = FALSE) {
  paths <- make_setup_workflow_inputs(point)
  on.exit(unlink(paths$root, recursive = TRUE), add = TRUE)
  answers <- setup_workflow_answers(paths, point)
  local_mocked_bindings(system2 = setup_workflow_system2, .package = "base")
  local_mocked_bindings(
    prompt_input = answers$prompt, menu_safe = answers$menu, select_list_safe = answers$select,
    # Empty terminal lines finish optional CLI arguments/resolution entry.
    getline = function(...) "", discover_cli_path = function(...) "",
    cluster_job_submit = function(...) stop("Setup/preview attempted scheduler submission"),
    record_run_provenance = function(...) stop("Preview attempted provenance writes"),
    setup_project_directories = function(...) stop("Preview attempted directory creation"),
    .package = "BrainGnomes"
  )
  input <- if (headless) setup_project(project_name = point, project_directory = paths$project,
    interactive = FALSE) else NULL
  cfg <- setup_project(input, project_name = point, project_directory = paths$project)
  file <- file.path(paths$project, "project_config.yaml")
  expect_true(file.exists(file))
  expect_identical(answers$prompts[1L], "Starting point (0 to cancel)")
  expect_identical(cfg$metadata$starting_point, point)
  expect_false(file.exists(cfg$metadata$sqlite_db))
  reloaded <- load_project(file, validate = FALSE)
  # YAML empty sequences reload as list(), rather than character(0), for optional
  # CLI arguments. Compare their serialized settings, not that R-only distinction.
  expect_identical(yaml::as.yaml(as.list(reloaded)), yaml::as.yaml(as.list(cfg)))
  stages <- switch(point, flywheel = c("flywheel_sync", "bids_conversion", "fmriprep"),
    dicom = c("bids_conversion", "fmriprep"), bids = "fmriprep",
    fmriprep = c("postprocess", "extract_rois"), custom = c("mriqc", "fmriprep"))
  expect_setequal(supported_project_steps()[vapply(supported_project_steps(),
    function(stage) isTRUE(reloaded[[stage]]$enable), logical(1))], stages)
  if (point %in% c("flywheel", "dicom", "bids", "fmriprep")) {
    expect_false("Run Flywheel sync?" %in% answers$prompts)
    expect_false("Run BIDS conversion?" %in% answers$prompts)
  } else {
    expect_true(all(c("Run Flywheel sync?", "Run BIDS conversion?", "Run MRIQC?") %in% answers$prompts))
  }
  before <- setup_workflow_snapshot(paths$root)
  # All three public input forms must yield the same validated plan. Validation
  # is selected-work validation because configured future outputs need not exist.
  plans <- lapply(list(reloaded, file, paths$project), function(input) {
    report <- validate_project_config(input, steps = "all", quiet = TRUE)
    expect_true(report$valid, info = paste(report$issues$message, collapse = "\n"))
    plan_project(input, steps = "all", quiet = TRUE)
  })
  preview <- run_project(paths$project, steps = "all", dry_run = TRUE)
  for (plan in plans) {
    expect_setequal(plan$request$steps, stages)
    expect_identical(plan$preview, preview$preview)
    expect_identical(plan$validation, preview$validation)
  }
  work <- preview$preview$work
  if (point == "flywheel") {
    expect_true(preview$scope_deferred)
    expect_equal(nrow(preview$subjects), 0L)
    expect_false(dir.exists(reloaded$metadata$dicom_directory))
    expect_path_identical(reloaded$metadata$dicom_directory, file.path(paths$dicom, "downloaded project"), mustWork = FALSE)
    deferred <- work[work$stage %in% c("bids_conversion", "fmriprep"), ]
    expect_true(all(deferred$action == "deferred" & is.na(deferred$sub_id)))
  } else {
    expect_false(preview$scope_deferred)
    expect_setequal(paste(preview$subjects$sub_id, preview$subjects$ses_id),
      c("01 1", "01 2", "02 1", "02 2"))
    if (point == "dicom") {
      conversions <- work[work$stage == "bids_conversion", ]
      expect_equal(nrow(conversions), 4L)
      expect_path_identical(conversions$input_directory,
        file.path(paths$dicom, paste0("participant-P", conversions$sub_id), paste0("visit-V", conversions$ses_id)))
    } else {
      expect_path_identical(reloaded$metadata$bids_directory, paths$bids)
    }
    if (point == "fmriprep") {
      expect_false("Do you want to include fMRIPrep as part of your preprocessing pipeline?" %in% answers$prompts)
      expect_path_identical(reloaded$metadata$fmriprep_directory, paths$derivatives)
      expect_identical(preview$request$postprocess_streams, "clean")
      expect_identical(preview$request$extract_streams, "roi")
      expect_identical(reloaded$extract_rois$roi$input_streams, "clean")
      expect_equal(nrow(work[work$stage == "postprocess" & work$stream == "clean", ]), 4L)
      expect_equal(nrow(work[work$stage == "extract_rois" & work$stream == "roi", ]), 4L)
    }
  }
  expect_identical(setup_workflow_snapshot(paths$root), before)
  expect_false(file.exists(reloaded$metadata$sqlite_db))
  invisible(reloaded)
}

for (point in c("flywheel", "dicom", "bids", "fmriprep", "custom")) {
  test_that(paste(point, "setup saves, reloads, validates, and previews selected work"), {
    check_setup_workflow(point)
  })
}

test_that("a headless skeleton continues through BIDS setup to a valid preview", {
  check_setup_workflow("bids", headless = TRUE)
})

test_that("existing-project entry inspects, reloads, validates, and previews without saving", {
  paths <- make_setup_workflow_inputs("bids")
  withr::defer(unlink(paths$root, recursive = TRUE))
  answers <- setup_workflow_answers(paths, "bids")
  local_mocked_bindings(system2 = setup_workflow_system2, .package = "base")
  local_mocked_bindings(prompt_input = answers$prompt, menu_safe = answers$menu,
    select_list_safe = answers$select, getline = function(...) "",
    .package = "BrainGnomes")
  cfg <- setup_project(project_name = "existing", project_directory = paths$project)
  file <- attr(cfg, "yaml_file")
  before <- setup_workflow_snapshot(paths$root)
  # Inspection must bypass every setup/save boundary, even with enabled stages.
  local_mocked_bindings(
    prompt_input = function(prompt, ...) {
      switch(prompt, "Starting point (0 to cancel)" = 5L,
        "Existing project directory or configuration YAML" = paths$project,
        stop("Inspection asked an unexpected setup question: ", prompt))
    },
    save_project_config = function(...) stop("Inspection attempted to save"),
    setup_project_metadata = function(...) stop("Inspection entered metadata setup"),
    setup_project_directories = function(...) stop("Preview attempted directory creation"),
    record_run_provenance = function(...) stop("Preview attempted provenance writes"),
    cluster_job_submit = function(...) stop("Inspection/preview attempted submission"),
    .package = "BrainGnomes"
  )
  for (input in list(cfg, file, paths$project, NULL)) {
    inspected <- if (is.null(input)) setup_project() else setup_project(input, starting_point = "existing")
    expect_identical(inspected$metadata, cfg$metadata)
    reloaded <- load_project(attr(inspected, "yaml_file"), validate = FALSE)
    expect_true(validate_project_config(reloaded, steps = "all", quiet = TRUE)$valid)
    plan <- plan_project(reloaded, steps = "all", quiet = TRUE)
    preview <- run_project(reloaded, steps = "all", dry_run = TRUE)
    expect_identical(preview$preview, plan$preview)
    expect_equal(nrow(preview$subjects), 4L)
  }
  expect_identical(setup_workflow_snapshot(paths$root), before)
})
