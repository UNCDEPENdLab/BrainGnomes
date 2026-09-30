#' Describe the supported entry points to guided project setup
#' @return Named list of labels, explanations, and upstream stage selections.
#' @noRd
project_starting_points <- function() {
  list(
    flywheel = list(
      label = "My DICOMs are on Flywheel; download them first",
      route = "Flywheel sync -> local DICOMs -> BIDS conversion -> optional preprocessing and analysis.",
      stages = c(flywheel_sync = TRUE, bids_conversion = TRUE)
    ),
    dicom = list(
      label = "I already have DICOMs on this filesystem",
      route = "Local DICOMs -> BIDS conversion -> optional preprocessing and analysis. Flywheel sync is skipped.",
      stages = c(flywheel_sync = FALSE, bids_conversion = TRUE)
    ),
    bids = list(
      label = "I already have a BIDS dataset",
      route = "Existing BIDS -> optional fMRIPrep, quality checks, and analysis. Sync and DICOM conversion are skipped.",
      stages = c(flywheel_sync = FALSE, bids_conversion = FALSE)
    ),
    fmriprep = list(
      label = "I already have fMRIPrep outputs and their BIDS dataset",
      route = paste(
        "Existing fMRIPrep outputs -> optional ICA-AROMA, postprocessing, and ROI extraction.",
        "Sync, DICOM conversion, and fMRIPrep are skipped. The corresponding BIDS directory is still needed for subject/session discovery.",
        "For a new configuration, raw-data QC is left off; enable MRIQC or BIDS validation later with edit_project() if needed."
      ),
      stages = c(flywheel_sync = FALSE, bids_conversion = FALSE, fmriprep = FALSE)
    ),
    existing = list(
      label = "Inspect an existing BrainGnomes project without changing it",
      route = "Open project status without setup questions, configuration writes, or job submission."
    ),
    custom = list(
      label = "Custom setup: let me choose each stage",
      route = "Review stages individually, using any existing settings as defaults."
    )
  )
}

#' Identify a new configuration or an untouched headless setup skeleton
#' @param scfg Project configuration list.
#' @return TRUE when no starting point or substantive stage settings exist.
#' @noRd
project_needs_starting_point <- function(scfg) {
  if (!is.null(scfg$metadata$starting_point)) return(FALSE)
  !any(vapply(c(supported_project_steps(), "bids_validation"), function(stage) {
    isTRUE(scfg[[stage]]$enable) || length(setdiff(names(scfg[[stage]]), "enable")) > 0L
  }, logical(1)))
}

#' Choose and review a starting point before asking for paths or resources
#' @param scfg Existing settings, used to make replacement confirmation cautious.
#' @param starting_point Optional explicit choice, bypassing the numbered menu.
#' @return A supported starting-point name; cancellation stops without saving.
#' @noRd
choose_project_starting_point <- function(scfg, starting_point = NULL) {
  points <- project_starting_points()
  cli_setup_section("Where are you starting?")
  cli_instruction("Choose the data you want this project to start from. You will choose optional downstream steps next; this does not select a denoising recipe or start processing.", before = FALSE)
  repeat {
    if (is.null(starting_point)) {
      cli_numbered_values("What do you already have?", vapply(points, `[[`, character(1), "label"))
      choice <- prompt_input(prompt = "Starting point (0 to cancel)", type = "integer",
        lower = 0L, upper = length(points), len = 1L)
      if (choice == 0L) stop("Setup cancelled before changing or saving a configuration.", call. = FALSE)
      starting_point <- names(points)[choice]
    }
    cli_instruction(points[[starting_point]]$route, before = FALSE)
    if (starting_point == "existing") return(starting_point)
    if (!project_needs_starting_point(scfg)) {
      cli::cli_alert_info("This choice may change upstream stage selections. Existing stage settings and files are retained; optional downstream selections are kept.")
    }
    if (isTRUE(prompt_input(prompt = "Continue with this starting point?", type = "flag",
        default = project_needs_starting_point(scfg)))) return(starting_point)
    starting_point <- NULL
  }
}

#' Apply only the stage selections implied by a starting point
#' @param scfg Project configuration list.
#' @param starting_point Supported starting-point name other than existing.
#' @return Configuration with explicit upstream selections and optional choices retained.
#' @noRd
apply_project_starting_point <- function(scfg, starting_point) {
  fresh <- project_needs_starting_point(scfg)
  if (fresh) {
    # Headless initialization disables every stage. Reopen undecided choices in
    # the guided workflow without discarding any stage-specific settings.
    for (stage in c(supported_project_steps(), "bids_validation")) scfg[[stage]]$enable <- NULL
  }
  selections <- project_starting_points()[[starting_point]]$stages
  for (stage in names(selections)) scfg[[stage]]$enable <- unname(selections[[stage]])
  if (fresh && starting_point == "fmriprep") {
    scfg$mriqc$enable <- FALSE
    scfg$bids_validation$enable <- FALSE
  }
  scfg$metadata$starting_point <- starting_point
  scfg
}

#' Ask for an existing readable input directory without creating one
#' @param prompt Short description of the input root being requested.
#' @param default Optional existing configured path.
#' @param instruct Optional guidance describing the expected contents.
#' @return Normalized absolute path to a readable existing directory.
#' @noRd
prompt_setup_input_directory <- function(prompt, default = NULL, instruct = NULL) {
  repeat {
    path <- path.expand(prompt_input(prompt = prompt, default = default,
      instruct = instruct, type = "character", len = 1L))
    if (checkmate::test_directory_exists(path, access = "r")) {
      return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }
    cli::cli_alert_warning("That input directory does not exist or is not readable. Choose the existing data root; no directory has been created.")
  }
}

#' Collect data roots appropriate to the selected starting point
#' @param scfg Configuration with project metadata and stage selections.
#' @param starting_point Supported starting-point name.
#' @return Configuration with input roots and required conversion destinations.
#' @noRd
setup_starting_point_paths <- function(scfg, starting_point) {
  if (starting_point == "custom") return(scfg)
  cli_setup_section("Data locations")
  if (starting_point == "flywheel") {
    scfg$metadata$flywheel_sync_directory <- prompt_directory(
      prompt = "Where should Flywheel download the DICOMs?",
      instruct = "This is a destination, not an existing input. It can be empty or not exist yet; sync runs before subject discovery.",
      default = value_or_default(scfg$metadata$flywheel_sync_directory,
        file.path(scfg$metadata$project_directory, "data_dicoms")), check_writable = TRUE
    )
    scfg$metadata$dicom_directory <- prompt_directory(
      prompt = "Which directory will contain the DICOM subject folders after sync?",
      instruct = "Usually this is the download destination. If Flywheel creates additional enclosing folders, choose the expected subject root inside it. The data need not be present yet.",
      default = scfg$metadata$flywheel_sync_directory
    )
  } else if (starting_point == "dicom") {
    scfg$metadata$dicom_directory <- prompt_setup_input_directory(
      "Where are the existing DICOM subject folders?", scfg$metadata$dicom_directory,
      "Choose the parent of the subject folders, not one subject's scan folder. Subject/session naming will be configured during BIDS conversion setup."
    )
  }
  if (starting_point %in% c("flywheel", "dicom")) {
    scfg$metadata$bids_directory <- prompt_directory(
      prompt = "Where should converted BIDS data be written?",
      default = value_or_default(scfg$metadata$bids_directory,
        file.path(scfg$metadata$project_directory, "data_bids")), check_writable = TRUE
    )
  } else {
    scfg$metadata$bids_directory <- prompt_setup_input_directory(
      "Where is the existing BIDS dataset?", scfg$metadata$bids_directory,
      "Choose the dataset root containing the sub-* folders. BrainGnomes uses this directory to discover subjects and sessions, including when reusing fMRIPrep outputs."
    )
  }
  if (starting_point == "fmriprep") {
    scfg$metadata$fmriprep_directory <- prompt_setup_input_directory(
      "Where are the existing fMRIPrep outputs?", scfg$metadata$fmriprep_directory,
      "Choose the derivative root containing sub-* folders, not a subject folder. These inputs stay in place; BrainGnomes will not rerun fMRIPrep."
    )
  }
  scfg
}

#' Open project inspection from setup without running configuration helpers
#' @param input Optional project object, directory, or configuration YAML.
#' @return The loaded configuration, invisibly, after displaying project status.
#' @noRd
inspect_setup_project <- function(input = NULL) {
  if (is.null(input)) {
    repeat {
      path <- path.expand(prompt_input(prompt = "Existing project directory or configuration YAML",
        default = getwd(), type = "character", len = 1L))
      scfg <- tryCatch(load_project(path, validate = FALSE), error = function(e) {
        cli::cli_alert_warning("Could not open that project: {conditionMessage(e)}")
        NULL
      })
      if (!is.null(scfg)) break
    }
  } else {
    scfg <- load_project(input, validate = FALSE)
  }
  print(inspect_project(scfg))
  cli_instruction("Nothing was configured or submitted. Use edit_project(scfg) to change settings, inspect_project(scfg) for status, or diagnose_project(scfg) to investigate a failure.")
  invisible(scfg)
}

#' Summarize configured stages and the next safe workflow actions
#' @param scfg Completed project configuration.
#' @return NULL, invisibly, after printing the review summary.
#' @noRd
review_setup_workflow <- function(scfg) {
  cli_setup_section("Review your workflow")
  stages <- supported_project_steps()
  enabled <- stages[vapply(stages, function(stage) isTRUE(scfg[[stage]]$enable), logical(1))]
  cli::cli_text("Enabled stages: {if (length(enabled)) paste(enabled, collapse = ', ') else 'none'}.")
  if (isTRUE(scfg$bids_validation$enable)) {
    cli::cli_text("BIDS validation is configured separately; submit it with run_bids_validation(scfg).")
  }
  if (length(enabled)) {
    cli_instruction("No jobs have been submitted. After saving, review choices with edit_project(scfg), preview with run_project(scfg, dry_run = TRUE), and submit only when ready with run_project(scfg).")
  } else {
    cli_instruction("No workflow stages are enabled and no jobs have been submitted. After saving, use edit_project(scfg) to enable the work you want before calling run_project().")
  }
  invisible(NULL)
}
