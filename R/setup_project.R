#' Load a project configuration from a file
#' @param input A `bg_project_cfg` object, a path to a YAML file, or a project
#'   directory containing \code{project_config.yaml}. Defaults to the current
#'   working directory.
#' @param validate Logical indicating whether to validate the configuration after loading. Validation is
#'   non-interactive and never changes or saves the configuration. The structured validation result is
#'   attached as the `validation` attribute. Default: TRUE.
#' @details Project YAML files must be stored in their project root. A relative
#'   `metadata/project_directory` is resolved from the YAML directory; all other
#'   recognized relative filesystem paths are resolved from that project root.
#'   Returned runtime paths are absolute and independent of the working directory.
#' @return A list representing the project configuration (class `"bg_project_cfg"`). If `validate` is TRUE,
#'   the returned object has a `validation` attribute produced by [validate_project_config()].
#' @importFrom yaml read_yaml
#' @export
load_project <- function(input = getwd(), validate = TRUE) {
  checkmate::assert_flag(validate)
  if (is.null(input)) input <- getwd()
  if (inherits(input, "bg_project_cfg")) {
    scfg <- resolve_project_paths(input)
    assert_project_config_colocation(scfg)
    if (validate) {
      validation <- validate_project_config(scfg, quiet = TRUE)
      attr(scfg, "validation") <- validation[c("valid", "issues", "messages")]
    }
    return(scfg)
  }
  # Keep diagnostics consistent with runtime paths, including Windows short
  # names and macOS/symlink aliases when the directory has no configuration.
  input <- normalize_project_path(input)
  if (checkmate::test_directory_exists(input) && checkmate::test_file_exists(file.path(input, "project_config.yaml"))) {
    input <- file.path(input, "project_config.yaml") # if input is directory, look for project_config.yaml in it.
  }
  if (!checkmate::test_file_exists(input)) stop("Cannot find file: ", input)
  yaml_path <- normalizePath(input, winslash = "/", mustWork = TRUE)
  scfg <- read_yaml(yaml_path)
  scfg <- resolve_project_paths(scfg, yaml_file = yaml_path)
  assert_project_config_colocation(scfg, yaml_path)
  if (validate) {
    validation <- validate_project_config(scfg, quiet = TRUE)
    attr(scfg, "validation") <- validation[c("valid", "issues", "messages")]
  }

  return(scfg)
}

#' summary method for project configuration object
#' @param object The project configuration object (`bg_project_cfg`) to summarize.
#' @param ... additional parameters to summary (not used)
#' @return Invisibly returns `x` after printing its contents. This function is called 
#'   for its side effect of printing a formatted summary of the project configuration.
#' @export
summary.bg_project_cfg <- function(object, ...) {
  pretty_print_list(object, indent=2)
}

get_scfg_from_input <- function(input = NULL) {
  if (is.null(input)) {
    scfg <- list()
  } else if (inherits(input, "bg_project_cfg")) {
    scfg <- input
  } else if (checkmate::test_string(input)) {
    if (grepl("\\.ya?ml$", input, ignore.case = TRUE)) {
      if (!checkmate::test_file_exists(input)) {
        stop("Cannot find file: ", input)
      }
      scfg <- load_project(input, validate = FALSE)
    } else if (checkmate::test_directory_exists(input)) {
      cfg_file <- file.path(input, "project_config.yaml")
      if (file.exists(cfg_file)) {
        scfg <- load_project(cfg_file, validate = FALSE)
      } else {
        warning("project_config.yaml not found in ", input)
        scfg <- list()
      }
    } else {
      stop("input must be a bg_project_cfg object, YAML file, or project directory")
    }
  } else {
    stop("input must be a bg_project_cfg object, YAML file, or project directory")
  }

  return(scfg)
}

#' Set up a BrainGnomes project workflow for a new fMRI study
#'
#' By default, this function opens the guided project-configuration workflow.
#' Set `interactive = FALSE` to create a portable project from deterministic
#' defaults without prompting. Non-interactive setup creates the project and
#' standard data directories and writes `project_config.yaml`. Processing stages
#' are disabled unless inherited from a template.
#' New guided configurations start by asking what data you already have. Steps
#' completed outside BrainGnomes are skipped, and downstream choices remain
#' explicit. Existing configured projects retain their stage selections.
#'
#' @param input A `bg_project_cfg` object, a path to a YAML file, or a project
#'   directory containing \code{project_config.yaml}. If a directory is supplied
#'   but the file is missing, \code{setup_project} starts from an empty list with
#'   a warning. This argument may also be \code{NULL} to create a new
#'   configuration from scratch. In non-interactive mode, `input` can be used as
#'   the starting template.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all
#'   fields will be prompted for. Only available in interactive mode.
#' @param project_name Project label. Required in non-interactive mode. When
#'   supplied in interactive mode, it is used as the initial project name.
#' @param project_directory Project root directory. Relative setup inputs are
#'   resolved once from the setup working directory. The saved configuration is
#'   colocated in this directory and records the root as `.`. Required in
#'   non-interactive mode; when supplied interactively, it is the initial root.
#' @param template Optional configuration object, YAML file, or project
#'   directory to use as a base. Supply either `input` or `template`, not both.
#' @param interactive Whether to use the guided configuration workflow. Defaults
#'   to `TRUE` to preserve the standard interactive R workflow.
#' @param overwrite Replace an existing `project_config.yaml` in non-interactive
#'   mode.
#' @param reuse_template_paths In non-interactive mode, retain the template's
#'   original paths, including outputs and its tracking database. Defaults to
#'   `FALSE`: project-owned directories are rebased into the new root, mutable
#'   external destinations are replaced, and a new database path is used.
#'   External input directories, shared TemplateFlow caches, and resource files
#'   such as containers, atlases, and licenses are retained. Enabled stages and
#'   processing settings are inherited. Set `TRUE` only to intentionally share
#'   the original project state.
#' @param starting_point Optional starting point for guided setup: `"flywheel"`
#'   (download DICOMs first), `"dicom"` (local DICOMs), `"bids"` (existing BIDS),
#'   `"fmriprep"` (existing derivatives plus the corresponding BIDS dataset),
#'   `"existing"` (inspect a project without changing it), or `"custom"` (choose
#'   stages individually). When omitted, new or headless-only configurations
#'   offer a menu; configured projects keep their settings. Only available with
#'   `interactive = TRUE` and `fields = NULL`.
#' @return A `bg_project_cfg` list containing the project configuration. New
#'   fields are added based on user input or portable defaults. The configuration
#'   is written to `project_config.yaml` in the project directory. Interactive
#'   setup asks before replacing a changed file; non-interactive setup requires
#'   `overwrite = TRUE`. Selecting `"existing"` instead returns the loaded
#'   configuration invisibly after showing project status, without saving it.
#' @importFrom yaml read_yaml
#' @importFrom checkmate test_file_exists
#' @export
setup_project <- function(input = NULL, fields = NULL, project_name = NULL,
                          project_directory = NULL, template = NULL,
                          interactive = TRUE, overwrite = FALSE,
                          reuse_template_paths = FALSE, starting_point = NULL) {
  checkmate::assert_flag(interactive)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(reuse_template_paths)
  checkmate::assert_choice(starting_point, names(project_starting_points()), null.ok = TRUE)
  if (!is.null(starting_point) && (!interactive || !is.null(fields))) {
    stop("starting_point is only available for full guided setup (interactive = TRUE, fields = NULL).", call. = FALSE)
  }
  if (interactive && reuse_template_paths) {
    stop("reuse_template_paths is only available when interactive = FALSE.", call. = FALSE)
  }
  if (!is.null(input) && !is.null(template)) {
    stop("Supply either input or template, not both.", call. = FALSE)
  }
  starting_config <- if (is.null(template)) input else template

  if (!interactive) {
    if (!is.null(fields)) {
      stop("fields is only available when interactive = TRUE.", call. = FALSE)
    }
    return(create_project_from_defaults(
      project_name = project_name,
      project_directory = project_directory,
      template = starting_config,
      overwrite = overwrite,
      reuse_template_paths = reuse_template_paths
    ))
  }

  scfg <- get_scfg_from_input(starting_config)

  if (is.null(fields)) {
    cli::cli_h1("BrainGnomes project setup")
    if (!is.null(starting_point) || project_needs_starting_point(scfg)) {
      starting_point <- choose_project_starting_point(scfg, starting_point)
      if (starting_point == "existing") {
        return(inspect_setup_project(starting_config))
      }
      scfg <- apply_project_starting_point(scfg, starting_point)
    } else {
      cli::cli_alert_info("Keeping the existing project stage selections. Use edit_project() to change individual choices.")
    }
    cli_instruction(
      "Answer each prompt to configure the project. Press Enter to accept a displayed default. Setup does not submit jobs.",
      before = FALSE
    )
  }

  if (!checkmate::test_class(scfg, "bg_project_cfg")) {
    class(scfg) <- c(class(scfg), "bg_project_cfg")
  }
  if (is.null(scfg$schema_version)) scfg$schema_version <- 1L
  if (!is.null(project_name)) {
    checkmate::assert_string(project_name)
    if (is.null(scfg$metadata)) scfg$metadata <- list()
    scfg$metadata$project_name <- project_name
  }
  if (!is.null(project_directory)) {
    checkmate::assert_string(project_directory)
    if (is.null(scfg$metadata)) scfg$metadata <- list()
    scfg$metadata$project_directory <- normalizePath(
      path.expand(project_directory), winslash = "/", mustWork = FALSE
    )
  }

  # run through configuration of each step
  scfg <- setup_project_metadata(scfg, fields)
  if (!is.null(starting_point)) scfg <- setup_starting_point_paths(scfg, starting_point)
  scfg <- setup_flywheel_sync(scfg, fields)
  scfg <- setup_bids_conversion(scfg, fields)
  scfg <- setup_fmriprep(scfg, fields)
  scfg <- setup_mriqc(scfg, fields)
  scfg <- setup_aroma(scfg, fields)
  scfg <- setup_postprocess_streams(scfg, fields)
  scfg <- setup_extract_streams(scfg, fields)
  scfg <- setup_bids_validation(scfg, fields)
  scfg <- setup_compute_environment(scfg, fields)

  if (is.null(fields)) {
    review_setup_workflow(scfg)
    cli_setup_section("Save configuration")
  }
  scfg <- save_project_config(scfg)

  return(scfg)
}

#' Create a project configuration from portable defaults without prompts
#'
#' @param project_name Project label.
#' @param project_directory Project root directory.
#' @param template Optional project configuration object, YAML file, or project
#'   directory to use as a base.
#' @param overwrite Replace an existing `project_config.yaml`.
#' @param reuse_template_paths Retain template paths and tracking state.
#' @return A `bg_project_cfg` object with its YAML path attached.
#' @noRd
create_project_from_defaults <- function(project_name, project_directory,
                                         template = NULL, overwrite = FALSE,
                                         reuse_template_paths = FALSE) {
  checkmate::assert_string(project_name)
  checkmate::assert_string(project_directory)
  checkmate::assert_flag(overwrite)
  project_directory <- path.expand(project_directory)

  dir.create(project_directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(project_directory)) {
    stop("Failed to create project directory: ", project_directory, call. = FALSE)
  }
  # Resolve aliases only after the directory exists. On macOS, /var resolves to
  # /private/var; on Windows, this also standardizes separators and long names.
  project_directory <- normalizePath(
    project_directory, winslash = "/", mustWork = TRUE
  )

  scfg <- if (is.null(template)) {
    list()
  } else {
    as.list(project_config_from_input(template))
  }
  scfg$schema_version <- value_or_default(scfg$schema_version, 1L)
  if (is.null(scfg$metadata)) scfg$metadata <- list()

  default_dirs <- c(
    dicom_directory = "data_dicoms",
    bids_directory = "data_bids",
    fmriprep_directory = "data_fmriprep",
    mriqc_directory = "data_mriqc",
    postproc_directory = "data_postproc",
    rois_directory = "data_rois",
    log_directory = "logs",
    scratch_directory = "scratch",
    templateflow_home = "templateflow"
  )
  if (!is.null(template) && !reuse_template_paths) {
    scfg <- rebase_project_template(scfg, project_directory, default_dirs)
    scfg$metadata$sqlite_db <- NULL
  }
  scfg$metadata$project_name <- project_name
  scfg$metadata$project_directory <- project_directory
  # Execution context belongs to an invocation, never to a new project.
  for (attribute in c(
      "yaml_file", "validation", "provenance_context", "retry_work_units")) {
    attr(scfg, attribute) <- NULL
  }
  for (field in names(default_dirs)) {
    if (!checkmate::test_string(scfg$metadata[[field]])) {
      scfg$metadata[[field]] <- file.path(
        project_directory, default_dirs[[field]]
      )
    }
    dir.create(
      scfg$metadata[[field]], recursive = TRUE, showWarnings = FALSE
    )
  }

  scfg$metadata$sqlite_db <- value_or_default(
    scfg$metadata$sqlite_db,
    file.path(project_directory, paste0(project_name, ".sqlite"))
  )
  if (is.null(scfg$compute_environment)) scfg$compute_environment <- list()
  if (!checkmate::test_string(scfg$compute_environment$scheduler)) {
    scfg$compute_environment$scheduler <- if (
      nzchar(Sys.which("qsub")) && !nzchar(Sys.which("sbatch"))
    ) {
      "torque"
    } else {
      "slurm"
    }
  }
  for (stage in c(supported_project_steps(), "bids_validation")) {
    if (is.null(scfg[[stage]])) scfg[[stage]] <- list()
    if (!checkmate::test_flag(scfg[[stage]]$enable)) {
      scfg[[stage]]$enable <- FALSE
    }
  }

  class(scfg) <- unique(c("bg_project_cfg", class(scfg)))
  write_project_config(scfg, overwrite = overwrite)
}

#' Resolve template directory aliases even when the destination does not exist
#'
#' @param path A single directory path, possibly containing missing descendants.
#' @return Absolute directory path with existing ancestor aliases resolved and
#'   missing descendants appended, without creating or modifying directories.
#' @noRd
normalize_template_directory <- function(path) {
  path <- path.expand(path)
  if (.Platform$OS.type == "windows") path <- gsub("\\\\", "/", path)
  ancestor <- path
  suffix <- character()
  # normalizePath() may leave a missing path unchanged on macOS and Windows.
  # Resolve only an existing ancestor so parent/child comparisons use the same
  # symlink targets and Windows long names before any output is created.
  while (!dir.exists(ancestor)) {
    parent <- dirname(ancestor)
    if (identical(parent, ancestor)) {
      return(normalizePath(path, winslash = "/", mustWork = FALSE))
    }
    suffix <- c(basename(ancestor), suffix)
    ancestor <- parent
  }
  resolved <- normalizePath(ancestor, winslash = "/", mustWork = TRUE)
  for (component in suffix) {
    if (component == ".") next
    resolved <- if (component == "..") dirname(resolved) else file.path(resolved, component)
  }
  resolved
}

#' Isolate the writable paths of a project created from a template
#'
#' @param scfg Template configuration before changing its root.
#' @param project_directory New absolute project root.
#' @param default_dirs Named default directory basenames.
#' @return Configuration with rebased project directories and report destination.
#' @noRd
rebase_project_template <- function(scfg, project_directory, default_dirs) {
  old_root <- scfg$metadata$project_directory
  if (checkmate::test_string(old_root)) {
    old_root <- sub("/+$", "", normalize_template_directory(old_root))
  }
  defaults <- c(default_dirs, flywheel_temp_directory = "flywheel_tmp",
    flywheel_sync_directory = "flywheel_sync")
  # These directories may be external inputs when their producing stages are off.
  shared_inputs <- c("dicom_directory", "templateflow_home")
  if (!isTRUE(scfg$bids_conversion$enable) && !isTRUE(scfg$flywheel_sync$enable)) {
    shared_inputs <- c(shared_inputs, "bids_directory")
  }
  if (!isTRUE(scfg$fmriprep$enable) && !isTRUE(scfg$aroma$enable)) {
    shared_inputs <- c(shared_inputs, "fmriprep_directory")
  }
  if (!isTRUE(scfg$postprocess$enable)) shared_inputs <- c(shared_inputs, "postproc_directory")
  original_paths <- list()
  for (field in names(defaults)) {
    path <- scfg$metadata[[field]]
    if (!checkmate::test_string(path)) next
    path <- path.expand(path)
    # Relative directory values in a template are interpreted against its root.
    absolute <- grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
    if (!absolute && checkmate::test_string(old_root)) path <- file.path(old_root, path)
    path <- normalize_template_directory(path)
    original_paths[[field]] <- path
    key <- if (.Platform$OS.type == "windows") tolower(path) else path
    root_key <- if (.Platform$OS.type == "windows") tolower(old_root) else old_root
    inside <- checkmate::test_string(root_key) &&
      (identical(key, root_key) || startsWith(key, paste0(root_key, "/")))
    if (inside) {
      suffix <- substring(path, nchar(old_root) + 1L)
      scfg$metadata[[field]] <- paste0(project_directory, suffix)
    } else if (!field %in% shared_inputs) {
      scfg$metadata[[field]] <- file.path(project_directory, defaults[[field]])
    } else {
      scfg$metadata[[field]] <- path
    }
  }
  # Flywheel commonly writes directly into the DICOM input tree. Preserve that
  # relationship while moving the writable sync destination into the new root.
  sync <- original_paths$flywheel_sync_directory
  dicom <- original_paths$dicom_directory
  if (isTRUE(scfg$flywheel_sync$enable) &&
      checkmate::test_string(sync) && checkmate::test_string(dicom)) {
    sync <- sub("/+$", "", sync)
    sync_key <- if (.Platform$OS.type == "windows") tolower(sync) else sync
    dicom_key <- if (.Platform$OS.type == "windows") tolower(dicom) else dicom
    if (identical(dicom_key, sync_key) || startsWith(dicom_key, paste0(sync_key, "/"))) {
      scfg$metadata$dicom_directory <- paste0(
        scfg$metadata$flywheel_sync_directory, substring(dicom, nchar(sync) + 1L)
      )
    }
  }
  # Relative validator reports resolve under the new log directory at execution.
  if (checkmate::test_string(scfg$bids_validation$outfile)) {
    scfg$bids_validation$outfile <- basename(scfg$bids_validation$outfile)
  }
  scfg
}

#' Set up project metadata for an fMRI preprocessing study
#'
#' Prompts the user to configure essential metadata fields for a study-level configuration object.
#' This includes directories for DICOM inputs, BIDS-formatted outputs, fMRIPrep outputs, MRIQC reports,
#' TemplateFlow cache, and scratch space for intermediate files. It also ensures required directories
#' exist or offers to create them interactively.
#'
#' The function is designed to be used during initial study setup, but can also be used later to fill in
#' missing metadata or revise selected fields. If specific `fields` are provided, only those fields will be prompted.
#'
#' @param scfg A project configuration object created by `setup_project()`.
#' @param fields A character vector of metadata fields to prompt for (e.g., `"metadata/project_name"`).
#'   If `NULL`, all missing or unset fields will be prompted.
#'
#' @return A modified version of `scfg` with the `$metadata` field populated with validated paths and project details.
#' @keywords internal
setup_project_metadata <- function(scfg = NULL, fields = NULL) {
  default_sqlite_db <- function(scfg) {
    if (!test_string(scfg$metadata$project_directory) || !test_string(scfg$metadata$project_name)) {
      return(NULL)
    }
    file.path(scfg$metadata$project_directory, paste0(scfg$metadata$project_name, ".sqlite"))
  }

  # If fields is not null, then the caller wants to make specific edits to config. Thus, don't prompt for invalid settings for other fields.
  # For dicom_directory, bids_directory, and fmriprep_directory, only prompt if relevant parts of the pipeline are enabled (e.g., DICOMs for BIDS conversion)
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$metadata$project_name)) fields <- c(fields, "metadata/project_name")
    if (is.null(scfg$metadata$project_directory)) fields <- c(fields, "metadata/project_directory")
    if (is.null(scfg$metadata$log_directory)) fields <- c(fields, "metadata/log_directory")
    if (is.null(scfg$metadata$templateflow_home)) fields <- c(fields, "metadata/templateflow_home")
    if (is.null(scfg$metadata$scratch_directory)) fields <- c(fields, "metadata/scratch_directory")
  }

  if ("metadata/project_name" %in% fields) {
    scfg$metadata$project_name <- prompt_input("What is the name of your project?", type = "character")
  }

  if ("metadata/project_directory" %in% fields) {
    scfg$metadata$project_directory <- prompt_input("What is the root directory where project files will be stored?", type = "character")
    # Capture the setup working directory exactly once. The saved YAML will use
    # `project_directory: .`, while runtime code receives this absolute root.
    scfg$metadata$project_directory <- normalize_project_path(
      scfg$metadata$project_directory
    )
    if (!checkmate::test_directory_exists(scfg$metadata$project_directory)) {
      create <- prompt_input(
        instruct = glue("The directory {scfg$metadata$project_directory} does not exist."),
        prompt = "Create this directory now?",
        type = "flag"
      )
      if (create) dir.create(scfg$metadata$project_directory, recursive = TRUE)
    } else if (!checkmate::test_directory_exists(scfg$metadata$project_directory, access="r")) {
      stop("Project directory exists, but is not readable by you. Fix read permissions on: ", scfg$metadata$project_directory, call. = FALSE)
    } else if (!checkmate::test_directory_exists(scfg$metadata$project_directory, access="w")) {
      stop("Project directory exists, but is not writable by you. Fix write permissions on: ", scfg$metadata$project_directory, call. = FALSE)
    }
  }

  # Flywheel sync directory defaults to metadata$dicom_directory
  if ("metadata/flywheel_sync_directory" %in% fields) {
    if (!test_string(scfg$metadata$flywheel_sync_directory)) {
      default <- if (test_string(scfg$metadata$dicom_directory)) scfg$metadata$dicom_directory else NULL
    } else {
      default <- scfg$metadata$flywheel_sync_directory
    }

    scfg$metadata$flywheel_sync_directory <- prompt_directory(
      prompt = "Where should Flywheel place downloaded DICOM files?",
      default = default, check_readable = TRUE
    )
  }

  # use scratch directory as base of flywheel sync temps if not otherwise specified
  if ("metadata/flywheel_temp_directory" %in% fields) {
    if (!test_string(scfg$metadata$flywheel_temp_directory)) scfg$metadata$flywheel_temp_directory <- file.path(scfg$metadata$scratch_directory, "flywheel")
    scfg$metadata$flywheel_temp_directory <- prompt_directory(
      prompt = "Where should temporary files for Flywheel sync go?",
      default = scfg$metadata$flywheel_temp_directory, check_readable = TRUE
    )
  }

  # location of DICOMs -- only needed if BIDS conversion enabled
  if ("metadata/dicom_directory" %in% fields) {
    scfg$metadata$dicom_directory <- prompt_directory(prompt="Where is your DICOM directory?", default = scfg$metadata$dicom_directory, check_readable = TRUE)
  }

  # location of BIDS data -- default within project directory, but allow external paths
  if ("metadata/bids_directory" %in% fields) {
    default <- if (!test_string(scfg$metadata$bids_directory)) file.path(scfg$metadata$project_directory, "data_bids") else scfg$metadata$bids_directory
    scfg$metadata$bids_directory <- prompt_directory(prompt="Where is your BIDS directory?", default = default, check_readable = TRUE)
  }

  # location of fMRIPrep outputs -- default within project directory, but allow external paths. Only prompt if fmriprep enabled
  if ("metadata/fmriprep_directory" %in% fields) {
    default <- if (!test_string(scfg$metadata$fmriprep_directory)) file.path(scfg$metadata$project_directory, "data_fmriprep") else scfg$metadata$fmriprep_directory
    scfg$metadata$fmriprep_directory <- prompt_directory(
      instruct = glue("\n\n
        We recommend that fmriprep outputs be placed in data_fmriprep within the BrainGnomes project directory.
        You can specify a different location if you wish. Also, if you're working from extant fmriprep files, you
        can point to an existing directory containing the results of fmriprep.\n
      "),
      prompt = "Specify the directory for fmriprep files", default = default
    )
  }

  # location of logs -- default within project directory, but allow external paths
  if ("metadata/log_directory" %in% fields) {
    default <- if (!test_string(scfg$metadata$log_directory)) file.path(scfg$metadata$project_directory, "logs") else scfg$metadata$log_directory
    scfg$metadata$log_directory <- prompt_directory(
      instruct = glue("\n\n
        We recommend placing logs in a logs directory within the BrainGnomes project folder.
        You can specify a different location if you prefer, including a shared external logs path."),
      prompt = "Specify the directory for project logs", default = default
    )
  }
  
  if ("metadata/scratch_directory" %in% fields) {
    scfg$metadata$scratch_directory <- prompt_directory(prompt = "Where is your work directory?",
      instruct = glue("\n\n
      Some BrainGnomes tools (especially fmriprep) use a lot of disk space for processing intermediate files. 
      It's best if these are written to a scratch/temporary directory that is cleared regularly so that you don't
      use up precious disk space for unnecessary files. Please indicate where these intermediate
      files should be written.\n
      ")
    )
  }

  if ("metadata/templateflow_home" %in% fields) {
    scfg$metadata$templateflow_home <- prompt_input("Templateflow directory: ",
      instruct = glue("\n\n
      The pipeline uses TemplateFlow to download and cache templates for use in fMRI processing.
      Please specify the location of the TemplateFlow cache directory. The default is $HOME/.cache/templateflow.
      You can also point to a different location if you have a shared cache directory for multiple users.\n
      "), type = "character", default = file.path(Sys.getenv("HOME"), ".cache", "templateflow")
    )
  }

  if ("metadata/sqlite_db" %in% fields) {
    default <- if (!test_string(scfg$metadata$sqlite_db)) {
      default_sqlite_db(scfg)
    } else {
      scfg$metadata$sqlite_db
    }
    scfg$metadata$sqlite_db <- prompt_input(
      prompt = "SQLite database for job tracking:",
      type = "character",
      default = default
    )
  }

  # singularity bind paths are unhappy with symbolic links and ~/ notation
  if (test_string(scfg$metadata$templateflow_home)) {
    scfg$metadata$templateflow_home <- normalizePath(scfg$metadata$templateflow_home, mustWork = FALSE)
  }

  # default log directory if one has not been provided
  if (!test_string(scfg$metadata$log_directory)) {
    scfg$metadata$log_directory <- file.path(scfg$metadata$project_directory, "logs")
  }

  # location of postproc outputs -- currently forcing to be within BG project
  if ("metadata/postproc_directory" %in% fields) {
    scfg$metadata$postproc_directory <- file.path(scfg$metadata$project_directory, "data_postproc")
  }

  # location for ROI timeseries and connectivity data -- currently forcing to be within BG project
  if ("metadata/rois_directory" %in% fields) {
    scfg$metadata$rois_directory <- file.path(scfg$metadata$project_directory, "data_rois")
  }


  # Define the default SQLite database path only when it has not been set.
  if (!test_string(scfg$metadata$sqlite_db)) {
    scfg$metadata$sqlite_db <- default_sqlite_db(scfg)
  }

  return(scfg)
}

#' Configure fMRIPrep preprocessing settings
#'
#' This function sets up fMRIPrep job configuration, including scheduling and resource parameters,
#' output specifications, and the location of required files such as the FreeSurfer license.
#' It prompts the user interactively (or selectively if `fields` is supplied) and modifies the
#' project configuration list (`scfg`) to include settings for running fMRIPrep.
#'
#' @param scfg A project configuration object, as produced by `load_project()` or `setup_project()`.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all fMRIPrep fields will be prompted for.
#'
#' @return A modified version of `scfg` with the `$fmriprep` entry populated.
#'
#' @details
#' fMRIPrep is a robust and standardized preprocessing pipeline for BOLD and structural MRI data
#' organized according to the BIDS standard. It performs motion correction, susceptibility distortion
#' correction, brain extraction, spatial normalization, confound estimation, and other key steps
#' to prepare fMRI data for statistical analysis.
#'
#' This function allows you to specify memory, number of cores, and maximum runtime for fMRIPrep jobs,
#' as well as fMRIPrep-specific options such as output spaces and the FreeSurfer license file location.
#'
#' @keywords internal
setup_fmriprep <- function(scfg = NULL, fields = NULL) {
  # https://fmriprep.org/en/stable/usage.html
  # [--omp-nthreads OMP_NTHREADS] [--mem MEMORY_MB] [--low-mem]  [--nprocs NPROCS]
  defaults <- list(
    memgb = 48L,
    nhours = 24L,
    ncores = 12L,
    cli_options = "",
    sched_args = ""
  )

  if (is.null(scfg$fmriprep$enable) || (isFALSE(scfg$fmriprep$enable) && any(grepl("fmriprep/", fields))) || ("fmriprep/enable" %in% fields)) {
    scfg$fmriprep$enable <- prompt_input(
      instruct = glue("
      fMRIPrep is a standardized and robust tool for preprocessing BIDS-organized functional and anatomical MRI data.
      It performs essential steps such as motion correction, susceptibility distortion correction, tissue segmentation,
      coregistration, normalization to standard space, and estimation of nuisance regressors.

      Running fMRIPrep is typically a required step before any model-based analysis of fMRI data.

      You will have the option to specify output spaces (e.g., MNI152NLin2009cAsym, T1w) and provide 
      a FreeSurfer license file, which is necessary for anatomical processing. You can also pass custom CLI
      options and schedule settings.
      "),
      heading = "fMRIPrep",
      prompt = "Do you want to include fMRIPrep as part of your preprocessing pipeline?",
      type = "flag",
      default = if (is.null(scfg$fmriprep$enable)) TRUE else isTRUE(scfg$fmriprep$enable)
    )
  }

  if (isFALSE(scfg$fmriprep$enable)) return(scfg)

  # prompt for fmriprep container at this step, but only if it is not already in fields
  # if compute_environment/fmriprep_container is already in fields, it will be caught by setup_compute_environment
  if (!validate_exists(scfg$compute_environment$fmriprep_container) && !"compute_environment/fmriprep_container" %in% fields) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/fmriprep_container")
  }

  scfg <- setup_job(scfg, "fmriprep", defaults, fields)

  # prompt for BIDS directory -- input to fmriprep
  if (is.null(scfg$metadata$bids_directory)) {
    scfg <- setup_project_metadata(scfg, fields = "metadata/bids_directory")
  }

  # prompt for fmriprep directory -- outputs of fmriprep
  if (is.null(scfg$metadata$fmriprep_directory)) {
    scfg <- setup_project_metadata(scfg, fields = "metadata/fmriprep_directory")
  }

  # If fields is not null, then the caller wants to make specific edits to config. Thus, don't prompt for invalid settings for other fields.
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$fmriprep$output_spaces)) fields <- c(fields, "fmriprep/output_spaces")
    if (!validate_exists(scfg$fmriprep$fs_license_file)) fields <- c(fields, "fmriprep/fs_license_file")
  }

  if ("fmriprep/output_spaces" %in% fields) {
    scfg$fmriprep$output_spaces <- choose_fmriprep_spaces(scfg$fmriprep$output_spaces)
  }

  if ("fmriprep/fs_license_file" %in% fields) {
    scfg$fmriprep$fs_license_file <- prompt_input(
      instruct = glue("\n
      What is the location of your FreeSurfer license file? This is required for fmriprep to run.
      The license file might be called FreeSurferLicense.txt and is available from the FreeSurfer website.
      https://surfer.nmr.mgh.harvard.edu/fswiki/License\n
      "),
      prompt = "What is the location of your FreeSurfer license file?",
      type = "file", default = scfg$fmriprep$fs_license_file
    ) |> normalizePath(mustWork=TRUE)
  }

  return(scfg)

}

#' Specify the BIDS validation settings
#' @param scfg A project configuration object, as produced by `load_project()` or `setup_project()`.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all BIDS validation fields will be prompted for.
#' @return A modified version of `scfg` with the `$bids_validation` entry populated.
#' @keywords internal
setup_bids_validation <- function(scfg, fields=NULL) {
  defaults <- list(
    memgb = 32,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  if (is.null(scfg$bids_validation$enable) || (isFALSE(scfg$bids_validation$enable) && any(grepl("bids_validation/", fields))) || ("bids_validation/enable" %in% fields)) {
    scfg$bids_validation$enable <- prompt_input(
      instruct = glue("
      BIDS validation checks whether your dataset adheres to the Brain Imaging Data Structure (BIDS) standard.
      This can be helpful for ensuring that all filenames, metadata, and required files follow expected conventions.
      It can identify missing fields, naming issues, or formatting problems that could cause downstream tools to fail.
      
      The validator can be run quickly, and produces an HTML report that summarizes any warnings or errors.
      Note: Even if you say 'no' here, fmriprep will run BIDS validation on each subject's dataset prior to execution.
      
      Saying 'Yes' configures BIDS validation with this project, but validation is submitted separately
      through run_bids_validation().
      "),
      heading = "BIDS validation",
      prompt = "Enable BIDS validation?",
      type = "flag",
      default = if (is.null(scfg$bids_validation$enable)) TRUE else isTRUE(scfg$bids_validation$enable)
    )
  }

  if (isFALSE(scfg$bids_validation$enable)) return(scfg)

  # prompt for BIDS validator at this point if not already in fields
  if (!validate_exists(scfg$compute_environment$bids_validator) && !"compute_environment/bids_validator" %in% fields) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/bids_validator")
  }

  scfg <- setup_job(scfg, "bids_validation", defaults, fields)

  if (is.null(scfg$bids_validation$outfile) || "bids_validation/outfile" %in% fields) {
    scfg$bids_validation$outfile <- prompt_input(
      instruct = glue("\n
      What should be the output HTML file for bids_validator? The default is bids_validator_output.html.
      Relative paths are written under your project log directory (recommended) so they do not alter the BIDS dataset.
      Absolute paths are also supported if you want to write elsewhere.
      \n
    "),
      prompt = "What is the name of the output file for bids-validator?",
      type = "character", default = "bids_validator_output.html"
    )
  }

  return(scfg)
}

#' Specify the MRIQC settings
#' @param scfg A project configuration object, as produced by `load_project()` or `setup_project()`.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all MRIQC fields will be prompted for.
#' @return A modified version of `scfg` with the `$mriqc` entry populated.
#' @keywords internal
setup_mriqc <- function(scfg, fields = NULL) {
  defaults <- list(
    memgb = 32,
    nhours = 12,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  if (is.null(scfg$mriqc$enable) || (isFALSE(scfg$mriqc$enable) && any(grepl("mriqc/", fields))) || ("mriqc/enable" %in% fields)) {
    scfg$mriqc$enable <- prompt_input(
      instruct = glue("
      MRIQC is a tool for automated quality assessment of structural and functional MRI data. 
      It calculates a wide array of image quality metrics (IQMs) for each scan, such as signal-to-noise ratio, 
      motion estimates, and image sharpness. It also produces visual reports to help you identify 
      scans with artifacts, excessive motion, or other issues that might compromise analysis.

      Running MRIQC is a recommended step, as it can help you detect problematic scans early 
      and guide decisions about inclusion, exclusion, or further inspection.

      MRIQC supports both group-level and individual-level analyses and produces HTML reports and TSV files.
      Saying 'Yes' here only runs the individual-level QC checks on each dataset.
      "),
      heading = "MRIQC",
      prompt = "Run MRIQC?",
      type = "flag",
      default = if (is.null(scfg$mriqc$enable)) TRUE else isTRUE(scfg$mriqc$enable)
    )
  }

  if (isFALSE(scfg$mriqc$enable)) return(scfg)

  # location of mriqc reports -- enforce that this must be within the project directory
  scfg$metadata$mriqc_directory <- file.path(scfg$metadata$project_directory, "mriqc_reports")

  # prompt for mriqc container at this step
  if (!validate_exists(scfg$compute_environment$mriqc_container) && !"compute_environment/mriqc_container" %in% fields) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/mriqc_container")
  }

  scfg <- setup_job(scfg, "mriqc", defaults, fields)

  return(scfg)
}

#' Configure flywheel sync settings
#'
#' Sets up synchronization from a Flywheel instance prior to BIDS conversion.
#' Prompts for the Flywheel project URL, drop-off directory for downloaded DICOMs,
#' and a temporary directory used during transfer. Standard job settings are also
#' collected through `setup_job`.
#'
#' @param scfg A project configuration object, as produced by `load_project()` or `setup_project()`.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all Flywheel fields will be prompted for.
#' @return A modified version of `scfg` with the `$flywheel_sync` entry populated.
#' @keywords internal
setup_flywheel_sync <- function(scfg, fields = NULL) {
  defaults <- list(
    memgb = 16,
    nhours = 4,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  if (is.null(scfg$flywheel_sync$enable) || (isFALSE(scfg$flywheel_sync$enable) && any(grepl("flywheel_sync/", fields))) || ("flywheel_sync/enable" %in% fields)) {
    scfg$flywheel_sync$enable <- prompt_input(
      instruct = glue("
      Flywheel sync will download DICOM files from a Flywheel project using the
      'fw sync' command-line interface. This step should be run prior to BIDS
      conversion to ensure all data are available locally."),
      heading = "Flywheel sync",
      prompt = "Run Flywheel sync?",
      type = "flag",
      default = if (is.null(scfg$flywheel_sync$enable)) FALSE else isTRUE(scfg$flywheel_sync$enable)
    )
  }

  if (isFALSE(scfg$flywheel_sync$enable)) return(scfg)

  if (!validate_exists(scfg$compute_environment$flywheel) && !"compute_environment/flywheel" %in% fields) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/flywheel")
  }

  scfg <- setup_job(scfg, "flywheel_sync", defaults, fields)

  if (is.null(scfg$flywheel_sync$source_url) || "flywheel_sync/source_url" %in% fields) {
    scfg$flywheel_sync$source_url <- prompt_input(
      instruct = "Use the Flywheel project URL format: fw://server/group/project.",
      prompt = "Flywheel project URL:",
      type = "character"
    )
  }

  if (is.null(scfg$flywheel_sync$save_audit_logs) || "flywheel_sync/save_audit_logs" %in% fields) {
    scfg$flywheel_sync$save_audit_logs <- prompt_input(
      instruct = glue("\n\n
        Flywheel can save an audit log (CSV) that includes the result of
        each dataset. This is helpful for debugging sync failures or glitches
        and is recommended. The file is saved in the project's logs directory."),
      prompt = "Save Flywheel sync audit logs?",
      type = "flag",
      default = TRUE
    )
  }

  # prompt for sync directory
  if (is.null(scfg$metadata$flywheel_sync_directory)) {
    scfg <- setup_project_metadata(scfg, fields = "metadata/flywheel_sync_directory")
  }

  # prompt for temp directory
  if (is.null(scfg$metadata$flywheel_temp_directory)) {
    scfg <- setup_project_metadata(scfg, fields = "metadata/flywheel_temp_directory")
  }

  return(scfg)
}

#' Specify the BIDS conversion settings
#' @param scfg a project configuration object, as produced by `load_project` or `setup_project`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with `$bids_conversion` populated
#' @keywords internal
setup_bids_conversion <- function(scfg, fields = NULL) {
  defaults <- list(
    memgb = 16,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  if (is.null(scfg$bids_conversion$enable) || (isFALSE(scfg$bids_conversion$enable) && any(grepl("bids_conversion/", fields))) || ("bids_conversion/enable" %in% fields)) {
    scfg$bids_conversion$enable <- prompt_input(
      instruct = glue("
      This step sets up DICOM to BIDS conversion using heudiconv. Heudiconv uses a heuristic
      file to match DICOM files to expected scans, allowing the tool to convert DICOMs to NIfTI images
      and reorganize them into BIDS format.

      The heuristic file is a python script that tells heudiconv how to convert the DICOM files
      to BIDS format. For details, see https://heudiconv.readthedocs.io/en/latest/usage.html

      If you say 'Yes' to this step, you will later be asked for the location of the folder containing
      DICOM images for all subjects. This should be a folder that contains subfolders for each subject, with
      the DICOM files inside those subfolders. For example, if you have a folder called 'data_DICOMs' that
      contains subfolders 'sub-001', 'sub-002', etc., then you would specify the location of 'data_DICOMs' here.

      The BIDS-compatible outputs will be written to a folder called 'data_bids' within the project directory.

      You will also be asked for a regular expression that matches the subject IDs in the DICOM folder names.
      The default is 'sub-[0-9]+', which matches sub-001, sub-002, etc. If you have a different naming scheme,
      please specify it here. For example, if your subject folders are named '001', '002', etc., you would
      specify '^[0-9]+$' here. Importantly, the pipeline will always extract only the numeric portion of the
      subject ID, so if you have a folder called 'sub-001', the subject ID will be '001' regardless of the
      regex you specify here.

      Similarly, if you have multisession data, you will be asked for a regular expression that matches the
      session IDs in the DICOM folder names. Crucially, sessions must always be subfolders within a given subject
      folder. Here is an example where the subject regex is 'sub-[0-9]+' and the session regex is 'ses-[0-9]+':

      /data/dicom/
      |-- sub-01/
      |   |-- ses-01/
      |   |   |-- 1.dcm
      |   |   |-- 2.dcm
      |   |-- ses-02/
      |       |-- 1.dcm
      |       |-- 2.dcm

      You will also be asked for the location of the heuristic file. If you don't have a heuristic file,
      please see some examples here: https://github.com/nipy/heudiconv/tree/master/heudiconv/heuristics.
      "),
      heading = "BIDS conversion",
      prompt = "Run BIDS conversion?",
      type = "flag",
      default = if (is.null(scfg$bids_conversion$enable)) TRUE else isTRUE(scfg$bids_conversion$enable)
    )
  }

  if (isFALSE(scfg$bids_conversion$enable)) return(scfg)

  # prompt for heudiconv container at this step
  if (!validate_exists(scfg$compute_environment$heudiconv_container) && !"compute_environment/heudiconv_container" %in% fields) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/heudiconv_container")
  }

  # prompt for DICOM directory
  if (is.null(scfg$metadata$dicom_directory)) {
    scfg <- setup_project_metadata(scfg, fields = "metadata/dicom_directory")
  }

  scfg <- setup_job(scfg, "bids_conversion", defaults, fields)

  if (is.null(scfg$bids_conversion$sub_regex) || "bids_conversion/sub_regex" %in% fields) {
    scfg$bids_conversion$sub_regex <- prompt_input(
      instruct = glue("
      \nWhat is the regex pattern for the subject IDs? This is used to identify the subject folders
      within the DICOM directory. The default is sub-[0-9]+, which matches sub-001, sub-002, etc.
      If you have a different naming scheme, please specify it here.\n
    "),
      prompt = "What is the regex pattern for the subject IDs?",
      type = "character", default = "sub-[0-9]+"
    )
  }

  if (is.null(scfg$bids_conversion$sub_id_match) || "bids_conversion/sub_id_match" %in% fields) {
    scfg$bids_conversion$sub_id_match <- prompt_input(
      instruct = glue("\n
      What is the regex pattern for extracting the ID from the subject folder name? You
      can use multiple capturing groups if the ID has multiple parts. The default is ([0-9]+),
      which extracts the first number-like sequence from the folder name. For example, if your
      subject folder is named 'sub-001', the ID will be '001'. If your subject folder is named
      '001', the ID will be '001'. If the entire folder name is the subject ID, such as '001ra_2May2024',
      the id matching expression should be (.+), which matches all characters in the folder name.\n
    "),
      prompt = "What is the regex pattern for extracting the subject ID from the folder name?",
      type = "character", default = "([0-9]+)"
    )
  }

  if (is.null(scfg$bids_conversion$ses_regex) || "bids_conversion/ses_regex" %in% fields) {
    scfg$bids_conversion$ses_regex <- prompt_input(
      instruct = glue("
      If you have multisession data, specify the regex pattern for session IDs within subject folders.
      If you don't have multisession data, just press Enter to skip this step.
    ", .trim = FALSE),
      prompt = "What is the regex pattern for the session IDs?",
      type = "character", required = FALSE
    )
  }

  if (!is.na(scfg$bids_conversion$ses_regex) && (is.null(scfg$bids_conversion$ses_id_match) || "bids_conversion/ses_id_match" %in% fields)) {
    scfg$bids_conversion$ses_id_match <- prompt_input(
      instruct = glue("\n
      What is the regex pattern for extracting the ID from the session folder name? You
      can use multiple capturing groups if the ID has multiple parts. The default is ([0-9]+),
      which extracts the first number-like sequence from the folder name. For example, if your
      session folder is named 'ses-01', the ID will be '01'. If your session folder is named
      '001', the ID will be '001'. If the entire folder name is the session ID, such as '001ra_2May2024',
      the ID matching expression should be (.+), which matches all characters in the folder name.\n
    "),
      prompt = "What is the regex pattern for extracting the session ID from the folder name?",
      type = "character", default = "([0-9]+)"
    )
  } else {
    scfg$bids_conversion$ses_id_match <- NA_character_
  }


  if (is.null(scfg$bids_conversion$heuristic_file) || "bids_conversion/heuristic_file" %in% fields) {
    scfg$bids_conversion$heuristic_file <- prompt_input(
      instruct = "Provide the path to your heudiconv heuristic file.",
      prompt = "Heudiconv heuristic file:",
      type = "file"
    )
  }

  if (is.null(scfg$bids_conversion$overwrite) || "bids_conversion/overwrite" %in% fields) {
    scfg$bids_conversion$overwrite <- prompt_input(
      instruct = "Enable this if you want heudiconv to replace existing BIDS outputs.",
      prompt = "Overwrite existing BIDS files?",
      type = "flag",
      default = TRUE
    )
  }

  if (is.null(scfg$bids_conversion$clear_cache) || "bids_conversion/clear_cache" %in% fields) {
    scfg$bids_conversion$clear_cache <- prompt_input(
      instruct = glue("\n\n
      Heudiconv caches its matching results inside the root of the BIDS folder in a hidden
      directory called .heudiconv. This provides a record of what heudiconv did for each subject conversion.
      It also speeds up conversions in future if you reprocess data. That said, if you modify the heuristic file,
      the cache can interfere because it will use the old heuristic file to match DICOMs to BIDS.
      If you want to clear the cache, say 'yes' here. If you want to keep the cache, say 'no'.
      "),
      prompt = glue("Should the heudiconv cache be cleared?"),
      type = "flag", default = FALSE
    )
  }

  return(scfg)
}

#' Configure ICA-AROMA denoising
#'
#' This function configures the ICA-AROMA (Independent Component Analysis-based Automatic Removal Of Motion Artifacts)
#' step for post-fMRIPrep processing.
#'
#' @param scfg A project configuration object, as produced by `load_project()` or `setup_project()`.
#' @param fields A character vector of field names to prompt for. If `NULL`, all fields related to AROMA will be prompted.
#'
#' @return A modified version of the `scfg` list with the `$aroma` entry added or updated.
#'
#' @details
#' ICA-AROMA is a data-driven denoising method that identifies motion-related independent components and removes them
#' from BOLD time series using non-aggressive regression. This step should be run **after fMRIPrep** has completed.
#' The settings configured here specify compute resource usage (e.g., memory, cores, time),
#' command-line options, and scheduler-specific arguments for running AROMA on each subject/session.
#'
#' By default, this function sets:
#' - `memgb`: 32 (memory in GB)
#' - `nhours`: 36 (max runtime in hours)
#' - `ncores`: 1 (number of CPU cores)
#' - `cli_options`: "" (any extra command-line flags for the wrapper)
#' - `sched_args`: "" (additional job scheduler directives)
#'
#' Users may also opt to remove large intermediate AROMA outputs after
#' completion via the `cleanup` flag. These NIfTI/JSON files are not required for applying AROMA
#' during postprocessing and can be deleted to save disk space. Cleanup is only
#' available when fMRIPrep output spaces do not include
#' `MNI152NLin6Asym:res-2`; if that space is later added, cleanup will be
#' skipped.
#' 
#' @keywords internal
setup_aroma <- function(scfg, fields = NULL) {
  defaults <- list(
    memgb = 32,
    nhours = 36,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  if (is.null(scfg$aroma$enable) || (isFALSE(scfg$aroma$enable) && any(grepl("aroma/", fields))) || ("aroma/enable" %in% fields)) {
    scfg$aroma$enable <- prompt_input(
      instruct = glue("
      ICA-AROMA (Independent Component Analysis-based Automatic Removal Of Motion Artifacts) is a data-driven
      method for identifying and removing motion-related independent components from BOLD fMRI data using 
      non-aggressive regression. It is designed to reduce motion artifacts without relying on motion estimates 
      from realignment parameters.

      As of fMRIPrep v24, ICA-AROMA has been removed from the core pipeline and is now available as part of a 
      standalone BIDS app called `fmripost-aroma`. If you enable this step, your data will be passed through 
      ICA-AROMA after fMRIPrep preprocessing is complete.

      Note: Enabling this step **does not** remove motion-related components from the data. Instead, it extracts 
      the AROMA noise components and prepares them for optional regression in a later postprocessing step.

      If you wish to run ICA-AROMA denoising on your BOLD data, answer 'Yes' here.
      "),
      heading = "ICA-AROMA",
      prompt = "Run ICA-AROMA?",
      type = "flag",
      default = if (is.null(scfg$aroma$enable)) TRUE else isTRUE(scfg$aroma$enable)
    )
  }

  if (isFALSE(scfg$aroma$enable)) return(scfg)
  
  # prompt for aroma container at this step if not already in fields
  if (!validate_exists(scfg$compute_environment$aroma_container) && !"compute_environment/aroma_container" %in% fields) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/aroma_container")
  }

  scfg <- setup_job(scfg, "aroma", defaults, fields)

  cleanup_possible <- is.null(scfg$fmriprep$output_spaces) ||
    !grepl("MNI152NLin6Asym:res-2", scfg$fmriprep$output_spaces, fixed = TRUE)

  if (cleanup_possible) {
    if (is.null(scfg$aroma$cleanup) || "aroma/cleanup" %in% fields) {
      scfg$aroma$cleanup <- prompt_input(
        instruct = glue(
          "\n      After running AROMA, large intermediate NIfTI and JSON files are produced.\n",
          "      These are not required for applying AROMA during postprocessing and can\n",
          "      be removed to save disk space while keeping the melodic outputs.\n",
          "      Cleanup will not occur if 'MNI152NLin6Asym:res-2' is included in fmriprep output spaces.\n"
        ),
        prompt = "Remove unnecessary AROMA outputs after completion?",
        type = "flag",
        default = TRUE
      )
    }
  } else {
    scfg$aroma$cleanup <- FALSE
  }

  return(scfg)
}

# Safely resolve a CLI on PATH without propagating the zero-length result from
# `command -v` when the executable is absent.
discover_cli_path <- function(command) {
  if (!checkmate::test_string(command)) return("")

  path <- tryCatch(
    suppressWarnings(system(sprintf("command -v %s", shQuote(command)), intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) character()
  )

  if (length(path) == 0L) return("")

  path <- trimws(path[[1L]])
  if (!nzchar(path)) return("")

  path
}


#' Setup the compute environment for a study
#' @param scfg a project configuration object, as produced by `load_project` or `setup_project`
#' @return a modified version of `scfg` with `$compute_environment` populated
#' @keywords internal
#' @importFrom checkmate assert_list
setup_compute_environment <- function(scfg = list(), fields = NULL) {
  checkmate::assert_list(scfg)

  # if empty, allow population from external file
  # scfg <- get_compute_environment_from_file(scfg) ## TODO: decide how to make this work

  # If fields is not null, then the caller wants to make specific edits to config. Thus, don't prompt for invalid settings for other fields.
  if (is.null(fields)) {
    fields <- c()
    if (!checkmate::test_subset(scfg$compute_environment$scheduler, c("slurm", "torque"), empty.ok=FALSE)) fields <- c(fields, "compute_environment/scheduler")
    if (isTRUE(scfg$fmriprep$enable) && !validate_exists(scfg$compute_environment$fmriprep_container)) fields <- c(fields, "compute_environment/fmriprep_container")
    if (isTRUE(scfg$bids_conversion$enable) && !validate_exists(scfg$compute_environment$heudiconv_container)) fields <- c(fields, "compute_environment/heudiconv_container")
    if (isTRUE(scfg$bids_validation$enable) && !validate_exists(scfg$compute_environment$bids_validator)) fields <- c(fields, "compute_environment/bids_validator")
    if (isTRUE(scfg$flywheel_sync$enable) && !validate_exists(scfg$compute_environment$flywheel)) fields <- c(fields, "compute_environment/flywheel")
    if (isTRUE(scfg$mriqc$enable) && !validate_exists(scfg$compute_environment$mriqc_container)) fields <- c(fields, "compute_environment/mriqc_container")
    if (isTRUE(scfg$aroma$enable) && !validate_exists(scfg$compute_environment$aroma_container)) fields <- c(fields, "compute_environment/aroma_container")
    if (isTRUE(scfg$postprocess$enable) && !validate_exists(scfg$compute_environment$fsl_container)) fields <- c(fields, "compute_environment/fsl_container")
  }

  if ("compute_environment/scheduler" %in% fields) {
    scfg$compute_environment$scheduler <- prompt_input("Scheduler (slurm/torque):",
      instruct = "BrainGnomes currently supports SLURM and TORQUE (qsub) clusters.",
      heading = "Compute environment",
      type = "character", len = 1L, among = c("slurm", "torque")
    )
  }

  if ("compute_environment/flywheel" %in% fields) {
    fw_path <- discover_cli_path("fw")
    if (nzchar(fw_path)) {
      use_fw <- prompt_input(
        instruct = glue("Found Flywheel CLI at {fw_path}"),
        prompt = "Use this to run flywheel?",
        type = "flag",
        default = TRUE
      )
      if (isTRUE(use_fw)) scfg$compute_environment$flywheel <- fw_path
    }
    if (!checkmate::test_file_exists(scfg$compute_environment$flywheel)) {
      default_fw <- scfg$compute_environment$flywheel
      if (!checkmate::test_file_exists(default_fw)) default_fw <- NULL

      scfg$compute_environment$flywheel <- prompt_input(
        instruct = if (nzchar(fw_path)) {
          "Specify the location of the Flywheel CLI (fw):"
        } else {
          paste(
            "Flywheel CLI (fw) was not found on your PATH.",
            "Install it or provide the full path to the executable."
          )
        },
        prompt = "Location of Flywheel CLI:",
        type = "file",
        default = default_fw
      ) |> normalizePath(mustWork = TRUE)
    }
  }

  # location of fmriprep container -- use normalizePath() to follow any symbolic links or home directory shortcuts
  if ("compute_environment/fmriprep_container" %in% fields) {
    scfg$compute_environment$fmriprep_container <- prompt_input(
      instruct = glue("\n
      The pipeline depends on having a working fmriprep container (docker or singularity).
      If you don't have this yet, follow these instructions first:
          https://fmriprep.org/en/stable/installation.html#containerized-execution-docker-and-singularity\n
      "),
      prompt = "Location of fmriprep container: ",
      type = "file",
      default = scfg$compute_environment$fmriprep_container
    ) |> normalizePath()
  }

  # location of heudiconv container
  if ("compute_environment/heudiconv_container" %in% fields) {
    scfg$compute_environment$heudiconv_container <- prompt_input(
      instruct = glue("
      \nBIDS conversion depends on having a working heudiconv container (docker or singularity).
      If you don't have this yet, follow these instructions first:
          https://heudiconv.readthedocs.io/en/latest/installation.html#install-container\n
      "),
      prompt = "Location of heudiconv container: ",
      type = "file",
      default = scfg$compute_environment$heudiconv_container
    ) |> normalizePath(mustWork = TRUE)
  }

  # location of bids-validator binary
  if ("compute_environment/bids_validator" %in% fields) {
    scfg$compute_environment$bids_validator <- prompt_input(
      instruct = glue("
      \nThe configured bids-validator can verify that resulting BIDS folders conform to the BIDS specification.
      This validation is submitted separately through run_bids_validation(). You can read more about validation here:
      https://bids-validator.readthedocs.io/en/stable/index.html.
      
      To configure BIDS validation with the project, specify the location of the bids-validator program here.
      Validation is submitted separately through run_bids_validation(), not as a run_project() stage.
      If you need help building this program, follow these instructions:
      https://bids-validator.readthedocs.io/en/stable/user_guide/command-line.html.\n
    "),
      prompt = "Location of bids-validator program: ",
      type = "file", default = scfg$compute_environment$bids_validator
    ) |> normalizePath(mustWork = TRUE)
  }

  # location of mriqc container
  if ("compute_environment/mriqc_container" %in% fields) {
    scfg$compute_environment$mriqc_container <- prompt_input(
      instruct = glue("\n
      The pipeline can use MRIQC to produce automated QC reports. This is suggested, but not required.
      If you'd like to use MRIQC, you need a working mriqc container (docker or singularity).
      If you don't have this yet, this should work to build the latest version:
          singularity build /location/to/mriqc-latest.simg docker://nipreps/mriqc:latest\n
      "),
      prompt = "Location of mriqc container: ",
      type = "file", default = scfg$compute_environment$mriqc_container
    ) |> normalizePath(mustWork = TRUE)
  }

  # location of ICA-AROMA fMRIprep container
  if ("compute_environment/aroma_container" %in% fields) {
    scfg$compute_environment$aroma_container <- prompt_input(
      instruct = glue("\n
      The pipeline can use ICA-AROMA to denoise fMRI timeseries. As described in Pruim et al. (2015), this
      is a data-driven step that produces a set of temporal regressors that are thought to be motion-related.
      If you would like to use ICA-AROMA in the pipeline, you need to build a singularity container of this
      workflow. Follow the instructions here: https://fmripost-aroma.readthedocs.io/latest/

      This is required if you say 'yes' to running AROMA during study setup.\n
      "),
      prompt = "Location of ICA-AROMA container: ",
      type = "file", default = scfg$compute_environment$aroma_container
    ) |> normalizePath(mustWork = TRUE)
  }

  # location of ICA-AROMA fMRIprep container
  if ("compute_environment/fsl_container" %in% fields) {
    scfg$compute_environment$fsl_container <- prompt_input(
      instruct = glue("\n
      Postprocessing uses FSL for a number of processing steps, including spatial smoothing and temporal filtering.
      If you plan to use postprocessing, you need to provide a Singularity image for FSL here.\n
      "),
      prompt = "Location of FSL container: ",
      type = "file", default = scfg$compute_environment$fsl_container
    ) |> normalizePath(mustWork = TRUE)
  }

  return(scfg)
}
