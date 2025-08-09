#' Load a project configuration from a file
#' @param input A path to a YAML file, or a project directory containing \code{project_config.yaml}.
#' @param validate Logical indicating whether to validate the configuration after loading. Default: TRUE
#' @return A list representing the project configuration (class `"bg_project_cfg"`). If `validate` is TRUE,
#'   the returned object is validated (missing fields may be set to NULL and noted).
#' @importFrom yaml read_yaml
#' @export
load_project <- function(input = NULL, validate = TRUE) {
  if (checkmate::test_directory_exists(input) && checkmate::test_file_exists(file.path(input, "project_config.yaml"))) {
    input <- file.path(input, "project_config.yaml") # if input is directory, look for project_config.yaml in it.
  }
  if (!checkmate::test_file_exists(input)) stop("Cannot find file: ", input)
  checkmate::test_flag(validate)
  scfg <- read_yaml(input)
  class(scfg) <- c(class(scfg), "bg_project_cfg") # add class to the object
  if (validate) scfg <- validate_project(scfg)

  # fill in any gaps in the config
  if (!is.null(attr(scfg, "gaps"))) scfg <- setup_project(scfg, fields = attr(scfg, "gaps"))
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

#' Setup the processing pipeline for a new fMRI study
#' @param input A `bg_project_cfg` object, a path to a YAML file, or a project
#'   directory containing \code{project_config.yaml}. If a directory is supplied
#'   but the file is missing, \code{setup_project} starts from an empty list with
#'   a warning. For \code{setup_project} only, this argument may also be
#'   \code{NULL} to create a new configuration from scratch.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return A `bg_project_cfg` list containing the project configuration. New fields are added based on user input,
#'   and missing entries are filled with defaults. The configuration is written
#'   to `project_config.yaml` in the project directory unless the user declines
#'   to overwrite an existing file.
#' @importFrom yaml read_yaml
#' @importFrom checkmate test_file_exists
#' @export
setup_project <- function(input = NULL, fields = NULL) {
  scfg <- get_scfg_from_input(input)

  if (!checkmate::test_class(scfg, "bg_project_cfg")) {
    class(scfg) <- c(class(scfg), "bg_project_cfg")
  }

  # run through configuration of each step
  scfg <- setup_project_metadata(scfg, fields)
  scfg <- setup_bids_conversion(scfg, fields)
  scfg <- setup_fmriprep(scfg, fields)
  scfg <- setup_mriqc(scfg, fields)
  scfg <- setup_aroma(scfg, fields)
  scfg <- setup_postprocess_streams(scfg, fields)
  scfg <- setup_bids_validation(scfg, fields)
  scfg <- setup_compute_environment(scfg, fields)

  scfg <- save_project_config(scfg)

  return(scfg)
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
  # If fields is not null, then the caller wants to make specific edits to config. Thus, don't prompt for invalid settings for other fields.
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$metadata$project_name)) fields <- c(fields, "metadata/project_name")
    if (is.null(scfg$metadata$project_directory)) fields <- c(fields, "metadata/project_directory")
    # if (is.null(scfg$metadata$dicom_directory)) fields <- c(fields, "metadata/dicom_directory") # defer to setup_bids_conversion
    if (is.null(scfg$metadata$templateflow_home)) fields <- c(fields, "metadata/templateflow_home")
    if (is.null(scfg$metadata$scratch_directory)) fields <- c(fields, "metadata/scratch_directory")
  }

  if ("metadata/project_name" %in% fields) {
    scfg$metadata$project_name <- prompt_input("What is the name of your project?", type = "character")
  }

  if ("metadata/project_directory" %in% fields) {
    scfg$metadata$project_directory <- prompt_input("What is the root directory where project files will be stored?", type = "character")
  }

  if (!checkmate::test_directory_exists(scfg$metadata$project_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$metadata$project_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$metadata$project_directory, recursive = TRUE)
  }

  if (!checkmate::test_directory_exists(scfg$metadata$project_directory, "r")) {
    warning(glue("You seem not to have read permission to: {scfg$metadata$project_directory}. This could cause problems in trying to run anything!"))
  }

  # location of DICOMs
  if ("metadata/dicom_directory" %in% fields) {
    scfg$metadata$dicom_directory <- prompt_input("Where are DICOM files stored?", type = "character")

    if (!checkmate::test_directory_exists(scfg$metadata$dicom_directory)) {
      create <- prompt_input(instruct = glue("The directory {scfg$metadata$dicom_directory} does not exist. Would you like me to create it?\n"), type = "flag")
      if (create) dir.create(scfg$metadata$dicom_directory, recursive = TRUE)
    }
  }


  # location of BIDS data -- enforce that this must be within the project directory with a fixed name
  scfg$metadata$bids_directory <- file.path(scfg$metadata$project_directory, "data_bids")
  if (!checkmate::test_directory_exists(scfg$metadata$bids_directory)) {
    # default to creating the directory
    # create <- prompt_input(instruct = glue("The directory {scfg$metadata$bids_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    create <- TRUE
    if (create) dir.create(scfg$metadata$bids_directory, recursive = TRUE) # should probably force this to happen
  }

  if ("metadata/scratch_directory" %in% fields) {
    scfg$metadata$scratch_directory <- prompt_input("Work directory: ",
      instruct = glue("\n\n
      fmriprep uses a lot of disk space for processing intermediate files. It's best if these
      are written to a scratch/temporary directory that is cleared regularly so that you don't
      use up precious disk space for unnecessary files. Please indicate where these intermediate
      file should be written.\n
      "), type = "character"
    )

    if (!checkmate::test_directory_exists(scfg$metadata$scratch_directory)) {
      create <- prompt_input(instruct = glue("The directory {scfg$metadata$scratch_directory} does not exist. Would you like me to create it?\n"), type = "flag")
      if (create) dir.create(scfg$metadata$scratch_directory, recursive = TRUE)
    }
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

  if (!checkmate::test_directory_exists(scfg$metadata$templateflow_home)) {
    create <- prompt_input(instruct = glue("The directory {scfg$metadata$templateflow_home} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$metadata$templateflow_home, recursive = TRUE)
  }

  # singularity bind paths are unhappy with symbolic links and ~/ notation
  scfg$metadata$templateflow_home <- normalizePath(scfg$metadata$templateflow_home)

  scfg$metadata$log_directory <- file.path(scfg$metadata$project_directory, "logs")
  if (!checkmate::test_directory_exists(scfg$metadata$log_directory)) dir.create(scfg$metadata$log_directory, recursive = TRUE)

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

  if (is.null(scfg$fmriprep$enable) || (isFALSE(scfg$fmriprep$enable) && any(grepl("fmriprep/", fields)))) {
    scfg$fmriprep$enable <- prompt_input(
      instruct = glue("\n\n
      -----------------------------------------------------------------------------------------------------------------
      fMRIPrep is a standardized and robust tool for preprocessing BIDS-organized functional and anatomical MRI data.
      It performs essential steps such as motion correction, susceptibility distortion correction, tissue segmentation,
      coregistration, normalization to standard space, and estimation of nuisance regressors.

      Running fMRIPrep is typically a required step before any model-based analysis of fMRI data.

      You will have the option to specify output spaces (e.g., MNI152NLin2009cAsym, T1w) and provide 
      a FreeSurfer license file, which is necessary for anatomical processing. You can also pass custom CLI
      options and schedule settings.\n\n
      "),
      prompt = "Do you want to include fMRIPrep as part of your preprocessing pipeline?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(scfg$fmriprep$enable)) return(scfg)

  # location of fmriprep outputs -- enforce that this must be within the project directory
  scfg$metadata$fmriprep_directory <- file.path(scfg$metadata$project_directory, "data_fmriprep")
  if (!checkmate::test_directory_exists(scfg$metadata$fmriprep_directory)) {
    dir.create(scfg$metadata$fmriprep_directory, recursive = TRUE)
  }

  # prompt for fmriprep container at this step
  if (!validate_exists(scfg$compute_environment$fmriprep_container)) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/fmriprep_container")
  }

  scfg <- setup_job(scfg, "fmriprep", defaults, fields)

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
      The license file is might be called FreeSurferLicense.txt and is available from the FreeSurfer website.
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

  if (is.null(scfg$bids_validation$enable) || (isFALSE(scfg$bids_validation$enable) && any(grepl("bids_validation/", fields)))) {
    scfg$bids_validation$enable <- prompt_input(
      instruct = glue("\n\n
      -----------------------------------------------------------------------------------------------------------------
      BIDS validation checks whether your dataset adheres to the Brain Imaging Data Structure (BIDS) standard.
      This can be helpful for ensuring that all filenames, metadata, and required files follow expected conventions.
      It can identify missing fields, naming issues, or formatting problems that could cause downstream tools to fail.
      
      The validator can be run quickly, and produces an HTML report that summarizes any warnings or errors.
      Note: Even if you say 'no' here, fmriprep will run BIDS validation on each subject's dataset prior to execution.
      
      Saying 'Yes' to this step will enable BIDS validation, but that step must be run separately, at your leisure,
      using run_bids_validation().
      "),
      prompt = "Enable BIDS validation?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(scfg$bids_validation$enable)) return(scfg)

  # prompt for BIDS validator at this point
  if (!validate_exists(scfg$compute_environment$bids_validator)) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/bids_validator")
  }

  scfg <- setup_job(scfg, "bids_validation", defaults, fields)

  if (is.null(scfg$bids_validation$outfile) || "bids_validation/outfile" %in% fields) {
    scfg$bids_validation$outfile <- prompt_input(
      instruct = glue("\n
      What should be the name of the output file created by bids_validator? The default is bids_validator_output.html
      You can also include the subject and session IDs in the filename by using the following
      placeholders: {{sub_id}} and {{ses_id}}. For example, bids_validation_sub-{{sub_id}}_ses-{{ses_id}}.html will substitute
      the subject and session IDs into the filename. This is useful if you want to place the output files in a common
      directory for all subjects and sessions, but still want to be able to identify which file belongs to which subject.
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

  if (is.null(scfg$mriqc$enable) || (isFALSE(scfg$mriqc$enable) && any(grepl("mriqc/", fields)))) {
    scfg$mriqc$enable <- prompt_input(
      instruct = glue("\n\n
      -----------------------------------------------------------------------------------------------------------------
      MRIQC is a tool for automated quality assessment of structural and functional MRI data. 
      It calculates a wide array of image quality metrics (IQMs) for each scan, such as signal-to-noise ratio, 
      motion estimates, and image sharpness. It also produces visual reports to help you identify 
      scans with artifacts, excessive motion, or other issues that might compromise analysis.

      Running MRIQC is a recommended step, as it can help you detect problematic scans early 
      and guide decisions about inclusion, exclusion, or further inspection.

      MRIQC supports both group-level and individual-level analyses and produces HTML reports and TSV files.
      Saying 'Yes' here only runs the individual-level QC checks on each dataset.\n\n
      "),
      prompt = "Run MRIQC?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(scfg$mriqc$enable)) return(scfg)

  # location of mriqc reports -- enforce that this must be within the project directory
  scfg$metadata$mriqc_directory <- file.path(scfg$metadata$project_directory, "mriqc_reports")
  if (!checkmate::test_directory_exists(scfg$metadata$mriqc_directory)) {
    dir.create(scfg$metadata$mriqc_directory, recursive = TRUE)
  }

  # prompt for mriqc container at this step
  if (!validate_exists(scfg$compute_environment$mriqc_container)) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/mriqc_container")
  }

  scfg <- setup_job(scfg, "mriqc", defaults, fields)

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

  if (is.null(scfg$bids_conversion$enable) || (isFALSE(scfg$bids_conversion$enable) && any(grepl("bids_conversion/", fields)))) {
    scfg$bids_conversion$enable <- prompt_input(
      instruct = glue("\n\n
      -----------------------------------------------------------------------------------------------------------------
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
      please see some examples here: https://github.com/nipy/heudiconv/tree/master/heudiconv/heuristics.\n\n
      "),
      prompt = "Run BIDS conversion?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(scfg$bids_conversion$enable)) return(scfg)

  # prompt for heudiconv container at this step
  if (!validate_exists(scfg$compute_environment$heudiconv_container)) {
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
      If you have multisession data, specify the the regex pattern for session IDs within the subject folders.
      If you don't have multisession data, just press Enter to skip this step.
    ", .trim = FALSE),
      prompt = "What is the regex pattern for the session IDs?",
      type = "character", required = FALSE
    )
  }

  if (!is.na(scfg$bids_conversion$ses_regex) && (is.null(scfg$bids_conversion$ses_id_match) || "bids_conversion/ses_id_match" %in% fields)) {
    scfg$bids_conversion$ses_id_match <- prompt_input(
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
  } else {
    scfg$bids_conversion$ses_id_match <- NA_character_
  }


  if (is.null(scfg$bids_conversion$heuristic_file) || "bids_conversion/heuristic_file" %in% fields) {
    scfg$bids_conversion$heuristic_file <- prompt_input(instruct = glue("What is the location of the heudiconv heuristic file?"), type = "file")
  }

  if (is.null(scfg$bids_conversion$overwrite) || "bids_conversion/overwrite" %in% fields) {
    scfg$bids_conversion$overwrite <- prompt_input(instruct = glue("Should existing BIDS files be overwritten by heudiconv?"), type = "flag", default = TRUE)
  }

  if (is.null(scfg$bids_conversion$clear_cache) || "bids_conversion/clear_cache" %in% fields) {
    scfg$bids_conversion$clear_cache <- prompt_input(
      instruct = glue("Heudiconv caches its matching results inside the root of the BIDS folder in a hidden
      directory called .heudiconv. This provides a record of what heudiconv did for each subject conversion.
      It also speeds up conversions in future if you reprocess data. That said, if you modify the heuristic file,
      the cache can interfere because it will use the old heuristic file to match DICOMs to BIDS.
      If you want to clear the cache, say 'yes' here. If you want to keep the cache, say 'no'.
      ", .trim = FALSE),
      prompt = glue("Should the heudiconv cache be cleared?"),
      type = "flag", default = FALSE
    )
  }

  return(scfg)
}

#' Configure ICA-AROMA denoising
#'
#' This function configures the ICA-AROMA (Independent Component Analysis-based Automatic Removal Of Motion Artifacts)
#' step for post-fMRIPrep processing. It sets scheduling and resource parameters that will be used to apply
#' AROMA-based denoising to BOLD fMRI data using FSL's `fsl_regfilt` or an equivalent wrapper.
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
#' @keywords internal
setup_aroma <- function(scfg, fields = NULL) {
  defaults <- list(
    memgb = 32,
    nhours = 36,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  if (is.null(scfg$aroma$enable) || (isFALSE(scfg$aroma$enable) && any(grepl("aroma/", fields)))) {
    scfg$aroma$enable <- prompt_input(
      instruct = glue("\n\n
      -----------------------------------------------------------------------------------------------------------------
      ICA-AROMA (Independent Component Analysis-based Automatic Removal Of Motion Artifacts) is a data-driven
      method for identifying and removing motion-related independent components from BOLD fMRI data using 
      non-aggressive regression. It is designed to reduce motion artifacts without relying on motion estimates 
      from realignment parameters.

      As of fMRIPrep v24, ICA-AROMA has been removed from the core pipeline and is now available as part of a 
      standalone BIDS app called `fmripost-aroma`. If you enable this step, your data will be passed through 
      ICA-AROMA after fMRIPrep preprocessing is complete.

      Note: Enabling this step **does not** remove motion-related components from the data. Instead, it extracts 
      the AROMA noise components and prepares them for optional regression in a later postprocessing step.

      If you wish to run ICA-AROMA denoising on your BOLD data, answer 'Yes' here.\n
      "),
      prompt = "Run ICA-AROMA?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(scfg$aroma$enable)) return(scfg)
  
  # prompt for aroma container at this step
  if (!validate_exists(scfg$compute_environment$aroma_container)) {
    scfg <- setup_compute_environment(scfg, fields="compute_environment/aroma_container")
  }

  scfg <- setup_job(scfg, "aroma", defaults, fields)

  return(scfg)
}


get_compute_environment_from_file <- function(scfg) {
  if (length(scfg) > 0L && !"compute_environment" %in% names(scfg)) {
    cat(glue("
      No compute environment settings were found in your configuration. This includes settings such
      as the location of the fmriprep container or the location of heudiconv.
      ", .trim = FALSE))

    from_file <- prompt_input("Would you like to import these from an existing YAML file?", type = "flag")
    if (from_file) {
      file_loc <- prompt_input("Specify the file location (or press Enter to cancel):", type = "file", len = 1L)
      if (!is.na(file_loc)) {
        ff <- read_yaml(file_loc)
        possible_fields <- c(
          "fmriprep_container", "heudiconv_container", "mriqc_container",
          "aroma_container", "scheduler"
        )

        if ("compute_environment" %in% names(ff)) {
          scfg$compute_environment <- ff$compute_environment
        } else if (any(possible_fields %in% names(ff))) {
          # config has the settings as the top layer (malformed, but okay)
          for (field in possible_fields) {
            scfg$compute_environment[[field]] <- ff[[field]]
          }
        } else {
          warning("Could not find any compute environment information in file: ", file_loc)
        }
      }
    }
  }

  return(scfg)
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
    if (isTRUE(scfg$mriqc$enable) && !validate_exists(scfg$compute_environment$mriqc_container)) fields <- c(fields, "compute_environment/mriqc_container")
    if (isTRUE(scfg$aroma$enable) && !validate_exists(scfg$compute_environment$aroma_container)) fields <- c(fields, "compute_environment/aroma_container")
  }

  if ("compute_environment/scheduler" %in% fields) {
    scfg$compute_environment$scheduler <- prompt_input("Scheduler (slurm/torque): ",
      instruct = "The pipeline currently runs on TORQUE (aka qsub) and SLURM clusters.\nWhich will you use?",
      type = "character", len = 1L, among = c("slurm", "torque")
    )
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
      \nAfter BIDS conversion, the pipeline can pass resulting BIDS folders to bids-validator to verify that 
      the folder conforms to the BIDS specification. You can read more about validtion here: 
      https://bids-validator.readthedocs.io/en/stable/index.html.
      
      If you'd like to include BIDS validation in the processing pipeline, specify the location of the 
      bids-validator program here. If you need help building this program, follow these instructions: 
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
      The pipeline can use ICA-AROMA to denoise fMRI timeseries. As descried in Pruim et al. (2015), this
      is a data-driven step that produces a set of temporal regressors that are thought to be motion-related.
      If you would like to use ICA-AROMA in the pipeline, you need to build a singularity container of this
      workflow. Follow the instructions here: https://fmripost-aroma.readthedocs.io/latest/

      This is required if you say 'yes' to running AROMA during study setup.\n
      "),
      prompt = "Location of ICA-AROMA container: ",
      type = "file", default = scfg$compute_environment$aroma_container
    ) |> normalizePath(mustWork = TRUE)
  }

  return(scfg)
}

