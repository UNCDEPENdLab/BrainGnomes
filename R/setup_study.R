#' Load a study configuration from a file
#' @param file A YAML file containing a valid study configuration.
#' @param validate Logical indicating whether to validate the configuration after loading. Default: TRUE
#' @return A list representing the study configuration (class `"bg_study_cfg"`). If `validate` is TRUE, 
#'   the returned object is validated (missing fields may be set to NULL and noted).
#' @importFrom yaml read_yaml
#' @export
load_study <- function(file = NULL, validate=TRUE) {
  if (!checkmate::test_file_exists(file)) stop("Cannot find file: ", file)
  checkmate::test_flag(validate)
  scfg <- read_yaml(file)
  class(scfg) <- c(class(scfg), "bg_study_cfg") # add class to the object
  if (validate) scfg <- validate_study(scfg)

  # fill in any gaps in the config
  if (!is.null(attr(scfg, "gaps"))) scfg <- setup_study(scfg, fields = attr(scfg, "gaps"))
  return(scfg)
}

#' summary method for study configuration object
#' @param x The study configuration object (`bg_study_cfg`) to summarize.
#' @return Invisibly returns `x` after printing its contents. This function is called 
#'   for its side effect of printing a formatted summary of the study configuration.
#' @export
summary.bg_study_cfg <- function(x) {
  pretty_print_list(x, indent=2)
}

#' Setup the processing pipeline for a new fMRI study
#' @param input An existing `bg_study_cfg` object to be modified, or a string specifying the location of an existing configuration YAML file to load.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return A `bg_study_cfg` list containing the study configuration. New fields are added based on user input, 
#'   and missing entries are filled with defaults.
#' @importFrom yaml read_yaml
#' @importFrom checkmate test_file_exists
#' @export
setup_study <- function(input = NULL, fields = NULL) {
  if (checkmate::test_string(input) && checkmate::test_file_exists(input)) {
    scfg <- load_study(input, validate=FALSE)
  } else if (inherits(input, "bg_study_cfg")) {
    scfg <- input
  } else if (!is.null(input)) {
    stop("input must be a bg_study_cfg object or a string specifying the location of a YAML file")
  } else {
    scfg <- list()
  }

  if (!checkmate::test_class(scfg, "bg_study_cfg")) {
    class(scfg) <- c(class(scfg), "bg_study_cfg")
  }

  # If fields is not null, then the caller wants to make specific edits to config. Thus, don't prompt for invalid settings for other fields.
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$project_name)) fields <- c(fields, "project_name")
    if (is.null(scfg$project_directory)) fields <- c(fields, "project_directory")
    if (is.null(scfg$dicom_directory)) fields <- c(fields, "dicom_directory")
    if (is.null(scfg$templateflow_home)) fields <- c(fields, "templateflow_home")
    if (is.null(scfg$scratch_directory)) fields <- c(fields, "scratch_directory")
    if (is.null(scfg$log_txt)) fields <- c(fields, "log_txt")
  }

  if (is.null(scfg$run_aroma) || "run_aroma" %in% fields) {
    scfg$run_aroma <- prompt_input("Run AROMA?",
      instruct = glue("
      \nAs of v24, fmriprep has now removed ICA-AROMA from its codebase, splitting this off to
      a separate BIDS app called fmripost-aroma. Do you want to run the fMRI data through ICA-AROMA?
      If so, you will subsequently be asked for the location of an ICA-AROMA container file. Note that
      saying 'yes' to this, only runs AROMA, but does not remove motion-related components from the fMRI
      timeseries. That is a postprocessing decision, which you will be asked about in that context.\n
      "), type = "flag"
    )
  }

  # run through configuration of each step
  scfg <- setup_study_globals(scfg, fields)
  scfg <- setup_compute_environment(scfg, fields)
  scfg <- setup_bids_conversion(scfg, fields)
  scfg <- setup_bids_validation(scfg, fields)
  scfg <- setup_fmriprep(scfg, fields)
  scfg <- setup_mriqc(scfg, fields)
  if (isTRUE(scfg$run_aroma)) scfg <- setup_aroma(scfg, fields)
  scfg <- setup_postprocess(scfg, fields)

  return(scfg)
}

setup_study_globals <- function(scfg = NULL, fields = NULL) {
  if ("project_name" %in% fields) {
    scfg$project_name <- prompt_input("What is the name of your project?", type = "character")
  }

  if ("project_directory" %in% fields) {
    scfg$project_directory <- prompt_input("What is the root directory where project files will be stored?", type = "character")
  }

  if (!checkmate::test_directory_exists(scfg$project_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$project_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$project_directory, recursive = TRUE)
  }

  if (!checkmate::test_directory_exists(scfg$project_directory, "r")) {
    warning(glue("You seem not to have read permission to: {scfg$project_directory}. This could cause problems in trying to run anything!"))
  }

  # location of DICOMs
  # /nas/longleaf/home/willasc/repos/clpipe/tests/temp/clpipe_dir0/data_DICOMs
  if ("dicom_directory" %in% fields) {
    scfg$dicom_directory <- prompt_input("Where are DICOM files files stored?", type = "character")
  }

  if (!checkmate::test_directory_exists(scfg$dicom_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$dicom_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$dicom_directory, recursive = TRUE)
  }

  # location of BIDS data -- enforce that this must be within the project directory with a fixed name
  scfg$bids_directory <- file.path(scfg$project_directory, "data_bids")
  if (!checkmate::test_directory_exists(scfg$bids_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$bids_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$bids_directory, recursive = TRUE) # should probably force this to happen
  }

  # location of fmriprep outputs -- enforce that this must be within the project directory
  scfg$fmriprep_directory <- file.path(scfg$project_directory, "data_fmriprep")
  if (!checkmate::test_directory_exists(scfg$fmriprep_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$fmriprep_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$fmriprep_directory, recursive = TRUE) # should probably force this to happen
  }

  # location of mriqc reports -- enforce that this must be within the project directory
  scfg$mriqc_directory <- file.path(scfg$project_directory, "mriqc_reports")
  if (!checkmate::test_directory_exists(scfg$mriqc_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$mriqc_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$mriqc_directory, recursive = TRUE) # should probably force this to happen
  }

  if ("scratch_directory" %in% fields) {
    scfg$scratch_directory <- prompt_input("Work directory: ",
      instruct = glue("
      \nfmriprep uses a lot of disk space for processing intermediate files. It's best if these
      are written to a scratch/temporary directory that is cleared regularly so that you don't
      use up precious disk space for unnecessary files. Please indicate where these intermediate
      file should be written.\n
      "), type = "character"
    )
  }

  if (!checkmate::test_directory_exists(scfg$scratch_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$scratch_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$scratch_directory, recursive = TRUE)
  }

    if ("templateflow_home" %in% fields) {
    scfg$templateflow_home <- prompt_input("Templateflow directory: ",
      instruct = glue("
      \nThe pipeline uses TemplateFlow to download and cache templates for use in fMRI processing.
      Please specify the location of the TemplateFlow cache directory. The default is $HOME/.cache/templateflow.
      You can also point to a different location if you have a shared cache directory for multiple users.
      "), type = "character", default = file.path(Sys.getenv("HOME"), ".cache", "templateflow")
    )
  }

  if (!checkmate::test_directory_exists(scfg$templateflow_home)) {
    create <- prompt_input(instruct = glue("The directory {scfg$templateflow_home} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$templateflow_home, recursive = TRUE)
  }

  # logging
  if ("log_txt" %in% fields) {
    scfg$log_txt <- prompt_input("Create subject-level logs?",
      instruct = glue("
      The package can write plain-text logs to each subject's sub-<id> directory.
      These contain messages related to job submission and job status.
      We strongly recommend these for tracking and debugging purposes.
      ", .trim = FALSE), type = "flag"
    )
  }

  scfg$log_directory <- file.path(scfg$project_directory, "logs")
  if (!checkmate::test_directory_exists(scfg$log_directory)) dir.create(scfg$log_directory, recursive = TRUE)

  return(scfg)
}

#' Specify the fMRIPrep settings                             
#' @param scfg A study configuration object, as produced by `load_study()` or `setup_study()`.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all fMRIPrep fields will be prompted for.
#' @return A modified version of `scfg` with the `$fmriprep` entry populated.
#' @keywords internal
setup_fmriprep <- function(scfg = NULL, fields = NULL) {
  # https://fmriprep.org/en/stable/usage.html
  # [--omp-nthreads OMP_NTHREADS] [--mem MEMORY_MB] [--low-mem]  [--nprocs NPROCS]
  defaults <- list(
    memgb = 48,
    nhours = 24,
    ncores = 12,
    cli_options = "",
    sched_args = ""
  )  

  scfg <- setup_job(scfg, "fmriprep", defaults, fields)

  # If fields is not null, then the caller wants to make specific edits to config. Thus, don't prompt for invalid settings for other fields.
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$fmriprep$output_spaces)) fields <- c(fields, "fmriprep/output_spaces")
    if (!validate_exists(scfg$fmriprep$fs_license_file)) fields <- c(fields, "fmriprep/fs_license_file")

    cat("This step sets up fmriprep.  (For details, see https://fmriprep.org/en/stable/usage.html)\n")
  }

  if ("fmriprep/output_spaces" %in% fields) {
    scfg$fmriprep$output_spaces <- choose_fmriprep_spaces(scfg$fmriprep$output_spaces)
  }

  if ("fmriprep/fs_license_file" %in% fields) {
    scfg$fmriprep$fs_license_file <- prompt_input(
      instruct = glue("
      \nWhat is the location of your FreeSurfer license file? This is required for fmriprep to run.
      The license file is might be called FreeSurferLicense.txt and is available from the FreeSurfer website.
      https://surfer.nmr.mgh.harvard.edu/fswiki/License
      \n
    "),
      prompt = "What is the location of your FreeSurfer license file?",
      type = "file"
    )
  }

  return(scfg)

}

# edit_study <- function(scfg) {
#   assert_class(scfg, "bg_study_cfg")

#   config_areas <- list(
#     # top-level
#     General = list(
#       setup_fn = setup_study, # DONE
#       path = "",
#       fields = c(
#         "project_name", "project_directory", "dicom_directory", "bids_directory", 
#         "scratch_directory", "fmriprep_directory", "mriqc_directory", "log_directory", "templateflow_home"
#       )
#     ),
#     Job_Settings = list(
#       setup_fn = setup_job,
#       # job_name -- additional argument to pass into setup_job
#       path="depends",
#       fields = c("memgb", "nhours", "ncores", "cli_options", "sched_args")
#     ),
#     BIDS_conversion = list( # DONE
#       setup_fn = setup_bids_conversion,
#       path = "heudiconv",
#       fields = c(
#         "heuristic_file", "sub_regex", "sub_id_match", "ses_regex", "ses_id_match",
#         "overwrite", "clear_cache", "validate_bids"
#       )
#     ),
#     BIDS_validation = list(
#       setup_fn = setup_bids_validation,
#       path = "bids_validation",
#       fields = c("outfile")
#     ),
#     fMRIPrep = list(
#       setup_fn = setup_fmriprep,
#       path = "fmriprep",
#       fields = c("output_spaces", "fs_license_file")
#     ),
#     MRIQC = list(
#       setup_fn = setup_mriqc,
#       path = "mriqc",
#       fields = c("run_group_level")
#     ),
#     # AROMA = list(
#     #   setup_fn = setup_aroma,
#     #   path = "aroma",
#     #   fields = c("use_aroma")
#     # ),
#     Postprocessing = list(
#       setup_fn = setup_postprocess,
#       path = "postprocess",
#       fields = c("apply_mask", "brain_mask", "keep_intermediates", "force_processing_order",
#       "input_regex", "spatial_smooth/fwhm_mm", "apply_aroma/nonaggressive",
#       "temporal_filter/low_pass_hz", "temporal_filter/high_pass_hz", "intensity_normalize",
#       "confound_calculate", "confound_regress")
#     ),
#     Compute = list( # DONE
#       setup_fn = setup_compute_environment,
#       path = "compute_environment",
#       fields = c(
#         "scheduler", "fmriprep_container", "heudiconv_container", 
#         "bids_validator", "mriqc_container", "aroma_container"
#       )
#     )
#   )

#   top_options <- c(names(config_areas), "Quit")

#   repeat {
#     cat("\nWhich configuration area would you like to edit?\n")
#     area_idx <- utils::menu(sub("_", " ", top_options), title = "Select a configuration domain:")

#     if (area_idx == 0 || area_idx > length(config_areas)) {
#       message("Exiting configuration editor.")
#       break
#     }

#     area_name <- names(config_areas)[area_idx]
#     area_info <- config_areas[[area_name]]
#     full_path <- area_info$path
#     field_names <- area_info$fields

#     # Retrieve current values to show in the menu
#     get_current_value <- function(field) {
#       path_parts <- if (full_path != "") c(full_path, field) else field
#       val <- tryCatch(
#         purrr::pluck(scfg, !!!path_parts),
#         error = function(e) NULL
#       )
#       if (is.null(val)) return("[NULL]")
#       if (is.logical(val)) return(ifelse(val, "TRUE", "FALSE"))
#       if (is.character(val) && length(val) > 1) return(paste0(val, collapse = ", "))
#       as.character(val)
#     }

#     field_display <- purrr::map_chr(field_names, function(f) {
#       val <- get_current_value(f)
#       sprintf("%s [%s]", f, val)
#     })

#     selected <- utils::select.list(
#       choices = field_display,
#       multiple = TRUE,
#       title = glue::glue("Select fields to modify in {area_name}:")
#     )

#     if (length(selected) == 0) {
#       message("No fields selected. Returning to main menu.")
#       next
#     }

#     # Strip brackets to recover true field names
#     selected_fields <- sub(" \\[.*\\]$", "", selected)

#     passthrough <- area_info[setdiff(c("setup_fn", "path", "fields"), names(area_info))]

#     # Run setup function on those fields
#     browser()
#     scfg <- do.call(area_info$setup_fn, list(scfg = scfg, fields = selected_fields, passthrough))
#   }

#   return(scfg)
# }

#' Interactively edit a study configuration by field (field-guided)
#'
#' Allows the user to interactively browse and edit individual fields within the
#' configuration object, grouped by domain. Field paths are defined within the function
#' to avoid relying on a complete `scfg` structure.
#'
#' @param scfg A `bg_study_cfg` object representing the study configuration.
#' @return An updated `bg_study_cfg` object.
#' @export
edit_study <- function(scfg) {
  checkmate::assert_class(scfg, "bg_study_cfg")

  # Define editable fields per setup function
  config_map <- list(
    "General" = list(setup_fn = setup_study_globals, prefix = "", fields = c(
      "project_name", "project_directory", "dicom_directory", "bids_directory", "scratch_directory", "templateflow_home", "log_txt"
    )),
    "Compute Environment" = list(setup_fn = setup_compute_environment, prefix = "compute_environment/", fields = c(
      "scheduler", "fmriprep_container", "heudiconv_container", "bids_validator", "mriqc_container", "aroma_container"
    )),
    "BIDS Conversion" = list(setup_fn = setup_bids_conversion, prefix = "bids_conversion/", fields = c(
      "sub_regex", "sub_id_match", "ses_regex", "ses_id_match",
      "heuristic_file", "overwrite", "clear_cache", "validate_bids"
    )),
    "BIDS Validation" = list(setup_fn = setup_bids_validation, prefix = "bids_validation/", fields = c(
      "outfile"
    )),
    "fMRIPrep" = list(setup_fn = setup_fmriprep, prefix = "fmriprep/", fields = c(
      "output_spaces", "fs_license_file"
    )),
    "MRIQC" = list(setup_fn = setup_mriqc, prefix = "mriqc/", fields = character(0)),
    "ICA-AROMA" = list(setup_fn = setup_aroma, prefix = "aroma/", fields = character(0)),
    "Postprocessing" = list(setup_fn = setup_postprocess, prefix = "postprocess/", fields = c(
      "apply_mask", "brain_mask", "tr", "processing_steps", "input_regex",
      "keep_intermediates", "force_processing_order",
      "spatial_smooth/fwhm_mm", "apply_aroma/nonaggressive",
      "temporal_filter/low_pass_hz", "temporal_filter/high_pass_hz",
      "intensity_normalize/global_median",
      "confound_calculate", "confound_regress"

    ))
  )

  job_targets <- c("bids_conversion", "bids_validation", "fmriprep", "mriqc", "aroma", "postprocess")
  job_fields <- c("memgb", "nhours", "ncores", "cli_options", "sched_args")

  show_val <- function(val) {
    if (is.null(val)) "[NULL]"
    else if (is.logical(val)) toupper(as.character(val))
    else if (is.character(val) && length(val) > 1) paste(val, collapse = ", ")
    else as.character(val)
  }

  # Top-level menu loop
  repeat {
    choice <- utils::menu(c(names(config_map), "Job settings", "Quit"),
                          title = "Select a configuration area to edit:")

    if (choice == 0 || choice > length(config_map) + 1) {
      message("Exiting configuration editor.")
      break
    }

    if (choice <= length(config_map)) {
      # Standard config section
      area <- names(config_map)[choice]
      info <- config_map[[area]]
      prefix <- info$prefix
      fields <- info$fields
      setup_fn <- info$setup_fn

      if (length(fields) == 0) {
        message(glue::glue("No individual fields listed for {area}, opening full setup..."))
        scfg <- setup_fn(scfg)
        next
      }

      field_display <- sapply(fields, function(fld) {
        val <- get_nested_values(scfg, paste0(prefix, fld))
        sprintf("%s [ %s ]", fld, show_val(val))
      })

      selected <- utils::select.list(field_display, multiple = TRUE,
                                     title = glue::glue("Select fields to edit in {area}:"))

      if (length(selected) == 0) next

      scfg <- setup_fn(scfg, fields = paste0(prefix, names(selected)))

    } else {
      # Job settings logic
      job <- utils::select.list(job_targets, title = "Select which job to configure:")
      if (length(job) == 0 || job == "") next

      job_field_display <- sapply(job_fields, function(fld) {
        val <- get_nested_values(scfg, paste0(job, "/", fld))
        sprintf("%s [ %s ]", fld, show_val(val))
      })

      selected_job_fields <- utils::select.list(job_field_display, multiple = TRUE,
                                                title = glue::glue("Select fields to edit for job '{job}':"))

      if (length(selected_job_fields) == 0) next

      scfg <- setup_job(scfg, job_name = job, fields = paste(job, names(selected_job_fields), sep = "/"))
    }
  }

  return(scfg)
}

#' Specify the BIDS validation settings
#' @param scfg A study configuration object, as produced by `load_study()` or `setup_study()`.
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

  scfg <- setup_job(scfg, "bids_validation", defaults, fields)

  if (is.null(scfg$bids_validation$outfile) || "bids_validation/outfile" %in% fields) {
    scfg$bids_validation$outfile <- prompt_input(
      instruct = glue("
      \nWhat should be the name of the output file created by bids_validator? The default is bids_validator_output.html
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
#' @param scfg A study configuration object, as produced by `load_study()` or `setup_study()`.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all MRIQC fields will be prompted for.   # [Added]
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

  # scfg$mriqc <- populate_defaults(scfg$fmriprep, defaults)
  scfg <- setup_job(scfg, "mriqc", defaults, fields)

  return(scfg)
}

#' Specify the BIDS conversion settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @param print_instructions Logical. If TRUE, print the overall instructions for this step prior to prompting for input.
#' @return a modified version of `scfg` with `$bids_conversion` populated
#' @keywords internal
setup_bids_conversion <- function(scfg, fields = NULL, print_instructions = TRUE) {
  defaults <- list(
    memgb = 16,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  if (print_instructions) {
    cat(glue("
    \nThis step sets up DICOM to BIDS conversion using heudiconv. Heudiconv uses a heuristic
    file to match DICOM files to expected scans, allowing the tool to convert DICOMs to NIfTI images
    and reorganize them into BIDS format.

    The heuristic file is a python script that tells heudiconv how to convert the DICOM files
    to BIDS format. For details, see https://heudiconv.readthedocs.io/en/latest/usage.html

    Here, you will be asked for the location of the folder containing DICOM images for all subjects.
    This should be a folder that contains subfolders for each subject, with the DICOM files inside
    those subfolders. For example, if you have a folder called 'data_DICOMs' that contains
    subfolders 'sub-001', 'sub-002', etc., then you would specify the location of 'data_DICOMs' here.

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
    ├── sub-01/
    │   ├── ses-01/
    │   │   ├── 1.dcm
    │   │   ├── 2.dcm
    │   └── ses-02/
    │       ├── 1.dcm
    │       ├── 2.dcm

    You will also be asked for the location of the heuristic file. If you don't have a heuristic file,
    please see some examples here: https://github.com/nipy/heudiconv/tree/master/heudiconv/heuristics.\n\n
    "))
  }

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
      instruct = glue("
      \nWhat is the regex pattern for extracting the ID from the subject folder name? You
      can use multiple capturing groups if the ID has multiple parts. The default is ([0-9]+),
      which extracts the first number-like sequence from the folder name. For example, if your
      subject folder is named 'sub-001', the ID will be '001'. If your subject folder is named
      '001', the ID will be '001'. If the entire folder name is the subject ID, such as '001ra_2May2024',
      the id matching expression should be (.+), which matches all characters in the folder name.\n
    "),
      prompt = "What is the regex pattern for extracting the subject ID from the folder name?",
      type = "character", default = "(.+)"
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
      instruct = glue("
      \nWhat is the regex pattern for extracting the ID from the subject folder name? You
      can use multiple capturing groups if the ID has multiple parts. The default is ([0-9]+),
      which extracts the first number-like sequence from the folder name. For example, if your
      subject folder is named 'sub-001', the ID will be '001'. If your subject folder is named
      '001', the ID will be '001'. If the entire folder name is the subject ID, such as '001ra_2May2024',
      the id matching expression should be (.+), which matches all characters in the folder name.\n
    "),
      prompt = "What is the regex pattern for extracting the subject ID from the folder name?",
      type = "character", default = "(.+)"
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

  if (is.null(scfg$bids_conversion$validate_bids) || "bids_conversion/validate_bids" %in% fields) {
    scfg$bids_conversion$validate_bids <- prompt_input(
      instruct = glue("
      Should the BIDS folder be validated after conversion? This requires the bids-validator program to be installed.
      This is generally a good idea to ensure that the BIDS folder is valid and conforms to the BIDS specification.
      It can prevent downstream errors in fmriprep and other processing steps.
      "), type = "flag", default = TRUE
    )
  }

  scfg <- setup_job(scfg, "bids_conversion", defaults, fields)

  return(scfg)
}

#' Configure ICA-AROMA denoising
#'
#' This function configures the ICA-AROMA (Independent Component Analysis–based Automatic Removal Of Motion Artifacts)
#' step for post-fMRIPrep processing. It sets scheduling and resource parameters that will be used to apply
#' AROMA-based denoising to BOLD fMRI data using FSL's `fsl_regfilt` or an equivalent wrapper.
#'
#' @param scfg A study configuration object, as produced by `load_study()` or `setup_study()`.
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
#' - `nhours`: 24 (max runtime in hours)
#' - `ncores`: 1 (number of CPU cores)
#' - `cli_options`: "" (any extra command-line flags for the wrapper)
#' - `sched_args`: "" (additional job scheduler directives)
#'
#' @keywords internal
setup_aroma <- function(scfg, fields = NULL) {
  defaults <- list(
    memgb = 32,
    nhours = 24,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  # cat("Configuring ICA-AROMA denoising step...\n")
  # cat("This step applies non-aggressive regression of motion-related ICA components from fMRIPrep outputs.\n")
  # cat("You will be asked to provide job parameters (memory, time, cores) and any optional command-line settings.\n\n")

  scfg <- setup_job(scfg, "aroma", defaults, fields)
  return(scfg)
}


#' Find preprocessed BOLD NIfTI files in a fmriprep derivatives directory
#'
#' @param root Path to the derivatives/fmriprep directory
#' @param subject_ids Optional character vector of subject IDs to include
#' @param session_ids Optional character vector of session IDs to include
#' @param task_filter Optional character vector of task names to include
#' @param desc_filter Optional character vector of desc labels to include (e.g., "preproc")
#' @return A data.frame of matching BOLD files and their metadata
#' @importFrom checkmate assert_directory_exists assert_character
# get_fmriprep_outputs <- function(root,
#                                      subject_ids = NULL,
#                                      session_ids = NULL,
#                                      task_filter = NULL,
#                                      desc_filter = NULL) {
#   checkmate::assert_directory_exists(root)
#   checkmate::assert_character(subject_ids, null.ok = TRUE)
#   checkmate::assert_character(session_ids, null.ok = TRUE)
#   checkmate::assert_character(task_filter, null.ok = TRUE)
#   checkmate::assert_character(desc_filter, null.ok = TRUE)

#   subject_info <- get_subject_dirs(root = root, full.names = FALSE)
#   bold_files <- list()

# browser()

#   for (i in seq_len(nrow(subject_info))) {
#     sub_id <- subject_info$sub_id[i]
#     ses_id <- subject_info$ses_id[i]
#     sub_dir <- subject_info$sub_dir[i]
#     ses_dir <- subject_info$ses_dir[i]

#     if (!is.null(subject_ids) && !(sub_id %in% subject_ids)) next
#     if (!is.null(session_ids) && !is.na(ses_id) && !(ses_id %in% session_ids)) next

#     func_path <- if (!is.na(ses_dir)) file.path(root, ses_dir, "func") else file.path(root, sub_dir, "func")
#     if (!dir.exists(func_path)) next

#     nifti_files <- list.files(func_path, pattern = ".*_bold.*\\.nii\\.gz$", full.names = TRUE)

#     for (f in nifti_files) {
#       fname <- basename(f)

#       extract_tag <- function(pattern, name) {
#         if (grepl(pattern, name)) sub(paste0(".*", pattern, "-([^_]+).*"), "\\1", name) else NA
#       }

#       task <- extract_tag("task", fname)
#       desc <- extract_tag("desc", fname)
#       run <- extract_tag("run", fname)
#       echo <- extract_tag("echo", fname)
#       space <- extract_tag("space", fname)

#       if (!is.null(task_filter) && !(task %in% task_filter)) next
#       if (!is.null(desc_filter) && !(desc %in% desc_filter)) next

#       bold_files[[length(bold_files) + 1]] <- data.frame(
#         sub_id = sub_id,
#         ses_id = ses_id,
#         task = task,
#         desc = desc,
#         run = run,
#         echo = echo,
#         space = space,
#         file = f,
#         stringsAsFactors = FALSE
#       )
#     }
#   }

#   do.call(rbind, bold_files)
# }

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
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$compute_environment` populated
#' @keywords internal
#' @importFrom checkmate assert_list
setup_compute_environment <- function(scfg = list(), fields = NULL) {
  checkmate::assert_list(scfg)

  # if empty, allow population from external file
  scfg <- get_compute_environment_from_file(scfg)

  # If fields is not null, then the caller wants to make specific edits to config. Thus, don't prompt for invalid settings for other fields.
  if (is.null(fields)) {
    fields <- c()
    if (!checkmate::test_subset(scfg$compute_environment$scheduler, c("slurm", "torque"), empty.ok=FALSE)) fields <- c(fields, "compute_environment/scheduler")
    if (!validate_exists(scfg$compute_environment$fmriprep_container)) fields <- c(fields, "compute_environment/fmriprep_container")
    if (!validate_exists(scfg$compute_environment$heudiconv_container)) fields <- c(fields, "compute_environment/heudiconv_container")
    if (!validate_exists(scfg$compute_environment$bids_validator)) fields <- c(fields, "compute_environment/bids_validator")
    if (!validate_exists(scfg$compute_environment$mriqc_container)) fields <- c(fields, "compute_environment/mriqc_container")
    if (!validate_exists(scfg$compute_environment$aroma_container)) fields <- c(fields, "compute_environment/aroma_container")
  }

  if ("compute_environment/scheduler" %in% fields) {
    scfg$compute_environment$scheduler <- prompt_input("Scheduler (slurm/torque): ",
      instruct = "The pipeline currently runs on TORQUE (aka qsub) and SLURM clusters.\nWhich will you use?",
      type = "character", len = 1L, among = c("slurm", "torque")
    )
  }

  # location of fmriprep container
  if ("compute_environment/fmriprep_container" %in% fields) {
    scfg$compute_environment$fmriprep_container <- prompt_input(
      instruct = glue("
      The pipeline depends on having a working fmriprep container (docker or singularity).
      If you don't have this yet, follow these instructions first:
        https://fmriprep.org/en/stable/installation.html#containerized-execution-docker-and-singularity
    ", .trim = FALSE),
      prompt = "Location of fmriprep container: ",
      type = "file",
      default = scfg$compute_environment$fmriprep_container
    )
  }

  # location of heudiconv container
  if ("compute_environment/heudiconv_container" %in% fields) {
    scfg$compute_environment$heudiconv_container <- prompt_input(
      instruct = glue("
      The pipeline depends on having a working heudiconv container (docker or singularity).
      If you don't have this yet, follow these instructions first:
        https://heudiconv.readthedocs.io/en/latest/installation.html#install-container
    ", .trim = FALSE),
      prompt = "Location of heudiconv container: ",
      type = "file",
      default = scfg$compute_environment$heudiconv_container
    )
  }

  # location of bids-validator binary
  if ("compute_environment/bids_validator" %in% fields) {
    scfg$compute_environment$bids_validator <- prompt_input(
      instruct = glue("
      After BIDS conversion, the pipeline can pass resulting BIDS folders to bids-validator to verify that 
      the folder conforms to the BIDS specification. You can read more about validtion here: 
      https://bids-validator.readthedocs.io/en/stable/index.html.
      
      If you'd like to include BIDS validation in the processing pipeline, specify the location of the 
      bids-validator program here. If you need help building this program, follow these instructions: 
      https://bids-validator.readthedocs.io/en/stable/user_guide/command-line.html.
    ", .trim = FALSE),
      prompt = "Location of bids-validator program: ",
      type = "file", required = ,
      default = scfg$compute_environment$bids_validator
    )
  }

  # location of mriqc container
  if ("compute_environment/mriqc_container" %in% fields) {
    scfg$compute_environment$mriqc_container <- prompt_input(
      instruct = glue("
      The pipeline can use MRIQC to produce automated QC reports. This is suggested, but not required.
      If you'd like to use MRIQC, you need a working mriqc container (docker or singularity).
      If you don't have this yet, this should work to build the latest version:
        singularity build /location/to/mriqc-latest.simg docker://nipreps/mriqc:latest
    ", .trim = FALSE),
      prompt = "Location of mriqc container: ",
      type = "file", required = FALSE,
      default = scfg$compute_environment$mriqc_container
    )
  }

  # location of ICA-AROMA fMRIprep container
  if ("compute_environment/aroma_container" %in% fields) {
    scfg$compute_environment$aroma_container <- prompt_input(
      instruct = glue("
      The pipeline can use ICA-AROMA to denoise fMRI timeseries. As descried in Pruim et al. (2015), this
      is a data-driven step that produces a set of temporal regressors that are thought to be motion-related.
      If you would like to use ICA-AROMA in the pipeline, you need to build a singularity container of this
      workflow. Follow the instructions here: https://fmripost-aroma.readthedocs.io/latest/

      This is required if you say 'yes' to running AROMA during study setup.
    ", .trim = FALSE),
      prompt = "Location of ICA-AROMA container: ",
      type = "file", required = FALSE,
      default = scfg$compute_environment$aroma_container
    )
  }

  return(scfg)
}
