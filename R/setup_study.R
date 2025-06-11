#' Load a study configuration from a file
#' @param file a YAML file containing a valid study configuration
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
#' @export
summary.bg_study_cfg <- function(x) {
  pretty_print_list(x, indent=2)
}

#' Setup the processing pipeline for a new fMRI study
#' @param input an existing `bg_study_cfg` object to be modified or a string
#'   specifying the location of an existing configuration YAML file to be loaded
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
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

  if (inherits(scfg, "bg_study_cfg")) {
    scfg <- validate_study(scfg)
    fields <- unique(c(fields, attr(scfg, "gaps")))
  }

  

  if (!checkmate::test_class(scfg, "bg_study_cfg")) {
    class(scfg) <- c(class(scfg), "bg_study_cfg")
  }

  if (is.null(scfg$project_directory) || "project_name" %in% fields) {
    scfg$project_name <- prompt_input("What is the name of your project?", type = "character")
  }

  if (is.null(scfg$project_directory) || "project_directory" %in% fields) {
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
  if (is.null(scfg$dicom_directory) || "dicom_directory" %in% fields) {
    scfg$dicom_directory <- prompt_input("Where are DICOM files files stored?", type = "character")
  }

  if (!checkmate::test_directory_exists(scfg$dicom_directory)) {
    create <- prompt_input(instruct = glue("The directory {scfg$dicom_directory} does not exist. Would you like me to create it?\n"), type = "flag")
    if (create) dir.create(scfg$dicom_directory, recursive = TRUE)
  }

  # location of BIDS data -- enforce that this must be within the project directory
  scfg$bids_directory <- file.path(scfg$project_directory, "data_BIDS")
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

  scfg$log_directory <- file.path(scfg$project_directory, "logs")
  if (!checkmate::test_directory_exists(scfg$log_directory)) dir.create(scfg$log_directory, recursive = TRUE)

  if (is.null(scfg$scratch_directory) || "scratch_directory" %in% fields) {
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

  if (is.null(scfg$templateflow_home) || "templateflow_home" %in% fields) {
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

  # logging
  if (is.null(scfg$log_txt) || "log_txt" %in% fields) {
    scfg$log_txt <- prompt_input("Create subject-level logs?",
      instruct = glue("
      The package can write plain-text logs to each subject's sub-<id> directory.
      These contain messages related to job submission and job status.
      We strongly recommend these for tracking and debugging purposes.
      ", .trim = FALSE), type = "flag"
    )
  }

  # run through configuration of each step
  scfg <- setup_compute_environment(scfg, fields)
  scfg <- setup_bids_conversion(scfg, fields)
  scfg <- setup_bids_validation(scfg, fields)
  scfg <- setup_fmriprep(scfg, fields)
  scfg <- setup_mriqc(scfg, fields)
  if (isTRUE(scfg$run_aroma)) scfg <- setup_aroma(scfg, fields)
  scfg <- setup_postprocess(scfg, fields)

  return(scfg)
}

#' Specify the fmriprep settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$fmriprep` populated
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

  #scfg$fmriprep <- populate_defaults(scfg$fmriprep, defaults)

  cat("This step sets up fmriprep.  (For details, see https://fmriprep.org/en/stable/usage.html)\n")

  scfg <- setup_job(scfg, "fmriprep", defaults = defaults)

  if (is.null(scfg$fmriprep$output_spaces) || "fmriprep/output_spaces" %in% fields) {
    scfg$fmriprep$output_spaces <- choose_fmriprep_spaces(scfg$fmriprep$output_spaces)
  }

  if (is.null(scfg$fmriprep$fs_license_file) || "fmriprep/fs_license_file" %in% fields) {
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

#' helper function to setup output spaces for fmriprep
#' @param output_spaces a string of exiting output spaces to be modified
#' @return a string of output spaces to be used in fmriprep
#' @keywords internal
#' @importFrom glue glue
#' @importFrom utils menu select.list
#' @noRd
choose_fmriprep_spaces <- function(output_spaces = NULL) {
  cat(glue("\nfmriprep uses --output-spaces to control the stereotaxic space and resolution
           of preprocessed images. Its default space is MNI152NLin2009cAsym and the default
           spatial resolution matches the raw/native data. Here, you can specify the output
           spaces to be generated. This is not a comprehensive list of all templates available
           in TemplateFlow (https://github.com/templateflow/templateflow), but it encompasses
           the most useful ones. The default of MNI152NLin2009cAsym is pretty good, too!
           For more detail, see: https://fmriprep.org/en/stable/spaces.html.
           
           Of note, the 'res-' specifier controls the output resolution, but it is not the
           voxel size! Rather, it refers to a resolution index in the files uploaded to
           TemplateFlow. Usually, res-1 is 1mm and res-2 is 2mm, but you mileage may vary.
           
           If you are using AROMA as part of your pipeline, we will automatically add
           MNI152NLin6Asym:res-2 so that fmripost-aroma can run.\n\n
           "))
  
  templates_available <- c(
    "MNI152NLin2009cAsym", "MNI152NLin6Asym", "MNI152NLin6Sym",
    "MNI152NLin2006Asym", "MNIPediatricAsym", "MNIInfant",
    "MNIColin27", "MNI305", "OASIS30ANTs"
  )
  additional_spaces <- c("T1w", "T2w", "anat", "fsnative", "fsaverage", "fsaverage5", "fsaverage6")
  
  # Parse input string into initial set
  current_spaces <- character()
  if (!is.null(output_spaces)) {
    current_spaces <- unlist(strsplit(output_spaces, "\\s+"))
  }
  
  repeat {
    cat("\nCurrent --output-spaces:\n")
    if (length(current_spaces) == 0) {
      cat("  (none selected yet)\n")
    } else {
      for (i in seq_along(current_spaces)) {
        cat(sprintf("  [%d] %s\n", i, current_spaces[i]))
      }
    }
    
    cat("\nWhat would you like to do?\n")
    choice <- menu(c("Add a space", "Delete a space", "Finish and return"), title = "Modify output spaces:")
    
    if (choice == 1) {
      # Add a space
      type_choice <- menu(c("Template space", "Other space (e.g., T1w, fsaverage)"), title = "Add space type:")
      if (type_choice == 1) {
        # Select a template
        selected_template <- utils::select.list(templates_available, multiple = FALSE, title = "Choose a template")
        if (selected_template != "") {
          res_input <- readline(paste0("Enter resolution index for ", selected_template, " (or press ENTER to skip): "))
          space_string <- if (res_input == "") {
            selected_template
          } else {
            paste0(selected_template, ":res-", res_input)
          }
          current_spaces <- unique(c(current_spaces, space_string))
        }
      } else if (type_choice == 2) {
        selected_additional <- utils::select.list(additional_spaces, multiple = TRUE, title = "Choose additional space(s)")
        current_spaces <- unique(c(current_spaces, selected_additional))
      }
    } else if (choice == 2) {
      # Delete a space
      if (length(current_spaces) == 0) {
        cat("No spaces to delete.\n")
      } else {
        del_choice <- utils::select.list(current_spaces, multiple = TRUE, title = "Select space(s) to remove:")
        current_spaces <- setdiff(current_spaces, del_choice)
      }
    } else if (choice == 3) {
      break
    }
  }
  
  final_output_spaces <- paste(current_spaces, collapse = " ")
  cat("\nFinal --output-spaces argument:\n")
  cat("  ", final_output_spaces, "\n")
  return(final_output_spaces)
}

setup_bids_validation <- function(scfg, fields=NULL) {
  defaults <- list(
    memgb = 32,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  scfg <- setup_job(scfg, "bids_validation", defaults = defaults)

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

#' Specify the mriqc settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$mriqc` populated
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
  scfg <- setup_job(scfg, "mriqc", defaults = defaults)

  return(scfg)
}

#' Specify the heudiconv settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with `$heudiconv` populated
#' @keywords internal
setup_bids_conversion <- function(scfg, fields = NULL) {
  defaults <- list(
    memgb = 16,
    nhours = 2,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  # scfg$heudiconv <- populate_defaults(scfg$heudiconv, defaults)

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

    The BIDS-compatible outputs will be written to a folder called 'data_BIDS' within the project directory.

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

  if (is.null(scfg$heudiconv$sub_regex) || "heudiconv/sub_regex" %in% fields) {
    scfg$heudiconv$sub_regex <- prompt_input(
      instruct = glue("
      \nWhat is the regex pattern for the subject IDs? This is used to identify the subject folders
      within the DICOM directory. The default is sub-[0-9]+, which matches sub-001, sub-002, etc.
      If you have a different naming scheme, please specify it here.\n
    "),
      prompt = "What is the regex pattern for the subject IDs?",
      type = "character", default = "sub-[0-9]+"
    )
  }

  if (is.null(scfg$heudiconv$sub_id_match) || "heudiconv/sub_id_match" %in% fields) {
    scfg$heudiconv$sub_id_match <- prompt_input(
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

  if (is.null(scfg$heudiconv$ses_regex) || "heudiconv/ses_regex" %in% fields) {
    scfg$heudiconv$ses_regex <- prompt_input(
      instruct = glue("
      If you have multisession data, specify the the regex pattern for session IDs within the subject folders.
      If you don't have multisession data, just press Enter to skip this step.
    ", .trim = FALSE),
      prompt = "What is the regex pattern for the session IDs?",
      type = "character", required = FALSE
    )
  }

  if (!is.na(scfg$heudiconv$ses_regex) && (is.null(scfg$heudiconv$ses_id_match) || "heudiconv/ses_id_match" %in% fields)) {
    scfg$heudiconv$ses_id_match <- prompt_input(
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
    scfg$heudiconv$ses_id_match <- NA_character_
  }


  if (is.null(scfg$heudiconv$heuristic_file) || "heudiconv/heuristic_file" %in% fields) {
    scfg$heudiconv$heuristic_file <- prompt_input(instruct = glue("What is the location of the heudiconv heuristic file?"), type = "file")
  }

  if (is.null(scfg$heudiconv$overwrite) || "heudiconv/overwrite" %in% fields) {
    scfg$heudiconv$overwrite <- prompt_input(instruct = glue("Should existing BIDS files be overwritten by heudiconv?"), type = "flag", default = TRUE)
  }

  if (is.null(scfg$heudiconv$clear_cache) || "heudiconv/clear_cache" %in% fields) {
    scfg$heudiconv$clear_cache <- prompt_input(
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

  if (is.null(scfg$heudiconv$validate_bids) || "heudiconv/validate_bids" %in% fields) {
    scfg$heudiconv$validate_bids <- prompt_input(
      instruct = glue("
      Should the BIDS folder be validated after conversion? This requires the bids-validator program to be installed.
      This is generally a good idea to ensure that the BIDS folder is valid and conforms to the BIDS specification.
      It can prevent downstream errors in fmriprep and other processing steps.
      "), type = "flag", default = TRUE
    )
  }

  scfg <- setup_job(scfg, "heudiconv", defaults = defaults)

  return(scfg)
}

#' Specify the aroma settings
#' @param scfg a study configuration object, as produced by `load_study` or `setup_study`
#' @return a modified version of `scfg` with `$aroma` populated
#' @keywords internal
setup_aroma <- function(scfg, fields = NULL) {
  defaults <- list(
    memgb = 32,
    nhours = 24,
    ncores = 1,
    cli_options = "",
    sched_args = ""
  )

  cat("This step sets up ICA-AROMA.\n")

  scfg <- setup_job(scfg, "aroma", defaults = defaults)
  return(scfg)
}

#' Helper function to obtain all subject and session directories from a root folder
#' @param root The path to a root folder containing subject folders. 
#' @param sub_regex A regex pattern to match the subject folders. Default: `"[0-9]+"`.
#' @param sub_id_match A regex pattern to indicate how to extract the subject ID from the subject folder name.
#'  Default: `"([0-9]+)"`. The default will extract the first number-like sequence from the folder name.
#'  Note that this can include multiple capturing groups if the ID has multiple parts.
#' @param ses_regex A regex pattern to match the session folders. Default: `NULL`.
#'  If `NULL`, only the subject folders are returned. If not `NULL`, the session folders
#'    are returned as well.
#' @param ses_id_match A regex pattern to indicate how to extract the session ID from the session folder name.
#'  Default: `"([0-9]+)"`. The default will extract the first number-like sequence from the folder name.
#' @param full.names If `TRUE`, return the absolute paths to the subject folders. If
#'   `FALSE`, return paths relative to `root`. Default: `TRUE`.
#' @details This function is used to find all subject folders within a root folder. 
#'   It is used internally by the package to find the subject DICOM and BIDS folders for processing.
#'   The function uses the `list.dirs` function to list all directories within the
#'   folder and then filters the directories based on the regex patterns provided.
#'   The function returns a character vector of the subject folders found.
#' 
#'   The function also extracts the subject and session IDs from the folder names
#'   using the regex patterns provided. The IDs are extracted using the `extract_capturing_groups`
#'   function, which uses the `regexec` and `regmatches` functions to extract the capturing groups
#'   from the folder names. The function returns a data frame with the subject and session IDs
#'   and the corresponding folder paths.
#' @return A data frame with the following columns:
#'   - `sub_id`: The subject ID extracted from the subject folder name.
#'   - `ses_id`: The session ID extracted from the session folder name. If no session folders are found,
#'     this column will be `NA`.
#'   - `sub_dir`: The path to the subject folder.
#'   - `ses_dir`: The path to the session folder. If no session folders are found, this column will be `NA`.
#' @examples
#'   get_subject_dirs(root = "/path/to/root", sub_regex = "[0-9]+", sub_id_match = "([0-9]+)",
#'     ses_regex = "ses-[0-9]+", ses_id_match = "([0-9]+)", full.names = TRUE)
#' @keywords internal
#' @importFrom checkmate assert_directory_exists assert_flag
get_subject_dirs <- function(root = NULL, sub_regex = "[0-9]+", sub_id_match = "([0-9]+)", 
  ses_regex = NULL, ses_id_match = "([0-9]+)", full.names = FALSE) {
  
  checkmate::assert_directory_exists(root)
  checkmate::assert_string(sub_regex)
  if (is.null(sub_id_match)) sub_id_match <- "(.*)" # all characters
  checkmate::assert_string(sub_id_match)
  checkmate::assert_string(ses_regex, null.ok = TRUE, na.ok = TRUE)
  if (is.null(ses_id_match) || is.na(ses_id_match[1L])) ses_id_match <- "(.*)" # all characters
  checkmate::assert_string(ses_id_match)
  checkmate::assert_flag(full.names)

  # List directories in the root folder
  entries <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  subject_entries <- entries[grepl(sub_regex, entries)]
  subject_ids <- extract_capturing_groups(subject_entries, sub_id_match)
  
  if (length(subject_entries) == 0) {
    warning("No subject directories found in the root folder matching the regex pattern.")
    return(data.frame(sub_id = character(0), ses_id = character(0), sub_dir = character(0), ses_dir = character(0), stringsAsFactors = FALSE))
  }

  result <- list()

  for (ss in seq_along(subject_entries)) {

    sub_dir <- if (full.names) file.path(root, subject_entries[ss]) else subject_entries[ss]

    # Create subject-level row
    subject_row <- list(sub_id = subject_ids[ss], ses_id = NA_character_, sub_dir = sub_dir, ses_dir = NA_character_)

    if (is.null(ses_regex) || is.na(ses_regex[1L])) {
      # not a multisession study
      result[[length(result) + 1]] <- subject_row
    } else {
      ses_dirs <- list.dirs(file.path(root, subject_entries[ss]), recursive = TRUE, full.names = FALSE)
      ses_matches <- ses_dirs[grepl(ses_regex, basename(ses_dirs))]

      if (length(ses_matches) > 0) {
        for (ses_dir in ses_matches) {
          ses_id <- extract_capturing_groups(basename(ses_dir), ses_id_match)
          ses_dir <- file.path(sub_dir, ses_dir) # add the subject directory to the session directory
          result[[length(result) + 1]] <- list(sub_id = subject_ids[ss], ses_id = ses_id, sub_dir = sub_dir,ses_dir = ses_dir)
        }
      } else {
        # warning is too noisy -- just noting that a ses-regex was provided but only a subject directory was found
        # warning(sprintf("No session directories found in '%s' matching '%s'", sub_dir, ses_regex))
        result[[length(result) + 1]] <- subject_row
      }
    }
  }

  return(do.call(rbind.data.frame, result))
}


# helper function to grab the subject id from a subject's bids directory
get_sub_id <- function(bids_dir, regex="sub-[0-9]+") {
  checkmate::assert_directory_exists(bids_dir)
  
  sub("sub-", "", basename(bids_dir))
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

  if (is.null(scfg$compute_environment$scheduler) || "compute_environment/scheduler" %in% fields || !checkmate::test_subset(scfg$compute_environment$scheduler, c("slurm", "torque"))) {
    scfg$compute_environment$scheduler <- prompt_input("Scheduler (slurm/torque): ",
      instruct = "The pipeline currently runs on TORQUE (aka qsub) and SLURM clusters.\nWhich will you use?",
      type = "character", len = 1L, among = c("slurm", "torque")
    )
  }

  # location of fmriprep container
  if (!validate_exists(scfg$compute_environment$fmriprep_container) || "compute_environment/fmriprep_container" %in% fields) {
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
  if (!validate_exists(scfg$compute_environment$heudiconv_container) || "compute_environment/heudiconv_container" %in% fields) {
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
  if (!validate_exists(scfg$compute_environment$bids_validator) || "compute_environment/bids_validator" %in% fields) {
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
  if (!validate_exists(scfg$compute_environment$mriqc_container) || "compute_environment/mriqc_container" %in% fields) {
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
  if (!validate_exists(scfg$compute_environment$aroma_container) || "compute_environment/aroma_container" %in% fields) {
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
