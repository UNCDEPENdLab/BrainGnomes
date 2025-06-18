
#' Extract fields from BIDS filenames
#' @param filenames A character vector of BIDS file names (or paths). 
#' @param drop_unused Logical; if `TRUE`, drop any BIDS entities that are not present in any of the filenames.
#' @return A data.frame containing the BIDS key–value fields extracted from each filename (each row corresponds to an input filename).
#' @details Based on the BIDS specification for file naming (see BIDS documentation appendix on entities).
#'   For more detail, see: https://bids-specification.readthedocs.io/en/stable/appendices/entities.html
#' 
#'   This function recognizes standard BIDS entities such as subject (`sub-`), session (`ses-`), task (`task-`), 
#'   acquisition (`acq-`), run, modality (`mod-`), echo (`echo-`), direction (`dir-`), reconstruction (`rec-`), 
#'   hemisphere (`hemi-`), space (`space-`), resolution (`res-`), description (`desc-`), and fieldmap (`fmap-`),
#'   as well as the file suffix and extension.
#' @examples 
#' filenames <- c(
#'   "sub-01_ses-02_task-memory_space-MNI2009c_acq-highres_desc-preproc_bold.nii.gz",
#'   "acq-lowres_desc-smoothed_sub-02_task-attention_run-2_bold.nii.gz",
#'   "sub-03_space-MNI152NLin6Asym_task-motor_desc-raw_echo-2_dir-PA_bold.nii.gz"
#' )
#' extract_bids_info(filenames)
#' @importFrom utils tail
#' @export
extract_bids_info <- function(filenames, drop_unused=FALSE) {
  checkmate::assert_character(filenames)
  filenames <- basename(filenames) # avoid matching on path components

  # Define regex patterns for each BIDS entity
  patterns <- list(
    subject = "sub-(\\d+)",
    session = "ses-(\\d+)",
    task = "task-([a-zA-Z0-9]+)",
    acquisition = "acq-([a-zA-Z0-9]+)",
    run = "run-(\\d+)",
    modality = "mod-([a-zA-Z0-9]+)",
    echo = "echo-(\\d+)",
    direction = "dir-([a-zA-Z0-9]+)",
    reconstruction = "rec-([a-zA-Z0-9]+)",
    hemisphere = "hemi-([a-zA-Z0-9]+)",
    space = "space-([a-zA-Z0-9]+)",
    resolution = "res-(\\d+)",
    description = "desc-([a-zA-Z0-9]+)",
    fieldmap = "fmap-([a-zA-Z0-9]+)"
  )
  
  # Function to extract an entity from a filename
  extract_entity <- function(filename, pattern) {
    match <- regmatches(filename, regexpr(pattern, filename))
    if (length(match) > 0) {
      return(sub(".*-", "", match))  # Extract value after the last "-"
    } else {
      return(NA)
    }
  }
  
  # Extract suffix (the last part before file extension)
  extract_suffix <- function(filename) {
    stripped <- sub("\\.nii\\.gz$|\\.tsv\\.gz$|\\.tsv$|\\.json$|\\.nii$", "", filename)
    parts <- unlist(strsplit(stripped, "_"))
    last_part <- tail(parts, 1)
    if (!grepl("-", last_part)) {
      return(last_part)
    } else {
      return(NA)
    }
  }

  # Extract extension (including .gz if present)
  extract_ext <- function(filename) {
    if (grepl("\\.nii\\.gz$", filename)) return(".nii.gz")
    if (grepl("\\.tsv\\.gz$", filename)) return(".tsv.gz")
    if (grepl("\\.tsv$", filename)) return(".tsv")
    if (grepl("\\.json$", filename)) return(".json")
    if (grepl("\\.nii$", filename)) return(".nii")
    return(NA_character_)
  }

  # Process each filename
  extracted_info <- lapply(filenames, function(filename) {
    # Extract each entity independently
    info <- lapply(patterns, extract_entity, filename = filename)
    info$suffix <- extract_suffix(filename)
    info$ext <- extract_ext(filename)
    return(as.data.frame(info, stringsAsFactors = FALSE))
  })
  
  # Combine results into a single data frame
  df <- do.call(rbind, extracted_info)

  if (isTRUE(drop_unused)) {
    all_na <- sapply(df, function(i) all(is.na(i)))
    df <- df[!all_na]
  }
  
  return(df)
}




#' Construct BIDS-Compatible Filenames from Extracted Entity Data
#'
#' Given a data frame of BIDS entities (as returned by `extract_bids_info()`),
#' this function reconstructs filenames following the BIDS specification.
#' It supports standard BIDS entities including subject, session, task, run,
#' acquisition, space, resolution, and description, along with the suffix and file extension.
#'
#' @param bids_df A `data.frame` containing one or more rows of BIDS entities.
#'   Must include at least the columns `suffix` and `ext`, and optionally:
#'   `subject`, `session`, `task`, `acquisition`, `run`, `modality`, `echo`,
#'   `direction`, `reconstruction`, `hemisphere`, `space`, `resolution`,
#'   `description`, and `fieldmap`.
#'
#' @return A character vector of reconstructed BIDS filenames, one per row of `bids_df`.
#'
#' @seealso [extract_bids_info()] for extracting BIDS fields from filenames.
#'
#' @examples
#' df <- data.frame(
#'   subject = "01", task = "rest", space = "MNI152NLin6Asym",
#'   resolution = "2", description = "preproc", suffix = "bold", ext = ".nii.gz",
#'   stringsAsFactors = FALSE
#' )
#' construct_bids_filename(df)
#' # Returns: "sub-01_task-rest_space-MNI152NLin6Asym_res-2_desc-preproc_bold.nii.gz"
#'
#' @importFrom checkmate assert_data_frame test_list
#' @export
construct_bids_filename <- function(bids_df) {
  if (checkmate::test_list(bids_df)) bids_df <- as.data.frame(bids_df, stringsAsFactors = FALSE)
  checkmate::assert_data_frame(bids_df)
  if (!"suffix" %in% names(bids_df)) stop("The input must include a 'suffix' column.")
  if (!"ext" %in% names(bids_df)) stop("The input must include an 'ext' column.")

  # Standard BIDS ordering
  entity_order <- c(
    "subject", "session", "task", "acquisition", "run", "modality",
    "echo", "direction", "reconstruction", "hemisphere", "space",
    "resolution", "description", "fieldmap"  # <- added "resolution"
  )

  # BIDS entity prefixes
  prefixes <- c(
    subject = "sub", session = "ses", task = "task", acquisition = "acq",
    run = "run", modality = "mod", echo = "echo", direction = "dir",
    reconstruction = "rec", hemisphere = "hemi", space = "space",
    resolution = "res", description = "desc", fieldmap = "fmap"
  )

  # only attempt to reconstruct entities present in the input data
  reconstruct_entities <- intersect(entity_order, names(bids_df))

  # Build filenames
  filenames <- apply(bids_df, 1, function(row) {
    parts <- character(0)
    for (entity in reconstruct_entities) {
      value <- row[entity]
      if (!is.na(value) && nzchar(value)) {
        parts <- c(parts, paste0(prefixes[entity], "-", value))
      }
    }

    suffix <- row["suffix"]
    ext <- row["ext"]
    if (is.na(suffix) || suffix == "") stop("Missing suffix.")
    if (is.na(ext) || ext == "") stop("Missing file extension.")

    paste0(paste(parts, collapse = "_"), "_", suffix, ext)
  })

  return(filenames)
}


#' Check for Existence of a BIDS-Formatted Output File with a given description
#'
#' This function constructs a BIDS-compliant filename based on an input file, replacing
#' the `desc` field with a specified `description`, and checks whether the corresponding output file
#' already exists. If the file exists and `overwrite = FALSE`, the function returns `skip = TRUE`.
#'
#' @param in_file Path to the input BIDS file (e.g., a preprocessed BOLD image).
#' @param description Character string to use as the new `desc` field in the expected output file.
#' @param overwrite Logical. If `FALSE`, existing files will not be overwritten.
#' @param prepend Logical. If `TRUE`, prepend the `description` to the existing description to build a compound description
#'
#' @return A list with elements:
#'   \item{out_file}{Path to the expected output file.}
#'   \item{skip}{Logical indicating whether to skip writing due to file existence.}
#'
#' @importFrom checkmate assert_file_exists assert_string assert_flag test_file_exists
#' @importFrom glue glue
#' @keywords internal
out_file_exists <- function(in_file, description, overwrite = TRUE, prepend = TRUE) {
    checkmate::assert_file_exists(in_file)
    checkmate::assert_string(description)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(prepend)

    # Parse and update BIDS fields
    bids_info <- extract_bids_info(in_file)
    bids_info$description <- if (prepend) paste0(bids_info$description, description) else description # set desc to new description

    # Reconstruct filename
    out_file <- file.path(dirname(in_file), construct_bids_filename(bids_info))

    # Check if file exists and whether to skip
    skip <- FALSE
    if (checkmate::test_file_exists(out_file)) {
        if (isFALSE(overwrite)) {
            message(glue::glue("Processed file already exists: {out_file}. Skipping this step."))
            skip <- TRUE
        } else {
            message(glue::glue("Overwriting existing file: {out_file}."))
        }
    }

    return(list(out_file = out_file, skip = skip))
}


#' Identify fMRIPrep-Derived Outputs for a NIfTI File
#'
#' Given the path to a preprocessed NIfTI file from fMRIPrep or fMRIPost, this function
#' identifies and returns associated derivative files in the same directory. This includes
#' the corresponding brain mask, confound regressors, ICA-AROMA melodic mixing matrix,
#' AROMA classification metrics, and a list of rejected noise components (if available).
#'
#' This function assumes filenames follow BIDS Derivatives conventions and uses the
#' extracted BIDS entities to reconstruct expected filenames via `construct_bids_filename()`.
#'
#' @param in_file A character string giving the path to a preprocessed NIfTI `.nii.gz` file
#'   generated by fMRIPrep (e.g., with suffix `_desc-preproc_bold.nii.gz`).
#'
#' @return A named list containing the following elements:
#' \describe{
#'   \item{bold}{The input BOLD file path (returned if found).}
#'   \item{brain_mask}{The corresponding brain mask file (or `NULL` if not found).}
#'   \item{confounds}{The path to the confounds `.tsv` file (or `NULL`).}
#'   \item{melodic_mix}{Path to the melodic mixing matrix from ICA-AROMA (if present).}
#'   \item{aroma_metrics}{Path to the AROMA classification metrics file (if present).}
#'   \item{noise_ics}{A vector of rejected ICA components based on AROMA classification (or `NULL`).}
#'   \item{prefix}{A string encoding the core BIDS identifier used to construct expected filenames.}
#' }
#'
#' @details
#' The function checks for two possible confounds files (`desc-confounds_timeseries.tsv` and
#' `desc-confounds_regressors.tsv`), and attempts to resolve AROMA-rejected ICs from the
#' AROMA classification metrics file (`_desc-aroma_metrics.tsv`) if present.
#'
#' @seealso [extract_bids_info()], [construct_bids_filename()]
#'
#' @examples
#' \dontrun{
#' f <- "/path/to/sub-01_task-rest_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz"
#' outputs <- get_fmriprep_outputs(f)
#' outputs$brain_mask
#' }
#'
#' @importFrom utils modifyList read.table
#' @export
get_fmriprep_outputs <- function(in_file) {
  checkmate::assert_file_exists(in_file)
  in_file <- normalizePath(in_file)
  f_info <- as.list(extract_bids_info(in_file)) # pull into BIDS fields for file expectations

  # Extract directory and filename
  dir_path <- dirname(in_file)
  base <- basename(in_file)

  # Remove extension
  base <- sub("\\.nii(\\.gz)?$", "", base)

  # Extract core identifier (everything up to desc-*)
  # prefix <- sub("_desc-preproc_bold$", "", base)

  prefix <- glue("sub-{f_info$subject}_task-{f_info$task}")
  if (!is.na(f_info$run)) prefix <- glue("{prefix}_run-{f_info$run}")

  # Possible base path (prefix may include space/acq/etc)
  bold <- file.path(dir_path, construct_bids_filename(modifyList(f_info, list(suffix = "bold"))))
  brain_mask <- file.path(dir_path, construct_bids_filename(modifyList(f_info, list(description = "brain", suffix = "mask"))))

  # Check for two variants of confounds
  conf1 <- file.path(dir_path, paste0(prefix, "_desc-confounds_timeseries.tsv"))
  conf2 <- file.path(dir_path, paste0(prefix, "_desc-confounds_regressors.tsv"))
  confounds <- if (file.exists(conf1)) conf1 else if (file.exists(conf2)) conf2 else NA

  melodic_mix <- file.path(dir_path, construct_bids_filename(modifyList(f_info, list(resolution = "2", space = NA, description = "melodic", suffix = "mixing", ext = ".tsv"))))

  # this is no longer output by fmripost-aroma
  # noise_ics <- file.path(dir_path, paste0(prefix, "_AROMAnoiseICs.csv"))

  # need to read the aroma metrics file and figure it out.
  aroma_metrics <- file.path(dir_path, glue("{prefix}_desc-aroma_metrics.tsv"))

  if (file.exists(aroma_metrics)) {
    adat <- read.table(aroma_metrics, header = TRUE, sep = "\t")
    noise_ics <- which(adat$classification == "rejected")
  } else {
    noise_ics <- NULL
  }

  # Assemble output
  output <- list(
    bold = if (file.exists(bold)) bold else NULL,
    brain_mask = if (file.exists(brain_mask)) brain_mask else NULL,
    confounds = if (!is.na(confounds)) confounds else NULL,
    melodic_mix = if (file.exists(melodic_mix)) melodic_mix else NULL,
    aroma_metrics = if (file.exists(aroma_metrics)) aroma_metrics else NULL,
    noise_ics = noise_ics,
    prefix = prefix
  )

  return(output)
}


#' Helper function to obtain all subject and session directories from a root folder
#' @param root The path to a root folder containing subject folders. 
#' @param sub_regex A regex pattern to match the subject folders. Default: `"[0-9]+"`.
#' @param sub_id_match A regex pattern for extracting the subject ID from the subject folder name. Default: `"([0-9]+)"`.
#' @param ses_regex A regex pattern to match session folders. Default: `NULL`. If `NULL`, session folders are not expected.
#' @param ses_id_match A regex pattern for extracting the session ID from the session folder name. Default: `"([0-9]+)"`.
#' @param full.names If `TRUE`, return absolute paths to the folders; if `FALSE`, return paths relative to `root`. Default: `FALSE`.
#' @return A data frame with one row per subject (or per subject-session combination) and columns:
#'   - `sub_id`: Subject ID extracted from each folder name.
#'   - `ses_id`: Session ID (or `NA` if no session level).
#'   - `sub_dir`: Path to the subject folder.
#'   - `ses_dir`: Path to the session folder (`NA` if no session).
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
#' @examples
#' get_subject_dirs(root = "/path/to/root", sub_regex = "[0-9]+", sub_id_match = "([0-9]+)",
#'                 ses_regex = "ses-[0-9]+", ses_id_match = "([0-9]+)", full.names = TRUE)
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

