### Utility functions for the pipeline


#' Get the HPC job script for a given job name
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @param job_name The name of the job (e.g., "fmriprep", "bids_conversion")
#' @return The path to the job script
#' @importFrom glue glue
#' @importFrom checkmate assert_string test_file_exists
#' @keywords internal
#' @noRd
get_job_script <- function(scfg = NULL, job_name) {
  checkmate::assert_string(job_name)
  
  ext <- ifelse(scfg$compute_environment$scheduler == "torque", "pbs", "sbatch")
  expect_file <- glue("hpc_scripts/{job_name}_subject.{ext}")
  script <- system.file(expect_file, package = "BrainGnomes")
  if (!checkmate::test_file_exists(script)) {
    stop("In get_job_script, cannot find expected script file: ", expect_file)
  }
  return(script)
}
#' Convert scheduler arguments into a scheduler-specific string
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @param job_name The name of the job (e.g., "fmriprep", "bids_conversion")
#' @return A character string of scheduler arguments
#' @importFrom glue glue
#' @importFrom checkmate assert_string
#' @keywords internal
#' @noRd
get_job_sched_args <- function(scfg = NULL, job_name) {
  checkmate::assert_string(job_name)

  # TODO: need to use cli_opts approach to remove conflicting/redundant fields in sched_args for -n, -N, etc.

  sched_args <- scfg[[job_name]]$sched_args

  memgb <- scfg[[job_name]]$memgb
  nhours <- scfg[[job_name]]$nhours
  ncores <- scfg[[job_name]]$ncores
  if (isTRUE(scfg$debug)) {
    memgb <- 4
    nhours <- 0.1
    ncores <- 1
  }
  # convert empty strings to NULL for compatibility with glue
  if (length(sched_args) == 0L || is.na(sched_args[1L]) || sched_args[1L] == "") sched_args <- NULL

  if (scfg$compute_environment$scheduler == "slurm") {
    # ensure that we strip off any #SBATCH prefix since we are passing arguments directly to sbatch or qsub
    if (!is.null(sched_args)) sched_args <- sub("^\\s*#SBATCH\\s+", "", sched_args, ignore.case = TRUE)

    sched_args <- glue(
      "-N 1",
      "-n {ncores}",
      "--time={hours_to_dhms(nhours)}",
      "--mem={memgb}g",
      "{paste(sched_args, collapse=' ')}",
      .trim = TRUE, .sep = " ", .null = NULL
    )
  } else {
    if (!is.null(sched_args)) sched_args <- sub("^\\s*#PBS\\s+", "", sched_args, ignore.case = TRUE)
    sched_args <- glue(
      "-l nodes=1:ppn={ncores}",
      "-l walltime={hours_to_dhms(nhours)}",
      "-l mem={memgb}",
      "{paste(sched_args, collapse=' ')}",
      .trim = TRUE, .sep = " ", .null = NULL
    )
  }

  return(trimws(sched_args))
}

setup_job <- function(scfg, job_name = NULL, defaults = NULL, fields = NULL) {
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg[[job_name]]$memgb)) fields <- c(fields, glue("{job_name}/memgb"))
    if (is.null(scfg[[job_name]]$nhours)) fields <- c(fields, glue("{job_name}/nhours"))
    if (is.null(scfg[[job_name]]$ncores)) fields <- c(fields, glue("{job_name}/ncores"))
    if (is.null(scfg[[job_name]]$cli_options)) fields <- c(fields, glue("{job_name}/cli_options"))
    if (is.null(scfg[[job_name]]$sched_args)) fields <- c(fields, glue("{job_name}/sched_args"))
  }

  if (glue("{job_name}/memgb") %in% fields) {
    scfg[[job_name]]$memgb <- prompt_input(
      instruct = glue("How many GB of memory should be used for running {job_name}?"),
      type = "numeric", lower = 1, upper = 1024, len = 1L, default = defaults$memgb
    )
  }

  if (glue("{job_name}/nhours") %in% fields) {
    scfg[[job_name]]$nhours <- prompt_input(
      instruct = glue("How many hours should each run of {job_name} request?"),
      type = "numeric", lower = 0.1, upper = 1000, len = 1L, default = defaults$nhours
    )
  }

  if (glue("{job_name}/ncores") %in% fields) {
    scfg[[job_name]]$ncores <- prompt_input(
      instruct = glue("How many cores/CPUs should each job request?"),
      type = "integer", lower = 1, upper = 1000, len = 1L, default = defaults$ncores
    )
  }

  if (glue("{job_name}/cli_options") %in% fields) {
    scfg[[job_name]]$cli_options <- build_cli_args(args = scfg[[job_name]]$cli_options, instruct = glue("Specify any other {job_name} command line arguments. Press Enter when done."))
  }

  if (glue("{job_name}/sched_args") %in% fields) {
    sched_queue <- ifelse(!is.null(scfg$compute_environment$scheduler) && scfg$compute_environment$scheduler == "torque", "#PBS", "#SBATCH")
    scfg[[job_name]]$sched_args <- build_cli_args(args = scfg[[job_name]]$sched_args, instruct = glue("Specify any other arguments to pass to the job scheduler These usually begin {sched_queue}. Press Enter when done."))
  }

  return(scfg)
}


pretty_arg <- function(x, width = 80) {
  if (is.null(x) || length(x) == 0L || is.na(x[1L])) {
    "[None]"
  } else {
    strwrap(x, width, exdent = 2)
  }
}


#' This function returns an lgr object corresponding to logging for a single subject directory
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @param sub_id The id of the subject whose logger we wish to access
#' @return a configured lgr object for logging subject processing messages
#' @importFrom lgr get_logger_glue
#' @keywords internal
get_subject_logger <- function(scfg, sub_id) {
  checkmate::assert_directory_exists(scfg$metadata$project_directory)
  sub_dir <- file.path(scfg$metadata$log_directory, glue("sub-{sub_id}"))
  if (!checkmate::test_directory_exists(sub_dir)) dir.create(sub_dir, showWarnings = FALSE, recursive = TRUE)
  lg <- lgr::get_logger_glue(c("sub", sub_id))
  if (!"subject_logger" %in% names(lg$appenders)) {
    lg$add_appender(lgr::AppenderFile$new(file.path(sub_dir, glue("sub-{sub_id}_log.txt"))), name = "subject_logger")
  }

  return(lg)
}


#' Helper function to check whether a given file or directory exists and, optionally, is readable
#' @param input a file or directory to check for existence
#' @param description a character string describing what this file is if we are prompted to change it
#' @param directory if TRUE, check whether a directory exists. If FALSE (default), check that the file exists
#' @param prompt_change if TRUE, if the file/directory exists, ask the user if they wish to change the value. If so, return FALSE
#' @param check_readable if TRUE, validation fails (return `FALSE`) when the file/directory exists but is not readable
#' @return a boolean (`TRUE/FALSE`) indicating whether the file or directory exists and is valid
#' @importFrom checkmate assert_flag assert_string test_directory_exists test_file_exists
#' @keywords internal
validate_exists <- function(input, description = "", directory = FALSE, prompt_change = FALSE, check_readable = TRUE) {
  checkmate::assert_string(description)
  checkmate::assert_flag(directory)
  checkmate::assert_flag(prompt_change)
  checkmate::assert_flag(check_readable)

  # return FALSE for NULL or other invalid input
  if (!checkmate::test_string(input)) return(FALSE)

  type <- if (directory) "directory" else "file"
  exists_fn <- if (directory) checkmate::test_directory_exists else checkmate::test_file_exists

  if (!exists_fn(input)) return(FALSE) # directory/file does not exist

  if (check_readable && !exists_fn(input, access = "r")) {
    warning(glue::glue("Found existing {type}, but you do not have read permission: {input}"))
    return(FALSE)
  }

  if (prompt_change) {
    cat(glue::glue("Found existing {description}: {input}\n"))
    change <- prompt_input("Change setting?", type = "flag")
    return(!isTRUE(change))
  }

  return(TRUE)
}

#' Check whether a pipeline step is complete
#'
#' Determines if the expected output directory and `.complete` marker
#' are present for a given subject/session and processing step.
#'
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @param sub_id Subject identifier
#' @param ses_id Optional session identifier
#' @param step_name Name of the processing step
#' @param pp_stream Name of the postprocessing stream when `step_name` is
#'   "postprocess"
#' @return List containing `complete` (logical), `dir`, and
#'   `complete_file`
#' @importFrom checkmate assert_choice
#' @keywords internal
is_step_complete <- function(scfg, sub_id, ses_id = NULL,
                             step_name, pp_stream = NULL) {
  checkmate::assert_choice(step_name,
    c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess"))
  if (is.null(ses_id) || is.na(ses_id)) ses_id <- NULL

  session_level <- step_name %in% c("bids_conversion", "postprocess")
  name_tag <- step_name
  if (step_name == "postprocess") {
    checkmate::assert_string(pp_stream)
    name_tag <- glue("{step_name}_{pp_stream}")
  }

  sub_str <- glue("_sub-{sub_id}")
  if (session_level && !is.null(ses_id)) {
    sub_str <- glue("{sub_str}_ses-{ses_id}")
  }

  complete_file <- file.path(
    scfg$metadata$log_directory,
    glue("sub-{sub_id}"),
    glue(".{name_tag}{sub_str}_complete")
  )

  out_dir <- switch(step_name,
    bids_conversion = if (session_level && !is.null(ses_id)) {
        file.path(scfg$metadata$bids_directory, glue("sub-{sub_id}"),
                  glue("ses-{ses_id}"))
      } else {
        file.path(scfg$metadata$bids_directory, glue("sub-{sub_id}"))
      },
    mriqc = if (!is.null(ses_id)) {
        file.path(scfg$metadata$mriqc_directory, glue("sub-{sub_id}"),
                  glue("ses-{ses_id}"))
      } else {
        file.path(scfg$metadata$mriqc_directory, glue("sub-{sub_id}"))
      },
    fmriprep = if (!is.null(ses_id)) {
        file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}"),
                  glue("ses-{ses_id}"))
      } else {
        file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}"))
      },
    aroma = if (!is.null(ses_id)) {
        file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}"),
                  glue("ses-{ses_id}"))
      } else {
        file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}"))
      },
    postprocess = if (!is.null(ses_id)) {
        file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}"),
                  glue("ses-{ses_id}"))
      } else {
        file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}"))
      }
  )

  complete <- checkmate::test_directory_exists(out_dir) &&
    checkmate::test_file_exists(complete_file)

  list(complete = complete, dir = out_dir, complete_file = complete_file)
}


#' helper function to extract capturing groups from a string
#' @param strings a character vector containing the strings to be processed
#' @param pattern a regex pattern to match the strings
#' @param sep a character string to separate the captured groups. Default: `"_"`.
#' @param groups a numeric vector specifying the indices of the capturing groups to be extracted.
#'   Default: `NULL`, which extracts all capturing groups.
#' @param ... additional arguments passed to `regexec` (e.g., `perl = TRUE`)
#' @details This function uses the `regexec` and `regmatches` functions to extract
#'   the capturing groups from the strings. The function returns a character vector
#'   containing the captured groups. If no matches are found, `NA` is returned.
#' @return a character vector containing the captured groups
#' @keywords internal
extract_capturing_groups <- function(strings, pattern, groups = NULL, sep = "_", ...) {
  stopifnot(is.character(strings), is.character(pattern), length(pattern) == 1)

  matches <- regexec(pattern, strings, ...)
  reg_list <- regmatches(strings, matches)

  # For each string, extract and paste selected groups
  result <- vapply(reg_list, function(m) {
    if (length(m) <= 1) return(NA_character_)
    capture_groups <- m[-1]  # drop full match
    if (!is.null(groups)) {
      if (any(groups > length(capture_groups))) {
        warning("Some requested group indices are out of range.")
        return(NA_character_)
      }
      capture_groups <- capture_groups[groups]
    }
    paste(capture_groups, collapse = sep)
  }, character(1))

  return(result)
}

#strings <- c("sub-123_datavisit1", "sub-456_datatest1", "badstring")
#pattern <- "sub-([0-9]+)_data((visit|test)1)"
#pattern <- "([0-9]+)"
#extract_capturing_groups(strings, pattern)



# helper function to populate defaults for config
populate_defaults <- function(target = NULL, defaults) {
  if (is.null(target)) target <- list()
  checkmate::assert_list(defaults, names = "unique")

  miss_fields <- setdiff(names(defaults), names(target))
  if (length(miss_fields) > 0L) {
    for (mm in miss_fields) {
      target[[mm]] <- defaults[[mm]]
    }
  }

  return(target)
}

#' Remove NIfTI files if they exist
#'
#' @param files A character vector of file paths (with or without `.nii` or `.nii.gz` extensions).
#' @details 
#'   Deletes one or more NIfTI files from disk. If a file path is provided
#'   without an extension, `.nii.gz` is appended before checking for existence.
#'
#' @return Invisibly returns \code{NULL}. Used for its side effect of deleting files.
#' @keywords internal
#' @importFrom checkmate assert_character test_file_exists
rm_niftis <- function(files=NULL) {
  if (is.null(files)) return(invisible(NULL))
  checkmate::assert_character(files)
  for (ff in files) {
    tnif <- ifelse(grepl(".*\\.nii(\\.gz)?$", ff), ff, paste0(ff, ".nii.gz")) # add suffix if none provided
    if (checkmate::test_file_exists(tnif)) unlink(tnif)
  }
}


truncate_str <- function(x, max_chars = 100, continuation = "...") {
  checkmate::assert_character(x, any.missing = TRUE)
  checkmate::assert_int(max_chars, lower = 1)
  checkmate::assert_string(continuation)

  continuation_len <- nchar(continuation)

  # Vectorized truncation
  truncated <- ifelse(
    is.na(x),
    NA_character_,
    ifelse(
      nchar(x) > max_chars,
      paste0(substr(x, 1, max_chars - continuation_len), continuation),
      x
    )
  )

  return(truncated)
}

#' Run an FSL command with optional Singularity container support and structured logging
#'
#' Executes an FSL command in a clean shell environment, with support for Singularity containers, optional logging via the `lgr` package, and flexible control over execution and output.
#'
#' @param args A character vector specifying the FSL command and arguments to run (e.g., `"fslmaths input.nii.gz -add 1 output.nii.gz"`).
#' @param fsldir Optional. Path to the FSL installation directory. If `NULL`, the function attempts to infer it from the environment or system configuration.
#' @param echo Logical. Whether to print the command to standard output. Defaults to `TRUE`.
#' @param run Logical. Whether to execute the command. If `FALSE`, returns as if the command succeeded. Defaults to `TRUE`.
#' @param intern Logical. If `TRUE`, returns the standard output of the command as a character vector with `"retcode"` attribute. If `FALSE`, returns only the exit code. Defaults to `FALSE`.
#' @param stop_on_fail Logical. If `TRUE`, stops execution if the FSL command returns a non-zero exit code. Defaults to `TRUE`.
#' @param log_file Optional. Path to a log file for recording output. Ignored if `use_lgr = TRUE`.
#' @param use_lgr Logical. Whether to use the `lgr` logging framework. If `TRUE`, configures and logs to console and/or file appenders. Defaults to `TRUE`.
#' @param fsl_img Optional. Path to a Singularity image containing FSL. If provided, the command is executed within the container using `singularity exec`.
#' @param bind_paths Optional. Character vector of additional directories to bind inside the Singularity container. The current working directory and temp directory are automatically added.
#'
#' @return If `intern = FALSE`, returns the exit code (integer) of the command. If `intern = TRUE`, returns the standard output as a character vector. In both cases, `"stdout"` and `"stderr"` are attached as attributes.
#'
#' @details
#' This function sets up the FSL environment by sourcing `fsl.sh`, and can optionally run FSL commands inside a Singularity container. Logging can be directed to both console and file using `lgr`, or to a file using basic `cat()` logging if `use_lgr = FALSE`.
#'
#' If `intern = TRUE`, the command output is returned and tagged with attributes `stdout`, `stderr`, and `retcode`. The `bind_paths` argument ensures that relevant file paths are visible inside the container.
#'
#' @examples
#' \dontrun{
#' run_fsl_command("fslmaths input.nii.gz -add 1 output.nii.gz")
#' run_fsl_command("fslhd input.nii.gz", intern = TRUE)
#' }
#'
#' @importFrom lgr AppenderConsole AppenderFile get_logger
#' @export
run_fsl_command <- function(args, fsldir=NULL, echo=TRUE, run=TRUE, intern=FALSE, stop_on_fail=TRUE, log_file=NULL, use_lgr=TRUE, fsl_img=NULL, bind_paths=NULL) {
  checkmate::assert_character(args)
  checkmate::assert_string(fsldir, null.ok = TRUE)
  if (!is.null(fsldir)) checkmate::assert_directory_exists(fsldir)

  checkmate::assert_flag(echo)
  checkmate::assert_flag(run)
  checkmate::assert_flag(intern)
  checkmate::assert_flag(stop_on_fail)
  checkmate::assert_string(log_file, null.ok = TRUE)
  checkmate::assert_flag(use_lgr)
  checkmate::assert_string(fsl_img, null.ok=TRUE)
  if (!is.null(fsl_img)) checkmate::assert_file_exists(fsl_img)
  checkmate::assert_character(bind_paths, null.ok = TRUE)

  if (use_lgr) {
    lg <- lgr::get_logger("run_fsl_command", reset = TRUE) # always get clean config
    lg$set_propagate(FALSE) # avoid inherited console output
    if (echo) lg$add_appender(lgr::AppenderConsole$new(), name = "console")
    if (!is.null(log_file)) lg$add_appender(lgr::AppenderFile$new(log_file), name = "file")
  }

  # paste into single string if multiple character arguments are passed
  if (length(args) > 1L) args <- paste(args, collapse=" ")

  if (!is.null(fsl_img)) {
    # if we are using a singularity container, always look inside the container for FSLDIR
    checkmate::assert_file_exists(fsl_img, access = "r")
    fsldir <- system(glue("singularity exec {fsl_img} printenv FSLDIR"), intern = TRUE)
    if (length(fsldir) == 0L) stop("Cannot find FSLDIR inside singularity container")
  } else if (is.null(fsldir)) {
    # look for FSLDIR in system environment if not passed in    
    fsldir <- Sys.getenv("FSLDIR")
    if (isFALSE(nzchar(fsldir))) {
      # check for FSLDIR in .bashrc or .profile
      bashrc_fsldir <- ""
      if (file.exists("~/.profile")) {
        bashrc_fsldir <- system("source ~/.profile && echo $FSLDIR", intern = TRUE)
      }

      if (nzchar(bashrc_fsldir) && file.exists("~/.bashrc")) {
        bashrc_fsldir <- system("source ~/.bashrc && echo $FSLDIR", intern = TRUE)
      }

      # Fallback: look for location of fsl feat on PATH
      if (nzchar(bashrc_fsldir)) {
        feat_loc <- system("command -v feat", intern = TRUE)
        exit_code <- attr(feat_loc, "status")
        if (!is.null(exit_code) && exit_code == 1) {
          warning("Could not find FSL using FSLDIR or system PATH. Defaulting to Defaulting to /usr/local/fsl.")
          fsldir <- "/usr/local/fsl"
        } else {
          fsldir <- dirname(dirname(feat_loc))
        }
      }
    }
  }

  Sys.setenv(FSLDIR=fsldir) #export to R environment
  fslsetup <- paste0("FSLDIR=", fsldir, "; PATH=${FSLDIR}/bin:${PATH}; . ${FSLDIR}/etc/fslconf/fsl.sh; ${FSLDIR}/bin/")

  # Command to run (basic or singularity-wrapped)
  base_cmd <- paste0(fslsetup, args)

  if (!is.null(fsl_img)) {
    # Get absolute working directory and always make this available to singularity
    bind_paths <- unique(c(bind_paths, getwd(), tempdir()))
    bind_str <- paste(sapply(bind_paths, function(x) paste0("-B ", normalizePath(x))), collapse = " ")
    
    singularity_cmd <- paste(
      "singularity exec",
      bind_str,
      fsl_img,
      "bash -c",
      shQuote(base_cmd)
    )
    full_cmd <- singularity_cmd
  } else {
    full_cmd <- base_cmd
  }

  ofile <- tempfile(pattern="stdout")
  efile <- tempfile(pattern = "stderr")
  full_cmd <- paste(full_cmd, ">", shQuote(ofile), "2>", shQuote(efile))

  if (use_lgr) {
    lg$info("FSL command: %s", args)
    lg$debug("Shell command: %s", full_cmd)
  } else if (checkmate::test_string(log_file)) {
    cat(args, file=log_file, append=TRUE, sep="\n")
    # cat("FSL command: ", full_cmd, "\n")
  }

  if (isTRUE(echo) && !use_lgr) cat(args, "\n")

  retcode <- if (isTRUE(run)) system(full_cmd) else 0 # return 0 if not run

  if (file.exists(efile)) {
    stderr <- readLines(efile, warn = FALSE)
    if (identical(character(0), stderr)) stderr <- ""
  } else {
    stderr <- ""
  }

  if (file.exists(ofile)) {
    stdout <- readLines(ofile, warn = FALSE)
    if (identical(character(0), stdout)) stdout <- ""
  } else {
    stdout <- ""
  }

  to_return <- retcode # return exit code of command
  # if specified, switch to stdout as return
  if (isTRUE(intern)) {
    to_return <- stdout # return output of command
    attr(to_return, "retcode") <- retcode
  }

  attr(to_return, "stdout") <- stdout
  attr(to_return, "stderr") <- stderr

  if (retcode != 0) {    
    errmsg <- glue("run_fsl_command failed with exit code: {retcode}, stdout: {paste(stdout, collapse='\n')}, stderr: {paste(stderr, collapse='\n')}")
    if (use_lgr) {
      lg$error(errmsg)
    } else {
      cat(errmsg, "\n", file = log_file, append = TRUE)
    }    
    if (isTRUE(stop_on_fail)) stop(errmsg)
  }

  return(to_return)
}




#' Convert a matrix to a 4D NIfTI image
#'
#' Writes a numeric matrix (e.g., confound regressors) to a 4D NIfTI file with singleton y and z dimensions,
#' suitable for processing with FSL tools. Each column becomes a voxel in the x dimension, and each row
#' corresponds to a time point (t dimension).
#'
#' @param mat A numeric matrix or data frame with dimensions \code{time x variables}.
#' @param ni_out Output filename (without extension) for the resulting NIfTI image.
#'
#' @return The function invisibly returns \code{NULL}. A NIfTI file is written to \code{ni_out}.
#'
#' @details Missing values are replaced with zero.
#'
#' @keywords internal
#' @importFrom RNifti asNifti writeNifti
#' @importFrom glue glue
mat_to_nii <- function(mat, ni_out="mat") {
  if (is.data.frame(mat)) mat <- as.matrix(mat)
  
  # populate nifti -- need to transpose to be consistent with column-wise array filling
  arr <- array(t(mat), dim = c(ncol(mat), 1, 1, nrow(mat))) # add singleton dimensions for y and z
  nif <- asNifti(arr)
  
  nif[is.na(nif)] <- 0 # cannot handle missingness in NIfTIs

  # enforce .nii.gz extension on ni_out since it defaults to .nii and downstream fsl commands usually assume .nii.gz
  ni_out <- sub("(\\.nii)?(\\.gz)?$", ".nii.gz", ni_out)

  # write NIfTI with regressors to file
  writeNifti(nif, file = ni_out)[["image"]] # this returns the filename to the caller
}

#' Convert a 4D NIfTI image to a matrix
#'
#' Reads a 4D NIfTI file (with singleton y and z dimensions) and converts it to a matrix
#' with dimensions \code{time x variables}. This is the inverse of \code{mat_to_nii()}.
#'
#' @param ni_in Path to a NIfTI file where the x dimension encodes variables and the 4th (time) dimension encodes observations.
#'
#' @return A numeric matrix of dimension \code{time x variables}.
#'
#' @details Assumes the input image has shape \code{[x, 1, 1, time]} as produced by \code{mat_to_nii()}.
#'
#' @keywords internal
#' @importFrom RNifti readNifti
#' @importFrom checkmate assert_file_exists
nii_to_mat <- function(ni_in) {
  checkmate::assert_file_exists(ni_in)

  nii <- readNifti(ni_in)
  mat <- as.array(nii[, 1, 1, , drop = FALSE]) # keep x and t
  dim(mat) <- dim(mat)[c(1, 4)] # selectively drop y and z dimensions (handles singleton cases correctly)
  mat <- t(mat) # make into time x variables
  return(mat)
}

### CURRENTLY ONLY USED BY POSTPROCESSING


# Jun 2024: brain mask is required for calculating image quantiles for 2nd and 50th percentiles -- smoothing and intensity normalization
# Given that it is used only for these quantiles, the fmriprep mask should be fine for this purpose
# apply_mask is now considered an additional step that is optional and uses the brain_mask in the cfg

#' convert a number of hours to a days, hours, minutes, seconds format
#'
#' @importFrom lubridate day hour minute second seconds_to_period dhours
#' @keywords internal
#' @details REDUNDANT WITH fmri.pipeline
hours_to_dhms <- function(hours, frac = FALSE) {
  checkmate::assert_number(hours, lower = 0)
  dur <- lubridate::dhours(hours)
  period <- seconds_to_period(dur)

  if (isTRUE(frac)) {
    str <- sprintf("%02d:%02d:%.03f", hour(period), minute(period), second(period))
  } else {
    str <- sprintf("%02d:%02d:%02d", hour(period), minute(period), round(second(period)))
  }

  if (day(period) > 0) {
    str <- paste0(sprintf("%d-", day(period)), str)
  }

  return(str)
}

#' internal function for returning a neuroimaging file (and path) without its extension
#' at present, it returns NA if no recognized extension is there
#' @keywords internal
file_sans_ext <- function(file) {
  matches <- grepl("^.*\\.(csv|dat|hdr|img|brik|head|nii|txt|tsv|yaml|json|1d)(\\.gz|\\.bz2|\\.zip|\\.xz)?$", file, ignore.case = TRUE)
  fout <- rep(NA, length=length(file)) # return NA for inputs that can't be parsed
  fout[matches] <- sub("^(.*)\\.(csv|dat|hdr|img|brik|head|nii|txt|tsv|yaml|json|1d)(\\.gz|\\.bz2|\\.zip|\\.xz)?$", "\\1", file[matches], ignore.case = TRUE)
  return(fout)
}


#' Return the Next Values in a Vector (Lead)
#'
#' Returns a copy of the input vector with values shifted \code{n} steps forward, inserting \code{default} at the end.
#' This function mimics \code{dplyr::lead()} but is implemented without external dependencies.
#'
#' @param x A vector.
#' @param n An integer indicating the number of positions to lead by. Defaults to 1. If negative, this is equivalent to lagging.
#' @param default A value to use for padding the end of the returned vector. Defaults to \code{NA}.
#'
#' @return A vector of the same type as \code{x}, with values shifted \code{n} places forward.
#'
#' @seealso \code{\link{lag}}
#'
#' @examples
#' lead(1:5)
#' lead(1:5, 2)
#' lead(1:5, -1)  # Equivalent to lag(1:5)
#'
#' @keywords internal
#' @importFrom utils tail
#' @noRd
lead <- function(x, n = 1L, default = NA) {
  if (n < 0L) return(lag(x, -n, default))
  c(tail(x, -n), rep(default, n))
}

#' Return the Previous Values in a Vector (Lag)
#'
#' Returns a copy of the input vector with values shifted \code{n} steps backward, inserting \code{default} at the beginning.
#' This function mimics \code{dplyr::lag()} but avoids external dependencies.
#'
#' @param x A vector.
#' @param n An integer specifying how many positions to lag by. Defaults to 1. If negative, this is equivalent to leading.
#' @param default A value to pad the beginning of the returned vector. Defaults to \code{NA}.
#'
#' @return A vector of the same type as \code{x}, with values shifted \code{n} places backward.
#'
#' @seealso \code{\link{lead}}
#'
#' @examples
#' lag(1:5)
#' lag(1:5, 2)
#' lag(1:5, -1)  # Equivalent to lead(1:5)
#'
#' @keywords internal
#' @importFrom utils head
#' @noRd
lag <- function(x, n = 1L, default = NA) {
  if (n < 0L) return(lead(x, -n, default))
  c(rep(default, n), head(x, -n))
}

get_pipeline_status <- function(scfg) {
  # adapted from get_feat_status.
  # use to read timing files
  # if (!is.null(timing_file)) {
  #     timing <- readLines(timing_file)
  #      if (length(timing) > 0L) {
  #        # convert to POSIXct object to allow for any date calculations
  #        timing <- anytime::anytime(timing)
  #        feat_checks$feat_execution_start <- timing[1L]
  #        if (length(timing) == 2L) {
  #          feat_checks$feat_execution_end <- timing[2L]
  #          feat_checks$feat_execution_min <- as.numeric(difftime(timing[2L], timing[1L], units = "mins"))
  #        } else {
  #          lg$warn("Did not find two timing entries in %s.", timing_file)
  #          lg$warn("File contents: %s", timing)
  #        }
  #      }
  #   }
}

to_log <- function(logger, condition = "info", msg, ...) {
  if (checkmate::test_string(logger)) {
    logger <- lgr::get_logger_glue(logger)
  } else {
    checkmate::assert_class(logger, "LoggerGlue")
  }

  checkmate::assert_string(msg)
  checkmate::assert_string(condition)
  checkmate::assert_subset(condition, c("fatal", "error", "warn", "info", "debug", "trace"))
  logger[[condition]](msg, .envir = parent.frame()) # emit log message

  # pass through glue explicitly and combine into one string
  msg <- paste(glue::glue(msg, ..., .envir = parent.frame()), collapse = "\n")

  if (condition == "fatal") { # only fatal stops execution
    stop(msg, call. = FALSE)
  } else if (condition %in% c("warn", "error")) {
    warning(msg, call. = FALSE, immediate. = TRUE)
  } else if (condition == "info") {
    message(msg)
  }

  return(invisible(NULL))
}