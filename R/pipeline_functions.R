### Utility functions for the pipeline


#' Get the HPC job script for a given job name
#' @param scfg A list of configuration settings
#' @param job_name The name of the job (e.g., "fmriprep", "heudiconv")
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
#' @param scfg A list of configuration settings
#' @param job_name The name of the job (e.g., "fmriprep", "heudiconv")
#' @return A character string of scheduler arguments
#' @importFrom glue glue
#' @importFrom checkmate assert_string
#' @keywords internal
#' @noRd
get_job_sched_args <- function(scfg=NULL, job_name) {
  checkmate::assert_string(job_name)

  # TODO: need to use cli_opts approach to remove conflicting/redundant fields in sched_args for -n, -N, etc.

  sched_args <- scfg[[job_name]]$sched_args
  # convert empty strings to NULL for compatibility with glue
  if (length(sched_args) == 0L || is.na(sched_args[1L]) || sched_args[1L] == "") sched_args <- NULL

   if (scfg$compute_environment$scheduler == "slurm") {
     sched_args <- glue(
       "-N 1",
       "-n {scfg[[job_name]]$ncores}",
       "--time={hours_to_dhms(scfg[[job_name]]$nhours)}",
       "--mem={scfg[[job_name]]$memgb}g",
       "{sched_args}",
       .trim = TRUE, .sep = " ", .null = NULL
     )
   } else {
     sched_args <- glue(
       "-l nodes1:ppn={scfg[[job_name]]$ncores}",
       "-l walltime={hours_to_dhms(scfg[[job_name]]$nhours)}",
       "-l mem={scfg[[job_name]]$memgb}",
       "{sched_args}",
       .trim = TRUE, .sep = " ", .null = NULL
     )
   }
   
  return(sched_args)

}


# pretty_print_list(defaults)


setup_job <- function(scfg, job_name = NULL, defaults = NULL, fields = NULL) {
  if (is.null(scfg[[job_name]]$memgb) || glue("{job_name}/memgb") %in% fields) {
    scfg[[job_name]]$memgb <- prompt_input(
      instruct = glue("How many GB of memory should be used for running {job_name}?"),
      type = "numeric", lower = 1, upper = 1024, len = 1L, default = defaults$memgb
    )
  }

  if (is.null(scfg[[job_name]]$nhours) || glue("{job_name}/nhours") %in% fields) {
    scfg[[job_name]]$nhours <- prompt_input(
      instruct = glue("How many hours should each run of {job_name} request?"),
      type = "numeric", lower = 0.1, upper = 1000, len = 1L, default = defaults$nhours
    )
  }

  if (is.null(scfg[[job_name]]$ncores) || glue("{job_name}/ncores") %in% fields) {
    scfg[[job_name]]$ncores <- prompt_input(
      instruct = glue("How many cores/CPUs should each job request?"),
      type = "integer", lower = 1, upper = 1000, len = 1L, default = defaults$ncores
    )
  }

  if (is.null(scfg[[job_name]]$cli_options) || glue("{job_name}/cli_options") %in% fields) {
    scfg[[job_name]]$cli_options <- build_cli_args(args = scfg[[job_name]]$cli_options, instruct = glue("Specify any other {job_name} command line arguments. Press Enter when done."))
  }

  if (is.null(scfg[[job_name]]$sched_args) || glue("{job_name}/sched_args") %in% fields) {
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
#' @param scfg The study configuration object
#' @param sub_dir The directory of the target subject's folder within the BIDS structure
#' @return a configured lgr object for logging subject processing messages
#' @importFrom lgr get_logger_glue
#' @keywords internal
get_subject_logger <- function(scfg, sub_id) {
  checkmate::assert_directory_exists(scfg$project_directory)
  sub_dir <- file.path(scfg$project_directory, "logs", glue("sub-{sub_id}"))
  lg <- lgr::get_logger_glue(c("sub", sub_id))
  if (isTRUE(scfg$log_txt) && !"subject_logger" %in% names(lg$appenders)) {
    lg$add_appender(lgr::AppenderFile$new(file.path(sub_dir, glue("sub-{sub_id}_log.txt"))), name = "subject_logger")
  }

  return(lg)
}


#' Helper function to check whether a given file or directory exists and, optionally, is readable
#' @param f a file or directory to check for existence
#' @param description a character string describing what this file is if we are prompted to change it
#' @param directory if TRUE, check whether a directory exists. If FALSE (default), check that the file exists
#' @param prompt_change if TRUE, if the file/directory exists, ask the user if they wish to change the value. If so, return FALSE
#' @param check_readable if TRUE, validation fails (return `FALSE`) when the file/directory exists but is not readable
#' @return a boolean (`TRUE/FALSE`) indicating whether the file or directory exists and is valid
#' @importFrom checkmate assert_flag assert_string test_directory_exists test_file_exists
#' @keywords internal
validate_exists <- function(f, description="", directory=FALSE, prompt_change=FALSE, check_readable=TRUE) {  
  checkmate::assert_string(description)
  checkmate::assert_flag(directory)
  checkmate::assert_flag(prompt_change)
  checkmate::assert_flag(check_readable)

  if (directory) {
    func <- checkmate::test_directory_exists
    type <- "directory"
  } else {
    func <- checkmate::test_file_exists
    type <- "file"
  }

  if (!checkmate::test_atomic(f) || is.null(f) || length(f) == 0L || is.na(f[1L])) {
    return(FALSE)
  } else if (checkmate::test_character(f)) {
    if (length(f) > 1L) {
      warning("validate_exists only works with a single string as input, but we received a character vector. Using first element.")
      f <- f[1]
    }

    if (func(f)) {
      if (check_readable && !func(f, access = "r")) {
        warning(glue("Found existing {type}, but you do not have read permission: {f}"))
        return(FALSE)
      }

      if (isTRUE(prompt_change)) {
        cat(glue("Found existing {description}: {f}\n"))
        change <- prompt_input("Change setting?", type = "flag")
        if (isFALSE(change)) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        return(TRUE)
      }

    }
  } else {
    # some other non-character data type that somehow made it past check_atomic?
    return(FALSE)
  }
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

run_fsl_command <- function(args, fsldir=NULL, echo=TRUE, run=TRUE, log_file=NULL, intern=FALSE, stop_on_fail=TRUE, fsl_img=NULL, bind_paths=NULL) {
  
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

  #cat("FSL command: ", full_cmd, "\n")
  if (!is.null(log_file)) cat(args, file=log_file, append=TRUE, sep="\n")
  # if (!is.null(log_file)) cat("# FULL CMD: ", full_cmd, file=log_file, append=TRUE, sep="\n")
  if (isTRUE(echo)) cat(args, "\n")

  retcode <- if (isTRUE(run)) system(full_cmd) else 0 # return 0 if not run

  if (file.exists(efile)) {
    stderr <- readLines(efile)
    if (identical(character(0), stderr)) stderr <- ""
  } else {
    stderr <- ""
  }

  if (file.exists(ofile)) {
    stdout <- readLines(ofile)
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
    cat(errmsg, "\n", file = log_file, append = TRUE)
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
#' @details This function creates a NIfTI image using FSL’s \code{fslcreatehd}, fills it using \code{oro.nifti},
#' and writes it back to disk with dimensions \code{[x, 1, 1, time]}. Missing values are replaced with zero.
#'
#' @keywords internal
#' @importFrom oro.nifti readNIfTI writeNIfTI
#' @importFrom glue glue
mat_to_nii <- function(mat, ni_out="mat", fsl_img=NULL) {
  if (is.data.frame(mat)) mat <- as.matrix(mat)
  # this always puts regressors along the x dimension; y and z are singletons
  ydim <- zdim <- 1 # size of y and z dimensions
  xsz <- ysz <- zsz <- 1 # voxel size in x y z
  tr <- 1
  xorigin <- yorigin <- zorigin <- 0

  run_fsl_command(glue("fslcreatehd {ncol(mat)} {ydim} {zdim} {nrow(mat)} {xsz} {ysz} {zsz} {tr} {xorigin} {yorigin} {zorigin} 64 {ni_out}"), fsl_img = fsl_img, bind_paths=dirname(ni_out))

  ## read empty NIfTI into R
  nif <- readNIfTI(ni_out, reorient = FALSE)
  nif <- drop_img_dim(nif) # need to cleanup dim_ attribute to avoid writeNIfTI failure

  # populate nifti -- need to transpose to be consistent with column-wise array filling
  nif@.Data <- array(t(mat), dim = c(ncol(mat), 1, 1, nrow(mat))) # add singleton dimensions for y and z
  nif[is.na(nif)] <- 0 # cannot handle missingness in NIfTIs

  # write NIfTI with regressors back to file
  writeNIfTI(nif, filename = ni_out) # this returns the filename to the caller
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
#' @importFrom oro.nifti readNIfTI
#' @importFrom checkmate assert_file_exists
nii_to_mat <- function(ni_in) {
  checkmate::assert_file_exists(ni_in)

  nii <- readNIfTI(ni_in, reorient = FALSE, rescale_data = FALSE)
  mat <- t(nii[, 1, 1, ]) # x and z -- make back into time x variables
  return(mat)
}



#' Compute an intensity quantile from a NIfTI image
#'
#' Uses FSL's \code{fslstats} to compute a specified intensity quantile from a NIfTI image,
#' optionally restricted to a brain mask and excluding zero-valued voxels.
#'
#' @param in_file Path to the input NIfTI image file.
#' @param brain_mask Optional path to a binary brain mask image. If provided, quantiles are computed within the masked region.
#' @param quantile Numeric value between 0 and 100 indicating the desired quantile (e.g., 50 for median).
#' @param exclude_zero Logical; if \code{TRUE}, exclude zero-valued voxels from the computation.
#' @param log_file Optional file path to capture FSL command output.
#' @param fsl_img Optional Singularity image to execute FSL commands in a containerized environment.
#'
#' @return A single numeric value representing the requested intensity quantile.
#'
#' @keywords internal
#' @importFrom checkmate assert_number assert_file_exists test_file_exists
#' @importFrom glue glue
get_image_quantile <- function(in_file, brain_mask=NULL, quantile=50, exclude_zero=FALSE, log_file=NULL, fsl_img = NULL) {
  # checkmate::assert_file_exists(in_file)
  checkmate::assert_number(quantile, lower = 0, upper = 100)
  pstr <- ifelse(isTRUE(exclude_zero), "-P", "-p")
  if (is.null(brain_mask)) {
     quantile_value <- as.numeric(run_fsl_command(glue("fslstats {in_file} {pstr} {quantile}"), intern = TRUE, log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(in_file)))
  } else {
    if (!checkmate::test_file_exists(brain_mask)) checkmate::assert_file_exists(paste0(brain_mask, ".nii.gz"))
    quantile_value <- as.numeric(run_fsl_command(glue("fslstats {in_file} -k {brain_mask} {pstr} {quantile}"), intern = TRUE, log_file = log_file, fsl_img = fsl_img, bind_paths=dirname(in_file)))
  }
  return(quantile_value)
}


### CURRENTLY ONLY USED BY POSTPROCESSING


# Jun 2024: brain mask is required for calculating image quantiles for 2nd and 50th percentiles -- smoothing and intensity normalization
# Given that it is used only for these quantiles, the fmriprep mask should be fine for this purpose
# apply_mask is now considered an additional step that is optional and uses the brain_mask in the cfg

to_log <- function(str=NULL, log_file=NULL, stdout=TRUE) {
  checkmate::assert_string(str)
  checkmate::assert_string(log_file, null.ok = TRUE)
  if (is.null(str)) return(invisible(NULL))
  if (isTRUE(stdout)) cat(str, sep = "\n")
  if (!is.null(log_file)) cat(str, file = log_file, sep = "\n", append = TRUE)
  return(invisible(NULL))
}


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

