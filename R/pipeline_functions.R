### Utility functions for the pipeline

# Summarize mode/uid/gid for a filesystem path (used in permission diagnostics).
# @param path Character scalar path to describe.
# @param fallback_label String returned when \code{path} is not a valid string.
# @return A formatted string like "path [mode=755, uid=1000, gid=1000]".
describe_path_permissions <- function(path, fallback_label = "<unset>") {
  if (!checkmate::test_string(path)) return(fallback_label)
  info <- suppressWarnings(file.info(path))
  mode <- "unknown"
  if (is.data.frame(info) && nrow(info) > 0L && "mode" %in% names(info) &&
      length(info$mode) > 0L && !is.na(info$mode[1])) {
    mode <- as.character(as.octmode(info$mode[1]))
  }
  uid <- "unknown"
  if (is.data.frame(info) && nrow(info) > 0L && "uid" %in% names(info) &&
      length(info$uid) > 0L && !is.na(info$uid[1])) {
    uid <- as.character(info$uid[1])
  }
  gid <- "unknown"
  if (is.data.frame(info) && nrow(info) > 0L && "gid" %in% names(info) &&
      length(info$gid) > 0L && !is.na(info$gid[1])) {
    gid <- as.character(info$gid[1])
  }
  glue("{path} [mode={mode}, uid={uid}, gid={gid}]")
}


#' Get the HPC job script for a given job name
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @param job_name The name of the job (e.g., "fmriprep", "bids_conversion")
#' @param subject_suffix If TRUE, assume that the job script should have the suffix "_suffix". Applies
#'   to all subject-level processing steps, like job_name="bids_conversion" -> bids_conversion_subject.sbatch
#' @return The path to the job script
#' @importFrom glue glue
#' @importFrom checkmate assert_string test_file_exists
#' @keywords internal
#' @noRd
get_job_script <- function(scfg = NULL, job_name, subject_suffix = TRUE) {
  checkmate::assert_string(job_name)

  ext <- ifelse(scfg$compute_environment$scheduler == "torque", "pbs", "sbatch")
  sub_str <- if (isTRUE(subject_suffix)) "_subject" else ""
  expect_file <- glue("hpc_scripts/{job_name}{sub_str}.{ext}")
  script <- system.file(expect_file, package = "BrainGnomes")
  if (!checkmate::test_file_exists(script)) stop("In get_job_script, cannot find expected script file: ", expect_file)
  return(script)
}

#' Convert scheduler arguments into a scheduler-specific string
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @param job_name The name of the job (e.g., "fmriprep", "bids_conversion")
#' @param jobid_str An optional character string naming the job, passed to the HPC scheduler
#' @param stdout_log The file path to the log file used for stdout
#' @param stderr_log The file path to the log file used for stderr
#' @return A character string of scheduler arguments
#' @importFrom glue glue
#' @importFrom checkmate assert_string
#' @keywords internal
#' @noRd
get_job_sched_args <- function(scfg = NULL, job_name, jobid_str = NULL, stdout_log = NULL, stderr_log = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_string(job_name)
  checkmate::assert_string(jobid_str, null.ok = TRUE)
  checkmate::assert_string(stdout_log, null.ok = TRUE)
  checkmate::assert_string(stderr_log, null.ok = TRUE)
  
  # job_name must be present in scfg
  if (is.null(scfg[[job_name]])) stop("Cannot find job in scfg: ", job_name)

  # if no explicit job id string is given, use the job name
  if (is.null(jobid_str)) jobid_str <- job_name
  jobid_str <- gsub("\\s", "", jobid_str) # job name can't have spaces

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

    if (!is.null(stdout_log)) stdout_log <- glue("--output={shQuote(stdout_log)}")
    if (!is.null(stderr_log)) stderr_log <- glue("--error={shQuote(stderr_log)}")

    sched_args <- glue(
      "-N 1",
      "-n {ncores}",
      "--time={hours_to_dhms(nhours)}",
      "--mem={memgb}g",
      "--job-name={jobid_str}",
      "{stdout_log}",
      "{stderr_log}",
      "{paste(sched_args, collapse=' ')}",
      .trim = TRUE, .sep = " ", .null = NULL
    )
  } else {
    if (!is.null(sched_args)) sched_args <- sub("^\\s*#PBS\\s+", "", sched_args, ignore.case = TRUE)

    if (!is.null(stdout_log)) stdout_log <- glue("-o {shQuote(stdout_log)}")
    if (!is.null(stderr_log)) stderr_log <- glue("-e {shQuote(stderr_log)}")

    sched_args <- glue(
      "-l nodes=1:ppn={ncores}",
      "-l walltime={hours_to_dhms(nhours)}",
      "-l mem={memgb}",
      "-N {jobid_str}",
      "{stdout_log}",
      "{stderr_log}",
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
      prompt = "Memory (GB):",
      type = "numeric", lower = 1, upper = 1024, len = 1L, default = defaults$memgb
    )
  }

  if (glue("{job_name}/nhours") %in% fields) {
    scfg[[job_name]]$nhours <- prompt_input(
      instruct = glue("How many hours should each run of {job_name} request?"),
      prompt = "Run time (hours):",
      type = "numeric", lower = 0.1, upper = 1000, len = 1L, default = defaults$nhours
    )
  }

  if (glue("{job_name}/ncores") %in% fields) {
    scfg[[job_name]]$ncores <- prompt_input(
      instruct = glue("How many cores/CPUs should each job request?"),
      prompt = "CPU cores:",
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
#' @importFrom lgr get_logger_glue AppenderFile
#' @importFrom checkmate assert_directory_exists test_directory_exists
#' @keywords internal
get_subject_logger <- function(scfg, sub_id) {
  checkmate::assert_directory_exists(scfg$metadata$project_directory)
  sub_dir <- file.path(scfg$metadata$log_directory, glue("sub-{sub_id}"))
  if (!checkmate::test_directory_exists(sub_dir)) dir.create(sub_dir, showWarnings = FALSE, recursive = TRUE)
  lg <- lgr::get_logger_glue(c("sub", sub_id))
  if (!"subject_logger" %in% names(lg$appenders)) {
    lg$add_appender(lgr::AppenderFile$new(file.path(sub_dir, glue("sub-{sub_id}_log.txt"))), name = "subject_logger")
  }
  set_logger_threshold(lg, scfg$log_level)
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
#' @param verify_manifest Logical. If TRUE and a manifest exists in the database,
#'   verify that output files still exist and match (default TRUE).
#' @return List containing `complete` (logical), `dir`, `complete_file`,
#'   `db_status` (character or NA), `manifest_verified` (logical or NA),
#'   `verification_source` (character indicating how completion was determined),
#'   and optional `db_error` when DB lookup fails
#' @importFrom checkmate assert_choice assert_string
#' @keywords internal
is_step_complete <- function(scfg, sub_id, ses_id = NULL,
                             step_name, pp_stream = NULL,
                             verify_manifest = TRUE) {
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
  fail_file <- sub("_complete$", "_fail", complete_file)

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
        file.path(scfg$metadata$postproc_directory, glue("sub-{sub_id}"),
                  glue("ses-{ses_id}"))
      } else {
        file.path(scfg$metadata$postproc_directory, glue("sub-{sub_id}"))
      }
  )

  # Initialize result structure
  result <- list(
    complete = FALSE,
    dir = out_dir,
    complete_file = complete_file,
    db_status = NA_character_,
    manifest_verified = NA,
    verification_source = "none"
  )

  # Tier 1: Check job tracking database if available
  sqlite_db <- scfg$metadata$sqlite_db
  job_name_pattern <- if (!is.null(ses_id)) {
    glue("{name_tag}_sub-{sub_id}_ses-{ses_id}")
  } else {
    glue("{name_tag}_sub-{sub_id}")
  }

  db_record <- NULL
  db_query_error <- NULL
  if (!is.null(sqlite_db) && checkmate::test_file_exists(sqlite_db)) {
    db_record <- tryCatch({
      # Query for the most recent job matching this step/subject pattern
      con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
      on.exit(try(DBI::dbDisconnect(con)), add = TRUE)
      
      db_cols <- DBI::dbGetQuery(con, "PRAGMA table_info(job_tracking)")
      has_manifest <- "output_manifest" %in% db_cols$name
      fields <- if (has_manifest) {
        "job_id, status, output_manifest, time_ended"
      } else {
        "job_id, status, time_ended"
      }
      query <- glue(
        "SELECT {fields} ",
        "FROM job_tracking ",
        "WHERE job_name = ? ",
        "ORDER BY time_submitted DESC ",
        "LIMIT 1"
      )
      DBI::dbGetQuery(con, query, params = list(job_name_pattern))
    }, error = function(e) {
      db_query_error <<- conditionMessage(e)
      NULL
    })
  }

  if (!is.null(db_query_error)) {
    warning(
      glue(
        "is_step_complete could not query job tracking database '{sqlite_db}' ",
        "for job_name '{job_name_pattern}': {db_query_error}. ",
        "Treating step as incomplete and skipping legacy .complete/.fail fallback."
      ),
      call. = FALSE
    )
    result$complete <- FALSE
    result$verification_source <- "db_query_error"
    result$db_error <- db_query_error
    return(result)
  }

  if (!is.null(db_record) && nrow(db_record) > 0) {
    result$db_status <- db_record$status[1]

    if (result$db_status == "COMPLETED") {
      # Tier 1a: DB shows COMPLETED - verify manifest if available
      manifest_json <- if ("output_manifest" %in% names(db_record)) {
        db_record$output_manifest[1]
      } else {
        NA_character_
      }
      dir_exists <- checkmate::test_directory_exists(out_dir)

      if (verify_manifest && !is.null(manifest_json) && !is.na(manifest_json) && nchar(manifest_json) > 0) {
        verification <- verify_output_manifest(out_dir, manifest_json, check_mtime = FALSE)
        result$manifest_verified <- verification$verified

        if (isTRUE(verification$verified)) {
          result$complete <- TRUE
          result$verification_source <- "db_manifest_verified"
        } else if (is.na(verification$verified)) {
          # Manifest couldn't be parsed; require output directory to exist
          result$complete <- dir_exists
          result$verification_source <- if (dir_exists) "db_status_dir_exists" else "db_status_dir_missing"
        } else {
          # Manifest verification failed - files missing or changed
          result$complete <- FALSE
          result$verification_source <- "db_manifest_failed"
          result$manifest_details <- verification
        }
      } else {
        # No manifest available; require output directory to exist
        result$complete <- dir_exists
        result$verification_source <- if (dir_exists) "db_status_dir_exists" else "db_status_dir_missing"
      }
      return(result)
    } else if (result$db_status %in% c("FAILED", "FAILED_BY_EXT")) {
      # DB shows failure
      result$complete <- FALSE
      result$verification_source <- "db_status_failed"
      return(result)
    }
    # If status is QUEUED or STARTED, fall through to .complete file check
  }

  # Tier 2: Fall back to .complete file check (legacy or no DB record)
  complete_exists <- checkmate::test_file_exists(complete_file)
  fail_exists <- checkmate::test_file_exists(fail_file)
  complete_newer_than_fail <- FALSE

  if (complete_exists && fail_exists) {
    complete_mtime <- file.info(complete_file)$mtime
    fail_mtime <- file.info(fail_file)$mtime
    if (!is.na(complete_mtime) && !is.na(fail_mtime)) {
      complete_newer_than_fail <- complete_mtime > fail_mtime
    }
  }

  dir_exists <- checkmate::test_directory_exists(out_dir)
  complete_by_file <- dir_exists && complete_exists && (!fail_exists || complete_newer_than_fail)

  result$complete <- complete_by_file
  result$verification_source <- if (complete_by_file) "complete_file" else "incomplete"

  result
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

format_submission_cmd <- function(job_id, truncate = FALSE, max_chars = 100,
                                  unavailable = "<submission command unavailable>") {
  checkmate::assert_flag(truncate)
  checkmate::assert_int(max_chars, lower = 1)
  checkmate::assert_string(unavailable)

  cmd <- attr(job_id, "cmd")
  if (!checkmate::test_string(cmd)) return(unavailable)
  if (!isTRUE(truncate)) return(cmd)
  truncate_str(cmd, max_chars = max_chars)
}

log_submission_command <- function(logger, job_id, label) {
  checkmate::assert_string(label)

  short_cmd <- format_submission_cmd(job_id, truncate = TRUE)
  full_cmd <- format_submission_cmd(job_id, truncate = FALSE)
  has_job_id <- checkmate::test_string(job_id) && nzchar(job_id)

  if (isTRUE(has_job_id)) {
    to_log(logger, "info", "Scheduled {label}: {short_cmd}")
  } else {
    to_log(logger, "warn", "Failed to schedule {label}. Submission command: {short_cmd}")
  }
  to_log(logger, "debug", "Full command: {full_cmd}")

  invisible(NULL)
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
#' @importFrom lgr AppenderConsole AppenderFile get_logger_glue
#' @importFrom checkmate assert_string assert_character assert_flag assert_directory_exists assert_file_exists
#' @export
run_fsl_command <- function(args, fsldir=NULL, echo=TRUE, run=TRUE, intern=FALSE, stop_on_fail=TRUE, log_file=NULL, use_lgr=TRUE, fsl_img=NULL, bind_paths=NULL) {
  checkmate::assert_character(args)
  checkmate::assert_string(fsldir, null.ok = TRUE)
  if (!is.null(fsldir) && is.null(fsl_img)) checkmate::assert_directory_exists(fsldir)

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
    lg <- lgr::get_logger_glue("run_fsl_command")
    lg$config(NULL) # clear any previous appenders/config to avoid conflicts
    lg$set_propagate(FALSE) # avoid inherited console output
    if (echo) lg$add_appender(lgr::AppenderConsole$new(), name = "console")
    if (!is.null(log_file)) lg$add_appender(lgr::AppenderFile$new(log_file), name = "file")
  }

  # paste into single string if multiple character arguments are passed
  if (length(args) > 1L) args <- paste(args, collapse=" ")

  if (!is.null(fsl_img)) {
    # if we are using a singularity container, always look inside the container for FSLDIR
    checkmate::assert_file_exists(fsl_img, access = "r")
    fsldir <- suppressWarnings(system2("singularity", c("exec", fsl_img, "printenv", "FSLDIR"), stdout = TRUE, stderr = FALSE))
    fsldir <- if (length(fsldir) > 0L) trimws(fsldir[1L]) else ""
    if (!nzchar(fsldir)) stop("Cannot find FSLDIR inside singularity container")
  } else if (is.null(fsldir)) {
    # look for FSLDIR in system environment if not passed in
    fsldir <- trimws(Sys.getenv("FSLDIR", unset = ""))
    if (!nzchar(fsldir)) {
      read_startup_fsldir <- function(startup_file) {
        startup_path <- path.expand(startup_file)
        if (!file.exists(startup_path)) return("")
        startup_cmd <- paste(
          ".", shQuote(startup_path),
          ">/dev/null 2>&1; printf \"%s\" \"$FSLDIR\""
        )
        out <- suppressWarnings(system2("bash", c("-lc", startup_cmd), stdout = TRUE, stderr = FALSE))
        if (length(out) == 0L) return("")
        trimws(out[1L])
      }

      # fallback order: profile, then bashrc, then feat on PATH
      fsldir <- read_startup_fsldir("~/.profile")
      if (!nzchar(fsldir)) fsldir <- read_startup_fsldir("~/.bashrc")
      if (!nzchar(fsldir)) {
        feat_loc <- suppressWarnings(system2("bash", c("-lc", "command -v feat"), stdout = TRUE, stderr = FALSE))
        if (length(feat_loc) > 0L && nzchar(trimws(feat_loc[1L]))) {
          fsldir <- dirname(dirname(trimws(feat_loc[1L])))
        }
      }
    }
  }

  fsldir <- trimws(as.character(fsldir)[1L])
  if (!nzchar(fsldir)) {
    stop("Cannot resolve FSLDIR. Set `fsldir` explicitly or export FSLDIR before calling run_fsl_command().")
  }
  fslconf_path <- file.path(fsldir, "etc", "fslconf", "fsl.sh")
  if (!is.null(fsl_img)) {
    # In container mode, FSLDIR usually refers to a path that only exists inside the image.
    fslconf_check <- paste("test -f", shQuote(fslconf_path))
    fslconf_status <- suppressWarnings(system2(
      "singularity",
      c("exec", fsl_img, "bash", "-lc", fslconf_check),
      stdout = FALSE,
      stderr = FALSE
    ))
    if (!identical(as.integer(fslconf_status), 0L)) {
      stop("Resolved FSLDIR is invalid (missing etc/fslconf/fsl.sh): ", fsldir)
    }
  } else if (!file.exists(fslconf_path)) {
    stop("Resolved FSLDIR is invalid (missing etc/fslconf/fsl.sh): ", fsldir)
  }

  # Avoid poisoning host environment with container-only FSLDIR values.
  if (is.null(fsl_img)) Sys.setenv(FSLDIR=fsldir)
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
    to_log(lg, "info", "FSL command: {args}")
    to_log(lg, "debug", "Shell command: {full_cmd}")
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
      to_log(lg, "error", errmsg)
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
# apply_mask is now considered an additional step that can use a user-specified mask file

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

#' Resolve a desired log level
#' @keywords internal
#' @noRd
resolve_log_level <- function(level = NULL) {
  if (length(level) > 1L) level <- level[1L]
  if (is.null(level) || is.na(level) || !nzchar(level)) {
    level <- getOption("BrainGnomes.log_level", NULL)
  }
  if (is.null(level) || is.na(level) || !nzchar(level)) return(NULL)
  toupper(level)
}

#' Ensure a logger honors the requested threshold
#' @keywords internal
#' @noRd
set_logger_threshold <- function(logger, level = NULL) {
  level <- resolve_log_level(level)
  if (!is.null(level)) {
    try(logger$set_threshold(level), silent = TRUE)
  }
  logger
}

#' Return an lgr LoggerGlue object with the configured threshold
#' @keywords internal
#' @noRd
resolve_logger <- function(logger = NULL, level = NULL) {
  if (is.null(logger)) {
    logger <- lgr::get_logger_glue("BrainGnomes")
  } else if (checkmate::test_string(logger)) {
    logger <- lgr::get_logger_glue(logger)
  } else if (inherits(logger, "LoggerGlue")) {
    # already a LoggerGlue; keep configuration as-is
  } else if (inherits(logger, "Logger")) {
    logger_name <- logger$name
    if (is.null(logger_name) || !nzchar(logger_name)) logger_name <- "BrainGnomes"
    logger <- lgr::get_logger_glue(logger_name)
  } else {
    checkmate::assert_class(logger, "LoggerGlue")
  }

  set_logger_threshold(logger, level)
}

format_numeric_preview <- function(x, digits = 3L) {
  out <- as.character(x)
  non_missing <- !is.na(x)
  if (any(non_missing)) {
    out[non_missing] <- sprintf(paste0("%.", digits, "g"), x[non_missing])
  }
  out
}

format_arg_preview <- function(value, max_values = 3L) {
  if (is.null(value)) return("NULL")
  if (is.atomic(value) && length(value) == 1L) {
    if (is.character(value)) return(glue::glue('"{truncate_str(value, 60)}"'))
    if (is.numeric(value)) return(format_numeric_preview(value))
    return(as.character(value))
  }
  if (is.atomic(value)) {
    vals <- head(value, max_values)
    if (is.numeric(vals)) {
      vals_chr <- paste(format_numeric_preview(vals), collapse = ", ")
    } else {
      vals_chr <- paste(as.character(vals), collapse = ", ")
    }
    suffix <- if (length(value) > max_values) glue::glue(", ... len={length(value)}") else ""
    return(glue::glue("c({vals_chr}{suffix})"))
  }
  if (is.matrix(value)) {
    return(glue::glue("matrix[{nrow(value)}x{ncol(value)}]"))
  }
  if (is.data.frame(value)) {
    return(glue::glue("data.frame[{nrow(value)}x{ncol(value)}]"))
  }
  if (inherits(value, "array")) {
    dims <- paste(dim(value), collapse = "x")
    return(glue::glue("array[{dims}]"))
  }
  if (is.list(value)) {
    return(glue::glue("list(len={length(value)})"))
  }
  if (is.environment(value)) return("<environment>")
  if (is.function(value)) return("<function>")
  classes <- paste(class(value), collapse = ",")
  glue::glue("<{classes}>")
}

run_logged <- function(fun, ..., logger = NULL, fun_label = NULL, log_level = "info") {
  fun_obj <- match.fun(fun)
  label <- if (!is.null(fun_label)) {
    fun_label
  } else if (is.character(fun)) {
    fun
  } else {
    paste(deparse(substitute(fun)), collapse = "")
  }
  args <- list(...)
  arg_names <- names(args)
  arg_desc <- vapply(seq_along(args), function(ii) {
    name <- arg_names[[ii]]
    preview <- format_arg_preview(args[[ii]])
    if (is.null(name) || !nzchar(name)) preview else glue::glue("{name}={preview}")
  }, character(1), USE.NAMES = FALSE)
  arg_text <- paste(arg_desc, collapse = ", ")
  if (!nzchar(arg_text)) arg_text <- ""
  to_log(logger, log_level, "Running {label}({arg_text})")
  start_time <- Sys.time()
  res <- tryCatch({
    result <- fun_obj(...)
    elapsed <- signif(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 3)
    to_log(logger, "debug", "{label} completed in {elapsed} seconds")
    result
  }, error = function(e) {
    err_msg <- glue::glue("Error running {label}: {conditionMessage(e)}")
    to_log(logger, "error", "{err_msg}")
    current_level <- resolve_log_level()
    if (!is.null(current_level) && current_level %in% c("DEBUG", "TRACE")) {
      trace_lines <- format(sys.calls())
      trace_lines <- paste(rev(trace_lines), collapse = "\n")
      to_log(logger, "debug", "Stack trace for {label}:\n{trace_lines}")
    }
    stop(e)
  })
  res
}

to_log <- function(logger, condition = "info", msg, info_message = FALSE, ...) {
  logger <- resolve_logger(logger)

  checkmate::assert_string(msg)
  checkmate::assert_string(condition)
  checkmate::assert_choice(condition, c("fatal", "error", "warn", "info", "debug", "trace"))
  logger[[condition]](msg, .envir = parent.frame()) # emit log message

  # pass through glue explicitly and combine into one string
  msg <- paste(glue::glue(msg, ..., .envir = parent.frame()), collapse = "\n")

  if (condition == "fatal") { # only fatal stops execution
    stop(msg, call. = FALSE)
  } else if (condition %in% c("warn", "error")) {
    warning(msg, call. = FALSE, immediate. = FALSE)
  } else if (condition == "info" && isTRUE(info_message)) { # don't issue message() by default
    message(msg)
  }

  return(invisible(NULL))
}

#' Determine if a path lies outside the project directory
#'
#' @param path The path to evaluate.
#' @param project_dir The root project directory.
#' @return `TRUE` if `path` is not within `project_dir`, otherwise `FALSE`.
#' @keywords internal
is_external_path <- function(path, project_dir) {
  # not sure this will ever be useful, but convert paths to forward slash no matter what
  path <- gsub("\\\\", "/", path)
  project_dir <- gsub("\\\\", "/", project_dir)
  
  # only normalize if the directories both exist.
  # otherwise, we get odd /var -> /private/var expansion when TRUE, but not when FALSE on MacOS
  if (dir.exists(path) && dir.exists(project_dir)) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  }
  
  proj_slash <- paste0(project_dir, "/")
  inside <- identical(path, project_dir) || startsWith(path, proj_slash)
  !inside
}

# little function to attempt to make a file/directory user-writable
ensure_user_writable <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)
  if (!file.exists(path)) stop("Path does not exist: ", path)
  
  # Already writable for current user?
  if (isTRUE(file.access(path, 2L) == 0L)) return(TRUE)
  
  # ---- helpers -------------------------------------------------------------
  # Cross-platform stat to get owner *name*
  get_owner <- function(p) {
    p <- normalizePath(p, mustWork = TRUE)
    q <- shQuote(p)
    
    # GNU stat
    ow <- suppressWarnings(system2("stat", c("-c", "%U", q),
                                   stdout = TRUE, stderr = FALSE))
    if (length(ow) == 1L && !grepl("invalid option|usage:", ow, ignore.case = TRUE)) {
      return(ow)
    }
    
    # BSD/macOS stat
    ow <- suppressWarnings(system2("stat", c("-f", "%Su", q),
                                   stdout = TRUE, stderr = FALSE))
    if (length(ow) == 1L) return(ow)
    
    stop("Could not retrieve file owner via 'stat'.")
  }
  
  add_user_write <- function(p) {
    info <- file.info(p, extra_cols = TRUE)
    cur  <- info$mode
    if (is.na(cur)) return(FALSE)
    new_mode <- bitwOr(cur, as.integer(strtoi("0200", base = 8L)))  # user-write bit
    ok <- isTRUE(Sys.chmod(p, mode = as.octmode(new_mode)))
    ok && isTRUE(file.access(p, 2L) == 0L)
  }
  
  # ---- main logic ----------------------------------------------------------
  whoami <- suppressWarnings(system2("id", "-un", stdout = TRUE, stderr = FALSE))
  if (!length(whoami)) whoami <- Sys.info()[["user"]]
  if (!length(whoami) || is.na(whoami) || !nzchar(whoami)) {
    whoami <- Sys.getenv("USER", unset = NA_character_)
  }
  
  owner <- get_owner(path)
  
  if (!is.na(owner) && identical(owner, whoami)) {
    if (add_user_write(path)) return(TRUE)
  }
  
  # Either not the owner, or chmod failed
  FALSE
}
