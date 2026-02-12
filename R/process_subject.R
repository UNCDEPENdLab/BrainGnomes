get_postprocess_stream_names <- function(scfg) {
  if (is.null(scfg$postprocess)) {
    # warning("No postprocess streams available because $postprocess is not populated")
    return(NULL)
  } else {
    # only enable: TRUE/FALSE exists as a setting at the top level of $postprocess
    # otherwise, any element of $postprocess is the name of a postprocess stream, like $postprocess$my_stream1
    return(setdiff(names(scfg$postprocess), "enable"))
  }
}

null_empty <- function(x) {
  if (is.na(x) || identical(x, character(0)) || identical(x, list()) || length(x) == 0L || x[1L] == "") {
    x <- NULL
  }
  return(x)
}

check_write_target <- function(path, label) {
  if (!checkmate::test_string(path)) {
    return(NULL)
  }

  if (file.exists(path)) {
    writable <- isTRUE(file.access(path, 2L) == 0L)
    traversable <- if (dir.exists(path)) isTRUE(file.access(path, 1L) == 0L) else TRUE
    if (writable && traversable) return(NULL)
    return(glue("{label} is not writable: {describe_path_permissions(path)}"))
  }

  parent <- normalizePath(dirname(path), winslash = "/", mustWork = FALSE)
  while (!dir.exists(parent)) {
    next_parent <- dirname(parent)
    if (identical(next_parent, parent)) break
    parent <- next_parent
  }

  if (!dir.exists(parent)) {
    return(glue("Cannot locate an existing parent directory for {label}: {path}"))
  }

  can_create <- isTRUE(file.access(parent, 2L) == 0L) && isTRUE(file.access(parent, 1L) == 0L)
  if (can_create) return(NULL)
  glue("{label} does not exist and cannot be created because parent is not writable: {describe_path_permissions(parent)}")
}

check_write_target_cached <- function(path, label, check_cache = NULL) {
  if (!checkmate::test_string(path)) return(NULL)
  if (is.null(check_cache)) return(check_write_target(path, label))
  if (!is.environment(check_cache)) stop("check_cache must be an environment when provided.")

  # Cache key is the normalized path only (label-agnostic).  We only cache
  # writable (NULL) outcomes so that failures are always rechecked with the
  # caller's label for an accurate error message.
  cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (exists(cache_key, envir = check_cache, inherits = FALSE)) {
    return(NULL)        # previously verified writable
  }

  issue <- check_write_target(path, label)
  if (is.null(issue)) {
    assign(cache_key, TRUE, envir = check_cache)
  }
  issue
}

collect_submit_permission_issues <- function(scfg, step_name, sub_id, ses_id = NULL,
                                            stdout_log = NULL, stderr_log = NULL,
                                            sqlite_db = NULL, check_cache = NULL) {
  if (!is.null(check_cache) && !is.environment(check_cache)) {
    stop("check_cache must be an environment when provided.")
  }

  issues <- c()

  if (checkmate::test_string(stdout_log)) {
    issue <- check_write_target_cached(dirname(stdout_log), "stdout log directory", check_cache)
    if (!is.null(issue)) issues <- c(issues, issue)
  }
  if (checkmate::test_string(stderr_log)) {
    issue <- check_write_target_cached(dirname(stderr_log), "stderr log directory", check_cache)
    if (!is.null(issue)) issues <- c(issues, issue)
  }
  if (checkmate::test_string(sqlite_db)) {
    issue <- check_write_target_cached(sqlite_db, "job tracking SQLite database", check_cache)
    if (!is.null(issue)) issues <- c(issues, issue)
  }

  if (step_name %in% c("fmriprep", "mriqc", "aroma", "postprocess", "extract_rois")) {
    issue <- check_write_target_cached(scfg$metadata$scratch_directory, "scratch directory", check_cache)
    if (!is.null(issue)) issues <- c(issues, issue)
  }
  if (step_name %in% c("fmriprep", "mriqc")) {
    issue <- check_write_target_cached(scfg$metadata$templateflow_home, "templateflow_home directory", check_cache)
    if (!is.null(issue)) issues <- c(issues, issue)
  }

  output_dir <- switch(step_name,
    bids_conversion = scfg$metadata$bids_directory,
    mriqc = scfg$metadata$mriqc_directory,
    fmriprep = scfg$metadata$fmriprep_directory,
    aroma = scfg$metadata$fmriprep_directory,
    postprocess = {
      d <- file.path(scfg$metadata$postproc_directory, glue("sub-{sub_id}"))
      if (!is.null(ses_id) && !is.na(ses_id)) d <- file.path(d, glue("ses-{ses_id}"))
      d
    },
    extract_rois = scfg$metadata$rois_directory,
    NULL
  )
  if (!is.null(output_dir)) {
    issue <- check_write_target_cached(output_dir, glue("{step_name} output directory"), check_cache)
    if (!is.null(issue)) issues <- c(issues, issue)
  }

  return(issues)
}

#' Preprocess a single subject
#' @param scfg A list of configuration settings
#' @param sub_cfg A data.frame of subject configuration settings
#' @param steps A named logical vector indicating which steps to run
#' @param postprocess_streams Optional character vector of postprocess configuration names to run. If NULL,
#'   all available streams will be run.
#' @param parent_ids An optional character vector of HPC job ids that must complete before this subject is run.
#' @param sequence_id An identifying ID for a set of jobs in a sequence used for job tracking
#' @param permission_check_cache Optional environment used to memoize write-permission checks
#'   across repeated submissions.
#' @return A logical value indicating whether the preprocessing was successful
#' @details
#' When postprocessing is requested without running `fmriprep`, the function verifies
#' that the expected fMRIPrep outputs exist. If the configured fMRIPrep directory lies
#' outside the project directory, only the existence of the subject's directory is
#' required. For fMRIPrep directories inside the project directory, a `.complete`
#' file in the project's log directory is still necessary.
#' @importFrom glue glue
#' @importFrom checkmate assert_class assert_list assert_names assert_logical
#' @keywords internal
process_subject <- function(scfg, sub_cfg = NULL, steps = NULL, postprocess_streams = NULL, extract_streams = NULL,
                            parent_ids = NULL, sequence_id = NULL, permission_check_cache = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_data_frame(sub_cfg)
  expected_fields <- c("sub_id", "ses_id", "dicom_sub_dir", "dicom_ses_dir", "bids_sub_dir", "bids_ses_dir")
  checkmate::assert_names(names(sub_cfg), must.include = expected_fields, type = "unique")
  stopifnot(length(unique(sub_cfg$sub_id)) == 1L)
  multi_session <- nrow(sub_cfg) > 1L
  if (multi_session) {
    if (any(is.na(sub_cfg$ses_id))) stop("Session IDs are required for multi-session inputs to process_subject.")
    if (any(duplicated(sub_cfg$ses_id))) stop("Duplicate session IDs found in sub_cfg. process_subject requires unique session IDs.")
  }
  checkmate::assert_logical(steps, names = "unique")
  checkmate::assert_character(postprocess_streams, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_character(extract_streams, null.ok = TRUE, any.missing = FALSE)
  if (!is.null(permission_check_cache) && !is.environment(permission_check_cache)) {
    stop("permission_check_cache must be an environment when provided.")
  }
  if (is.null(permission_check_cache)) permission_check_cache <- new.env(parent = emptyenv())
  expected <- c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess", "extract_rois")
  for (ee in expected) if (is.na(steps[ee])) steps[ee] <- FALSE # ensure we have valid logicals for expected fields
  
  sub_id <- sub_cfg$sub_id[1L]
  bids_sub_dir <- sub_cfg$bids_sub_dir[1L]

  # Guard: verify the subject log directory is writable *before* setting up the
  # logger, which does dir.create + file appender and would otherwise crash with
  # an opaque lgr error if the log directory is unwritable.
  sub_log_dir <- file.path(scfg$metadata$log_directory, glue("sub-{sub_id}"))
  log_dir_issue <- check_write_target_cached(sub_log_dir, "subject log directory", permission_check_cache)
  if (!is.null(log_dir_issue)) {
    warning("Skipping subject ", sub_id, " because subject log directory is not writable:\n  ",
            log_dir_issue, call. = FALSE)
    return(TRUE)
  }

  lg <- get_subject_logger(scfg, sub_id)
  preflight_state <- new.env(parent = emptyenv())
  preflight_state$failed <- FALSE
  
  bids_conversion_ids <- mriqc_id <- fmriprep_id <- aroma_id <- postprocess_ids <- extract_ids <- NULL
  
  # BIDS conversion and postprocessing are session-specific, so we need to check for the session ID
  # fmriprep, MRIQC, and AROMA are subject-level processes (sessions nested within subjects)
  
  # .*complete files should always be placed in the subject BIDS directory
  # determine status of processing -- seems like we could swap in queries from job tracker
  submit_step <- function(name, row_idx = 1L, parent_ids = NULL, pp_stream = NULL, ex_stream = NULL) {
    session_level <- name %in% c("bids_conversion", "postprocess") # only these two are session-level
    
    name_tag <- name # identifier for this step used in complete file and job names
    if (name == "postprocess") {
      if (is.null(pp_stream)) {
        stop("Cannot run submit_step for postprocessing without a stream specified by pp_stream")
      } else {
        name_tag <- glue("{name}_{pp_stream}") # modify the tag to be specific to postprocessing this stream
      }
    }
    
    sub_id <- null_empty(sub_cfg$sub_id[row_idx]) # make NULL on empty to avoid env export in submit
    ses_id <- null_empty(sub_cfg$ses_id[row_idx])
    has_ses <- !is.null(ses_id)
    sub_str <- glue("_sub-{sub_id}") # qualifier for .complete file
    if (has_ses && session_level) sub_str <- glue("{sub_str}_ses-{ses_id}")
    sub_dir <- file.path(scfg$metadata$log_directory, glue("sub-{sub_id}"))
    complete_file <- file.path(sub_dir, glue(".{name_tag}{sub_str}_complete")) # full path to expected complete file
    file_exists <- checkmate::test_file_exists(complete_file)
    fail_file <- sub("_complete$", "_fail", complete_file)
    fail_exists <- checkmate::test_file_exists(fail_file)
    
    job_id <- NULL
    # skip out if this step is not requested or it is already complete
    if (!isTRUE(steps[[name]])) {
      to_log(lg, "debug", "Skipping {name} for {sub_id} because step is not requested.")
      return(job_id)
    } else if (file_exists && !isTRUE(scfg$force)) {
      to_log(lg, "info", "Skipping {name_tag} for {sub_id} because {complete_file} already exists and force = FALSE.")
      return(job_id)
    }
    
    # clear existing complete file if we are starting over on this step
    if (file_exists) {
      to_log(lg, "info", "Removing existing .complete file: {complete_file}")
      unlink(complete_file)
    }
    if (isTRUE(scfg$force) && fail_exists) {
      to_log(lg, "info", "Removing existing .fail file: {fail_file}")
      unlink(fail_file)
    }
    
    # shared components across specific jobs
    jobid_str <- ifelse(has_ses, glue("{name_tag}_sub-{sub_id}_ses-{ses_id}"), glue("{name_tag}_sub-{sub_id}"))
    
    tracking_args <- list(
      job_name = jobid_str, 
      sequence_id = sequence_id, 
      n_nodes = 1
      )
    tracking_sqlite_db <- scfg$metadata$sqlite_db
    
    log_level_value <- resolve_log_level(scfg$log_level)
    if (is.null(log_level_value)) log_level_value <- "INFO"
    env_variables <- c(
      debug_pipeline = scfg$debug,
      pkg_dir = find.package(package = "BrainGnomes"), # location of installed R package
      R_HOME = R.home(), # populate location of R installation so that it can be used by any child R jobs
      log_file = lg$appenders$subject_logger$destination, # write to same file as subject lgr
      stdout_log = glue("{scfg$metadata$log_directory}/sub-{sub_id}/{jobid_str}_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out"),
      stderr_log = glue("{scfg$metadata$log_directory}/sub-{sub_id}/{jobid_str}_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err"),
      complete_file = complete_file,
      insert_tracked_job_path = system.file("insert_tracked_job.R", package = "BrainGnomes"),
      upd_job_status_path = system.file("upd_job_status.R", package = "BrainGnomes"),
      add_parent_path = system.file("add_parent.R", package = "BrainGnomes"),
      log_level = log_level_value
    )

    preflight_issues <- collect_submit_permission_issues(
      scfg = scfg,
      step_name = name,
      sub_id = sub_id,
      ses_id = ses_id,
      stdout_log = unname(env_variables["stdout_log"]),
      stderr_log = unname(env_variables["stderr_log"]),
      sqlite_db = tracking_sqlite_db,
      check_cache = permission_check_cache
    )
    if (length(preflight_issues) > 0L) {
      preflight_state$failed <- TRUE
      issue_text <- paste(paste0("  - ", preflight_issues), collapse = "\n")
      to_log(
        lg, "warn",
        "Exiting process_subject for {sub_id} because preflight permission checks failed for {name_tag}:\n{issue_text}"
      )
      return(job_id)
    }
    
    sched_script <- get_job_script(scfg, name) # lookup location of HPC script to run
    sched_call <- list(job_name = name, jobid_str = jobid_str, stdout_log = env_variables["stdout_log"], stderr_log = env_variables["stderr_log"])
    if (name == "postprocess") {
      scfg_tmp <- scfg # postprocessing has a nested structure, with multiple configurations -- use the one currently requested
      scfg_tmp$postprocess <- scfg$postprocess[[pp_stream]]
      sched_call$scfg <- scfg_tmp
      tracking_args <- c(
        tracking_args,
        n_cpus = scfg_tmp[[name]]$ncores,
        wall_time = hours_to_dhms(scfg_tmp[[name]]$nhours),
        mem_total = scfg_tmp[[name]]$memgb,
        scheduler = scfg_tmp$compute_environment$scheduler
      )
    } else if (name == "extract_rois") {
      scfg_tmp <- scfg # postprocessing has a nested structure, with multiple configurations -- use the one currently requested
      scfg_tmp$extract_rois <- scfg$extract_rois[[ex_stream]]
      sched_call$scfg <- scfg_tmp
      tracking_args <- c(
        tracking_args,
        n_cpus = scfg_tmp[[name]]$ncores,
        wall_time = hours_to_dhms(scfg_tmp[[name]]$nhours),
        mem_total = scfg_tmp[[name]]$memgb,
        scheduler = scfg_tmp$compute_environment$scheduler
      )
    } else {
      sched_call$scfg <- scfg
      tracking_args <- c(
        tracking_args,
        n_cpus = scfg[[name]]$ncores,
        wall_time = hours_to_dhms(scfg[[name]]$nhours),
        mem_total = scfg[[name]]$memgb,
        scheduler = scfg$compute_environment$scheduler
      )
    }
    
    sched_args <- do.call(get_job_sched_args, sched_call)
    tracking_args$scheduler_options <- sched_args
    
    # determine the directory to use for the job submission
    if (session_level && has_ses) {
      # if it's a session-level process and we have a valid session-level input, use the session directory
      dir <- ifelse(name == "bids_conversion", sub_cfg$dicom_ses_dir[row_idx], sub_cfg$bids_ses_dir[row_idx])
    } else {
      # if it's a subject-level process or we don't have a valid session-level input, use the subject directory
      dir <- ifelse(name == "bids_conversion", sub_cfg$dicom_sub_dir[row_idx], sub_cfg$bids_sub_dir[row_idx])
    }
    
    # launch submission function -- these all follow the same input argument structure
    parent_txt <- if (!is.null(parent_ids) && length(parent_ids) > 0L) paste(parent_ids, collapse = ", ") else "<none>"
    to_log(lg, "debug", "submit_{name_tag} will run from {dir} with parent IDs: {parent_txt}")
    to_log(lg, "debug", "Launching submit_{name_tag} for subject: {sub_id}")
    args <- list(scfg, dir, sub_id, ses_id, env_variables, sched_script, sched_args, parent_ids, lg, tracking_sqlite_db, tracking_args)
    if (name == "postprocess") args$pp_stream <- pp_stream # populate the current postprocess config to run
    if (name == "extract_rois") args$ex_stream <- ex_stream # populate the current extract_rois config to run
    job_id <- do.call(glue("submit_{name}"), args)
    
    return(job_id)
  }

  to_log(lg, "info", "Processing subject {sub_id} with {nrow(sub_cfg)} sessions.")
  # to_log(lg, "info", "Processing steps: {glue_collapse(names(steps), sep = ', ')}")
  
  ## Handle BIDS conversion -- session-level
  n_inputs <- nrow(sub_cfg)
  
  # need unlist because NULL will be returned for jobs not submitted -- yielding a weird list of NULLs
  bids_conversion_ids <- unlist(lapply(seq_len(n_inputs), function(idx) submit_step("bids_conversion", row_idx = idx, parent_ids = parent_ids)))
  if (isTRUE(preflight_state$failed)) return(TRUE)
  
  if (isTRUE(steps["bids_conversion"])) {
    if (!is.na(bids_sub_dir)) to_log(lg, "info", "BIDS directory: {bids_sub_dir}")

    # Use expected directory as input to subsequent steps, anticipating that conversion completes
    # and the expected directory is created. If conversion fails, the dependent jobs should automatically fail.
    bids_sub_dir <- file.path(scfg$metadata$bids_directory, glue("sub-{sub_cfg$sub_id[1L]}"))
    bids_ses_dir <- if (multi_session) file.path(scfg$metadata$bids_directory, glue("sub-{sub_cfg$sub_id}"), glue("ses-{sub_cfg$ses_id}")) else rep(NA_character_, nrow(sub_cfg))
    
    # When bids_sub_dir and bids_ses_dir exist, do they match these expectations?
    extant_bids <- !is.na(sub_cfg$bids_sub_dir)
    if (!identical(sub_cfg$bids_sub_dir[extant_bids], bids_sub_dir[extant_bids])) {
      to_log(lg, "warn", "Exiting process_subject for {sub_id} because expected BIDS directory does not match: {bids_sub_dir}")
      return(TRUE)
    }
    
    extant_bids_ses <- !is.na(sub_cfg$bids_ses_dir)
    if (multi_session && !identical(sub_cfg$bids_ses_dir[extant_bids_ses], bids_ses_dir[extant_bids_ses])) {
      to_log(lg, "warn", "Exiting process_subject for {sub_id} because expected BIDS session directory does not match: {bids_ses_dir[1L]}")
      return(TRUE)
    }
    
  } else if (!checkmate::test_directory_exists(bids_sub_dir)) {
    to_log(lg, "warn", "Exiting process_subject for {sub_id} because expected BIDS directory does not exist: {bids_sub_dir}")
    return(TRUE)
  }
  
  # N.B. Everything after BIDS conversion depends on the BIDS directory existing
  
  ## Handle MRIQC
  mriqc_id <- submit_step("mriqc", parent_ids = c(parent_ids, bids_conversion_ids))
  if (isTRUE(preflight_state$failed)) return(TRUE)
  
  ## Handle fmriprep
  fmriprep_id <- submit_step("fmriprep", parent_ids = c(parent_ids, bids_conversion_ids))
  if (isTRUE(preflight_state$failed)) return(TRUE)
  
  ## Handle aroma
  aroma_id <- submit_step("aroma", parent_ids = c(parent_ids, bids_conversion_ids, fmriprep_id))
  if (isTRUE(preflight_state$failed)) return(TRUE)
  
  ## Handle postprocessing (session-level, multiple configs)
  postprocess_ids <- c()
  if (isTRUE(steps["postprocess"])) {
    missing_fmriprep <- FALSE
    ## If postprocessing is requested without running fmriprep, validate that the expected fmriprep outputs exist before scheduling jobs
    if (!isTRUE(steps["fmriprep"])) {
      if (is_external_path(scfg$metadata$fmriprep_directory,
                           scfg$metadata$project_directory)) {
        fp_dir <- file.path(scfg$metadata$fmriprep_directory,
                            glue("sub-{sub_id}"))
        if (!checkmate::test_directory_exists(fp_dir)) {
          to_log(lg, "warn", "Exiting process_subject for {sub_id} because fmriprep outputs are missing (expected {fp_dir})")
          missing_fmriprep <- TRUE
        }
      } else {
        chk <- is_step_complete(scfg, sub_id, step_name = "fmriprep")
        if (!chk$complete) {
          if (!checkmate::test_directory_exists(chk$dir)) {
            to_log(lg, "warn", "Missing fmriprep output directory: {chk$dir}")
          }
          if (!checkmate::test_file_exists(chk$complete_file)) {
            to_log(lg, "warn", "Missing {basename(chk$complete_file)} in {dirname(chk$complete_file)}")
          }
          to_log(lg, "warn", "Exiting process_subject for {sub_id} because fmriprep outputs are missing (expected {chk$dir} and {basename(chk$complete_file)})")
          missing_fmriprep <- TRUE
        }
      }
    }

    all_streams <- get_postprocess_stream_names(scfg)
    if (is.null(postprocess_streams)) {
      if (is.null(all_streams)) {
        stop("Cannot run postprocessing in submit_subject because no streams exist")
      } else {
        to_log(lg, "debug", "Running all postprocessing streams because postprocess_streams was NULL in process_subject")
        postprocess_streams <- all_streams # run all
      }
    }

    if (!isTRUE(steps["fmriprep"])) {
      input_dirs <- if (multi_session) {
        vapply(seq_len(n_inputs), function(idx) {
          ses_id <- sub_cfg$ses_id[idx]
          dir <- file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}"))
          if (!is.na(ses_id)) dir <- file.path(dir, glue("ses-{ses_id}"))
          dir
        }, character(1))
      } else {
        file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}"))
      }
      input_dirs <- unique(input_dirs[!is.na(input_dirs)])

      input_regexes <- vapply(postprocess_streams, function(pp_nm) {
        rx <- scfg$postprocess[[pp_nm]]$input_regex
        if (is.null(rx) || is.na(rx) || rx == "") {
          "desc:preproc suffix:bold"
        } else {
          rx
        }
      }, character(1), USE.NAMES = FALSE)

      has_inputs <- FALSE
      if (length(postprocess_streams) > 0L && length(input_dirs) > 0L) {
        for (dir in input_dirs) {
          if (!dir.exists(dir)) next
          for (rx in unique(input_regexes)) {
            pattern <- tryCatch(construct_bids_regex(rx), error = function(e) NULL)
            if (is.null(pattern)) {
              to_log(lg, "warn", "Invalid postprocess input_regex: {rx}")
              next
            }
            if (length(list.files(path = dir, pattern = pattern, recursive = TRUE, full.names = TRUE)) > 0L) {
              has_inputs <- TRUE
              break
            }
          }
          if (has_inputs) break
        }
      }

      if (!has_inputs) {
        input_dirs_txt <- if (length(input_dirs) > 0L) paste(input_dirs, collapse = ", ") else "<none>"
        rx_label <- paste(unique(input_regexes), collapse = "; ")
        to_log(lg, "warn", "no fmriprep NIfTI inputs matched expected patterns in {input_dirs_txt} (patterns: {rx_label})")
      }

      if (!has_inputs) return(TRUE)
      if (missing_fmriprep) {
        to_log(lg, "warn", "Proceeding with postprocessing for {sub_id} because inputs were found even though fmriprep .complete markers are missing.")
      }
    }
    # loop over inputs and processing streams
    postprocess_ids <- unlist(lapply(seq_len(n_inputs), function(idx) {
      unlist(lapply(postprocess_streams, function(pp_nm) {
        submit_step("postprocess", row_idx = idx, parent_ids = c(parent_ids, bids_conversion_ids, fmriprep_id, aroma_id), pp_stream = pp_nm)
      }))
    }))
    if (isTRUE(preflight_state$failed)) return(TRUE)
  }
  
  if (isTRUE(steps["extract_rois"])) {
    all_extract_streams <- get_extract_stream_names(scfg)
    if (is.null(extract_streams)) {
      if (is.null(all_extract_streams)) {
        stop("Cannot run ROI extraction in submit_subject because no extraction streams exist")
      } else {
        to_log(lg, "debug", "Running all ROI extraction streams because extract_streams was NULL in process_subject")
        extract_streams <- all_extract_streams # run all
      }
    }
    
    # loop over inputs and extraction streams
    extract_ids <- unlist(lapply(seq_len(n_inputs), function(idx) {
      unlist(lapply(extract_streams, function(ex_nm) {
        submit_step("extract_rois", row_idx = idx, parent_ids = c(parent_ids, bids_conversion_ids, fmriprep_id, aroma_id, postprocess_ids), ex_stream = ex_nm)
      }))
    }))
    
  }
  
  return(TRUE) # nothing interesting for now
}


submit_bids_conversion <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, tracking_sqlite_db = NULL, tracking_args = NULL) {
  # heudiconv  --files dicom/219/itbs/*/*.dcm -o Nifti -f Nifti/code/heuristic1.py -s 219 -ss itbs -c dcm2niix -b --minmeta --overwrite

  env_variables <- c(
    env_variables,
    heudiconv_container = scfg$compute_environment$heudiconv_container,
    loc_sub_dicoms = sub_dir,
    loc_bids_root = scfg$metadata$bids_directory,
    heudiconv_heuristic = scfg$bids_conversion$heuristic_file,
    sub_id = sub_id,
    ses_id = ses_id
  )

  forced_cli <- c("--bids")
  if (isTRUE(scfg$bids_conversion$overwrite)) forced_cli <- c(forced_cli, "--overwrite")
  cli_options <- set_cli_options(scfg$bids_conversion$cli_options, forced_cli, collapse = TRUE)

  env_variables <- c(env_variables,
    cli_options = cli_options,
    heudiconv_clear_cache = if (isTRUE(scfg$bids_conversion$clear_cache)) "1" else "0"
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE, 
    tracking_sqlite_db = tracking_sqlite_db, tracking_args = tracking_args
  )

  log_submission_command(lg, job_id, "bids_conversion job")

  return(job_id)

}

submit_bids_validation <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, outfile = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, tracking_sqlite_db = NULL, tracking_args = NULL) {
  
  env_variables <- c(
    env_variables,
    bids_validator = scfg$compute_environment$bids_validator,
    bids_dir = sub_dir,
    sub_id = sub_id,
    outfile = if (is.null(outfile)) scfg$bids_validation$outfile else outfile
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE,
    tracking_sqlite_db = tracking_sqlite_db, tracking_args = tracking_args
  )

  log_submission_command(lg, job_id, "bids_validation job")

  return(job_id)
}

submit_fmriprep <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, tracking_sqlite_db = NULL, tracking_args = NULL) {
  checkmate::assert_list(scfg)
  checkmate::assert_character(parent_ids, null.ok = TRUE)

  if (!validate_exists(scfg$compute_environment$fmriprep_container)) {
    #to_log(lg, "debug", "Unable to submit fmriprep for {sub_dir} because $compute_environment$fmriprep_container is missing.")
    warning("Unable to submit fmriprep for {sub_dir} because $compute_environment$fmriprep_container is missing.")
    return(NULL)
  }

  scfg <- ensure_aroma_output_space(scfg, require_aroma = isTRUE(scfg$aroma$enable), verbose = FALSE)

  # for the mem request, have fmriprep request a bit less than the job gets itself
  # https://neurostars.org/t/fmriprep-failing-on-hpc-via-singularity/26342/27
  cli_options <- set_cli_options(scfg$fmriprep$cli_options, c(
    glue("--nthreads {scfg$fmriprep$ncores}"),
    glue("--omp-nthreads {scfg$fmriprep$ncores}"),
    glue("--participant_label {sub_id}"),
    glue("-w {scfg$metadata$scratch_directory}"),
    glue("--fs-license-file {scfg$fmriprep$fs_license_file}"),
    glue("--output-spaces {trimws(scfg$fmriprep$output_spaces)}"),
    glue("--mem {format(max(4, scfg$fmriprep$memgb - 4)*1000, scientific=FALSE)}") # convert to MB
  ), collapse = TRUE)

  if (!checkmate::test_directory_exists(scfg$metadata$templateflow_home)) {
    to_log(lg, "debug", "Creating missing templateflow_home directory: {scfg$metadata$templateflow_home}")
    dir.create(scfg$metadata$templateflow_home, showWarnings = FALSE, recursive = TRUE)
  }

  env_variables <- c(
    env_variables,
    fmriprep_container = scfg$compute_environment$fmriprep_container,
    sub_id = sub_id,
    ses_id = ses_id,
    loc_bids_root = scfg$metadata$bids_directory,
    loc_mrproc_root = scfg$metadata$fmriprep_directory,
    loc_scratch = scfg$metadata$scratch_directory,
    templateflow_home = normalizePath(scfg$metadata$templateflow_home),
    fs_license_file = scfg$fmriprep$fs_license_file,
    cli_options = cli_options
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE,
    tracking_sqlite_db = tracking_sqlite_db, tracking_args = tracking_args
  )

  log_submission_command(lg, job_id, "fmriprep job")

  return(job_id)
}


submit_mriqc <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, tracking_sqlite_db = NULL, tracking_args = NULL) {
   if (!validate_exists(scfg$compute_environment$mriqc_container)) {
    message(glue("Skipping MRIQC in {sub_dir} because could not find MRIQC container {scfg$compute_environment$mriqc_container}"))
    return(NULL)
  }

  cli_options <- set_cli_options(scfg$mriqc$cli_options, c(
    glue("--nprocs {scfg$mriqc$ncores}"),
    glue("--omp-nthreads {scfg$mriqc$ncores}"),
    glue("--participant_label {sub_id}"),
    glue("-w {scfg$metadata$scratch_directory}"),
    glue("--mem-gb {scfg$mriqc$memgb}")
  ), collapse=TRUE)

  if (!checkmate::test_directory_exists(scfg$metadata$templateflow_home)) {
    to_log(lg, "debug", "Creating missing templateflow_home directory for MRIQC: {scfg$metadata$templateflow_home}")
    dir.create(scfg$metadata$templateflow_home, showWarnings = FALSE, recursive = TRUE)
  }

  env_variables <- c(
    env_variables,
    mriqc_container = scfg$compute_environment$mriqc_container,
    sub_id = sub_id,
    ses_id = ses_id,
    loc_bids_root = scfg$metadata$bids_directory,
    loc_mriqc_root = scfg$metadata$mriqc_directory,
    loc_scratch = scfg$metadata$scratch_directory,
    templateflow_home = normalizePath(scfg$metadata$templateflow_home),
    cli_options = cli_options
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE,
    tracking_sqlite_db = tracking_sqlite_db, tracking_args = tracking_args
  )

  log_submission_command(lg, job_id, "mriqc job")

  return(job_id)
}

submit_aroma <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, tracking_sqlite_db = NULL, tracking_args = NULL) {
   if (!validate_exists(scfg$compute_environment$aroma_container)) {
    message(glue("Skipping AROMA in {sub_dir} because could not find AROMA container {scfg$compute_environment$aroma_container}"))
    return(NULL)
  }

  if (!isTRUE(scfg$aroma$enable)) {
    message(glue("Skipping AROMA in {sub_dir} because AROMA is disabled"))
    return(NULL)
  }

  # for now, inherit key options from fmriprep rather than asking user to respecify
  # https://fmripost-aroma.readthedocs.io/latest/usage.html

  cli_options <- set_cli_options(scfg$aroma$cli_options, c(
    glue("--nthreads {scfg$aroma$ncores}"),
    glue("--omp-nthreads {scfg$aroma$ncores}"),
    glue("--participant_label {sub_id}"),
    glue("-w {scfg$metadata$scratch_directory}"),
    glue("--mem {scfg$aroma$memgb*1000}"), # convert to MB
    glue("--derivatives fmriprep={scfg$metadata$fmriprep_directory}")
  ), collapse = TRUE)

  cleanup <- isTRUE(scfg$aroma$cleanup)
  auto_added_aroma <- isTRUE(scfg$fmriprep$auto_added_aroma_space)
  if (cleanup && !auto_added_aroma && !is.null(scfg$fmriprep$output_spaces) &&
      grepl("MNI152NLin6Asym:res-2", scfg$fmriprep$output_spaces, fixed = TRUE)) {
    msg <- "AROMA cleanup requested but will not occur because MNI152NLin6Asym:res-2 is in fmriprep --output-spaces."
    if (!is.null(lg)) to_log(lg, "warn", msg) else warning(msg)
    cleanup <- FALSE
  }

  env_variables <- c(
    env_variables,
    aroma_container = scfg$compute_environment$aroma_container,
    sub_id = sub_id,
    ses_id = ses_id,
    loc_bids_root = scfg$metadata$bids_directory,
    loc_mrproc_root = scfg$metadata$fmriprep_directory,
    loc_scratch = scfg$metadata$scratch_directory,
    cli_options = cli_options,
    aroma_cleanup = as.integer(cleanup)
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE,
    tracking_sqlite_db = tracking_sqlite_db, tracking_args = tracking_args
  )

  log_submission_command(lg, job_id, "aroma job")

  return(job_id)
}

submit_postprocess <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, 
sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, pp_stream = NULL, tracking_sqlite_db = NULL, tracking_args = NULL) {
  if (is.null(pp_stream)) stop("Cannot submit a postprocessing job without specifying a pp_stream")

  postprocess_rscript <- system.file("postprocess_cli.R", package = "BrainGnomes")
  postprocess_image_sched_script <- get_job_script(scfg, "postprocess_image")

  # postprocessing
  input_dir <- file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}")) # populate the location of this sub/ses dir into the config to pass on as CLI
  if (!is.null(ses_id) && !is.na(ses_id)) input_dir <- file.path(input_dir, glue("ses-{ses_id}")) # add session subdir if relevant
  out_dir <- file.path(scfg$metadata$postproc_directory, glue("sub-{sub_id}"))
  if (!is.null(ses_id) && !is.na(ses_id)) out_dir <- file.path(out_dir, glue("ses-{ses_id}"))

  # pull the requested postprocessing stream from the broader list
  pp_cfg <- scfg$postprocess[[pp_stream]]
  if (isTRUE(scfg$force)) pp_cfg$overwrite <- TRUE # overwrite existing postprocessed files
  pp_cfg$fsl_img <- scfg$compute_environment$fsl_container
  pp_cfg$input_regex <- construct_bids_regex(pp_cfg$input_regex)
  pp_cfg$output_dir <- out_dir
  pp_cfg$scratch_directory <- scfg$metadata$scratch_directory
  pp_cfg$project_name <- scfg$metadata$project_name
  # drop postproc scheduling arguments from fields before converting to cli argument string for postprocess_cli.R
  pp_cfg$nhours <- pp_cfg$ncores <- pp_cfg$cli_options <- pp_cfg$sched_args <- pp_cfg$sched_args <- NULL
  postprocess_cli <- nested_list_to_args(pp_cfg, collapse = TRUE) # create command line for calling postprocessing R script

  env_variables <- c(
    env_variables,
    sub_id = sub_id,
    ses_id = ses_id,
    postprocess_cli = postprocess_cli,
    postprocess_rscript = postprocess_rscript,
    input_dir = input_dir, # postprocess_subject.sbatch will figure out files to postprocess using input and input_regex
    input_regex = pp_cfg$input_regex,
    postprocess_image_sched_script = postprocess_image_sched_script,
    sched_args = sched_args, # pass through to child processes
    stream_name = pp_stream,
    out_dir = out_dir
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE,
    tracking_sqlite_db = tracking_sqlite_db, tracking_args = tracking_args
  )

  log_submission_command(lg, job_id, glue("postprocess stream {pp_stream} job"))

  return(job_id)
}


submit_extract_rois <- function(
    scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL,
    sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, ex_stream = NULL,
    tracking_sqlite_db = NULL, tracking_args = NULL) {
  
  if (is.null(ex_stream)) stop("Cannot submit an ROI extraction job without specifying an ex_stream")

  extract_rscript <- system.file("extract_cli.R", package = "BrainGnomes")
  extract_sched_script <- get_job_script(scfg, "extract_rois")

  input_dir <- file.path(scfg$metadata$postproc_directory, glue("sub-{sub_id}")) # populate the location of this sub/ses dir into the config to pass on as CLI
  if (!is.null(ses_id) && !is.na(ses_id)) input_dir <- file.path(input_dir, glue("ses-{ses_id}")) # add session subdir if relevant

  # pull the requested extraction stream from the broader list
  ex_cfg <- scfg$extract_rois[[ex_stream]]
  if (!is.null(ex_cfg$input_regex)) {
    msg <- "extract_rois/input_regex is ignored by run_project; inputs are derived from postprocess streams."
    if (!is.null(lg)) {
      to_log(lg, "warn", msg)
    } else {
      warning(msg, call. = FALSE)
    }
  }
  if (isTRUE(scfg$force)) ex_cfg$overwrite <- TRUE # enable overwrite of ROIs if force=TRUE

  # Every extract_rois stream can pull for 1+ postprocess streams. Pass through the input spec
  # and bids_desc for each stream so extract_cli.R can target postprocessed outputs directly.
  ex_cfg$input_regex <- sapply(ex_cfg$input_streams, function(ss) scfg$postprocess[[ss]]$input_regex, USE.NAMES = FALSE)

  # the bids_desc of the postprocess stream is used to update the matched files (to get the outputs of postprocessing)
  ex_cfg$bids_desc <- sapply(ex_cfg$input_streams, function(ss) scfg$postprocess[[ss]]$bids_desc, USE.NAMES = FALSE)

  # drop extraction scheduling arguments from fields before converting to cli argument string for extract_cli.R
  ex_cfg$nhours <- ex_cfg$ncores <- ex_cfg$cli_options <- ex_cfg$sched_args <- ex_cfg$memgb <- NULL
  ex_cfg$input <- input_dir
  ex_cfg$out_dir <- scfg$metadata$rois_directory
  extract_cli <- nested_list_to_args(ex_cfg, collapse = TRUE) # create command line for calling extraction R script

  env_variables <- c(
    env_variables,
    loc_postproc_root = scfg$metadata$postproc_directory,
    sub_id = sub_id,
    ses_id = ses_id,
    extract_cli = extract_cli,
    extract_rscript = extract_rscript,
    extract_sched_script = extract_sched_script,
    sched_args = sched_args, # pass through to child processes
    stream_name = ex_stream
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE,
    tracking_sqlite_db = tracking_sqlite_db, tracking_args = tracking_args
  )

  log_submission_command(lg, job_id, glue("ROI extraction stream {ex_stream} job"))

  return(job_id)
}
