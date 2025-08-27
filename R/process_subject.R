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

#' Preprocess a single subject
#' @param scfg A list of configuration settings
#' @param sub_cfg A data.frame of subject configuration settings
#' @param steps A named logical vector indicating which steps to run
#' @param postprocess_streams Optional character vector of postprocess configuration names to run. If NULL,
#'   all available postprocessing streams will be run.
#' @param postprocess_streams Optional character vector of ROI extraction configuration names to run. If NULL,
#'   all available extraction streams will be run.
#' @param parent_ids An optional character vector of HPC job ids that must complete before this subject is run.
#' @return A logical value indicating whether the preprocessing was successful
#' @importFrom glue glue
#' @importFrom checkmate assert_class assert_list assert_names assert_logical
#' @keywords internal
process_subject <- function(scfg, sub_cfg = NULL, steps = NULL, postprocess_streams = NULL, extract_streams = NULL, parent_ids = NULL) {
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
  expected <- c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess", "extract_rois")
  for (ee in expected) if (is.na(steps[ee])) steps[ee] <- FALSE # ensure we have valid logicals for expected fields

  sub_id <- sub_cfg$sub_id[1L]
  bids_sub_dir <- sub_cfg$bids_sub_dir[1L]
  lg <- get_subject_logger(scfg, sub_id)
  
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

    job_id <- NULL
    # skip out if this step is not requested or it is already complete
    if (!isTRUE(steps[[name]])) {
      lg$debug("Skipping {name} for {sub_id} because step is not requested.")
      return(job_id)
    } else if (file_exists && !isTRUE(scfg$force)) {
      lg$info("Skipping {name_tag} for {sub_id} because {complete_file} already exists and force = FALSE.")
      return(job_id)
    }

    # clear existing complete file if we are starting over on this step
    if (file_exists) {
      lg$info("Removing existing .complete file: {complete_file}")
      unlink(complete_file)
    }

    # shared components across specific jobs
    jobid_str <- ifelse(has_ses, glue("{name_tag}_sub-{sub_id}_ses-{ses_id}"), glue("{name_tag}_sub-{sub_id}"))
    env_variables <- c(
      debug_pipeline = scfg$debug,
      pkg_dir = find.package(package = "BrainGnomes"), # location of installed R package
      R_HOME = R.home(), # populate location of R installation so that it can be used by any child R jobs
      log_file = lg$appenders$subject_logger$destination, # write to same file as subject lgr
      stdout_log = glue("{scfg$metadata$log_directory}/sub-{sub_id}/{jobid_str}_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.out"),
      stderr_log = glue("{scfg$metadata$log_directory}/sub-{sub_id}/{jobid_str}_jobid-%j_{format(Sys.time(), '%d%b%Y_%H.%M.%S')}.err"),
      complete_file = complete_file
    )

    sched_script <- get_job_script(scfg, name) # lookup location of HPC script to run
    if (name == "postprocess") {
      scfg_tmp <- scfg # postprocessing has a nested structure, with multiple configurations -- use the one currently requested
      scfg_tmp$postprocess <- scfg$postprocess[[pp_stream]]
      sched_args <- get_job_sched_args(scfg_tmp, name)
    } else if (name == "extract_rois") {
      scfg_tmp <- scfg # postprocessing has a nested structure, with multiple configurations -- use the one currently requested
      scfg_tmp$extract_rois <- scfg$extract_rois[[ex_stream]]
      sched_args <- get_job_sched_args(scfg_tmp, name)
    } else {
      sched_args <- get_job_sched_args(scfg, name)
    }

    sched_args <- set_cli_options( # setup files for stdout and stderr, job name
      sched_args,
      c(
        glue("--job-name={jobid_str}"),
        glue("--output={env_variables['stdout_log']}"),
        glue("--error={env_variables['stderr_log']}")
      )
    )

    # determine the directory to use for the job submission
    if (session_level && has_ses) {
      # if it's a session-level process and we have a valid session-level input, use the session directory
      dir <- ifelse(name == "bids_conversion", sub_cfg$dicom_ses_dir[row_idx], sub_cfg$bids_ses_dir[row_idx])
    } else {
      # if it's a subject-level process or we don't have a valid session-level input, use the subject directory
      dir <- ifelse(name == "bids_conversion", sub_cfg$dicom_sub_dir[row_idx], sub_cfg$bids_sub_dir[row_idx])
    }

    # launch submission function -- these all follow the same input argument structure
    lg$debug("Launching submit_{name_tag} for subject: {sub_id}")
    args <- list(scfg, dir, sub_id, ses_id, env_variables, sched_script, sched_args, parent_ids, lg)
    if (name == "postprocess") args$pp_stream <- pp_stream # populate the current postprocess config to run
    if (name == "extract_rois") args$ex_stream <- ex_stream # populate the current extract_rois config to run
    job_id <- do.call(glue("submit_{name}"), args)

    return(job_id)
  }

  lg$info("Processing subject {sub_id} with {nrow(sub_cfg)} sessions.")
  # lg$info("Processing steps: {glue_collapse(names(steps), sep = ', ')}")
  
  ## Handle BIDS conversion -- session-level
  n_inputs <- nrow(sub_cfg)

  # need unlist because NULL will be returned for jobs not submitted -- yielding a weird list of NULLs
  bids_conversion_ids <- unlist(lapply(seq_len(n_inputs), function(idx) submit_step("bids_conversion", row_idx = idx, parent_ids = parent_ids)))
  
  if (isTRUE(steps["bids_conversion"])) {
    if (!is.na(bids_sub_dir)) lg$info("BIDS directory: {bids_sub_dir}")

    # Use expected directory as input to subsequent steps, anticipating that conversion completes
    # and the expected directory is created. If conversion fails, the dependent jobs should automatically fail.
    bids_sub_dir <- file.path(scfg$metadata$bids_directory, glue("sub-{sub_cfg$sub_id[1L]}"))
    bids_ses_dir <- if (multi_session) file.path(scfg$metadata$bids_directory, glue("sub-{sub_cfg$sub_id}"), glue("ses-{sub_cfg$ses_id}")) else rep(NA_character_, nrow(sub_cfg))

    # When bids_sub_dir and bids_ses_dir exist, do they match these expectations?
    extant_bids <- !is.na(sub_cfg$bids_sub_dir)
    if (!identical(sub_cfg$bids_sub_dir[extant_bids], bids_sub_dir[extant_bids])) {
      lg$warn("Exiting process_subject for {sub_id} because expected BIDS directory does not match: {bids_sub_dir}")
      return(TRUE)
    }

    extant_bids_ses <- !is.na(sub_cfg$bids_ses_dir)
    if (multi_session && !identical(sub_cfg$bids_ses_dir[extant_bids_ses], bids_ses_dir[extant_bids_ses])) {
      lg$warn("Exiting process_subject for {sub_id} because expected BIDS session directory does not match: {bids_ses_dir[1L]}")
      return(TRUE)
    }
    
  } else if (!checkmate::test_directory_exists(bids_sub_dir)) {
    lg$warn("Exiting process_subject for {sub_id} because expected BIDS directory does not exist: {bids_sub_dir}")
    return(TRUE)
  }

  # N.B. Everything after BIDS conversion depends on the BIDS directory existing

  ## Handle MRIQC
  mriqc_id <- submit_step("mriqc", parent_ids = c(parent_ids, bids_conversion_ids))

  ## Handle fmriprep
  fmriprep_id <- submit_step("fmriprep", parent_ids = c(parent_ids, bids_conversion_ids))

  ## Handle aroma
  aroma_id <- submit_step("aroma", parent_ids = c(parent_ids, bids_conversion_ids, fmriprep_id))

  ## Handle postprocessing (session-level, multiple configs)
  postprocess_ids <- c()
  if (isTRUE(steps["postprocess"])) {
    ## If postprocessing is requested without running fmriprep, validate that the expected fmriprep outputs exist before scheduling jobs
    if (!isTRUE(steps["fmriprep"])) {
      chk <- is_step_complete(scfg, sub_id, step_name = "fmriprep")
      if (!chk$complete) {
        lg$warn("Exiting process_subject for {sub_id} because fmriprep outputs are missing (expected {chk$dir} and {basename(chk$complete_file)})")
        return(TRUE)
      }
    }

    all_streams <- get_postprocess_stream_names(scfg)
    if (is.null(postprocess_streams)) {
      if (is.null(all_streams)) {
        stop("Cannot run postprocessing in submit_subject because no streams exist")
      } else {
        lg$debug("Running all postprocessing streams because postprocess_streams was NULL in process_subject")
        postprocess_streams <- all_streams # run all
      }
    }

    # loop over inputs and processing streams
    postprocess_ids <- unlist(lapply(seq_len(n_inputs), function(idx) {
      unlist(lapply(postprocess_streams, function(pp_nm) {
        submit_step("postprocess", row_idx = idx, parent_ids = c(parent_ids, bids_conversion_ids, fmriprep_id, aroma_id), pp_stream = pp_nm)
      }))
    }))
  }

  if (isTRUE(steps["extract_rois"])) {
    all_extract_streams <- get_extract_stream_names(scfg)
    if (is.null(extract_streams)) {
      if (is.null(all_extract_streams)) {
        stop("Cannot run ROI extraction in submit_subject because no extraction streams exist")
      } else {
        lg$debug("Running all ROI extraction streams because extract_streams was NULL in process_subject")
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

submit_bids_conversion <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL) {
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

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE
  )

  # log submission command
  lg$info("Scheduled bids_conversion job: {truncate_str(attr(job_id, 'cmd'))}")
  lg$debug("Full command: {attr(job_id, 'cmd')}")

  return(job_id)

}

submit_bids_validation <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, outfile = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL) {
  
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
    wait_jobs = parent_ids, echo = FALSE
  )

  lg$info("Scheduled bids_validation job: {truncate_str(attr(job_id, 'cmd'))}")
  lg$debug("Full command: {attr(job_id, 'cmd')}")

  return(job_id)
}

submit_fmriprep <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL) {
  checkmate::assert_list(scfg)
  checkmate::assert_character(parent_ids, null.ok = TRUE)

  if (!validate_exists(scfg$compute_environment$fmriprep_container)) {
    #lg$debug("Unable to submit fmriprep for {sub_dir} because $compute_environment$fmriprep_container is missing.")
    warning("Unable to submit fmriprep for {sub_dir} because $compute_environment$fmriprep_container is missing.")
    return(NULL)
  }

  if (isTRUE(scfg$aroma$enable) && (is.null(scfg$fmriprep$output_spaces) || !grepl("MNI152NLin6Asym:res-2", scfg$fmriprep$output_spaces, fixed = TRUE))) {
    message("Adding MNI152NLin6Asym:res-2 to output spaces for fmriprep to allow AROMA to run.")
    scfg$fmriprep$output_spaces <- paste(scfg$fmriprep$output_spaces, "MNI152NLin6Asym:res-2")
  }

  # for the mem request, have fmriprep request a bit less than the job gets itself
  # https://neurostars.org/t/fmriprep-failing-on-hpc-via-singularity/26342/27
  cli_options <- set_cli_options(scfg$fmriprep$cli_options, c(
    glue("--nthreads {scfg$fmriprep$ncores}"),
    glue("--omp-nthreads {scfg$fmriprep$ncores}"),
    glue("--participant_label {sub_id}"),
    glue("-w {scfg$metadata$scratch_directory}"),
    glue("--fs-license-file {scfg$fmriprep$fs_license_file}"),
    glue("--output-spaces {scfg$fmriprep$output_spaces}"),
    glue("--mem {format(max(4, scfg$fmriprep$memgb - 4)*1000, scientific=FALSE)}") # convert to MB
  ), collapse = TRUE)

  if (!checkmate::test_directory_exists(scfg$metadata$templateflow_home)) {
    lg$debug("Creating missing templateflow_home directory: {scfg$metadata$templateflow_home}")
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
    wait_jobs = parent_ids, echo = FALSE
  )

  lg$info("Scheduled fmriprep job: {truncate_str(attr(job_id, 'cmd'))}")
  lg$debug("Full command: {attr(job_id, 'cmd')}")

  return(job_id)
}


submit_mriqc <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL) {
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

  env_variables <- c(
    env_variables,
    mriqc_container = scfg$compute_environment$mriqc_container,
    sub_id = sub_id,
    ses_id = ses_id,
    loc_bids_root = scfg$metadata$bids_directory,
    loc_mriqc_root = scfg$metadata$mriqc_directory,
    loc_scratch = scfg$metadata$scratch_directory,
    cli_options = cli_options
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE
  )

  lg$info("Scheduled mriqc job: {truncate_str(attr(job_id, 'cmd'))}")
  lg$debug("Full command: {attr(job_id, 'cmd')}")

  return(job_id)
}

submit_aroma <- function(scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL, sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL) {
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

  env_variables <- c(
    env_variables,
    aroma_container = scfg$compute_environment$aroma_container,
    sub_id = sub_id,
    ses_id = ses_id,
    loc_bids_root = scfg$metadata$bids_directory,
    loc_mrproc_root = scfg$metadata$fmriprep_directory,
    loc_scratch = scfg$metadata$scratch_directory,
    cli_options = cli_options
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE
  )

  lg$info("Scheduled aroma job: {truncate_str(attr(job_id, 'cmd'))}")
  lg$debug("Full command: {attr(job_id, 'cmd')}")

  return(job_id)
}

submit_postprocess <- function(
    scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL,
    sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, pp_stream = NULL) {

  if (is.null(pp_stream)) stop("Cannot submit a postprocessing job without specifying a pp_stream")

  postprocess_rscript <- system.file("postprocess_cli.R", package = "BrainGnomes")
  postprocess_image_sched_script <- get_job_script(scfg, "postprocess_image")

  # postprocessing
  input_dir <- file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}")) # populate the location of this sub/ses dir into the config to pass on as CLI
  if (!is.null(ses_id) && !is.na(ses_id)) input_dir <- file.path(input_dir, glue("ses-{ses_id}")) # add session subdir if relevant

  # pull the requested postprocessing stream from the broader list
  pp_cfg <- scfg$postprocess[[pp_stream]]
  pp_cfg$fsl_img <- scfg$compute_environment$aroma_container # always pass aroma container for running FSL commands in postprocessing
  pp_cfg$input_regex <- construct_bids_regex(pp_cfg$input_regex)
  # drop postproc scheduling arguments from fields before converting to cli argument string for postprocess_cli.R
  pp_cfg$nhours <- pp_cfg$ncores <- pp_cfg$cli_options <- pp_cfg$sched_args <- pp_cfg$sched_args <- NULL
  postprocess_cli <- nested_list_to_args(pp_cfg, collapse = TRUE) # create command line for calling postprocessing R script

  env_variables <- c(
    env_variables,
    loc_mrproc_root = scfg$metadata$fmriprep_directory,
    sub_id = sub_id,
    ses_id = ses_id,
    postprocess_cli = postprocess_cli,
    postprocess_rscript = postprocess_rscript,
    input_dir = input_dir, # postprocess_subject.sbatch will figure out files to postprocess using input and input_regex
    input_regex = pp_cfg$input_regex,
    postprocess_image_sched_script = postprocess_image_sched_script,
    sched_args = sched_args, # pass through to child processes
    stream_name = pp_stream
  )

  job_id <- cluster_job_submit(sched_script,
    scheduler = scfg$compute_environment$scheduler,
    sched_args = sched_args, env_variables = env_variables,
    wait_jobs = parent_ids, echo = FALSE
  )

  lg$info("Scheduled postprocess stream {pp_stream} job: {truncate_str(attr(job_id, 'cmd'))}")
  lg$debug("Full command: {attr(job_id, 'cmd')}")

  return(job_id)
}


submit_extract_rois <- function(
    scfg, sub_dir = NULL, sub_id = NULL, ses_id = NULL, env_variables = NULL,
    sched_script = NULL, sched_args = NULL, parent_ids = NULL, lg = NULL, ex_stream = NULL) {
  
  if (is.null(ex_stream)) stop("Cannot submit an ROI extraction job without specifying an ex_stream")

  extract_rscript <- system.file("extract_cli.R", package = "BrainGnomes")
  extract_sched_script <- get_job_script(scfg, "extract_rois")

  input_dir <- file.path(scfg$metadata$fmriprep_directory, glue("sub-{sub_id}")) # populate the location of this sub/ses dir into the config to pass on as CLI
  if (!is.null(ses_id) && !is.na(ses_id)) input_dir <- file.path(input_dir, glue("ses-{ses_id}")) # add session subdir if relevant

  # pull the requested extraction stream from the broader list
  ex_cfg <- scfg$extract_rois[[ex_stream]]

  # Every extract_rois stream can pull for 1+ postprocess streams. Based on postprocess input stream(s), generate regular expressions
  # need to find outputs of postproc stream. A little tricky given that desc may not be in input_regex. This is handled inside extract_cli.R,
  # which runs once the job fires (and any expected files are now available from earlier processing stages)
  ex_cfg$input_regex <- sapply(ex_cfg$input_streams, function(ss) scfg$postprocess[[ss]]$input_regex, USE.NAMES = FALSE)

  # the bids_desc of the postprocess stream is used to update the matched files (to get the outputs of postprocessing)
  ex_cfg$bids_desc <- sapply(ex_cfg$input_streams, function(ss) scfg$postprocess[[ss]]$bids_desc, USE.NAMES = FALSE)

  # drop extraction scheduling arguments from fields before converting to cli argument string for extract_cli.R
  ex_cfg$nhours <- ex_cfg$ncores <- ex_cfg$cli_options <- ex_cfg$sched_args <- ex_cfg$memgb <- NULL
  ex_cfg$input <- input_dir
  ex_cfg$out_dir <- scfg$metadata$roi_directory
  extract_cli <- nested_list_to_args(ex_cfg, collapse = TRUE) # create command line for calling extraction R script

  env_variables <- c(
    env_variables,
    loc_mrproc_root = scfg$metadata$fmriprep_directory,
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
    wait_jobs = parent_ids, echo = FALSE
  )

  lg$info("Scheduled ROI extraction stream {ex_stream} job: {truncate_str(attr(job_id, 'cmd'))}")
  lg$debug("Full command: {attr(job_id, 'cmd')}")

  return(job_id)
}
