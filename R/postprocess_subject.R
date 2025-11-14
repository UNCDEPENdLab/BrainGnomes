#' Postprocess a single fMRI BOLD image using a configured pipeline
#'
#' Applies a sequence of postprocessing operations to a single subject-level BOLD NIfTI file, as specified by
#' the user-defined configuration object. Operations may include brain masking, spatial smoothing, ICA-AROMA denoising,
#' temporal filtering, confound regression, and intensity normalization. The function also optionally computes and saves
#' a filtered confounds file for downstream analyses.
#'
#' The processing sequence can be enforced by the user (`force_processing_order = TRUE`) or determined dynamically based
#' on the `enable` flags in the configuration. Intermediate NIfTI and confound files may be deleted to save disk space,
#' depending on the `keep_intermediates` setting. Logging is handled via the `lgr` package and is directed to subject-specific
#' log files inferred from BIDS metadata.
#'
#' @param in_file Path to a subject-level BOLD NIfTI file output by fMRIPrep.
#' @param cfg A list containing configuration options, including TR (`cfg$tr`), enabled processing steps (`cfg$<step>$enable`),
#'   logging (`cfg$log_file`), and paths to resources such as singularity images (`cfg$fsl_img`). A whole-brain mask is
#'   automatically generated using `automask()` and used for relevant processing steps.
#'
#' @return The path to the final postprocessed BOLD NIfTI file. Side effects include writing a confounds TSV file (if enabled),
#'   and logging to a subject-level log file.
#'
#' @details
#' Required `cfg` entries:
#' - `tr`: Repetition time in seconds.
#' - `bids_desc`: A BIDS-compliant `desc` label for the output filename.
#' - `processing_steps`: Optional character vector specifying processing order (if `force_processing_order = TRUE`).
#'
#' Optional steps controlled by `cfg$<step>$enable`:
#' - `apply_mask`
#' - `spatial_smooth`
#' - `apply_aroma`
#' - `temporal_filter`
#' - `confound_regression`
#' - `intensity_normalize`
#'
#' @importFrom checkmate assert_list assert_file_exists test_character test_number
#' @export
postprocess_subject <- function(in_file, cfg=NULL) {
  checkmate::assert_file_exists(in_file)
  checkmate::assert_list(cfg)
  if (!checkmate::test_character(cfg$bids_desc)) {
    stop("postprocess_subject requires a bids_desc field containing the intended description field of the postprocessed filename.")
  }
  
  # checkmate::assert_list(processing_sequence)
  proc_files <- get_fmriprep_outputs(in_file)

  # determine if input is in a stereotaxic space
  input_bids_info <- as.list(extract_bids_info(in_file))
  native_space <- is.na(input_bids_info$space) || input_bids_info$space %in% c("T1w", "T2w", "anat")

  # log_file should come through as an environment variable, pointing to the subject-level log.
  # Use this to get the location of the subject log directory
  sub_log_file <- Sys.getenv("log_file")
  if (!nzchar(sub_log_file)) {
    warning("Cannot find log_file as an environment variable. Logs may not appear in the expected location!")
    attempt_dir <- normalizePath(file.path(dirname(in_file), glue("../../../logs/sub-{input_bids_info$sub}")))
    log_dir <- if (dir.exists(attempt_dir)) attempt_dir else dirname(in_file)
  } else {
    log_dir <- dirname(sub_log_file)
  }
  
  # Setup default postprocess log file -- need to make sure it always goes in the subject log folder
  if (is.null(cfg$log_file)) {
    cfg$log_file <- construct_bids_filename(modifyList(input_bids_info, list(ext=".log", description=cfg$bids_desc)), full.names=FALSE)
  } else {
    cfg$log_file <- glue(cfg$log_file) # evaluate location of log, allowing for glue expressions
  }

  # force log file to be in the right directory
  log_file <- file.path(log_dir, basename(cfg$log_file))

  lg <- lgr::get_logger_glue(c("postprocess", input_bids_info$sub))
  lg$add_appender(lgr::AppenderFile$new(log_file), name = "postprocess_log")

  # determine output directory for postprocessed files
  if (is.null(cfg$output_dir)) cfg$output_dir <- input_bids_info$directory
  cfg$output_dir <- normalizePath(cfg$output_dir, mustWork = FALSE)
  if (!dir.exists(cfg$output_dir)) dir.create(cfg$output_dir, recursive = TRUE)

  # Reconstruct expected output file
  output_bids_info <- modifyList(input_bids_info, list(description = cfg$bids_desc, directory = cfg$output_dir))
  final_filename <- construct_bids_filename(output_bids_info, full.names = TRUE)

  # determine if final output file already exists
  if (checkmate::test_file_exists(final_filename)) {
    to_log(lg, "info", "Postprocessed file already exists: {final_filename}")

    if (isTRUE(cfg$overwrite)) {
      to_log(lg, "info", "Removing {final_filename} because overwrite is TRUE")
      file.remove(final_filename)
    } else {
      to_log(lg, "info", "Skipping postprocessing for {in_file} because postprocessed file already exists")
      return(final_filename)
    }
  }

  # The initial fMRIPrep output may reside outside of cfg$output_dir.
  # We'll operate on the original file and ensure later steps move
  # outputs into the requested directory.

  # location of FSL singularity container
  fsl_img <- cfg$fsl_img

  if (!checkmate::test_number(cfg$tr, lower = 0.01, upper = 30)) {
    stop("YAML config must contain a tr field specifying the repetition time in seconds")
  }

  # default to not enforcing user-specified order of processing steps
  if (!checkmate::test_flag(cfg$force_processing_order)) cfg$force_processing_order <- FALSE

  start_time <- Sys.time()
  to_log(lg, "info", "Start preprocessing: {as.character(start_time)}")
  
  # compute a data-driven whole-brain mask using automask
  brain_mask <- tempfile(fileext = ".nii.gz")
  automask(in_file, outfile = brain_mask, clfrac = 0.5, NN = 1L,
           SIhh = 0, peels = 1L, fill_holes = TRUE, dilate_steps = 1L)

  # if apply_mask is enabled, determine which mask file to apply
  apply_mask_file <- NULL
  if (isTRUE(cfg$apply_mask$enable)) {
    apply_mask_file <- cfg$apply_mask$mask_file
    if (checkmate::test_string(apply_mask_file) && !is.na(apply_mask_file)) {
      if (apply_mask_file == "template") {
        apply_mask_file <- resample_template_to_img(in_file)
      } else if (!checkmate::test_file_exists(apply_mask_file)) {
        to_log(lg, "warn", "Cannot find apply_mask mask_file: {apply_mask_file}. This step will be skipped!")
        apply_mask_file <- NULL
      }
    } else {
      apply_mask_file <- NULL # not a string or is NA?
    }
  }

  cur_file <- proc_files$bold
  file_set <- cur_file # tracks all of the files used in the postprocessing stream

  ## setup order of processing steps
  if (isTRUE(cfg$force_processing_order)) {

    checkmate::assert_character(cfg$processing_steps) # ensure we have a character vector
    cfg$processing_steps <- tolower(cfg$processing_steps) # avoid case issues

    # handle small glitches in nomenclature
    cfg$processing_steps <- sub("spatial_smoothing", "spatial_smooth", cfg$processing_steps, fixed=TRUE)
    cfg$processing_steps <- sub("temporal_filtering", "temporal_filter", cfg$processing_steps, fixed=TRUE)
    cfg$processing_steps <- sub("confound_regress", "confound_regression", cfg$processing_steps, fixed = TRUE)
    cfg$processing_steps <- sub("intensity_normalization", "intensity_normalize", cfg$processing_steps, fixed = TRUE)

    processing_sequence <- cfg$processing_steps
    to_log(lg, "info", "We will follow the user-specified processing order, with no guarantees on data validity.")
  } else {
    processing_sequence <- c()
    if (isTRUE(cfg$apply_mask$enable)) processing_sequence <- c(processing_sequence, "apply_mask")
    if (isTRUE(cfg$spatial_smooth$enable)) processing_sequence <- c(processing_sequence, "spatial_smooth")
    if (isTRUE(cfg$apply_aroma$enable)) processing_sequence <- c(processing_sequence, "apply_aroma")
    if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$interpolate)) processing_sequence <- c(processing_sequence, "scrub_interpolate")
    if (isTRUE(cfg$temporal_filter$enable)) processing_sequence <- c(processing_sequence, "temporal_filter")
    if (isTRUE(cfg$confound_regression$enable)) processing_sequence <- c(processing_sequence, "confound_regression")
    if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$apply)) processing_sequence <- c(processing_sequence, "scrub_timepoints")
    if (isTRUE(cfg$intensity_normalize$enable)) processing_sequence <- c(processing_sequence, "intensity_normalize")
  }

  to_log(lg, "info", "Processing will proceed in the following order: {paste(processing_sequence, collapse=', ')}")

  #### handle confounds, filtering to match MRI data. This will also calculate scrubbing information, if requested
  to_regress <- postprocess_confounds(
    proc_files = proc_files,
    cfg = cfg,
    processing_sequence = processing_sequence,
    output_bids_info = output_bids_info,
    fsl_img = fsl_img,
    lg = lg
  )

  # expected censor file for scrubbing
  censor_file <- get_censor_file(output_bids_info)

  # output files use camelCase, with desc on the end, like desc-ismPostproc1, where ism are the steps that have been applied
  prefix_chain <- "" # used for accumulating prefixes with each step
  base_desc <- paste0(toupper(substr(cfg$bids_desc, 1, 1)), substr(cfg$bids_desc, 2, nchar(cfg$bids_desc)))
  first_file <- FALSE

  if (is.null(apply_mask_file)) { # skip apply_mask if we lack a valid mask file
    processing_sequence <- processing_sequence[processing_sequence != "apply_mask"]
  }
  

  #### Loop over fMRI processing steps in sequence
  for (step in processing_sequence) {

    # build up output file desc field for each step
    step_prefix <- switch(step,
      apply_mask = cfg$apply_mask$prefix,
      spatial_smooth = cfg$spatial_smooth$prefix,
      apply_aroma = cfg$apply_aroma$prefix,
      scrub_interpolate = cfg$scrubbing$interpolate_prefix,
      temporal_filter = cfg$temporal_filter$prefix,
      confound_regression = cfg$confound_regression$prefix,
      scrub_timepoints = cfg$scrubbing$prefix,
      intensity_normalize = cfg$intensity_normalize$prefix,
      stop("Unknown step: ", step)
    )

    prefix_chain <- paste0(step_prefix, prefix_chain)
    out_desc <- paste0(prefix_chain, base_desc)

    # determine output file path in postprocessing directory
    bids_info <- as.list(extract_bids_info(cur_file))
    bids_info$description <- out_desc

    # if cur_file input exists outside of output_dir, ensure that its result goes into output_dir
    if (dirname(cur_file) != cfg$output_dir) bids_info$directory <- cfg$output_dir
    out_file <- construct_bids_filename(bids_info, full.names = TRUE)

    # handle extant output
    if (file.exists(out_file) && !isTRUE(cfg$overwrite)) {
      to_log(lg, "info", "Skipping {step}; output exists: {out_file}")
      cur_file <- out_file
      file_set <- c(file_set, cur_file)
      first_file <- TRUE
      next
    }

    if (step == "apply_mask") {
      to_log(lg, "info", "Masking fMRI data using file: {apply_mask_file}")
      cur_file <- apply_mask(cur_file,
        mask_file = apply_mask_file,
        out_file = out_file,
        overwrite=cfg$overwrite, lg = lg, fsl_img = fsl_img
      )
    } else if (step == "spatial_smooth") {
      cur_file <- spatial_smooth(cur_file,
        out_file = out_file,
        brain_mask = brain_mask, fwhm_mm = cfg$spatial_smooth$fwhm_mm,
        overwrite = cfg$overwrite, lg = lg, fsl_img = fsl_img
      )
    } else if (step == "apply_aroma") {
      to_log(lg, "info", "Removing AROMA noise components from fMRI data")
      nonaggressive_val <- cfg$apply_aroma$nonaggressive
      nonaggressive_flag <- if (is.null(nonaggressive_val) || is.na(nonaggressive_val)) TRUE else isTRUE(nonaggressive_val)
      cur_file <- apply_aroma(cur_file,
        out_file = out_file,
        mixing_file = proc_files$melodic_mix,
        noise_ics = proc_files$noise_ics,
        overwrite=cfg$overwrite, lg=lg, nonaggressive = nonaggressive_flag
      )
    } else if (step == "scrub_interpolate") {
      cur_file <- scrub_interpolate(cur_file,
        out_file = out_file,
        censor_file = censor_file, confound_files = to_regress,
        overwrite=cfg$overwrite, lg=lg
      )
    } else if (step == "temporal_filter") {
      cur_file <- temporal_filter(cur_file,
        out_file = out_file,
        tr = cfg$tr, low_pass_hz = cfg$temporal_filter$low_pass_hz,
        high_pass_hz = cfg$temporal_filter$high_pass_hz,
        overwrite=cfg$overwrite, lg=lg, fsl_img = fsl_img,
        method = cfg$temporal_filter$method
      )
    } else if (step == "confound_regression") {
      to_log(lg, "info", "Removing confound regressors from fMRI data using file: {to_regress}")
      cur_file <- confound_regression(cur_file,
        out_file = out_file,
        to_regress = to_regress, censor_file = censor_file,
        overwrite=cfg$overwrite, lg = lg, fsl_img = fsl_img
      )
    } else if (step == "scrub_timepoints") {
      cur_file <- scrub_timepoints(cur_file,
        out_file = out_file,
        censor_file = censor_file,
        overwrite = cfg$overwrite, lg = lg
      )
    } else if (step == "intensity_normalize") {
      cur_file <- intensity_normalize(cur_file,
        out_file = out_file,
        brain_mask = brain_mask,
        global_median = cfg$intensity_normalize$global_median,
        overwrite=cfg$overwrite, lg=lg, fsl_img = fsl_img
      )
    } else {
      stop("Unknown step: ", step)
    }

    file_set <- c(file_set, cur_file)
    first_file <- TRUE
  }

  # clean up intermediate NIfTIs
  if (isFALSE(cfg$keep_intermediates) && length(file_set) > 2L) {
    # initial file is the BOLD input from fmriprep, last file is the final processed image
    to_delete <- file_set[2:(length(file_set) - 1)]
    for (ff in to_delete) {
      if (file.exists(ff)) {
        to_log(lg, "debug", "Removing intermediate file: {ff}")
        unlink(ff)
      }
    }
  }

  # clean up confound regressors file
  if (isFALSE(cfg$keep_intermediates) && isTRUE(cfg$confound_regression$enable)) {
    if (file.exists(to_regress)) {
      to_log(lg, "debug", "Removing intermediate confound regression file: {to_regress}")
      unlink(to_regress)
    }
  }

  # move the final file into a BIDS-friendly file name with a desc field
  if (cur_file != proc_files$bold) {
    to_log(lg, "debug", "Renaming last file in stream: {cur_file} to postprocessed file name: {final_filename}")
    file.rename(cur_file, final_filename)
  } else {
    to_log(lg, "warn", "Not renaming last postprocessed file to {final_filename} because it is the same as the input file {proc_files$bold}")
  }
  
  end_time <- Sys.time()
  to_log(lg, "info", "Final postprocessed is: {final_filename}")
  to_log(lg, "info", "End postprocessing: {as.character(end_time)}")
  return(final_filename)
}
