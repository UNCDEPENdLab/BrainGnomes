#' @noRd
validate_intensity_normalization_order <- function(processing_sequence) {
  normalization_index <- which(processing_sequence == "intensity_normalize")
  if (length(normalization_index) == 0L) return(invisible(TRUE))
  if (length(normalization_index) != 1L) {
    stop("processing_steps must contain intensity_normalize at most once.")
  }

  spatial_steps <- c("apply_mask", "spatial_smooth")
  temporal_steps <- c(
    "apply_aroma", "scrub_interpolate", "temporal_filter",
    "confound_regression", "scrub_timepoints"
  )
  spatial_after <- if (normalization_index < length(processing_sequence)) {
    intersect(
      processing_sequence[seq.int(normalization_index + 1L, length(processing_sequence))],
      spatial_steps
    )
  } else {
    character()
  }
  temporal_before <- if (normalization_index > 1L) {
    intersect(processing_sequence[seq_len(normalization_index - 1L)], temporal_steps)
  } else {
    character()
  }

  if (length(spatial_after) > 0L || length(temporal_before) > 0L) {
    stop(
      paste0(
        "intensity_normalize must occur after apply_mask/spatial_smooth and before ",
        "AROMA, interpolation, temporal filtering, confound regression, and ",
        "timepoint removal. Invalid processing order: ",
        paste(processing_sequence, collapse = ", ")
      )
    )
  }
  invisible(TRUE)
}

smoothness_input_mask_condition <- function(completed_steps, mask_setting,
                                             resolved_mask_file = NULL) {
  if (!"apply_mask" %in% completed_steps) return("none")
  if (checkmate::test_string(mask_setting) &&
      identical(tolower(mask_setting), "template")) {
    return("template")
  }
  mask_name <- if (checkmate::test_string(resolved_mask_file)) {
    basename(resolved_mask_file)
  } else {
    ""
  }
  if (grepl("templatemask\\.nii(?:\\.gz)?$", mask_name, ignore.case = TRUE)) {
    return("template")
  }
  if (grepl("desc-brain_mask\\.nii(?:\\.gz)?$", mask_name, ignore.case = TRUE)) {
    return("fmriprep")
  }
  "custom"
}

#' Create the standard postprocessing whole-brain mask
#'
#' Centralizes the automask configuration shared by `postprocess_subject()` and
#' the spatial-smoothness calibration workflow.
#'
#' @keywords internal
#' @noRd
postprocess_automask <- function(in_file, outfile) {
  automask(
    in_file, outfile = outfile, clfrac = 0.5, NN = 1L,
    SIhh = 0, peels = 1L, fill_holes = TRUE, dilate_steps = 1L
  )
  outfile
}

#' Evaluate one postprocessing validator with a stable result contract
#'
#' Validator warnings and errors are captured so that orchestration can apply
#' `stop_on_failed_validation` consistently. A validator error is distinct from
#' a validator returning `FALSE`, and an intentional skip is distinct from a
#' pass.
#'
#' @keywords internal
#' @noRd
evaluate_postproc_validation <- function(step_name, validator,
                                          input_file, output_file,
                                          output_source = "computed") {
  checkmate::assert_string(step_name, min.chars = 1L)
  checkmate::assert_function(validator)
  checkmate::assert_string(input_file, min.chars = 1L)
  checkmate::assert_string(output_file, min.chars = 1L)
  checkmate::assert_choice(
    output_source,
    c("computed", "reused_workspace", "reused_destination")
  )

  started_at <- Sys.time()
  warning_messages <- character()
  value <- tryCatch(
    withCallingHandlers(
      validator(),
      warning = function(condition) {
        warning_messages <<- c(warning_messages, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) condition
  )
  elapsed_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))

  if (inherits(value, "error")) {
    status <- "error"
    passed <- FALSE
    message <- paste0("Validator error: ", conditionMessage(value))
    details <- list(
      error_class = class(value),
      error_message = conditionMessage(value)
    )
  } else if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    status <- "error"
    passed <- FALSE
    message <- "Validator returned an invalid result; expected one nonmissing logical value."
    details <- list(
      returned_class = class(value),
      returned_length = length(value)
    )
  } else {
    passed <- isTRUE(value)
    details <- attr(value, "details", exact = TRUE)
    if (is.null(details)) details <- list()
    if (!is.list(details)) details <- list(value = details)
    skipped <- passed && isTRUE(details$skipped)
    status <- if (skipped) "skipped" else if (passed) "passed" else "failed"
    message <- attr(value, "message", exact = TRUE)
    if (!checkmate::test_string(message, min.chars = 1L)) {
      message <- switch(
        status,
        passed = "Validation passed.",
        failed = "Validation failed.",
        skipped = "Validation was intentionally skipped."
      )
    }
  }

  list(
    step = step_name,
    output_source = output_source,
    status = status,
    passed = passed,
    message = message,
    warnings = unique(warning_messages),
    input_file = input_file,
    output_file = output_file,
    started_at = format(started_at, "%Y-%m-%dT%H:%M:%OS3%z"),
    elapsed_seconds = elapsed_seconds,
    details = details
  )
}

#' Write a durable postprocessing-validation summary
#'
#' @keywords internal
#' @noRd
write_postproc_validation_summary <- function(path, summary) {
  checkmate::assert_string(path, min.chars = 1L)
  checkmate::assert_list(summary)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(path))) return(FALSE)

  temporary_path <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = dirname(path), fileext = ".tmp"
  )
  on.exit(unlink(temporary_path), add = TRUE)
  json <- jsonlite::toJSON(
    summary, auto_unbox = TRUE, pretty = TRUE, digits = NA,
    null = "null", na = "null"
  )
  writeLines(json, con = temporary_path, useBytes = TRUE)
  isTRUE(file.copy(temporary_path, path, overwrite = TRUE))
}

#' Postprocess a single fMRI BOLD image using a configured pipeline
#'
#' Applies a sequence of postprocessing operations to a single subject-level BOLD NIfTI file, as specified by
#' the user-defined configuration object. Operations may include brain masking, spatial smoothing, ICA-AROMA denoising,
#' temporal filtering, confound regression, and intensity normalization. Intensity
#' normalization is applied after masking/smoothing and before temporal denoising.
#' It can use one robust run multiplier (`run_scalar`) or a denominator-guarded
#' positive voxelwise multiplier map targeting percent signal change
#' (`voxel_psc`). Guarding bounds very low positive denominators and replaces
#' denominators that are invalid or based on too few eligible frames with a
#' run-level fallback; it does not clip BOLD observations or mask voxels. Both
#' modes share the configured prefix and never apply the reference core as an
#' output mask. PSC is defined relative to the smoothed signal when smoothing
#' is enabled; users requiring unsmoothed voxelwise PSC should disable
#' smoothing in that postprocessing stream.
#' The function also optionally computes and saves
#' a filtered confounds file for downstream analyses.
#'
#' The processing sequence can be enforced by the user (`force_processing_order = TRUE`) or determined dynamically based
#' on the `enable` flags in the configuration. Intermediate NIfTI and confound files are staged inside a scratch workspace
#' (located under `cfg$scratch_directory`) and final outputs are written or moved into the postprocessing output directory.
#' Logging is handled via the `lgr` package and is directed to subject-specific log files inferred from BIDS metadata.
#'
#' @param in_file Path to a subject-level BOLD NIfTI file output by fMRIPrep.
#' @param cfg A list containing configuration options, including TR (`cfg$tr`), enabled processing steps (`cfg$<step>$enable`),
#'   logging (`cfg$log_file`), and paths to resources such as singularity images (`cfg$fsl_img`). Processing and intensity-
#'   reference masks are generated internally with `automask()` for their distinct roles.
#'
#' @return The path to the final postprocessed BOLD NIfTI file. Side effects include writing a confounds TSV file (if enabled),
#'   intensity-reference provenance, the reference-core mask, a PSC multiplier
#'   map when requested, and logging to a subject-level log file. When
#'   postprocessing validation is enabled, a machine-readable JSON audit is
#'   written beside the subject log. Newly computed final images remain in the
#'   scratch workspace until their last-step validation has completed.
#'   A portable JSON sidecar and a `.provenance` companion directory contain
#'   resolved processing details, methods text requiring human review,
#'   bibliography, available upstream reports, and separate attempt records.
#'
#' @details
#' Required `cfg` entries:
#' - `tr`: Repetition time in seconds.
#' - `bids_desc`: A BIDS-compliant `desc` label for the output filename.
#' - `processing_steps`: Optional character vector specifying processing order (if `force_processing_order = TRUE`).
#' - `scratch_directory`: Optional directory for staging intermediate files (defaults to `tempdir()` if unset).
#' - `project_name`: Optional project label used to organize scratch workspaces.
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
  checkmate::assert_string(cfg$bids_desc)
  info <- as.list(extract_bids_info(in_file))
  output <- construct_bids_filename(modifyList(info, list(
    description = cfg$bids_desc,
    directory = value_or_default(cfg$output_dir, info$directory)
  )), full.names = TRUE)
  requested <- cfg[intersect(names(cfg), c(
    "bids_desc", "tr", "processing_steps", "force_processing_order",
    "apply_mask", "spatial_smooth", "intensity_normalize", "apply_aroma",
    "temporal_filter", "confound_regression", "confound_calculate",
    "scrubbing", "motion_filter", "validate_postproc_steps", "stop_on_failed_validation"
  ))]
  provenance <- derivative_provenance_new(output, in_file, "postprocessing", requested)
  # Publish only after processing returns. Errors preserve a separate attempt
  # record and propagate to the caller with their original condition intact.
  withCallingHandlers({
    result <- postprocess_subject_impl(in_file, cfg, provenance)
    if (identical(result, in_file)) {
      derivative_provenance_attempt(provenance, "skipped")
    } else if (isTRUE(provenance$reused)) {
      derivative_provenance_reuse(provenance)
    } else {
      provenance$file <- result
      derivative_provenance_publish(provenance)
      derivative_provenance_attempt(provenance, "completed")
    }
    result
  }, error = function(e) {
    tryCatch(derivative_provenance_attempt(provenance, "failed", conditionMessage(e)),
             error = function(export_error) warning(conditionMessage(export_error), call. = FALSE))
  })
}

#' Execute native postprocessing while collecting resolved provenance
#'
#' @param in_file Existing fMRIPrep BOLD input.
#' @param cfg Requested native postprocessing configuration.
#' @param provenance Mutable derivative collector populated at resolution and
#'   execution boundaries, including failures and intermediate reuse.
#' @return Final derivative path, or the input path if processing is skipped.
#' @noRd
postprocess_subject_impl <- function(in_file, cfg, provenance) {
  checkmate::assert_file_exists(in_file)
  checkmate::assert_list(cfg)
  if (!checkmate::test_character(cfg$bids_desc)) {
    stop("postprocess_subject requires a bids_desc field containing the intended description field of the postprocessed filename.")
  }

  normalize_temp_path <- function(path) {
    out <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (.Platform$OS.type == "unix" && startsWith(out, "/private/var/")) {
      out <- sub("^/private", "", out)
    }
    if (grepl("/T/Rtmp", out, fixed = TRUE)) {
      out <- sub("/T/(Rtmp[^/]+)", "/T//\\1", out, perl = TRUE)
    }
    out
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
    attempt_dir <- normalizePath(
      file.path(dirname(in_file), glue("../../../logs/sub-{input_bids_info$sub}")),
      winslash = "/", mustWork = FALSE
    )
    log_dir <- if (dir.exists(attempt_dir)) attempt_dir else dirname(in_file)
  } else {
    log_dir <- dirname(sub_log_file)
  }
  log_dir <- normalize_temp_path(log_dir)
  
  # Setup default postprocess log file -- need to make sure it always goes in the subject log folder
  if (is.null(cfg$log_file)) {
    cfg$log_file <- construct_bids_filename(modifyList(input_bids_info, list(ext=".log", description=cfg$bids_desc)), full.names=FALSE)
  } else {
    cfg$log_file <- glue(cfg$log_file) # evaluate location of log, allowing for glue expressions
  }

  # force log file to be in the right directory
  log_file <- file.path(log_dir, basename(cfg$log_file))
  log_dir_exists <- dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE) || dir.exists(dirname(log_file))
  if (!log_dir_exists || file.access(dirname(log_file), 2) != 0) {
    # fall back to a writable temp directory if the requested log dir is unavailable
    log_dir <- file.path(tempdir(), "postprocess_logs", glue("sub-{input_bids_info$sub}"))
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    log_file <- file.path(log_dir, basename(cfg$log_file))
  }
  if (!file.exists(log_file)) file.create(log_file)

  lg <- lgr::get_logger_glue(c("postprocess", input_bids_info$sub))
  existing_appenders <- names(lg$appenders)
  stale_appenders <- existing_appenders[grepl("^postprocess_log", existing_appenders)]
  if (length(stale_appenders) > 0) {
    for (app_name in stale_appenders) {
      try(lg$appenders[[app_name]]$close(), silent = TRUE)
      try(lg$remove_appender(app_name), silent = TRUE)
    }
  }
  appender <- tryCatch(
    lgr::AppenderFile$new(log_file),
    error = function(e) {
      fallback <- file.path(tempdir(), basename(log_file))
      dir.create(dirname(fallback), recursive = TRUE, showWarnings = FALSE)
      if (!file.exists(fallback)) file.create(fallback)
      lgr::AppenderFile$new(fallback)
    }
  )
  lg$add_appender(appender, name = "postprocess_log")

  # quick header check to avoid 3D or single-volume inputs
  hdr <- suppressWarnings(tryCatch(RNifti::niftiHeader(in_file), error = function(...) NULL))
  if (!is.null(hdr)) {
    dims <- hdr$dim
    is_3d <- !is.null(dims) && length(dims) >= 2 && dims[1] == 3
    too_few_vols <- !is.null(dims) && length(dims) >= 5 && dims[5] < 2
    if (is_3d || too_few_vols) {
      to_log(lg, "warn", "Skipping postprocess_subject: input appears 3D (dim[1]={dims[1]}) or has too few volumes (dim[5]={dims[5]}): {in_file}")
      return(in_file)
    }
  }

  # determine output directory for postprocessed files
  if (is.null(cfg$output_dir)) cfg$output_dir <- input_bids_info$directory
  cfg$output_dir <- normalize_temp_path(cfg$output_dir)
  if (!dir.exists(cfg$output_dir)) dir.create(cfg$output_dir, recursive = TRUE)

  # configure scratch workspace for intermediates
  workspace_project <- cfg$project_name
  if (!checkmate::test_string(workspace_project) || !nzchar(workspace_project)) workspace_project <- "BrainGnomes"
  scratch_dir <- cfg$scratch_directory
  if (!checkmate::test_directory_exists(scratch_dir, access = "w")) {
    scratch_dir <- tempdir()
    to_log(lg, "warn", "scratch_directory is missing or not writable; staging intermediates in {scratch_dir}")
  }
  scratch_dir <- normalize_temp_path(scratch_dir)
  subj_component <- glue("sub-{input_bids_info$sub}")
  ses_component <- if (!is.na(input_bids_info$session)) glue("ses-{input_bids_info$session}") else NULL
  base_stem <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(in_file)))
  workspace_parent <- file.path(scratch_dir, workspace_project, subj_component)
  if (!is.null(ses_component)) workspace_parent <- file.path(workspace_parent, ses_component)
  workspace_dir <- file.path(workspace_parent, base_stem)
  dir.create(workspace_dir, recursive = TRUE, showWarnings = FALSE)
  to_log(lg, "debug", "Postprocess intermediates will be staged in: {workspace_dir}")

  # track temporary files and directories to ensure they're removed on function exit or crash
  temp_files_to_cleanup <- c(workspace_dir)
  on.exit({
    for (tf in temp_files_to_cleanup) {
      if (file.exists(tf) || dir.exists(tf)) {
        unlink_status <- unlink(tf, recursive = TRUE, force = TRUE)
        if (!identical(unlink_status, 0L)) {
          to_log(lg, "warn", "Unable to fully remove temporary path {tf}; unlink() returned {unlink_status}.")
        }
      }
    }
  }, add = TRUE)

  # Reconstruct expected output files for final destination and workspace staging
  final_bids_info <- modifyList(input_bids_info, list(description = cfg$bids_desc, directory = cfg$output_dir))
  final_filename <- construct_bids_filename(final_bids_info, full.names = TRUE)
  workspace_bids_info <- modifyList(final_bids_info, list(directory = workspace_dir))
  validation_summary_file <- file.path(
    log_dir,
    sub(
      "\\.nii(?:\\.gz)?$", "_postproc-validation.json",
      basename(final_filename), perl = TRUE
    )
  )
  validation_records <- list()
  validation_pipeline_completed <- FALSE
  validation_orchestration_started <- FALSE
  validation_started_at <- Sys.time()
  processing_sequence <- character()

  write_validation_summary <- function() {
    if (!validation_orchestration_started) return(invisible(FALSE))
    statuses <- vapply(
      validation_records,
      function(record) record$status,
      character(1)
    )
    overall_status <- if (!length(statuses)) {
      "not_run"
    } else if (any(statuses == "error")) {
      "error"
    } else if (any(statuses == "failed")) {
      "failed"
    } else if (all(statuses == "skipped")) {
      "skipped"
    } else {
      "passed"
    }
    summary <- list(
      schema_version = "postproc-validation-v1",
      input_file = in_file,
      intended_final_file = final_filename,
      validation_enabled = isTRUE(cfg$validate_postproc_steps),
      stop_on_failed_validation = isTRUE(cfg$stop_on_failed_validation),
      pipeline_completed = validation_pipeline_completed,
      overall_status = overall_status,
      processing_steps = processing_sequence,
      started_at = format(
        validation_started_at, "%Y-%m-%dT%H:%M:%OS3%z"
      ),
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      checks = validation_records
    )
    ok <- tryCatch(
      write_postproc_validation_summary(validation_summary_file, summary),
      error = function(condition) {
        to_log(
          lg, "warn",
          "Unable to write postprocessing validation summary {validation_summary_file}: {conditionMessage(condition)}"
        )
        FALSE
      }
    )
    if (!isTRUE(ok)) {
      to_log(
        lg, "warn",
        "Unable to persist postprocessing validation summary: {validation_summary_file}"
      )
    }
    invisible(ok)
  }

  # determine if final output file already exists
  if (checkmate::test_file_exists(final_filename)) {
    to_log(lg, "info", "Postprocessed file already exists: {final_filename}")

    if (isTRUE(cfg$overwrite)) {
      to_log(lg, "info", "Removing {final_filename} because overwrite is TRUE")
      file.remove(final_filename)
      # A replacement may have identical bytes but different processing history.
      # Invalidate the previous sidecar before any replacement is published.
      unlink(derivative_provenance_paths(final_filename)$json)
    } else {
      provenance$reused <- TRUE
      to_log(lg, "info", "Skipping postprocessing for {in_file} because postprocessed file already exists")
      if (isTRUE(cfg$validate_postproc_steps)) {
        prior_summary <- if (file.exists(validation_summary_file)) {
          tryCatch(
            jsonlite::fromJSON(
              validation_summary_file, simplifyVector = FALSE
            ),
            error = function(condition) NULL
          )
        } else {
          NULL
        }
        prior_status <- prior_summary$overall_status
        if (checkmate::test_string(prior_status, min.chars = 1L)) {
          prior_level <- if (prior_status %in% c("failed", "error")) {
            "warn"
          } else {
            "info"
          }
          to_log(
            lg, prior_level,
            paste0(
              "Per-step validation was not rerun because only the existing ",
              "final output is available. Prior audit status is ",
              "'{prior_status}': {validation_summary_file}."
            )
          )
        } else {
          to_log(
            lg, "warn",
            paste0(
              "Per-step validation was not rerun because only the existing ",
              "final output is available, and no readable prior validation ",
              "audit was found at {validation_summary_file}."
            )
          )
        }
      }
      return(final_filename)
    }
  }

  if (isTRUE(cfg$validate_postproc_steps)) {
    validation_orchestration_started <- TRUE
    write_validation_summary()
    on.exit(write_validation_summary(), add = TRUE)
    to_log(
      lg, "info",
      "Postprocessing validation audit will be written to {validation_summary_file}."
    )
  }

  # location of FSL singularity container
  fsl_img <- cfg$fsl_img

  if (!checkmate::test_number(cfg$tr, lower = 0.01, upper = 30)) {
    stop("YAML config must contain a tr field specifying the repetition time in seconds")
  }
  provenance$record$Resolved$TR <- cfg$tr
  provenance$record$Resolved$Space <- input_bids_info$space
  provenance$record$Resolved$IntensityConvention <-
    "Input intensity scale is retained except for explicitly recorded normalization; regression preserves temporal means."
  if (checkmate::test_file_exists(fsl_img)) {
    # Share the established checksum cache within this R process. Container
    # identity denotes configuration; operation records establish actual use.
    provenance$record$Software$ConfiguredContainers <- list(derivative_provenance_fingerprint(
      fsl_img, "fsl_container", file.path(tempdir(), "derivative-container-checksums.json")
    ))
  }

  # default to not enforcing user-specified order of processing steps
  if (!checkmate::test_flag(cfg$force_processing_order)) cfg$force_processing_order <- FALSE

  start_time <- Sys.time()
  to_log(lg, "info", "Start preprocessing: {as.character(start_time)}")
  
  # Compute a generous processing mask for smoothing and related validation.
  # Intensity normalization constructs a separate conservative reference mask.
  brain_mask <- tempfile(fileext = ".nii.gz")
  temp_files_to_cleanup <- c(temp_files_to_cleanup, brain_mask)
  postprocess_automask(proc_files$bold, brain_mask)

  # if apply_mask is enabled, determine which mask file to apply
  apply_mask_file <- NULL
  if (isTRUE(cfg$apply_mask$enable)) {
    apply_mask_file <- cfg$apply_mask$mask_file
    if (checkmate::test_string(apply_mask_file) && !is.na(apply_mask_file)) {
      if (apply_mask_file == "template") {
        apply_mask_file <- resample_template_to_img(in_file, lg = lg)
      } else if (!checkmate::test_file_exists(apply_mask_file)) {
        to_log(lg, "warn", "Cannot find apply_mask mask_file: {apply_mask_file}. This step will be skipped!")
        apply_mask_file <- NULL
      }
    } else {
      apply_mask_file <- NULL # not a string or is NA?
    }
  }

  cur_file <- proc_files$bold

  ## setup order of processing steps
  if (isTRUE(cfg$force_processing_order)) {

    checkmate::assert_character(cfg$processing_steps) # ensure we have a character vector
    cfg$processing_steps <- tolower(cfg$processing_steps) # avoid case issues

    # handle small glitches in nomenclature
    cfg$processing_steps <- sub("^spatial_smoothing$", "spatial_smooth", cfg$processing_steps)
    cfg$processing_steps <- sub("^temporal_filtering$", "temporal_filter", cfg$processing_steps)
    cfg$processing_steps <- sub("^confound_regress$", "confound_regression", cfg$processing_steps)
    cfg$processing_steps <- sub("^intensity_normalization$", "intensity_normalize", cfg$processing_steps)

    processing_sequence <- cfg$processing_steps
    to_log(lg, "info", "We will follow the user-specified processing order, with no guarantees on data validity.")
  } else {
    processing_sequence <- c()
    if (isTRUE(cfg$apply_mask$enable)) processing_sequence <- c(processing_sequence, "apply_mask")
    if (isTRUE(cfg$spatial_smooth$enable)) processing_sequence <- c(processing_sequence, "spatial_smooth")
    if (isTRUE(cfg$intensity_normalize$enable)) processing_sequence <- c(processing_sequence, "intensity_normalize")
    if (isTRUE(cfg$apply_aroma$enable)) processing_sequence <- c(processing_sequence, "apply_aroma")
    if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$interpolate)) processing_sequence <- c(processing_sequence, "scrub_interpolate")
    if (isTRUE(cfg$temporal_filter$enable)) processing_sequence <- c(processing_sequence, "temporal_filter")
    if (isTRUE(cfg$confound_regression$enable)) processing_sequence <- c(processing_sequence, "confound_regression")
    if (isTRUE(cfg$scrubbing$enable) && isTRUE(cfg$scrubbing$apply)) processing_sequence <- c(processing_sequence, "scrub_timepoints")
  }

  provenance$record$RequestedSequence <- derivative_provenance_array(processing_sequence)
  if (is.null(apply_mask_file)) {
    if ("apply_mask" %in% processing_sequence) {
      derivative_provenance_operation(provenance, "apply_mask", "skipped", message = "No usable mask was resolved.")
    }
    processing_sequence <- processing_sequence[processing_sequence != "apply_mask"]
  }
  validate_intensity_normalization_order(processing_sequence)
  derivative_provenance_sources(provenance, list(spatial_mask = apply_mask_file))
  if (any(vapply(cfg[c("confound_regression", "confound_calculate", "scrubbing",
                        "motion_filter", "intensity_normalize")], function(x) isTRUE(x$enable), logical(1)))) {
    derivative_provenance_sources(provenance, list(confounds = proc_files$confounds))
  }
  if ("apply_aroma" %in% processing_sequence) {
    derivative_provenance_sources(provenance, list(melodic_mix = proc_files$melodic_mix,
      aroma_metrics = proc_files$aroma_metrics))
  }

  to_log(lg, "info", "Processing will proceed in the following order: {paste(processing_sequence, collapse=', ')}")

  workspace_confounds_file <- construct_bids_filename(
    modifyList(workspace_bids_info, list(suffix = "confounds", ext = ".tsv")), full.names = TRUE
  )
  final_confounds_file <- construct_bids_filename(
    modifyList(final_bids_info, list(suffix = "confounds", ext = ".tsv")), full.names = TRUE
  )
  workspace_scrub_file <- construct_bids_filename(
    modifyList(workspace_bids_info, list(suffix = "scrub", ext = ".tsv")), full.names = TRUE
  )
  final_scrub_file <- construct_bids_filename(
    modifyList(final_bids_info, list(suffix = "scrub", ext = ".tsv")), full.names = TRUE
  )
  workspace_censor_file <- get_censor_file(workspace_bids_info)
  final_censor_file <- get_censor_file(final_bids_info)
  final_regressors_file <- construct_bids_filename(
    modifyList(final_bids_info, list(suffix = "regressors", ext = ".tsv")), full.names = TRUE
  )
  workspace_reference_core_file <- construct_bids_filename(
    modifyList(workspace_bids_info, list(
      description = "intensityReferenceCore", suffix = "mask", ext = ".nii.gz"
    )), full.names = TRUE
  )
  final_reference_core_file <- construct_bids_filename(
    modifyList(final_bids_info, list(
      description = "intensityReferenceCore", suffix = "mask", ext = ".nii.gz"
    )), full.names = TRUE
  )
  workspace_reference_json <- construct_bids_filename(
    modifyList(workspace_bids_info, list(
      description = "intensityReferenceCore", suffix = "mask", ext = ".json"
    )), full.names = TRUE
  )
  final_reference_json <- construct_bids_filename(
    modifyList(final_bids_info, list(
      description = "intensityReferenceCore", suffix = "mask", ext = ".json"
    )), full.names = TRUE
  )
  workspace_psc_scale_file <- construct_bids_filename(
    modifyList(workspace_bids_info, list(
      description = "intensityNormalizationScale", suffix = "map", ext = ".nii.gz"
    )), full.names = TRUE
  )
  final_psc_scale_file <- construct_bids_filename(
    modifyList(final_bids_info, list(
      description = "intensityNormalizationScale", suffix = "map", ext = ".nii.gz"
    )), full.names = TRUE
  )
  workspace_reference_automask <- file.path(workspace_dir, "intensity_reference_automask.nii.gz")

  #### handle confounds, filtering to match MRI data. This will also calculate scrubbing information, if requested
  to_regress <- postprocess_confounds(
    proc_files = proc_files,
    cfg = cfg,
    processing_sequence = processing_sequence,
    output_bids_info = workspace_bids_info,
    fsl_img = fsl_img,
    lg = lg,
    provenance = provenance
  )

  if (isTRUE(cfg$confound_regression$enable) && is.null(to_regress)) {
    derivative_provenance_operation(provenance, "confound_regression", "skipped", message = "No regressors were generated.")
    to_log(lg, "warn", "Confound regression was requested but no regressors were generated; skipping confound_regression step.")
    processing_sequence <- processing_sequence[processing_sequence != "confound_regression"]
  }

  # The target is resolved now, but the denominator and factor are deliberately
  # measured at the post-spatial, pre-temporal normalization checkpoint.
  normalization_reference <- NULL
  normalization_target <- NULL
  normalization_mode <- NULL
  if ("intensity_normalize" %in% processing_sequence) {
    normalization_mode <- resolve_intensity_normalization_mode(
      cfg$intensity_normalize
    )
    normalization_target <- resolve_intensity_normalization_target(
      cfg$intensity_normalize, mode = normalization_mode
    )
  }

  # expected censor file for scrubbing
  censor_file <- workspace_censor_file

  # output files use camelCase, with desc on the end, like desc-ismPostproc1, where ism are the steps that have been applied
  prefix_chain <- "" # used for accumulating prefixes with each step
  base_desc <- paste0(toupper(substr(cfg$bids_desc, 1, 1)), substr(cfg$bids_desc, 2, nchar(cfg$bids_desc)))
  intermediate_outputs <- list()

  n_steps <- length(processing_sequence)
  provenance$record$Resolved$ProcessingSequence <- derivative_provenance_array(processing_sequence)
  provenance$record$Resolved$CensorPolicy <- cfg$scrubbing
  provenance$record$Resolved$Censor <- value_or_default(
    provenance$record$Resolved$Confounds$Censor, derivative_provenance_censor(censor_file)
  )
  if (n_steps == 0L) {
    # Publication moves its staging file. Stage a copy when no image operation
    # runs so that this move cannot consume the original fMRIPrep input.
    cur_file <- file.path(workspace_dir, basename(proc_files$bold))
    if (!file.copy(proc_files$bold, cur_file, overwrite = TRUE)) stop("Unable to stage input copy.")
    derivative_provenance_operation(provenance, "copy_input", "executed")
  }

  make_postproc_validator <- function(step_name, pre_file, post_file,
                                      step_index, context = list()) {
    completed_steps <- if (step_index > 1L) {
      processing_sequence[seq_len(step_index - 1L)]
    } else {
      character()
    }
    switch(
      step_name,
      apply_mask = function() {
        validate_apply_mask(
          pre_file = pre_file, post_file = post_file,
          mask_file = apply_mask_file
        )
      },
      spatial_smooth = function() {
        validate_spatial_smooth(
          pre_file = pre_file,
          post_file = post_file,
          mask_file = brain_mask,
          fwhm_mm = cfg$spatial_smooth$fwhm_mm,
          input_mask = smoothness_input_mask_condition(
            completed_steps,
            mask_setting = cfg$apply_mask$mask_file,
            resolved_mask_file = apply_mask_file
          )
        )
      },
      apply_aroma = function() {
        nonaggressive_value <- cfg$apply_aroma$nonaggressive
        nonaggressive_flag <- if (
          is.null(nonaggressive_value) || is.na(nonaggressive_value)
        ) TRUE else isTRUE(nonaggressive_value)
        validate_apply_aroma(
          pre_file = pre_file,
          post_file = post_file,
          mixing_file = proc_files$melodic_mix,
          noise_ics = proc_files$noise_ics,
          nonaggressive = nonaggressive_flag,
          mask_file = brain_mask
        )
      },
      scrub_interpolate = function() {
        validate_scrub_interpolate(
          pre_file = pre_file, post_file = post_file,
          censor_file = censor_file
        )
      },
      temporal_filter = function() {
        validate_temporal_filter(
          pre_file = pre_file,
          post_file = post_file,
          tr = cfg$tr,
          band_low_hz = cfg$temporal_filter$high_pass_hz,
          band_high_hz = cfg$temporal_filter$low_pass_hz,
          mask_file = brain_mask
        )
      },
      confound_regression = function() {
        validate_confound_regression(
          pre_file = pre_file,
          post_file = post_file,
          to_regress = to_regress,
          censor_file = censor_file,
          mask_file = brain_mask
        )
      },
      scrub_timepoints = function() {
        validate_scrub_timepoints(
          pre_file = pre_file,
          post_file = post_file,
          censor_vec = context$censor_vec
        )
      },
      intensity_normalize = function() {
        validate_intensity_normalize(
          pre_file = pre_file,
          post_file = post_file,
          mode = normalization_mode,
          reference_location = normalization_reference$reference_location,
          target = normalization_reference$target,
          scale_factor = normalization_reference$scale_factor,
          scale_file = normalization_reference$scale_file,
          core_file = normalization_reference$core_file,
          include_frames = normalization_reference$include_frames,
          scratch_directory = workspace_dir,
          tolerance = 1e-5
        )
      },
      stop("No postprocessing validator is registered for step: ", step_name)
    )
  }

  postproc_validate_or_stop <- function(step_name, validator, input_file,
                                        output_file,
                                        output_source = "computed") {
    record <- evaluate_postproc_validation(
      step_name = step_name,
      validator = validator,
      input_file = input_file,
      output_file = output_file,
      output_source = output_source
    )
    record$sequence_index <- length(validation_records) + 1L
    validation_records[[length(validation_records) + 1L]] <<- record
    provenance$record$Validation <- validation_records

    if (length(record$warnings)) {
      for (warning_message in record$warnings) {
        to_log(
          lg, "warn",
          "{step_name} validation warning: {warning_message}"
        )
      }
    }
    log_level <- if (record$status %in% c("failed", "error")) {
      "error"
    } else {
      "info"
    }
    to_log(
      lg, log_level,
      paste0(
        "{step_name} validation {record$status} ",
        "[{record$output_source}; {format(record$elapsed_seconds, digits = 4)} s]: ",
        "{record$message}"
      )
    )
    write_validation_summary()

    if (record$status %in% c("failed", "error") &&
        isTRUE(cfg$stop_on_failed_validation)) {
      stop(
        glue(
          "{step_name} validation {record$status}: {record$message} ",
          "stop_on_failed_validation is TRUE. STOPPING postprocessing for this dataset."
        )
      )
    }
    invisible(record)
  }

  #### Loop over fMRI processing steps in sequence
  for (ii in seq_along(processing_sequence)) {
    step <- processing_sequence[[ii]]
    parameters <- switch(step,
      scrub_interpolate = cfg$scrubbing,
      scrub_timepoints = cfg$scrubbing,
      cfg[[step]]
    )
    if (step == "confound_regression") {
      parameters$columns <- provenance$record$Resolved$Confounds$RegressorColumns
      parameters$preserve_mean <- TRUE
    }
    if (step == "apply_aroma") {
      val <- cfg$apply_aroma$nonaggressive
      parameters$nonaggressive <- if (is.null(val) || is.na(val)) TRUE else isTRUE(val)
      parameters$noise_components <- derivative_provenance_array(proc_files$noise_ics)
    }
    if (step == "temporal_filter") {
      parameters$method <- value_or_default(parameters$method, "fslmaths")
      parameters$TR <- cfg$tr
      # Match temporal_filter()'s cutoff normalization; null means disabled.
      if (is.null(parameters$high_pass_hz) || is.infinite(parameters$high_pass_hz) ||
          abs(parameters$high_pass_hz) < 1e-6) parameters$high_pass_hz <- NULL
      if (is.null(parameters$low_pass_hz) || is.infinite(parameters$low_pass_hz)) parameters$low_pass_hz <- NULL
    }
    if (step == "apply_mask") parameters$resolved_mask <- derivative_provenance_fingerprint(apply_mask_file, "mask")
    if (step == "spatial_smooth") {
      parameters$mask <- derivative_provenance_fingerprint(brain_mask, "generated_automask")
      parameters$brightness_threshold <- "0.75 * (masked median intensity - masked 2nd percentile)"
      parameters$extent_policy <- "Reapply the nonzero temporal-minimum mask after SUSAN smoothing"
    }
    if (step == "intensity_normalize") {
      parameters$mode <- normalization_mode
      parameters$target <- normalization_target
    }
    derivative_provenance_operation(provenance, step, "running", parameters)
    is_last_step <- ii == n_steps

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
    bids_info$description <- if (is_last_step) cfg$bids_desc else out_desc
    # Every newly computed image, including the final image, remains in scratch
    # until its validator has run. This prevents a failed last-step validation
    # from publishing an output that a later run could mistake for complete.
    bids_info$directory <- workspace_dir
    out_file <- construct_bids_filename(bids_info, full.names = TRUE)
    dest_out_file <- if (is_last_step) {
      final_filename
    } else {
      file.path(cfg$output_dir, basename(out_file))
    }
    if (is_last_step) {
      to_log(lg, "debug", "Step {step}: input {cur_file}, staged final output {out_file}, destination {dest_out_file}, prefix chain {prefix_chain}")
    } else {
      to_log(lg, "debug", "Step {step}: input {cur_file}, workspace output {out_file}, destination {dest_out_file}, prefix chain {prefix_chain}")
    }

    # Reference membership comes from the original positive-scale BOLD, while
    # the denominator is measured on the image entering the normalization step.
    # Prepare this even when an existing normalized intermediate will be reused
    # so the core and provenance sidecar remain available.
    if (step == "intensity_normalize" && is.null(normalization_reference)) {
      completed_steps <- if (ii > 1L) processing_sequence[seq_len(ii - 1L)] else character()
      normalization_reference <- prepare_intensity_reference(
        in_file = proc_files$bold,
        calibration_file = cur_file,
        calibration_steps = completed_steps,
        target = normalization_target,
        mode = normalization_mode,
        confounds_file = proc_files$confounds,
        censor_file = workspace_censor_file,
        automask_file = workspace_reference_automask,
        core_file = workspace_reference_core_file,
        sidecar_file = workspace_reference_json,
        scale_file = if (identical(normalization_mode, "voxel_psc")) {
          workspace_psc_scale_file
        } else {
          ""
        },
        lg = lg
      )
    }

    pre_step_file <- cur_file
    validation_context <- list()
    if (step == "scrub_timepoints") {
      validation_context$censor_vec <- if (
        checkmate::test_file_exists(censor_file)
      ) {
        as.integer(readLines(censor_file))
      } else {
        integer()
      }
    }

    existing_workspace <- file.exists(out_file)
    existing_destination <- file.exists(dest_out_file)

    if (existing_workspace && !isTRUE(cfg$overwrite)) {
      derivative_provenance_operation(provenance, step, "reused", parameters,
                    origin = derivative_provenance_fingerprint(out_file, "reused_intermediate"))
      to_log(lg, "info", "Skipping {step}; workspace file exists: {out_file}")
      cur_file <- out_file
      intermediate_outputs[[out_file]] <- dest_out_file
      if (isTRUE(cfg$validate_postproc_steps)) {
        postproc_validate_or_stop(
          step_name = step,
          validator = make_postproc_validator(
            step, pre_step_file, cur_file, ii, validation_context
          ),
          input_file = pre_step_file,
          output_file = cur_file,
          output_source = "reused_workspace"
        )
      }
      next
    }

    if (!existing_workspace && existing_destination && !isTRUE(cfg$overwrite)) {
      derivative_provenance_operation(provenance, step, "reused", parameters,
                    origin = derivative_provenance_fingerprint(dest_out_file, "reused_intermediate"))
      to_log(lg, "info", "Reusing existing {step} output from {dest_out_file}")
      cur_file <- dest_out_file
      if (isTRUE(cfg$validate_postproc_steps)) {
        postproc_validate_or_stop(
          step_name = step,
          validator = make_postproc_validator(
            step, pre_step_file, cur_file, ii, validation_context
          ),
          input_file = pre_step_file,
          output_file = cur_file,
          output_source = "reused_destination"
        )
      }
      next
    }

    if (existing_workspace && isTRUE(cfg$overwrite)) {
      unlink(out_file)
    }

    if (!is_last_step && file.exists(dest_out_file) && isTRUE(cfg$overwrite)) {
      unlink(dest_out_file)
    }

    if (step == "apply_mask") {
      cur_file <- apply_mask(cur_file,
        mask_file = apply_mask_file,
        out_file = out_file,
        overwrite = cfg$overwrite, lg = lg, fsl_img = fsl_img
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
        mode = normalization_mode,
        scale_factor = normalization_reference$scale_factor,
        scale_file = normalization_reference$scale_file,
        overwrite=cfg$overwrite, lg=lg, fsl_img = fsl_img
      )
    } else {
      stop("Unknown step: ", step)
    }

    # Some helpers return their input when prerequisites are unavailable. That
    # return is a skip, even when the requested settings enabled the operation.
    operation_status <- if (identical(cur_file, pre_step_file)) "skipped" else "executed"
    if (step == "apply_aroma" && operation_status == "executed") {
      n_components <- ncol(data.table::fread(proc_files$melodic_mix, header = FALSE, nrows = 1L))
      selected <- sort(unique(as.integer(proc_files$noise_ics)))
      parameters$noise_components <- derivative_provenance_array(selected[selected >= 1L & selected <= n_components])
    }
    derivative_provenance_operation(provenance, step, operation_status, parameters,
      message = if (operation_status == "skipped") "Operation returned its input without a transformation." else NULL)

    if (isTRUE(cfg$validate_postproc_steps)) {
      postproc_validate_or_stop(
        step_name = step,
        validator = make_postproc_validator(
          step, pre_step_file, cur_file, ii, validation_context
        ),
        input_file = pre_step_file,
        output_file = cur_file,
        output_source = "computed"
      )
    }

    if (!is_last_step) intermediate_outputs[[out_file]] <- dest_out_file
  }

  # ensure we do not treat the last workspace file as an intermediate artifact
  if (!is.null(intermediate_outputs[[cur_file]])) intermediate_outputs[[cur_file]] <- NULL

  move_staged_file <- function(src, dest, overwrite = FALSE, label = "file") {
    if (is.null(src) || !nzchar(src) || !file.exists(src) || is.null(dest) || !nzchar(dest)) return(FALSE)
    src_norm <- normalizePath(src, winslash = "/", mustWork = FALSE)
    dest_norm <- normalizePath(dest, winslash = "/", mustWork = FALSE)
    if (identical(src_norm, dest_norm)) return(TRUE)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(dest)) {
      if (!isTRUE(overwrite)) {
        to_log(lg, "info", "Skipping move for {label}; destination exists: {dest}")
        return(TRUE)
      }
      unlink(dest)
    }
    ok <- file.rename(src, dest)
    if (!ok) {
      ok <- file.copy(src, dest, overwrite = TRUE)
      if (ok) unlink(src)
    }
    if (!ok) {
      to_log(lg, "warn", "Unable to move {label} from {src} to {dest}")
    } else {
      to_log(lg, "debug", "Moved {label} from {src} to {dest}")
    }
    return(ok)
  }

  # Helpers may return their input when skipping, and reused intermediates may
  # live outside scratch. Publication must not move those original files.
  if (!startsWith(normalizePath(cur_file, winslash = "/", mustWork = TRUE),
                  paste0(normalizePath(workspace_dir, winslash = "/"), "/"))) {
    staged_copy <- file.path(workspace_dir, "publish-copy.nii.gz")
    if (!file.copy(cur_file, staged_copy, overwrite = TRUE)) stop("Unable to stage publication copy.")
    cur_file <- staged_copy
  }

  # move the final file into a BIDS-friendly file name with a desc field
  final_ready <- if (!identical(cur_file, final_filename)) {
    move_staged_file(
      cur_file, final_filename, overwrite = isTRUE(cfg$overwrite),
      label = "final postprocessed file"
    )
  } else {
    to_log(lg, "debug", "Final postprocessed file already written to destination: {final_filename}")
    file.exists(final_filename)
  }
  if (!isTRUE(final_ready) || !file.exists(final_filename)) {
    stop("Unable to publish validated final postprocessed file: ", final_filename)
  }

  ancillary_candidates <- list(
    list(src = workspace_censor_file, dest = final_censor_file, label = "censor file"),
    list(src = workspace_scrub_file, dest = final_scrub_file, label = "scrubbing regressors"),
    list(src = workspace_confounds_file, dest = final_confounds_file, label = "postprocessed confounds"),
    list(src = to_regress, dest = final_regressors_file, label = "regressor file"),
    list(src = workspace_reference_core_file, dest = final_reference_core_file, label = "intensity reference-core mask"),
    list(src = workspace_reference_json, dest = final_reference_json, label = "intensity reference provenance"),
    list(src = workspace_psc_scale_file, dest = final_psc_scale_file, label = "voxelwise PSC multiplier map")
  )

  for (cand in ancillary_candidates) {
    if (!is.null(cand$src) && nzchar(cand$src) && file.exists(cand$src)) {
      # These companions were resolved for the newly published image. Keeping
      # an older censor or design file here would misdescribe that derivative.
      if (!move_staged_file(cand$src, cand$dest, overwrite = TRUE, label = cand$label)) {
        stop("Unable to publish postprocessing companion: ", cand$dest)
      }
    }
  }
  if (isTRUE(provenance$record$Resolved$Censor$Available)) {
    provenance$record$Resolved$Censor$File <- derivative_provenance_fingerprint(final_censor_file, "censor")
  }

  if (isTRUE(cfg$keep_intermediates) && length(intermediate_outputs) > 0L) {
    for (src in names(intermediate_outputs)) {
      dest <- intermediate_outputs[[src]]
      if (is.null(dest) || !nzchar(dest)) next
      move_staged_file(src, dest, overwrite = isTRUE(cfg$overwrite), label = "intermediate file")
    }
  }

  validation_pipeline_completed <- TRUE
  provenance$record$Validation <- validation_records
  provenance$record$Resolved$IntensityReference <- if (!is.null(normalization_reference)) {
    intensity_reference_provenance(
      normalization_reference, final_reference_core_file,
      final_reference_json, final_psc_scale_file
    )
  } else NULL
  if (validation_orchestration_started) {
    write_validation_summary()
    validation_statuses <- vapply(
      validation_records, function(record) record$status, character(1)
    )
    validation_counts <- table(factor(
      validation_statuses,
      levels = c("passed", "skipped", "failed", "error")
    ))
    validation_summary_level <- if (
      validation_counts[["failed"]] > 0L ||
        validation_counts[["error"]] > 0L
    ) "warn" else "info"
    to_log(
      lg, validation_summary_level,
      paste0(
        "Postprocessing validation summary: ",
        "passed={validation_counts[['passed']]}, ",
        "skipped={validation_counts[['skipped']]}, ",
        "failed={validation_counts[['failed']]}, ",
        "errors={validation_counts[['error']]}"
      )
    )
    to_log(
      lg, "info",
      "Postprocessing validation audit saved to {validation_summary_file}."
    )
  }

  end_time <- Sys.time()
  to_log(lg, "info", "Final postprocessed is: {final_filename}")
  to_log(lg, "info", "End postprocessing: {as.character(end_time)}")
  return(final_filename)
}
