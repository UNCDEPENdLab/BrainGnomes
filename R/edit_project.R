#' Interactively edit a project configuration by field (field-guided)
#'
#' Allows the user to interactively browse and edit individual fields within the
#' configuration object, grouped by domain. Field paths are defined within the function
#' to avoid relying on a complete `scfg` structure.
#'
#' @param input A `bg_project_cfg` object, a YAML file path, or a project
#'   directory containing \code{project_config.yaml}. If a directory is provided
#'   but the file is absent, \code{edit_project} will stop. This argument cannot
#'   be \code{NULL}.
#' @return An updated `bg_project_cfg` object. The updated configuration is
#'   written to `project_config.yaml` in the project directory unless the user
#'   chooses not to overwrite an existing file.
#' @importFrom utils select.list
#' @export
edit_project <- function(input = NULL) {
  scfg <- get_scfg_from_input(input)
  # a valid list must be returned for editing
  if (length(scfg) == 0L) stop("input must be a bg_project_cfg object, YAML file, or project directory")

  # Helper: after enabling a step, validate and prompt for missing fields specific to that step
  validate_after_enable <- function(scfg, step_name, setup_fn) {
    # step_name examples: "flywheel_sync", "bids_conversion", "bids_validation", "fmriprep", "mriqc", "aroma"
    gaps <- character(0)

    if (identical(step_name, "bids_conversion")) {
      scfg <- validate_bids_conversion(scfg, quiet = TRUE)
      gaps <- attr(scfg, "gaps")
    } else {
      v <- validate_project(scfg, quiet = TRUE, correct_problems = FALSE)
      gaps_all <- attr(v, "gaps")
      if (!is.null(gaps_all)) {
        keep_prefix <- c(
          paste0(step_name, "/"),
          if (step_name %in% c("fmriprep", "aroma", "postprocess")) c("metadata/fmriprep_directory") else character(0),
          if (step_name %in% c("mriqc")) c("metadata/mriqc_directory") else character(0),
          if (step_name %in% c("postprocess", "extract_rois")) c("metadata/postproc_directory") else character(0)
        )
        ce_needed <- switch(step_name,
          fmriprep = "compute_environment/fmriprep_container",
          mriqc = "compute_environment/mriqc_container",
          aroma = "compute_environment/aroma_container",
          bids_validation = "compute_environment/bids_validator",
          flywheel_sync = "compute_environment/flywheel",
          postprocess = "compute_environment/fsl_container",
          NULL
        )
        if (!is.null(ce_needed)) keep_prefix <- c(keep_prefix, ce_needed)
        keep_prefix <- unique(c(keep_prefix, paste0(step_name, "/enable")))
        gaps <- gaps_all[vapply(gaps_all, function(g) any(startsWith(g, keep_prefix)), logical(1))]
      }
    }

    if (is.null(gaps) || length(gaps) == 0L) return(scfg)

    md_gaps <- grep("^metadata/", gaps, value = TRUE)
    ce_gaps <- grep("^compute_environment/", gaps, value = TRUE)
    step_gaps <- setdiff(gaps, c(md_gaps, ce_gaps))

    if (length(md_gaps)) scfg <- setup_project_metadata(scfg, fields = md_gaps)
    if (length(ce_gaps)) scfg <- setup_compute_environment(scfg, fields = ce_gaps)
    if (length(step_gaps)) scfg <- setup_fn(scfg, fields = step_gaps)

    return(scfg)
  }

  # Define editable fields per setup function
  config_map <- list(
    "General" = list(setup_fn = setup_project_metadata, prefix = "metadata/", fields = c(
      "project_name", "project_directory", "dicom_directory", "bids_directory", "fmriprep_directory", "scratch_directory", "templateflow_home",
      "flywheel_temp_directory", "flywheel_sync_directory", "rois_directory"
    )),
    "Compute Environment" = list(setup_fn = setup_compute_environment, prefix = "compute_environment/", fields = c(
      "scheduler", "fmriprep_container", "heudiconv_container", "bids_validator", "mriqc_container", "aroma_container", "fsl_container", "flywheel"
    )),
    "Flywheel Sync" = list(setup_fn = setup_flywheel_sync, prefix = "flywheel_sync/", fields = c(
      "enable", "source_url", "save_audit_logs"
    )),
    "BIDS Conversion" = list(setup_fn = setup_bids_conversion, prefix = "bids_conversion/", fields = c(
      "enable", "sub_regex", "sub_id_match", "ses_regex", "ses_id_match",
      "heuristic_file", "overwrite", "clear_cache"
    )),
    "BIDS Validation" = list(setup_fn = setup_bids_validation, prefix = "bids_validation/", fields = c(
      "enable", "outfile"
    )),
    "fMRIPrep" = list(setup_fn = setup_fmriprep, prefix = "fmriprep/", fields = c(
      "enable", "output_spaces", "fs_license_file"
    )),
    "MRIQC" = list(setup_fn = setup_mriqc, prefix = "mriqc/", fields = c(
      "enable"
    )),
    "ICA-AROMA" = list(setup_fn = setup_aroma, prefix = "aroma/", fields = c(
      "enable", "cleanup"
    ))
  )

  job_targets <- c("flywheel_sync", "bids_conversion", "bids_validation", "fmriprep", "mriqc", "aroma", "postprocess", "extract_rois")
  job_fields <- c("memgb", "nhours", "ncores", "cli_options", "sched_args")

  show_val <- function(val) {
    if (is.null(val)) {
      "NULL"
    } else if (length(val) == 0L || all(is.na(val))) {
      "None"
    } else if (is.logical(val)) {
      toupper(as.character(val))
    } else if (is.character(val) && length(val) > 1) {
      paste(val, collapse = ", ")
    } else {
      as.character(val)
    }
  }

  # Top-level menu loop
  repeat {
    choice <- select_list_safe(c(names(config_map), "Postprocessing", "ROI extraction", "Job settings", "Quit"),
      title = "Select a configuration area to edit:"
    )

    if (choice == "Quit" || choice == "") {
      message("Exiting configuration editor.")
      break
    }

    if (choice == "Postprocessing") {
      old_enable <- isTRUE(scfg$postprocess$enable)
      scfg <- setup_postprocess_streams(scfg)
      new_enable <- isTRUE(scfg$postprocess$enable)
      if (new_enable && !old_enable) scfg <- validate_after_enable(scfg, "postprocess", setup_postprocess_streams)
    } else if (choice == "ROI extraction") {
      old_enable <- isTRUE(scfg$extract_rois$enable)
      scfg <- setup_extract_streams(scfg)
      new_enable <- isTRUE(scfg$extract_rois$enable)
      if (new_enable && !old_enable) scfg <- validate_after_enable(scfg, "extract_rois", setup_extract_streams)
    } else if (choice == "Job settings") {
      # Job settings logic
      job <- select_list_safe(job_targets, title = "Select which job to configure:")
      if (length(job) == 0 || job == "") next

      if (job == "postprocess") {
        streams <- get_postprocess_stream_names(scfg)
        if (length(streams) == 0) {
          message("No postprocess streams defined.")
          next
        }

        stream <- if (length(streams) == 1L) streams else select_list_safe(streams, title = "Select postprocess stream:")
        if (length(stream) == 0 || stream == "") next

        job_field_display <- sapply(job_fields, function(fld) {
          val <- get_nested_values(scfg, paste0("postprocess/", stream, "/", fld))
          sprintf("%s [ %s ]", fld, show_val(val))
        })

        selected_job_fields <- select_list_safe(job_field_display,
          multiple = TRUE,
          title = glue::glue("Select fields to edit for postprocess stream '{stream}':")
        )

        if (length(selected_job_fields) == 0) next

        scfg <- setup_postprocess_stream(
          scfg,
          stream_name = stream,
          fields = paste0("postprocess/", stream, "/", names(selected_job_fields))
        )
      } else if (job == "extract_rois") {
        streams <- get_extract_stream_names(scfg)
        if (length(streams) == 0) {
          message("No ROI extraction streams defined.")
          next
        }

        stream <- if (length(streams) == 1L) streams else select_list_safe(streams, title = "Select ROI extraction stream:")
        if (length(stream) == 0 || stream == "") next

        job_field_display <- sapply(job_fields, function(fld) {
          val <- get_nested_values(scfg, paste0("extract_rois/", stream, "/", fld))
          sprintf("%s [ %s ]", fld, show_val(val))
        })

        selected_job_fields <- select_list_safe(job_field_display,
          multiple = TRUE,
          title = glue::glue("Select fields to edit for extract stream '{stream}':")
        )

        if (length(selected_job_fields) == 0) next

        scfg <- setup_extract_stream(
          scfg,
          stream_name = stream,
          fields = paste0("extract_rois/", stream, "/", names(selected_job_fields))
        )
      } else {
        job_field_display <- sapply(job_fields, function(fld) {
          val <- get_nested_values(scfg, paste0(job, "/", fld))
          sprintf("%s [ %s ]", fld, show_val(val))
        })

        selected_job_fields <- select_list_safe(job_field_display,
          multiple = TRUE,
          title = glue::glue("Select fields to edit for job '{job}':")
        )

        if (length(selected_job_fields) == 0) next

        scfg <- setup_job(scfg, job_name = job, fields = paste(job, names(selected_job_fields), sep = "/"))
      }
    } else {
      # Standard config section
      info <- config_map[[choice]]
      prefix <- info$prefix
      fields <- info$fields
      setup_fn <- info$setup_fn

      if (length(fields) == 0) {
        message(glue::glue("No individual fields listed for {choice}, opening full setup..."))
        scfg <- setup_fn(scfg)
        next
      }

      # Track enable state to detect FALSE -> TRUE transitions
      step_name <- sub("/$", "", prefix)
      old_enable <- isTRUE(scfg[[step_name]]$enable)

      field_display <- sapply(fields, function(fld) {
        val <- get_nested_values(scfg, paste0(prefix, fld))
        sprintf("%s [ %s ]", fld, show_val(val))
      })

      selected <- select_list_safe(field_display,
        multiple = TRUE,
        title = glue::glue("Select fields to edit in {choice}:")
      )

      if (length(selected) == 0) next

      scfg <- setup_fn(scfg, fields = paste0(prefix, names(selected)))

      # If enabling a previously disabled step, immediately validate and prompt for missing fields
      if (!step_name %in% c("metadata", "compute_environment")) {
        new_enable <- isTRUE(scfg[[step_name]]$enable)
        if (new_enable && !old_enable) scfg <- validate_after_enable(scfg, step_name, setup_fn)
      }
    }
  }

  scfg <- save_project_config(scfg)

  return(scfg)
}
