
#' Interactively edit a study configuration by field (field-guided)
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
  
  # Define editable fields per setup function
  config_map <- list(
    "General" = list(setup_fn = setup_project_metadata, prefix = "metadata/", fields = c(
      "project_name", "project_directory", "dicom_directory", "bids_directory", "scratch_directory", "templateflow_home"
    )),
    "Compute Environment" = list(setup_fn = setup_compute_environment, prefix = "compute_environment/", fields = c(
      "scheduler", "fmriprep_container", "heudiconv_container", "bids_validator", "mriqc_container", "aroma_container"
    )),
    "BIDS Conversion" = list(setup_fn = setup_bids_conversion, prefix = "bids_conversion/", fields = c(
      "sub_regex", "sub_id_match", "ses_regex", "ses_id_match",
      "heuristic_file", "overwrite", "clear_cache"
    )),
    "BIDS Validation" = list(setup_fn = setup_bids_validation, prefix = "bids_validation/", fields = c(
      "outfile"
    )),
    "fMRIPrep" = list(setup_fn = setup_fmriprep, prefix = "fmriprep/", fields = c(
      "output_spaces", "fs_license_file"
    ))
    # At present, MRIQC and ICA-AROMA don't have any additional specific settings, just job settings
    # "MRIQC" = list(setup_fn = setup_mriqc, prefix = "mriqc/", fields = character(0)),
    # "ICA-AROMA" = list(setup_fn = setup_aroma, prefix = "aroma/", fields = character(0))
  )

  job_targets <- c("bids_conversion", "bids_validation", "fmriprep", "mriqc", "aroma", "postprocess")
  job_fields <- c("memgb", "nhours", "ncores", "cli_options", "sched_args")

  show_val <- function(val) {
    if (is.null(val)) "NULL"
    else if (length(val) == 0L || all(is.na(val))) "None"
    else if (is.logical(val)) toupper(as.character(val))
    else if (is.character(val) && length(val) > 1) paste(val, collapse = ", ")
    else as.character(val)
  }

  # Top-level menu loop
  repeat {
    choice <- select.list(c(names(config_map), "Postprocessing", "Job settings", "Quit"), graphics = FALSE,
                          title = "Select a configuration area to edit:")

    if (choice == "Quit" || choice == "") {
      message("Exiting configuration editor.")
      break
    }

    if (choice == "Postprocessing") {
      scfg <- manage_postprocess_streams(scfg, allow_empty = TRUE)
    } else if (choice == "Job settings") {
      # Job settings logic
      job <- utils::select.list(job_targets, title = "Select which job to configure:")
      if (length(job) == 0 || job == "") next

      if (job == "postprocess") {
        streams <- get_postprocess_stream_names(scfg)
        if (length(streams) == 0) {
          message("No postprocess streams defined.")
          next
        }

        stream <- if (length(streams) == 1L) streams else utils::select.list(streams, title = "Select postprocess stream:")
        if (length(stream) == 0 || stream == "") next

        job_field_display <- sapply(job_fields, function(fld) {
          val <- get_nested_values(scfg, paste0("postprocess/", stream, "/", fld))
          sprintf("%s [ %s ]", fld, show_val(val))
        })

        selected_job_fields <- utils::select.list(job_field_display,
          multiple = TRUE,
          title = glue::glue("Select fields to edit for postprocess stream '{stream}':")
        )

        if (length(selected_job_fields) == 0) next

        scfg <- setup_postprocess_stream(
          scfg,
          stream_name = stream,
          fields = paste0("postprocess/", stream, "/", names(selected_job_fields))
        )
      } else {
        job_field_display <- sapply(job_fields, function(fld) {
          val <- get_nested_values(scfg, paste0(job, "/", fld))
          sprintf("%s [ %s ]", fld, show_val(val))
        })

        selected_job_fields <- utils::select.list(job_field_display,
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

      field_display <- sapply(fields, function(fld) {
        val <- get_nested_values(scfg, paste0(prefix, fld))
        sprintf("%s [ %s ]", fld, show_val(val))
      })

      selected <- utils::select.list(field_display,
        multiple = TRUE,
        title = glue::glue("Select fields to edit in {choice}:")
      )

      if (length(selected) == 0) next

      scfg <- setup_fn(scfg, fields = paste0(prefix, names(selected)))
    }
  }

  scfg <- save_project_config(scfg)

  return(scfg)
}
