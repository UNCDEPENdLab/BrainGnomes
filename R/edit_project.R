
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
#' @export
edit_project <- function(input) {
  if (inherits(input, "bg_project_cfg")) {
    scfg <- input
  } else if (checkmate::test_string(input)) {
    if (grepl("\\.ya?ml$", input, ignore.case = TRUE)) {
      if (!checkmate::test_file_exists(input)) {
        stop("Cannot find file: ", input)
      }
      scfg <- load_project(input, validate = FALSE)
    } else if (checkmate::test_directory_exists(input)) {
      cfg_file <- file.path(input, "project_config.yaml")
      if (!file.exists(cfg_file)) {
        stop("project_config.yaml not found in ", input)
      }
      scfg <- load_project(cfg_file, validate = FALSE)
    } else {
      stop("input must be a bg_project_cfg object, YAML file, or project directory")
    }
  } else {
    stop("input must be a bg_project_cfg object, YAML file, or project directory")
  }

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
      "heuristic_file", "overwrite", "clear_cache", "validate_bids"
    )),
    "BIDS Validation" = list(setup_fn = setup_bids_validation, prefix = "bids_validation/", fields = c(
      "outfile"
    )),
    "fMRIPrep" = list(setup_fn = setup_fmriprep, prefix = "fmriprep/", fields = c(
      "output_spaces", "fs_license_file"
    )),
    "MRIQC" = list(setup_fn = setup_mriqc, prefix = "mriqc/", fields = character(0)),
    "ICA-AROMA" = list(setup_fn = setup_aroma, prefix = "aroma/", fields = character(0)),
    "Postprocessing" = list(setup_fn = setup_postprocess, prefix = "postprocess/", fields = c(
      "input_regex", "bids_desc", "keep_intermediates", "overwrite",
      "tr", "brain_mask",
      "apply_mask/mask_file", "apply_mask/prefix",
      "spatial_smooth/fwhm_mm", "spatial_smooth/prefix",
      "apply_aroma/nonaggressive", "apply_aroma/prefix",
      "temporal_filter/low_pass_hz", "temporal_filter/high_pass_hz",
      "temporal_filter/prefix",
      "intensity_normalize/global_median", "intensity_normalize/prefix",
      "confound_calculate/columns", "confound_calculate/noproc_columns",
      "confound_calculate/demean",
      "confound_regression/columns", "confound_regression/noproc_columns",
      "confound_regression/prefix",
      "force_processing_order", "processing_steps"
    ))
  )

  job_targets <- c("bids_conversion", "bids_validation", "fmriprep", "mriqc", "aroma", "postprocess")
  job_fields <- c("memgb", "nhours", "ncores", "cli_options", "sched_args")

  show_val <- function(val) {
    if (is.null(val)) "[NULL]"
    else if (is.logical(val)) toupper(as.character(val))
    else if (is.character(val) && length(val) > 1) paste(val, collapse = ", ")
    else as.character(val)
  }

  # Top-level menu loop
  repeat {
    choice <- utils::menu(c(names(config_map), "Job settings", "Quit"),
                          title = "Select a configuration area to edit:")

    if (choice == 0 || choice > length(config_map) + 1) {
      message("Exiting configuration editor.")
      break
    }

    if (choice <= length(config_map)) {
      # Standard config section
      area <- names(config_map)[choice]
      info <- config_map[[area]]
      prefix <- info$prefix
      fields <- info$fields
      setup_fn <- info$setup_fn

      if (length(fields) == 0) {
        message(glue::glue("No individual fields listed for {area}, opening full setup..."))
        scfg <- setup_fn(scfg)
        next
      }

      field_display <- sapply(fields, function(fld) {
        val <- get_nested_values(scfg, paste0(prefix, fld))
        sprintf("%s [ %s ]", fld, show_val(val))
      })

      selected <- utils::select.list(field_display, multiple = TRUE,
                                     title = glue::glue("Select fields to edit in {area}:"))

      if (length(selected) == 0) next

      scfg <- setup_fn(scfg, fields = paste0(prefix, names(selected)))

    } else {
      # Job settings logic
      job <- utils::select.list(job_targets, title = "Select which job to configure:")
      if (length(job) == 0 || job == "") next

      job_field_display <- sapply(job_fields, function(fld) {
        val <- get_nested_values(scfg, paste0(job, "/", fld))
        sprintf("%s [ %s ]", fld, show_val(val))
      })

      selected_job_fields <- utils::select.list(job_field_display, multiple = TRUE,
                                                title = glue::glue("Select fields to edit for job '{job}':"))

      if (length(selected_job_fields) == 0) next

      scfg <- setup_job(scfg, job_name = job, fields = paste(job, names(selected_job_fields), sep = "/"))
    }
  }

  scfg <- save_project_config(scfg)

  return(scfg)
}



# edit_project <- function(scfg) {
#   assert_class(scfg, "bg_project_cfg")

#   config_areas <- list(
#     # top-level
#     General = list(
#       setup_fn = setup_project, # DONE
#       path = "",
#       fields = c(
#         "project_name", "project_directory", "dicom_directory", "bids_directory", 
#         "scratch_directory", "fmriprep_directory", "mriqc_directory", "log_directory", "templateflow_home"
#       )
#     ),
#     Job_Settings = list(
#       setup_fn = setup_job,
#       # job_name -- additional argument to pass into setup_job
#       path="depends",
#       fields = c("memgb", "nhours", "ncores", "cli_options", "sched_args")
#     ),
#     BIDS_conversion = list( # DONE
#       setup_fn = setup_bids_conversion,
#       path = "heudiconv",
#       fields = c(
#         "heuristic_file", "sub_regex", "sub_id_match", "ses_regex", "ses_id_match",
#         "overwrite", "clear_cache", "validate_bids"
#       )
#     ),
#     BIDS_validation = list(
#       setup_fn = setup_bids_validation,
#       path = "bids_validation",
#       fields = c("outfile")
#     ),
#     fMRIPrep = list(
#       setup_fn = setup_fmriprep,
#       path = "fmriprep",
#       fields = c("output_spaces", "fs_license_file")
#     ),
#     MRIQC = list(
#       setup_fn = setup_mriqc,
#       path = "mriqc",
#       fields = c("run_group_level")
#     ),
#     # AROMA = list(
#     #   setup_fn = setup_aroma,
#     #   path = "aroma",
#     #   fields = c("use_aroma")
#     # ),
#     Postprocessing = list(
#       setup_fn = setup_postprocess,
#       path = "postprocess",
#       fields = c("apply_mask", "brain_mask", "keep_intermediates", "force_processing_order",
#       "input_regex", "spatial_smooth/fwhm_mm", "apply_aroma/nonaggressive",
#       "temporal_filter/low_pass_hz", "temporal_filter/high_pass_hz", "intensity_normalize",
#       "confound_calculate", "confound_regress")
#     ),
#     Compute = list( # DONE
#       setup_fn = setup_compute_environment,
#       path = "compute_environment",
#       fields = c(
#         "scheduler", "fmriprep_container", "heudiconv_container", 
#         "bids_validator", "mriqc_container", "aroma_container"
#       )
#     )
#   )

#   top_options <- c(names(config_areas), "Quit")

#   repeat {
#     cat("\nWhich configuration area would you like to edit?\n")
#     area_idx <- utils::menu(sub("_", " ", top_options), title = "Select a configuration domain:")

#     if (area_idx == 0 || area_idx > length(config_areas)) {
#       message("Exiting configuration editor.")
#       break
#     }

#     area_name <- names(config_areas)[area_idx]
#     area_info <- config_areas[[area_name]]
#     full_path <- area_info$path
#     field_names <- area_info$fields

#     # Retrieve current values to show in the menu
#     get_current_value <- function(field) {
#       path_parts <- if (full_path != "") c(full_path, field) else field
#       val <- tryCatch(
#         purrr::pluck(scfg, !!!path_parts),
#         error = function(e) NULL
#       )
#       if (is.null(val)) return("[NULL]")
#       if (is.logical(val)) return(ifelse(val, "TRUE", "FALSE"))
#       if (is.character(val) && length(val) > 1) return(paste0(val, collapse = ", "))
#       as.character(val)
#     }

#     field_display <- purrr::map_chr(field_names, function(f) {
#       val <- get_current_value(f)
#       sprintf("%s [%s]", f, val)
#     })

#     selected <- utils::select.list(
#       choices = field_display,
#       multiple = TRUE,
#       title = glue::glue("Select fields to modify in {area_name}:")
#     )

#     if (length(selected) == 0) {
#       message("No fields selected. Returning to main menu.")
#       next
#     }

#     # Strip brackets to recover true field names
#     selected_fields <- sub(" \\[.*\\]$", "", selected)

#     passthrough <- area_info[setdiff(c("setup_fn", "path", "fields"), names(area_info))]

#     # Run setup function on those fields
#     browser()
#     scfg <- do.call(area_info$setup_fn, list(scfg = scfg, fields = selected_fields, passthrough))
#   }

#   return(scfg)
# }

