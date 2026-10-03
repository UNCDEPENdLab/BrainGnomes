#' Restrict configuration checks to a resolved stage and stream selection
#' @param scfg Original project configuration, which is never modified.
#' @param selection Resolved bg_project_selection object.
#' @return A validation-only copy with unrelated stages and streams excluded.
#' @noRd
project_validation_scope <- function(scfg, selection) {
  checkmate::assert_class(selection, "bg_project_selection")
  for (stage in c(supported_project_steps(), "bids_validation")) {
    if (!stage %in% selection$steps) scfg[[stage]] <- list(enable = FALSE)
  }
  if ("postprocess" %in% selection$steps) {
    scfg$postprocess <- scfg$postprocess[c("enable", selection$postprocess_streams)]
  }
  if ("extract_rois" %in% selection$steps) {
    scfg$extract_rois <- scfg$extract_rois[c("enable", selection$extract_streams)]
  }
  scfg
}

#' Check configured directories without creating future workflow outputs
#' @param scfg Configuration being validated.
#' @param field Slash-separated metadata field.
#' @param selection NULL for full configuration inspection, or resolved work.
#' @return Whether the directory exists or is a configured future destination.
#' @noRd
project_validation_directory <- function(scfg, field, selection = NULL) {
  path <- get_nested_values(scfg, field)
  if (checkmate::test_directory_exists(path)) return(TRUE)
  if (is.null(selection) || !checkmate::test_string(path, min.chars = 1L) || file.exists(path)) return(FALSE)

  # These directories are made by project setup or a selected upstream job.
  # Missing input directories remain errors unless that producer is selected.
  future <- c("log_directory", "scratch_directory", "templateflow_home")
  producers <- list(
    flywheel_sync = c("flywheel_sync_directory", "flywheel_temp_directory", "dicom_directory"),
    bids_conversion = "bids_directory", mriqc = "mriqc_directory",
    fmriprep = "fmriprep_directory", postprocess = "postproc_directory",
    extract_rois = "rois_directory"
  )
  future <- c(future, unlist(producers[intersect(names(producers), selection$steps)], use.names = FALSE))
  field %in% paste0("metadata/", future)
}

#' Stop a plan or run with the shared, field-specific validation report
#' @param validation A bg_project_validation report.
#' @return The report invisibly if valid; otherwise signals an error.
#' @noRd
require_valid_project_selection <- function(validation) {
  if (!isTRUE(validation$valid)) {
    stop("Project configuration is invalid for the selected work:\n",
      paste0("- ", validation$issues$message, collapse = "\n"),
      "\nCorrect these settings before planning or running this selection.", call. = FALSE)
  }
  invisible(validation)
}
