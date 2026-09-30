#' Normalize an exact retry scope, including scopes read from saved YAML plans
#'
#' @param units Data frame or column list with stage, stream, sub_id, and ses_id.
#' @return A unique, validated work-unit data frame, or NULL for an ordinary run.
#' @noRd
normalize_retry_work_units <- function(units) {
  if (is.null(units)) return(NULL)
  units <- as.data.frame(units, stringsAsFactors = FALSE)
  fields <- c("stage", "stream", "sub_id", "ses_id")
  checkmate::assert_names(names(units), must.include = fields)
  units <- units[, fields, drop = FALSE]
  if (!nrow(units)) stop("An exact retry scope must not be empty.", call. = FALSE)
  for (field in fields) {
    units[[field]] <- as.character(units[[field]])
    units[[field]][!is.na(units[[field]]) & !nzchar(units[[field]])] <- NA_character_
  }
  if (any(is.na(units$stage) | !units$stage %in% supported_project_steps())) {
    stop("Unsupported stage in exact retry scope.", call. = FALSE)
  }
  streamed <- units$stage %in% c("postprocess", "extract_rois")
  if (any(is.na(units$sub_id) & units$stage != "flywheel_sync") ||
      any(is.na(units$stream) & streamed)) {
    stop("Cannot determine exact retry scope: a failed job lacks a subject or stream. Inspect its tracking records before retrying.", call. = FALSE)
  }
  units$stream[!streamed] <- NA_character_
  units$ses_id[!units$stage %in% c("bids_conversion", "postprocess", "extract_rois")] <- NA_character_
  units$sub_id[units$stage == "flywheel_sync"] <- NA_character_
  units <- unique(units)
  rownames(units) <- NULL
  units
}

#' Test whether an exact retry includes a subject/session/stage/stream
#'
#' @param units Normalized retry work units, or NULL for an unrestricted run.
#' @param stage Processing stage.
#' @param sub_id Subject ID.
#' @param ses_id Session ID; NA denotes a sessionless input, not all sessions.
#' @param stream Stream name for postprocessing or extraction.
#' @return Logical scalar indicating whether the work is requested.
#' @noRd
retry_includes_unit <- function(units, stage, sub_id, ses_id = NA_character_, stream = NA_character_) {
  if (is.null(units)) return(TRUE)
  rows <- units$stage == stage & !is.na(units$sub_id) & units$sub_id == sub_id
  if (stage %in% c("postprocess", "extract_rois")) {
    rows <- rows & !is.na(units$stream) & units$stream == stream
  }
  if (stage %in% c("bids_conversion", "postprocess", "extract_rois")) {
    rows <- rows & if (is.na(ses_id)) is.na(units$ses_id) else
      !is.na(units$ses_id) & units$ses_id == ses_id
  }
  any(rows, na.rm = TRUE)
}

#' Restrict discovered subject rows to exact retry units
#'
#' @param subjects Discovered subject/session table.
#' @param units Normalized retry units, or NULL.
#' @param allow_missing Permit unresolved inputs before Flywheel synchronization.
#' @return Subject table restricted to requested work; absent units fail closed.
#' @noRd
retry_subject_scope <- function(subjects, units, allow_missing = FALSE) {
  if (is.null(units)) return(subjects)
  keep <- rep(FALSE, nrow(subjects))
  for (i in which(units$stage != "flywheel_sync")) {
    unit <- units[i, ]
    rows <- !is.na(subjects$sub_id) & subjects$sub_id == unit$sub_id
    if (unit$stage %in% c("bids_conversion", "postprocess", "extract_rois")) {
      rows <- rows & if (is.na(unit$ses_id)) is.na(subjects$ses_id) else
        !is.na(subjects$ses_id) & subjects$ses_id == unit$ses_id
    }
    if (!any(rows) && !allow_missing) {
      stop("No inputs found for retry unit: ", paste(unit, collapse = "/"),
        ". The retry scope has not been expanded.", call. = FALSE)
    }
    keep <- keep | rows
  }
  subjects[keep, , drop = FALSE]
}
