#' Parse completion time from a .complete file
#'
#' @param file Path to .complete file.
#' @return POSIXct time or NA if parsing fails.
#' @keywords internal
#' @importFrom lubridate parse_date_time
parse_complete_time <- function(file) {
  if (!file.exists(file)) return(as.POSIXct(NA))
  tm <- tryCatch(readLines(file, n = 1L, warn = FALSE), error = function(e) NULL)
  if (is.null(tm) || length(tm) == 0L) {
    return(as.POSIXct(NA))
  }
  parsed <- suppressWarnings(lubridate::parse_date_time(tm[1L], orders = c("y-m-d H:M:S", "mdy@HM", "mdy@HMS", "ymd HMS", "ymd HM", "mdy HM", "mdy HMS")))
  if (length(parsed) == 0L || is.na(parsed[1L])) return(as.POSIXct(NA))
  as.POSIXct(parsed)
}

#' Get processing status for a single subject
#'
#' @param scfg Study configuration list.
#' @param sub_id Subject identifier.
#' @param ses_id Optional session identifier. When `NULL`, all sessions found in the
#'   subject's directory are returned.
#' @return A data.frame with columns indicating completion status and times for each enabled step.
#' @export
#' @importFrom checkmate assert_class assert_string
get_subject_status <- function(scfg, sub_id, ses_id = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_string(sub_id)
  checkmate::assert_string(ses_id, null.ok = TRUE)

  steps <- c()
  if (isTRUE(scfg$bids_conversion$enable)) steps <- c(steps, "bids_conversion")
  if (isTRUE(scfg$mriqc$enable)) steps <- c(steps, "mriqc")
  if (isTRUE(scfg$fmriprep$enable)) steps <- c(steps, "fmriprep")
  if (isTRUE(scfg$aroma$enable)) steps <- c(steps, "aroma")
  if (isTRUE(scfg$postprocess$enable)) steps <- c(steps, "postprocess")

  pp_streams <- if ("postprocess" %in% steps) get_postprocess_stream_names(scfg) else character(0)

  log_dir <- scfg$metadata$log_directory
  sub_log_dir <- file.path(log_dir, paste0("sub-", sub_id))
  comp_files <- list.files(sub_log_dir, pattern = "_complete$", full.names = FALSE)
  ses_ids <- if (!is.null(ses_id)) {
    ses_id
  } else {
    # sessions referenced by .complete files
    comp_ses <- unique(sub("^.*_ses-([^_]+)_complete$", "\\1", comp_files[grepl("_ses-", comp_files)]))
    comp_ses <- comp_ses[!is.na(comp_ses) & comp_ses != "^.*_ses-([^_]+)_complete$"]
    # sessions present as directories in BIDS layout
    bids_sub_dir <- file.path(scfg$metadata$bids_directory, paste0("sub-", sub_id))
    bids_ses_dirs <- if (dir.exists(bids_sub_dir)) {
      list.dirs(bids_sub_dir, recursive = FALSE, full.names = FALSE)
    } else character(0)
    bids_ses <- sub("^ses-", "", bids_ses_dirs[grepl("^ses-", bids_ses_dirs)])
    sids <- union(comp_ses, bids_ses)
    if (length(sids) == 0) NA_character_ else sids
  }

  res <- lapply(ses_ids, function(ss) {
    row <- list(sub_id = sub_id, ses_id = ifelse(is.na(ss), NA_character_, ss))
    for (st in steps) {
      if (st != "postprocess") {
        chk <- is_step_complete(scfg, sub_id,
          ses_id = if (st %in% c("bids_conversion") && !is.na(ss)) ss else NULL,
          step_name = st
        )
        row[[paste0(st, "_complete")]] <- chk$complete
        row[[paste0(st, "_time")]] <- if (chk$complete) parse_complete_time(chk$complete_file) else as.POSIXct(NA)
      } else {
        for (stream in pp_streams) {
          chk <- is_step_complete(scfg, sub_id, ses_id = if (!is.na(ss)) ss else NULL, step_name = "postprocess", pp_stream = stream)
          row[[paste0(stream, "_complete")]] <- chk$complete
          row[[paste0(stream, "_time")]] <- if (chk$complete) parse_complete_time(chk$complete_file) else as.POSIXct(NA)
        }
      }
    }
    as.data.frame(row) # needed to preserve posixct objects in rbind
  })

  df <- do.call(rbind.data.frame, res)
  class(df) <- c("bg_status_df", class(df))
  df
}

#' Get processing status for all subjects
#'
#' @param scfg Study configuration list.
#' @return data.frame with one row per subject/session containing completion status columns.
#' @export
#' @importFrom checkmate assert_class
get_project_status <- function(scfg) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  log_dir <- scfg$metadata$log_directory
  sub_dirs <- list.dirs(log_dir, recursive = FALSE, full.names = FALSE)
  sub_ids <- sub("^sub-", "", sub_dirs[grepl("^sub-", sub_dirs)])
  res <- lapply(sub_ids, function(id) {
    bids_sub_dir <- file.path(scfg$metadata$bids_directory, paste0("sub-", id))
    ses_dirs <- if (dir.exists(bids_sub_dir)) {
      list.dirs(bids_sub_dir, recursive = FALSE, full.names = FALSE)
    } else {
      character(0)
    }
    ses_ids <- sub("^ses-", "", ses_dirs[grepl("^ses-", ses_dirs)])
    if (length(ses_ids) == 0) {
      get_subject_status(scfg, id)
    } else {
      do.call(rbind, lapply(ses_ids, function(ss) get_subject_status(scfg, id, ss)))
    }
  })
  df <- do.call(rbind.data.frame, res)
  class(df) <- c("bg_status_df", class(df))
  df
}

#' Summarize project status
#'
#' @param object A data.frame produced by `get_project_status()`.
#' @param ... Additional arguments (unused)
#' @description Provides a tabular summary of completion counts for each step in the pipeline.
#' @return data.frame summarizing number of subjects completed for each step.
#' @export
summary.bg_status_df <- function(object, ...) {
  step_cols <- grep("_complete$", names(object), value = TRUE)
  counts <- vapply(step_cols, function(x) sum(object[[x]], na.rm = TRUE), numeric(1))
  data.frame(step = step_cols, n_complete = counts, row.names = NULL, stringsAsFactors = FALSE)
}

