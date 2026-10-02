#' Expand requested work into read-only, concrete preview rows
#' @param scfg Project configuration.
#' @param execution Resolved selection and discovered scope, including exact retries.
#' @param jobs Stage/stream resource and dependency summary.
#' @return A list containing work rows, counts, locations, and explicit limitations.
#' @noRd
build_project_preview <- function(scfg, execution, jobs) {
  work <- data.frame(
    stage = character(), stream = character(), sub_id = character(), ses_id = character(),
    action = character(), reason = character(), input_directory = character(),
    output_root = character(), log_directory = character(), completion_marker = character(),
    depends_on = character(), ncores = numeric(), memgb = numeric(), nhours = numeric(),
    stringsAsFactors = FALSE
  )
  roots <- scfg$metadata
  # Join a configured root with optional subject/session IDs, returning a path
  # or NA for an unspecified root. Missing settings must not turn into
  # plausible relative paths such as "sub-01" under the current directory.
  directory <- function(root, sub_id = NA_character_, ses_id = NA_character_) {
    if (!checkmate::test_string(root, min.chars = 1L)) return(NA_character_)
    if (!is.na(sub_id)) root <- file.path(root, paste0("sub-", sub_id))
    if (!is.na(ses_id)) root <- file.path(root, paste0("ses-", ses_id))
    root
  }
  for (i in seq_len(nrow(jobs))) {
    job <- jobs[i, ]
    stage <- job$stage
    stream <- job$stream
    project_scope <- job$scope == "project"
    deferred <- execution$scope_deferred && !project_scope
    if (project_scope) {
      units <- data.frame(sub_id = NA_character_, ses_id = NA_character_)
    } else if (!is.null(execution$work_units)) {
      selected <- execution$work_units$stage == stage
      if (!is.na(stream)) selected <- selected & !is.na(execution$work_units$stream) &
        execution$work_units$stream == stream
      units <- execution$work_units[selected, c("sub_id", "ses_id"), drop = FALSE]
    } else if (deferred) {
      # Existing local subjects are only a partial snapshot before sync. Do not
      # mistake them for the future scope, or show an empty download as no work.
      units <- data.frame(sub_id = NA_character_, ses_id = NA_character_)
    } else {
      units <- execution$subjects[, c("sub_id", "ses_id"), drop = FALSE]
      if (job$scope == "subject") units$ses_id <- NA_character_
      units <- unique(units)
    }
    input_root <- switch(stage,
      flywheel_sync = scfg$flywheel_sync$source_url,
      bids_conversion = roots$dicom_directory,
      mriqc = roots$bids_directory, fmriprep = roots$bids_directory,
      aroma = roots$fmriprep_directory, postprocess = roots$fmriprep_directory,
      extract_rois = roots$postproc_directory, NULL
    )
    output_root <- switch(stage,
      flywheel_sync = roots$flywheel_sync_directory,
      bids_conversion = roots$bids_directory,
      mriqc = roots$mriqc_directory,
      fmriprep = roots$fmriprep_directory, aroma = roots$fmriprep_directory,
      postprocess = roots$postproc_directory, extract_rois = roots$rois_directory,
      fsaverage_setup = roots$fmriprep_directory,
      prefetch_templates = roots$templateflow_home, NULL
    )
    for (j in seq_len(nrow(units))) {
      sub_id <- as.character(units$sub_id[j])
      ses_id <- as.character(units$ses_id[j])
      input <- directory(input_root, sub_id, ses_id)
      if (stage == "bids_conversion" && !deferred && !is.na(sub_id)) {
        subjects <- execution$subjects
        session_matches <- if (is.na(ses_id)) is.na(subjects$ses_id) else
          !is.na(subjects$ses_id) & subjects$ses_id == ses_id
        selected <- subjects$sub_id == sub_id & session_matches
        field <- if (is.na(ses_id)) "dicom_sub_dir" else "dicom_ses_dir"
        paths <- subjects[[field]][selected]
        input <- if (length(paths)) as.character(paths[1L]) else NA_character_
      }
      log_directory <- directory(roots$log_directory, sub_id)
      marker <- NA_character_
      if (!project_scope && !is.na(sub_id) && !is.na(log_directory)) {
        tag <- pipeline_step_name_tag(stage,
          pp_stream = if (stage == "postprocess") stream else NULL,
          ex_stream = if (stage == "extract_rois") stream else NULL)
        unit <- paste0("_sub-", sub_id)
        if (!is.na(ses_id)) unit <- paste0(unit, "_ses-", ses_id)
        marker <- file.path(log_directory, paste0(".", tag, unit, "_complete"))
      }
      completed <- !is.na(marker) && checkmate::test_file_exists(marker)
      action <- if (deferred) "deferred" else if (project_scope) "check_at_submission" else
        if (completed && !execution$force) "would_skip" else "would_submit"
      reason <- if (deferred) "Subject scope and completion checks wait for Flywheel sync." else
        if (project_scope) "Project setup/sync; reuse and runtime prerequisites checked at submission." else
        if (completed && !execution$force) "Completion marker exists and force is FALSE." else
        if (execution$force) "Force is TRUE; existing completion markers will not skip this unit." else
          "No completion marker; subject to input and preflight checks at submission."
      work[nrow(work) + 1L, ] <- list(
        stage, stream, sub_id, ses_id, action, reason, input,
        directory(output_root), log_directory, marker, job$depends_on,
        job$ncores, job$memgb, job$nhours
      )
    }
  }
  list(
    scheduler = value_or_default(scfg$compute_environment$scheduler, NA_character_),
    counts = list(
      would_submit = sum(work$action == "would_submit"),
      would_skip = sum(work$action == "would_skip"),
      project_checks = sum(work$action == "check_at_submission"),
      deferred = sum(work$action == "deferred")
    ),
    work = work,
    limitations = c(
      "Counts describe requested work units, not an exact scheduler job list. Postprocessing controllers may create image-array and sentinel jobs.",
      "Completion-marker decisions are a read-only snapshot; inputs, permissions, cached setup work, and scheduler state are checked again at submission.",
      "Resources are requested per job, not estimates of runtime or cost. Deferred rows are unknown scope, not zero jobs."
    )
  )
}

#' Print a bounded concrete preview while retaining the full structured result
#' @param preview Read-only preview produced by build_project_preview().
#' @param limit Maximum number of work rows printed to the console.
#' @return NULL, invisibly.
#' @noRd
print_project_preview <- function(preview, limit = 20L) {
  cat("\nConcrete work preview (scheduler: ", preview$scheduler, ")\n", sep = "")
  counts <- preview$counts
  cat("Would submit: ", counts$would_submit, "; would skip: ", counts$would_skip,
    "; project checks: ", counts$project_checks, "; deferred rows: ", counts$deferred, ".\n", sep = "")
  work <- preview$work
  if (nrow(work)) {
    shown <- utils::head(work, limit)
    print(shown[, c("stage", "stream", "sub_id", "ses_id", "action"), drop = FALSE], row.names = FALSE)
    for (i in seq_len(nrow(shown))) {
      row <- shown[i, ]
      cat("\n  ", row$stage,
        if (!is.na(row$stream)) paste0(" [", row$stream, "]") else "",
        if (!is.na(row$sub_id)) paste0(" sub-", row$sub_id) else "",
        if (!is.na(row$ses_id)) paste0(" ses-", row$ses_id) else "", ":\n", sep = "")
      cat("    Input: ", row$input_directory, "\n    Output root: ", row$output_root,
        "\n    Logs: ", row$log_directory, "\n    ", row$reason, "\n", sep = "")
    }
    if (nrow(work) > limit) {
      cat("... ", nrow(work) - limit, " more rows; inspect plan$preview$work or use --format=json for the complete preview.\n", sep = "")
    }
  }
  cat(paste0("Note: ", unlist(preview$limitations), "\n"), sep = "")
  cat("Preview only: no jobs submitted or project directories created.\n")
  invisible(NULL)
}
