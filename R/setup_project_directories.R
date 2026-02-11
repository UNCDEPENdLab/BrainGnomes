#' Ensure required project directories exist
#'
#' Given a project configuration, create any missing directories referenced in
#' the metadata, including the project root and standard subdirectories. A
#' message is emitted for each directory created, and existing but unreadable
#' directories trigger a warning. After creation, every directory is verified
#' writable; for directories owned by the current user an attempt is made to
#' add the user-write bit via \code{ensure_user_writable}.
#'
#' @param scfg A project configuration object.
#' @param check_cache Optional environment used to memoize write-permission
#'   results. Directories verified writable here will be recorded so that
#'   downstream preflight checks (e.g. in \code{collect_submit_permission_issues})
#'   can skip redundant \code{file.access()} calls.
#' @return Invisibly returns \code{scfg}.
#' @keywords internal
#' @importFrom checkmate assert_class test_directory_exists
setup_project_directories <- function(scfg, check_cache = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  if (!is.null(check_cache) && !is.environment(check_cache)) {
    stop("check_cache must be an environment when provided.")
  }

  dirs <- c(
    scfg$metadata$project_directory,
    scfg$metadata$bids_directory,
    scfg$metadata$log_directory,
    scfg$metadata$rois_directory,
    scfg$metadata$postproc_directory,
    scfg$metadata$fmriprep_directory,
    scfg$metadata$mriqc_directory,
    scfg$metadata$scratch_directory,
    scfg$metadata$templateflow_home,
    scfg$metadata$flywheel_temp_directory,
    scfg$metadata$flywheel_sync_directory
  )

  dirs <- dirs[!is.na(dirs) & nzchar(dirs)]

  # --- create missing directories ---------------------------------------------
  for (d in dirs) {
    if (checkmate::test_directory_exists(d, access = "r")) {
      next
    } else if (dir.exists(d)) {
      warning("Directory exists but is not readable: ", d, immediate. = TRUE)
    } else {
      message("Creating directory: ", d)
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # --- verify writability on *all* directories ---------------------------------
  for (d in dirs) {
    if (!dir.exists(d)) next   # creation may have failed; preflight will catch it
    if (checkmate::test_directory_exists(d, access = "w")) next  # already writable

    # Attempt to remediate: ensure_user_writable adds the user-write bit when
    # the current user owns the directory.
    warning("Directory is not user-writable. Attempting to modify permissions: ", d, immediate. = TRUE)
    okay <- tryCatch(ensure_user_writable(d), error = function(e) FALSE)
    if (!isTRUE(okay)) {
      warning("Cannot make directory writable: ", d,
              ". Downstream jobs that write here will fail.", immediate. = TRUE)
    }
  }

  # --- prime the permission cache ----------------------------------------------
  if (!is.null(check_cache)) {
    for (d in dirs) {
      if (checkmate::test_directory_exists(d, access = "w")) {
        cache_key <- normalizePath(d, winslash = "/", mustWork = FALSE)
        assign(cache_key, TRUE, envir = check_cache)
      }
    }
  }

  invisible(scfg)
}
