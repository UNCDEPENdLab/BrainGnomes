#' Ensure required project directories exist
#'
#' Given a project configuration, create any missing directories referenced in
#' the metadata, including the project root and standard subdirectories. A
#' message is emitted for each directory created, and existing but unreadable
#' directories trigger a warning.
#'
#' @param scfg A project configuration object.
#' @return Invisibly returns `scfg`.
#' @keywords internal
#' @importFrom checkmate assert_class test_directory_exists
setup_project_directories <- function(scfg) {
  checkmate::assert_class(scfg, "bg_project_cfg")

  dirs <- c(
    scfg$metadata$project_directory,
    scfg$metadata$bids_directory,
    scfg$metadata$log_directory,
    scfg$metadata$rois_directory,
    scfg$metadata$postproc_directory,
    scfg$metadata$fmriprep_directory,
    scfg$metadata$mriqc_directory,
    scfg$metadata$scratch_directory,
    scfg$metadata$templateflow_home
  )

  dirs <- dirs[!is.na(dirs) & nzchar(dirs)]

  for (d in dirs) {
    if (checkmate::test_directory_exists(d)) {
      next
    } else if (dir.exists(d)) {
      warning("Directory exists but is not readable: ", d)
    } else {
      message("Creating directory: ", d)
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # check that scratch directory is user-writeable
  scratch_dir <- scfg$metadata$scratch_directory
  if (!checkmate::test_directory_exists(scratch_dir, access = "w")) {
    warning("Scratch directory is not user-writeable. Attempting to modify permissions: ", scratch_dir)
    okay <- ensure_user_writable(scratch_dir)
    if (!okay) stop("Cannot write to scratch directory. BrainGnomes cannot proceed. Fix permissions on: ", scratch_dir)
  }

  invisible(scfg)
}
