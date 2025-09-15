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
    scfg$metadata$templateflow_home,
    scfg$metadata$flywheel_temp_directory,
    scfg$metadata$flywheel_sync_directory
  )

  dirs <- dirs[!is.na(dirs) & nzchar(dirs)]

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

  # check that scratch directory is user-writable
  scratch_dir <- scfg$metadata$scratch_directory
  if (!checkmate::test_directory_exists(scratch_dir, access = "w")) {
    warning("Work/scratch directory is not user-writable. Attempting to modify permissions: ", scratch_dir, immediate. = TRUE)
    okay <- ensure_user_writable(scratch_dir)
    if (!okay) stop("Cannot write to scratch directory. BrainGnomes cannot proceed. Fix permissions on: ", scratch_dir)
  }

  # if flywheel_temp_directory is defined, make sure it is user-writable
  flywheel_temp_dir <- scfg$flywheel_sync$flywheel_temp_directory
  if (!is.null(flywheel_temp_dir)) {
    warning("Flywheel temp directory is not user-writable. Attempting to modify permissions: ", flywheel_temp_dir, immediate. = TRUE)
    okay <- ensure_user_writable(flywheel_temp_dir)
    if (!okay) stop("Cannot write to flywheel_temp_directory directory. BrainGnomes cannot proceed. Fix permissions on: ", flywheel_temp_dir)
  }

  invisible(scfg)
}
