# Project configuration path resolution
#
# Project configurations are stored in the project root. The YAML location
# bootstraps that root, while every other relative filesystem value is resolved
# against the root. Runtime configurations therefore contain absolute paths and
# never depend on the process or scheduler working directory.

#' Test whether a filesystem path is absolute
#'
#' @param path Character path.
#' @return A logical scalar.
#' @noRd
is_absolute_project_path <- function(path) {
  checkmate::test_string(path) && grepl(
    "^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path
  )
}

#' Normalize a project path through its nearest existing ancestor
#'
#' @param path Absolute or relative character path.
#' @return An absolute path with platform aliases and parent components
#'   resolved without requiring the final target to exist.
#' @noRd
normalize_project_path <- function(path) {
  checkmate::assert_string(path)
  path <- path.expand(path)
  if (!is_absolute_project_path(path)) {
    path <- file.path(getwd(), path)
  }

  ancestor <- path
  suffix <- character()
  while (!file.exists(ancestor)) {
    parent <- dirname(ancestor)
    if (identical(parent, ancestor)) break
    suffix <- c(basename(ancestor), suffix)
    ancestor <- parent
  }

  if (file.exists(ancestor)) {
    resolved <- normalizePath(ancestor, winslash = "/", mustWork = TRUE)
    for (component in suffix) {
      if (component %in% c("", ".")) next
      resolved <- if (component == "..") {
        dirname(resolved)
      } else if (endsWith(resolved, "/")) {
        paste0(resolved, component)
      } else {
        file.path(resolved, component)
      }
    }
    return(resolved)
  }

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Resolve one configured filesystem value
#'
#' @param path Character path.
#' @param base Absolute directory used for relative values.
#' @return The normalized absolute path, or the original non-path sentinel.
#' @noRd
resolve_config_path_value <- function(path, base) {
  if (!checkmate::test_string(path) || is.na(path) || !nzchar(trimws(path))) {
    return(path)
  }
  if (tolower(path) %in% c(".na", ".na.character")) return(path)
  expanded <- path.expand(path)
  if (!is_absolute_project_path(expanded)) expanded <- file.path(base, expanded)
  normalize_project_path(expanded)
}

#' Apply a function to every explicit project filesystem field
#'
#' This registry is intentionally explicit. Regexes, scheduler arguments,
#' command-line options, URLs, and special tokens must not be mistaken for
#' filesystem paths merely because of their names or values.
#'
#' @param scfg Project configuration list.
#' @param transform Function accepting a path and returning its replacement.
#' @return The transformed project configuration.
#' @noRd
map_project_config_paths <- function(scfg, transform) {
  metadata_fields <- c(
    "dicom_directory", "bids_directory", "fmriprep_directory",
    "mriqc_directory", "postproc_directory", "rois_directory",
    "log_directory", "scratch_directory", "templateflow_home",
    "flywheel_temp_directory", "flywheel_sync_directory", "sqlite_db"
  )
  for (field in metadata_fields) {
    value <- scfg$metadata[[field]]
    if (checkmate::test_string(value)) scfg$metadata[[field]] <- transform(value)
  }

  compute_fields <- c(
    "flywheel", "heudiconv_container", "bids_validator",
    "mriqc_container", "fmriprep_container", "aroma_container",
    "fsl_container"
  )
  for (field in compute_fields) {
    value <- scfg$compute_environment[[field]]
    if (checkmate::test_string(value)) {
      scfg$compute_environment[[field]] <- transform(value)
    }
  }

  if (checkmate::test_string(scfg$bids_conversion$heuristic_file)) {
    scfg$bids_conversion$heuristic_file <- transform(
      scfg$bids_conversion$heuristic_file
    )
  }
  if (checkmate::test_string(scfg$fmriprep$fs_license_file)) {
    scfg$fmriprep$fs_license_file <- transform(scfg$fmriprep$fs_license_file)
  }

  postprocess_streams <- setdiff(names(scfg$postprocess), "enable")
  for (stream in postprocess_streams) {
    if (!is.list(scfg$postprocess[[stream]])) next
    mask <- scfg$postprocess[[stream]]$apply_mask$mask_file
    if (checkmate::test_string(mask) && !identical(mask, "template")) {
      scfg$postprocess[[stream]]$apply_mask$mask_file <- transform(mask)
    }
  }

  extract_streams <- setdiff(names(scfg$extract_rois), "enable")
  for (stream in extract_streams) {
    if (!is.list(scfg$extract_rois[[stream]])) next
    atlases <- scfg$extract_rois[[stream]]$atlases
    if (checkmate::test_character(atlases, any.missing = FALSE)) {
      scfg$extract_rois[[stream]]$atlases <- vapply(
        atlases, transform, character(1), USE.NAMES = FALSE
      )
    }
    mask <- scfg$extract_rois[[stream]]$mask_file
    if (checkmate::test_string(mask)) {
      scfg$extract_rois[[stream]]$mask_file <- transform(mask)
    }
  }

  scfg
}

#' Resolve project configuration paths for runtime use
#'
#' @param scfg Project configuration list.
#' @param yaml_file Optional source YAML path. When supplied, its directory is
#'   the only valid project root and is used to resolve a relative
#'   `metadata/project_directory`.
#' @param base_directory Base for an in-memory relative project root when no
#'   YAML source exists. Defaults to the current directory and is captured once.
#' @return A configuration whose known filesystem values are absolute.
#' @noRd
resolve_project_paths <- function(scfg, yaml_file = attr(scfg, "yaml_file", exact = TRUE),
                                  base_directory = getwd()) {
  if (!is.list(scfg)) return(scfg)
  if (is.null(scfg$metadata)) scfg$metadata <- list()

  yaml_path <- NULL
  if (checkmate::test_string(yaml_file)) {
    yaml_path <- normalize_project_path(yaml_file)
    config_directory <- dirname(yaml_path)
  } else {
    config_directory <- normalize_project_path(base_directory)
  }

  declared_root <- scfg$metadata$project_directory
  if (!checkmate::test_string(declared_root)) declared_root <- "."
  project_root <- resolve_config_path_value(declared_root, config_directory)
  scfg$metadata$project_directory <- project_root
  scfg <- map_project_config_paths(
    scfg, function(path) resolve_config_path_value(path, project_root)
  )

  if (!is.null(yaml_path)) attr(scfg, "yaml_file") <- yaml_path
  class(scfg) <- unique(c("bg_project_cfg", class(scfg)))
  scfg
}

#' Require a project configuration to live in its project root
#'
#' @param scfg Resolved project configuration.
#' @param yaml_file Configuration file path.
#' @return `scfg`, invisibly.
#' @noRd
assert_project_config_colocation <- function(scfg, yaml_file = attr(scfg, "yaml_file", exact = TRUE)) {
  if (!checkmate::test_string(yaml_file)) return(invisible(scfg))
  yaml_directory <- normalize_project_path(dirname(yaml_file))
  project_root <- normalize_project_path(scfg$metadata$project_directory)
  same <- if (.Platform$OS.type == "windows") {
    identical(tolower(yaml_directory), tolower(project_root))
  } else {
    identical(yaml_directory, project_root)
  }
  if (!same) {
    stop(
      "metadata/project_directory resolves to\n  ", project_root,
      "\nbut the project configuration is located in\n  ", yaml_directory,
      ".\nProject configurations must be stored in the project root. ",
      "For a colocated configuration, use `project_directory: .`.",
      call. = FALSE
    )
  }
  invisible(scfg)
}

#' Serialize an absolute runtime path for a colocated project YAML
#'
#' Project-contained paths become project-relative. External paths remain
#' absolute so their meaning does not change when the project is moved.
#'
#' @param path Absolute runtime path.
#' @param project_root Absolute project root.
#' @return A YAML-safe path.
#' @noRd
serialize_project_path <- function(path, project_root) {
  if (!checkmate::test_string(path) || is.na(path) || !nzchar(path) ||
      tolower(path) %in% c(".na", ".na.character")) return(path)
  path <- normalize_project_path(path)
  project_root <- normalize_project_path(project_root)
  if (!grepl("^([A-Za-z]:)?/$", project_root)) {
    project_root <- sub("/+$", "", project_root)
  }
  path_key <- if (.Platform$OS.type == "windows") tolower(path) else path
  root_key <- if (.Platform$OS.type == "windows") tolower(project_root) else project_root
  if (identical(path_key, root_key)) return(".")
  prefix <- paste0(root_key, "/")
  if (startsWith(path_key, prefix)) {
    return(substring(path, nchar(project_root) + 2L))
  }
  path
}

#' Build the canonical YAML representation of a runtime configuration
#'
#' @param scfg Resolved project configuration.
#' @param file Destination YAML path.
#' @return An unclassed list suitable for `yaml::write_yaml()`.
#' @noRd
project_config_payload <- function(scfg, file) {
  assert_project_config_colocation(scfg, file)
  project_root <- scfg$metadata$project_directory
  payload <- as.list(scfg)
  payload <- map_project_config_paths(
    payload, function(path) serialize_project_path(path, project_root)
  )
  payload$metadata$project_directory <- "."
  payload$schema_version <- value_or_default(payload$schema_version, 1L)
  payload
}

#' Assert that scheduler-visible operational paths are absolute
#'
#' @param env_variables Named environment vector passed to a worker.
#' @param tracking_sqlite_db Optional tracking database path.
#' @return `TRUE`, invisibly.
#' @noRd
assert_absolute_scheduler_paths <- function(env_variables = NULL,
                                            tracking_sqlite_db = NULL) {
  path_names <- c(
    "pkg_dir", "R_HOME", "log_file", "stdout_log", "stderr_log",
    "complete_file", "insert_tracked_job_path", "upd_job_status_path",
    "add_parent_path", "flywheel_cmd", "flywheel_sync_directory",
    "heudiconv_container", "heudiconv_heuristic", "loc_bids_root",
    "log_directory", "bids_validator", "fmriprep_container",
    "loc_mrproc_root", "loc_scratch", "templateflow_home",
    "fs_license_file", "mriqc_container", "loc_mriqc_root",
    "aroma_container", "postprocess_rscript", "input_dir",
    "postprocess_image_sched_script", "out_dir", "loc_postproc_root",
    "prefetch_container", "prefetch_state_file", "BG_JOB_MANIFEST",
    "BG_CONTRACT_DIRECTORY", "sqlite_db"
  )
  values <- env_variables[intersect(names(env_variables), path_names)]
  if (checkmate::test_string(tracking_sqlite_db)) {
    values <- c(values, tracking_sqlite_db = tracking_sqlite_db)
  }
  invalid <- names(values)[vapply(values, function(value) {
    !is.na(value) && nzchar(value) && !is_absolute_project_path(path.expand(value))
  }, logical(1))]
  if (length(invalid) > 0L) {
    details <- paste0(invalid, "=", unname(values[invalid]), collapse = ", ")
    stop(
      "Scheduler-visible filesystem paths must be absolute after project ",
      "configuration resolution: ", details,
      call. = FALSE
    )
  }
  invisible(TRUE)
}
