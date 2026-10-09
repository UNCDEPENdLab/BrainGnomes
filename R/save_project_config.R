#' Save a project configuration to YAML
#'
#' Writes the configuration to a file named `project_config.yaml` inside
#' the project's root directory. The function verifies that the output
#' directory exists and offers to create it. If a configuration file already
#' exists, the user is shown a summary of differences and asked whether to
#' overwrite the file. After confirmation, the final save uses
#' [write_project_config()] so the destination is replaced atomically.
#'
#' @param scfg A `bg_project_cfg` object.
#' @param file Optional path for the YAML output. Defaults to
#'   `file.path(scfg$metadata$project_directory, "project_config.yaml")`. The
#'   destination must be colocated with the project root.
#' @return Invisibly returns `scfg`.
#' @keywords internal
#' @importFrom yaml write_yaml read_yaml
save_project_config <- function(scfg, file = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")

  # if a file is not passed in, default to the yaml_file attribute
  if (!checkmate::test_string(file)) {
    stored_file <- attr(scfg, "yaml_file")
    if (checkmate::test_string(stored_file)) file <- stored_file
  }

  # default to project_config.yaml in project_directory if file is not provided
  if (!checkmate::test_string(file)) {
    if (is.null(scfg$metadata$project_directory)) stop("Cannot determine project directory from scfg")
    file <- file.path(scfg$metadata$project_directory, "project_config.yaml")
  }

  file <- normalize_project_path(file)
  scfg <- resolve_project_paths(scfg, yaml_file = file)
  assert_project_config_colocation(scfg, file)
  dir <- dirname(file)
  if (!dir.exists(dir)) {
    create <- prompt_input(instruct = glue("The directory {dir} does not exist. Create it?"), type = "flag")
    if (create) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(dir)) {
        message("Configuration not saved: failed to create directory.")
        return(invisible(scfg))
      }
    } else {
      message("Configuration not saved: the project root was not created.")
      return(invisible(scfg))
    }
  }

  attr(scfg, "yaml_file") <- normalizePath(file, winslash = "/", mustWork = FALSE)

  overwrite <- TRUE
  if (file.exists(file)) {
    old_cfg <- yaml::read_yaml(file)
    cfg_differences <- compare_lists(old_cfg, project_config_payload(scfg, file))

    if (length(cfg_differences) == 0L) {
      return(invisible(scfg)) # no changes
    } else {
      cat("Configuration differences:\n")
      for (dd in cfg_differences) {
        cat("  - ", dd, "\n", sep = "")
      }
    }

    overwrite <- prompt_input(instruct = glue("Overwrite existing {basename(file)}?"), type = "flag")
  }

  if (overwrite) {
    scfg <- write_project_config(scfg, file = file, overwrite = TRUE)
    message("Configuration saved to ", file)
  } else {
    message("Leaving configuration file unchanged: ", file)
  }
  invisible(scfg)
}

#' Recursively Compare Two List Objects
#'
#' Compares two list objects and prints a summary of any differences in structure or values.
#'
#' @param old First list object.
#' @param new Second list object.
#' @param path Internal parameter to track the location within the nested structure (used recursively).
#' @param max_diffs Maximum number of differences to report (default: 20).
#'
#' @return A list of human-readable differences, invisibly. Empty if the
#'   inputs are identical.
#' @keywords internal
compare_lists <- function(old, new, path = "", max_diffs = 100) {
  differences <- list()

  compare_recursive <- function(old, new, path) {
    if (length(differences) >= max_diffs) {
      return()
    }

    # If both are lists, compare their keys recursively
    if (is.list(old) && is.list(new) &&
        !is.null(names(old)) && !is.null(names(new)) &&
        all(nzchar(names(old))) && all(nzchar(names(new))) &&
        !anyDuplicated(names(old)) && !anyDuplicated(names(new))) {
      all_keys <- union(names(old), names(new))

      for (k in all_keys) {
        subpath <- if (nzchar(path)) paste0(path, "$", k) else k

        if (!k %in% names(old)) {
          differences[[length(differences) + 1]] <<- paste0(
            "$", subpath, ":\n      old: [absent]\n      new: ",
            deparse1(new[[k]]), " <", typeof(new[[k]]), ">"
          )
        } else if (!k %in% names(new)) {
          differences[[length(differences) + 1]] <<- paste0(
            "$", subpath, ":\n      old: ",
            deparse1(old[[k]]), " <", typeof(old[[k]]), ">\n      new: [absent]"
          )
        } else {
          compare_recursive(old[[k]], new[[k]], subpath)
        }

        if (length(differences) >= max_diffs) {
          return()
        }
      }
    } else if (!identical(old, new)) {
      differences[[length(differences) + 1]] <<- paste0(
        "$", path, ":\n      old: ",
        deparse1(old), " <", typeof(old), ">\n      new: ",
        deparse1(new), " <", typeof(new), ">"
      )
    }
  }
  compare_recursive(old, new, path)
  
  return(invisible(differences))
}
