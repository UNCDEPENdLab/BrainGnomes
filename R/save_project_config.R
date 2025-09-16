#' Save a project configuration to YAML
#'
#' Writes the configuration to a file named `project_config.yaml` inside
#' the project's root directory. The function verifies that the output
#' directory exists, offering to create it or allowing the user to select
#' an alternate location. If a configuration file already exists, the
#' user is shown a summary of differences and asked whether to overwrite
#' the file.
#'
#' @param scfg A `bg_project_cfg` object.
#' @param file Optional path for the YAML output. Defaults to
#'   `file.path(scfg$metadata$project_directory, "project_config.yaml")`.
#' @return Invisibly returns `scfg`.
#' @keywords internal
#' @importFrom yaml write_yaml read_yaml
save_project_config <- function(scfg, file = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")

  if (!checkmate::test_string(file)) {
    if (is.null(scfg$metadata$project_directory)) {
      stop("Cannot determine project directory from scfg")
    }
    file <- file.path(scfg$metadata$project_directory, "project_config.yaml")
  }

  dir <- dirname(file)
  if (!dir.exists(dir)) {
    create <- prompt_input(
      instruct = sprintf("The directory %s does not exist. Create it?", dir),
      type = "flag"
    )
    if (create) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(dir)) {
        message("Configuration not saved: failed to create directory.")
        return(invisible(scfg))
      }
    } else {
      new_dir <- prompt_input(
        instruct = "Specify an alternative directory for project_config.yaml (or press Enter to cancel):",
        type = "character", required = FALSE
      )
      if (is.na(new_dir[1L])) {
        message("Configuration not saved: no valid directory provided.")
        return(invisible(scfg))
      }
      dir <- path.expand(new_dir)
      if (!dir.exists(dir)) {
        create <- prompt_input(
          instruct = sprintf("The directory %s does not exist. Create it?", dir),
          type = "flag"
        )
        if (create) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
          if (!dir.exists(dir)) {
            message("Configuration not saved: failed to create directory.")
            return(invisible(scfg))
          }
        } else {
          message("Configuration not saved: no valid directory provided.")
          return(invisible(scfg))
        }
      }
      file <- file.path(dir, basename(file))
    }
  }

  overwrite <- TRUE
  if (file.exists(file)) {
    old_cfg <- yaml::read_yaml(file)
    cfg_differences <- compare_lists(old_cfg, scfg)

    if (length(cfg_differences) == 0L) {
      return(invisible(scfg)) # no changes
    } else {
      cat("Configuration differences:\n")
      for (dd in cfg_differences) {
        cat("  - ", dd, "\n", sep = "")
      }
    }

    overwrite <- prompt_input(instruct = "Overwrite existing project_config.yaml?", type = "flag")
  }

  if (overwrite) {
    yaml::write_yaml(scfg, file)
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
#' @return Invisibly returns \code{TRUE} if no differences are found; otherwise \code{FALSE}.
#' @keywords internal
compare_lists <- function(old, new, path = "", max_diffs = 100) {
  differences <- list()

  compare_recursive <- function(old, new, path) {
    if (length(differences) >= max_diffs) {
      return()
    }

    # If both are lists, compare their keys recursively
    if (is.list(old) && is.list(new)) {
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
