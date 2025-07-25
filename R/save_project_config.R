#' Save a study configuration to YAML
#'
#' Writes the configuration to a file named `project_config.yaml` inside
#' the project's root directory. If a configuration file already exists,
#' the user is shown a summary of differences and asked whether to
#' overwrite the file.
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

  overwrite <- TRUE
  if (file.exists(file)) {
    old_cfg <- yaml::read_yaml(file)
    cfg_differences <- compare_lists(old_cfg, scfg)

    if (length(cfg_differences) == 0L) {
      message("No configuration differences were found. File is unchanged.")
      return(invisible(scfg))
    } else {
      cat("Configuration differences:\n")
      for (dd in cfg_differences) {
        cat("  - ", dd, "\n", sep = "")
      }
    }

    overwrite <- if (interactive()) {
      prompt_input(instruct = "Overwrite existing project_config.yaml?", type = "flag")
    } else {
      FALSE
    }
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
    if (length(differences) >= max_diffs) return()
    
    if (is.list(old) && is.list(new)) {
      all_keys <- union(names(old), names(new))
      
      for (k in all_keys) {
        subpath <- if (nzchar(path)) paste0(path, "$", k) else k
        
        if (!k %in% names(old)) {
          # present in new, absent in old
          differences[[length(differences) + 1]] <<- paste0(
            "$", path, ":\n      old: [absent]\n      new: ",
            deparse1(new), " <", typeof(new), ">"
          )
        } else if (!k %in% names(new)) {
          differences[[length(differences) + 1]] <<- paste0(
            "$", path, ":\n      old: ",
            deparse1(old), " <", typeof(old), ">\n      new: [absent]"
          )
        } else {
          compare_recursive(old[[k]], new[[k]], subpath)
        }
      }
    } else if (!identical(old, new)) {
      differences[[length(differences) + 1]] <<- paste0(
        "$", path, ":\n      old: ",
        deparse1(old), " <", typeof(old), ">\n      new: ", 
        deparse1(new), " <", typeof(new), ">")
    }
  }
  
  compare_recursive(old, new, path)

  return(invisible(differences))
}

