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
    tmp_old <- tempfile()
    tmp_new <- tempfile()
    write_yaml(old_cfg, tmp_old)
    write_yaml(scfg, tmp_new)
    diff_out <- tools::Rdiff(tmp_old, tmp_new, Log = TRUE)
    unlink(c(tmp_old, tmp_new))
    if (diff_out$status == 0L) {
      message("No configuration differences were found. File is unchanged.")
      return(invisible(scfg))
    }
    cat("Old configuration settings begin with < and new settings begin with >\n")
    cat("Configuration differences:\n", paste(diff_out$out, collapse = "\n"), "\n\n")
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
