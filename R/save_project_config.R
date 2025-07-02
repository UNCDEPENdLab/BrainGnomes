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
#' @importFrom yaml write_yaml read_yaml as.yaml
save_project_config <- function(scfg, file = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")

  if (is.null(file)) {
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
    writeLines(yaml::as.yaml(old_cfg), tmp_old)
    writeLines(yaml::as.yaml(scfg), tmp_new)
    diff_out <- tools::Rdiff(tmp_old, tmp_new)
    message("Current differences:\n", paste(diff_out, collapse = "\n"))
    overwrite <- if (interactive()) {
      prompt_input(instruct = "Overwrite existing project_config.yaml?", type = "flag")
    } else {
      FALSE
    }
  }

  if (overwrite) {
    yaml::write_yaml(scfg, file)
    message("Configuration saved to ", file)
  }
  invisible(scfg)
}
