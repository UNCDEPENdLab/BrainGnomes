#' Determine if a path lies outside the project directory
#'
#' @param path The path to evaluate.
#' @param project_dir The root project directory.
#' @return `TRUE` if `path` is not within `project_dir`, otherwise `FALSE`.
#' @keywords internal
is_external_path <- function(path, project_dir) {
  checkmate::assert_string(path)
  checkmate::assert_string(project_dir)
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  proj_norm <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  path_norm <- sub("/+$", "", path_norm)
  proj_norm <- sub("/+$", "", proj_norm)
  inside <- startsWith(path_norm, paste0(proj_norm, "/")) || path_norm == proj_norm
  !inside
}
