#' @keywords internal 
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib BrainGnomes, .registration = TRUE
## usethis namespace: end
NULL

# Print CLI PATH guidance once per installed package version.
.onAttach <- function(libname, pkgname) {
  cli_script <- system.file("BrainGnomes", package = pkgname)
  if (!nzchar(cli_script)) return(invisible(NULL))

  cli_dir <- normalizePath(dirname(cli_script), winslash = "/", mustWork = FALSE)
  path_entries <- strsplit(Sys.getenv("PATH", unset = ""), .Platform$path.sep, fixed = TRUE)[[1]]
  path_entries <- normalizePath(path_entries[nzchar(path_entries)], winslash = "/", mustWork = FALSE)
  cli_in_path <- cli_dir %in% unique(path_entries)

  if (isTRUE(cli_in_path)) return(invisible(NULL))

  current_version <- as.character(utils::packageVersion(pkgname))
  state_dir <- tools::R_user_dir(pkgname, which = "cache")
  state_file <- file.path(state_dir, "cli_path_message_version.txt")
  shown_version <- suppressWarnings(
    tryCatch(readLines(state_file, n = 1L, warn = FALSE), error = function(e) "")
  )

  if (!identical(shown_version, current_version)) {
    packageStartupMessage(
      "If you want to call BrainGnomes on the command line, ensure this directory is in your system PATH: ",
      cli_dir
    )
    try({
      dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
      writeLines(current_version, con = state_file, useBytes = TRUE)
    }, silent = TRUE)
  }

  invisible(NULL)
}

# Add globals to avoid R CMD check complaint
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(":=", "resample_template_to_bold"))
}
