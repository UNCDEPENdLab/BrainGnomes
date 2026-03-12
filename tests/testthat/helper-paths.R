norm_path_raw <- function(path, mustWork = FALSE) {
  if (is.null(path)) return(NULL)

  normalizePath(path, winslash = "/", mustWork = mustWork)
}

norm_path <- function(path, mustWork = FALSE) {
  out <- norm_path_raw(path, mustWork = mustWork)
  if (.Platform$OS.type == "unix") {
    private_var <- startsWith(out, "/private/var/")
    out[private_var] <- sub("^/private", "", out[private_var])
  }
  temp_alias <- grepl("/T/Rtmp", out, fixed = TRUE)
  if (any(temp_alias)) {
    out[temp_alias] <- sub("/T/(Rtmp[^/]+)", "/T//\\1", out[temp_alias], perl = TRUE)
  }
  out
}

expect_path_identical <- function(actual, expected, mustWork = TRUE) {
  testthat::expect_identical(
    norm_path(actual, mustWork = mustWork),
    norm_path(expected, mustWork = mustWork)
  )
}
