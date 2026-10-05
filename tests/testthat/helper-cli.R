# Run the real entry point in a child R process and keep its output streams
# separate. Inputs are command arguments; output contains exit status, stdout,
# stderr, and a combined text view for legacy human-output assertions. Optional
# wd sets the child's working directory after resolving the installed script.
# Redirect stdin to an empty file so even Windows subprocesses cannot inherit
# a console. timeout bounds each command and turns hangs into immediate errors.
run_brain_gnomes_cli <- function(args = character(), wd = NULL, timeout = 60) {
  script <- system.file("BrainGnomes", package = "BrainGnomes")
  if (!nzchar(script)) script <- testthat::test_path("..", "..", "inst", "BrainGnomes")
  script <- normalizePath(script, mustWork = TRUE)
  if (!is.null(wd)) withr::local_dir(wd)
  out_file <- tempfile("cli-stdout-")
  err_file <- tempfile("cli-stderr-")
  in_file <- tempfile("cli-stdin-")
  file.create(in_file)
  on.exit(unlink(c(out_file, err_file, in_file)), add = TRUE)
  status <- suppressWarnings(system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", shQuote(c(script, args))),
    stdout = out_file, stderr = err_file, stdin = in_file, timeout = timeout
  ))
  out <- readLines(out_file, warn = FALSE)
  err <- readLines(err_file, warn = FALSE)
  if (identical(as.integer(status), 124L)) {
    stop("CLI command timed out after ", timeout, " seconds: ",
      paste(args, collapse = " "), "\n", paste(c(out, err), collapse = "\n"))
  }
  list(status = as.integer(status), stdout = out, stderr = err, output = c(out, err))
}
