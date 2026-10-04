# Run the real entry point in a child R process and keep its output streams
# separate. Inputs are command arguments; output contains exit status, stdout,
# stderr, and a combined text view for legacy human-output assertions. Optional
# wd sets the child's working directory after resolving the installed script.
run_brain_gnomes_cli <- function(args = character(), wd = NULL) {
  script <- system.file("BrainGnomes", package = "BrainGnomes")
  if (!nzchar(script)) script <- testthat::test_path("..", "..", "inst", "BrainGnomes")
  script <- normalizePath(script, mustWork = TRUE)
  if (!is.null(wd)) withr::local_dir(wd)
  out_file <- tempfile("cli-stdout-")
  err_file <- tempfile("cli-stderr-")
  on.exit(unlink(c(out_file, err_file)), add = TRUE)
  status <- suppressWarnings(system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", shQuote(c(script, args))),
    stdout = out_file, stderr = err_file
  ))
  out <- readLines(out_file, warn = FALSE)
  err <- readLines(err_file, warn = FALSE)
  list(status = as.integer(status), stdout = out, stderr = err, output = c(out, err))
}
