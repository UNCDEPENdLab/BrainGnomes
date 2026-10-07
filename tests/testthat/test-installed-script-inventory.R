run_installed_script_help <- function(script_name) {
  script <- system.file(script_name, package = "BrainGnomes")
  if (!nzchar(script)) {
    script <- normalizePath(testthat::test_path("..", "..", "inst", script_name), mustWork = TRUE)
  }
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script), "--help"),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = output)
}

test_that("installed R-script inventory contains only supported entry points and workers", {
  expected <- list(
    public = "BrainGnomes",
    scheduler_helpers = c(
      "add_parent.R", "insert_tracked_job.R", "prepare_job_manifest.R",
      "submit_job.R", "upd_job_status.R"
    ),
    internal_workers = c("extract_cli.R", "postprocess_cli.R"),
    runtime_support = c("worker_bootstrap.R", "worker_runtime_load.R")
  )
  inst_dir <- system.file(package = "BrainGnomes")
  if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "BrainGnomes"))) {
    inst_dir <- testthat::test_path("..", "..", "inst")
  }
  actual <- c(
    if (file.exists(file.path(inst_dir, "BrainGnomes"))) "BrainGnomes" else character(),
    basename(list.files(inst_dir, pattern = "\\.R$", full.names = TRUE))
  )

  expect_setequal(actual, unlist(expected, use.names = FALSE))
  expect_false("ROI_TempCorr.R" %in% actual)

  for (script_name in c(expected$public, expected$scheduler_helpers)) {
    result <- run_installed_script_help(script_name)
    expect_equal(result$status, 0L, info = script_name)
    expect_true(length(result$output) > 0L, info = script_name)
    expect_true(any(grepl("Usage:|Options:|--help", result$output)), info = script_name)
  }
})
