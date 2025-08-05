test_that("get_project_status reports completion", {
  root <- tempdir()
  log_dir <- file.path(root, "logs"); dir.create(log_dir)
  bids_dir <- file.path(root, "bids"); dir.create(bids_dir)
  fmriprep_dir <- file.path(root, "fmriprep"); dir.create(fmriprep_dir)
  mriqc_dir <- file.path(root, "mriqc"); dir.create(mriqc_dir)

  sub <- "01"; ses <- "A"
  dir.create(file.path(log_dir, paste0("sub-", sub)))
  dir.create(file.path(bids_dir, paste0("sub-", sub), paste0("ses-", ses)), recursive = TRUE)
  dir.create(file.path(fmriprep_dir, paste0("sub-", sub)), recursive = TRUE)
  dir.create(file.path(fmriprep_dir, paste0("sub-", sub), paste0("ses-", ses)), recursive = TRUE)

  cat("2024-05-04 10:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".bids_conversion_sub-", sub, "_ses-", ses, "_complete")))
  cat("2024-05-04 11:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".fmriprep_sub-", sub, "_complete")))
  cat("2024-05-04 12:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".postprocess_stream1_sub-", sub, "_ses-", ses, "_complete")))

  scfg <- list(
    metadata = list(log_directory = log_dir, bids_directory = bids_dir, fmriprep_directory = fmriprep_dir, mriqc_directory = mriqc_dir),
    bids_conversion = list(enable = TRUE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = TRUE, stream1 = list())
  )
  class(scfg) <- "bg_project_cfg"

  res <- get_project_status(scfg)
  expect_equal(nrow(res), 1)
  expect_true(res$bids_conversion_complete)
  expect_true(res$fmriprep_complete)
  expect_true(res$stream1_complete)

  sm <- summary(res)
  expect_equal(sm$n_complete[sm$step == "bids_conversion_complete"], 1)
})
