test_that("is_external_path identifies directories", {
  proj <- tempfile("proj_")
  dir.create(proj, showWarnings = FALSE)
  inside <- file.path(proj, "data_fmriprep")
  outside <- tempfile("fmriprep_")
  expect_false(is_external_path(inside, proj))
  expect_true(is_external_path(outside, proj))
})

test_that("process_subject checks complete file for internal fmriprep", {
  root <- tempfile("proj_int_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  proj_dir <- file.path(root, "proj_int"); dir.create(proj_dir)
  log_dir <- file.path(proj_dir, "logs"); dir.create(log_dir)
  bids_dir <- file.path(proj_dir, "bids"); dir.create(bids_dir)
  fmriprep_dir <- file.path(proj_dir, "data_fmriprep"); dir.create(file.path(fmriprep_dir, "sub-01"), recursive = TRUE)
  scfg <- list(
    metadata = list(project_directory = proj_dir, log_directory = log_dir,
                    bids_directory = bids_dir, fmriprep_directory = fmriprep_dir),
    postprocess = list(enable = TRUE),
    fmriprep = list(enable = FALSE),
    compute_environment = list(scheduler = "slurm"),
    force = FALSE
  )
  class(scfg) <- "bg_project_cfg"
  sub_cfg <- data.frame(sub_id = "01", ses_id = NA_character_, dicom_sub_dir = NA_character_,
                        dicom_ses_dir = NA_character_, bids_sub_dir = file.path(bids_dir, "sub-01"),
                        bids_ses_dir = NA_character_, stringsAsFactors = FALSE)
  if (!dir.exists(sub_cfg$bids_sub_dir[1L])) dir.create(sub_cfg$bids_sub_dir[1L])
  is_called <- FALSE
  ns <- asNamespace("BrainGnomes")
  orig <- get("is_step_complete", envir = ns)
  unlockBinding("is_step_complete", ns)
  assign("is_step_complete", function(...) {is_called <<- TRUE; orig(...)}, envir = ns)
  lockBinding("is_step_complete", ns)
  on.exit({
    unlockBinding("is_step_complete", ns)
    assign("is_step_complete", orig, envir = ns)
    lockBinding("is_step_complete", ns)
  })
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE, aroma = FALSE,
             postprocess = TRUE, extract_rois = FALSE)
  expect_warning(
    process_subject(scfg, sub_cfg, steps),
    "fmriprep outputs are missing"
  )
  expect_true(is_called)
})

test_that("process_subject accepts external fmriprep without complete file", {
  root <- tempfile("proj_ext_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  proj_dir <- file.path(root, "proj_ext"); dir.create(proj_dir)
  log_dir <- file.path(proj_dir, "logs"); dir.create(log_dir)
  bids_dir <- file.path(proj_dir, "bids"); dir.create(bids_dir)
  fmriprep_dir <- file.path(root, "external_fmriprep"); dir.create(file.path(fmriprep_dir, "sub-01"), recursive = TRUE)
  scfg <- list(
    metadata = list(project_directory = proj_dir, log_directory = log_dir,
                    bids_directory = bids_dir, fmriprep_directory = fmriprep_dir),
    postprocess = list(enable = TRUE),
    fmriprep = list(enable = FALSE),
    compute_environment = list(scheduler = "slurm"),
    force = FALSE
  )
  class(scfg) <- "bg_project_cfg"
  sub_cfg <- data.frame(sub_id = "01", ses_id = NA_character_, dicom_sub_dir = NA_character_,
                        dicom_ses_dir = NA_character_, bids_sub_dir = file.path(bids_dir, "sub-01"),
                        bids_ses_dir = NA_character_, stringsAsFactors = FALSE)
  if (!dir.exists(sub_cfg$bids_sub_dir[1L])) dir.create(sub_cfg$bids_sub_dir[1L], recursive = TRUE)
  is_called <- FALSE
  ns <- asNamespace("BrainGnomes")
  orig <- get("is_step_complete", envir = ns)
  unlockBinding("is_step_complete", ns)
  assign("is_step_complete", function(...) {is_called <<- TRUE; orig(...)}, envir = ns)
  lockBinding("is_step_complete", ns)
  on.exit({
    unlockBinding("is_step_complete", ns)
    assign("is_step_complete", orig, envir = ns)
    lockBinding("is_step_complete", ns)
  }, add = TRUE)
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE, aroma = FALSE,
             postprocess = TRUE, extract_rois = FALSE)
  process_subject(scfg, sub_cfg, steps)
  expect_false(is_called)
})
