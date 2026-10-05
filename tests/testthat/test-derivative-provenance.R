#' Create a small fMRIPrep-shaped dataset with known signals and supplied reports
#' @param root New temporary study directory.
#' @return Input, atlas, output, and native configuration paths below root.
#' @noRd
make_derivative_provenance_fixture <- function(root) {
  dir.create(root, recursive = TRUE)
  input_dir <- file.path(root, "fmriprep", "sub-01", "func")
  output_dir <- file.path(root, "postproc", "sub-01", "func")
  for (path in c(input_dir, output_dir, file.path(root, "scratch"), file.path(root, "logs"))) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  bold <- file.path(input_dir, "sub-01_task-rest_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz")
  # Distinct, nonconstant voxel signals give two nondegenerate regional series.
  values <- outer(1:8, 1:30, function(voxel, volume) 100 + sin(volume / voxel) + volume / 10)
  RNifti::writeNifti(array(values, c(2, 2, 2, 30)), bold)
  atlas <- file.path(root, "space-MNI152NLin6Asym_atlas-Demo_dseg.nii.gz")
  RNifti::writeNifti(array(rep(1:2, each = 4), c(2, 2, 2)), atlas)
  confounds <- file.path(input_dir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(data.frame(
    trans_x = sin(1:30), trans_y = rep(2, 30), white_matter = cos(1:30),
    framewise_displacement = c(0.7, rep(0.1, 29))
  ), confounds, sep = "\t")
  dataset <- file.path(root, "fmriprep")
  write_json_atomic(list(Name = "Test upstream", DatasetType = "derivative",
    GeneratedBy = list(list(Name = "fMRIPrep", Version = "test-version"))),
    file.path(dataset, "dataset_description.json"))
  write_json_atomic(list(RepetitionTime = 1, SpatialReference = "MNI152NLin6Asym"),
    derivative_provenance_paths(bold)$json)
  dir.create(file.path(dataset, "logs"))
  writeLines("@misc{supplied, title={Supplied upstream citation}, year={2026}}",
    file.path(dataset, "logs", "CITATION.bib"))
  writeLines("Supplied upstream methods.", file.path(dataset, "logs", "CITATION.md"))
  dir.create(file.path(dataset, "sub-01", "figures"))
  writeLines("<svg xmlns='http://www.w3.org/2000/svg'/>",
    file.path(dataset, "sub-01", "figures", "report.svg"))
  writeLines("<svg xmlns='http://www.w3.org/2000/svg'/>",
    file.path(dataset, "sub-01", "figures", "object.svg"))
  writeLines(paste0('<html><img src="sub-01/figures/report.svg">',
    '<object type="image/svg+xml" data = "sub-01/figures/object.svg"></object></html>'),
    file.path(dataset, "sub-01.html"))
  cfg <- list(
    bids_desc = "clean", tr = 1, output_dir = output_dir,
    scratch_directory = file.path(root, "scratch"), project_name = "provenance",
    overwrite = TRUE, keep_intermediates = FALSE, force_processing_order = FALSE,
    validate_postproc_steps = FALSE, fsl_img = NULL,
    apply_mask = list(enable = FALSE, prefix = "m"),
    spatial_smooth = list(enable = FALSE, prefix = "s"),
    intensity_normalize = list(enable = FALSE, prefix = "n"),
    apply_aroma = list(enable = FALSE, prefix = "a"),
    temporal_filter = list(enable = FALSE, prefix = "f", method = "butterworth"),
    confound_regression = list(enable = FALSE, prefix = "r", columns = NULL),
    confound_calculate = list(enable = FALSE, columns = NULL),
    motion_filter = list(enable = FALSE),
    scrubbing = list(enable = FALSE, apply = FALSE, interpolate = FALSE,
      add_to_confounds = FALSE, prefix = "x", interpolate_prefix = "i")
  )
  list(root = root, bold = bold, atlas = atlas, confounds = confounds, cfg = cfg)
}

#' Read saved derivative attempts separately from the producer JSON
#' @param file Derivative path.
#' @return Parsed attempt records in filename order.
#' @noRd
read_derivative_attempts <- function(file) {
  lapply(list.files(derivative_provenance_paths(file)$directory,
    pattern = "^attempt-.*\\.json$", full.names = TRUE), jsonlite::read_json)
}

test_that("native exports survive loss of the original study and execution database", {
  root <- tempfile("portable-study-")
  bundle <- tempfile("portable-copy-")
  on.exit(unlink(c(root, bundle), recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"))
  # Contract and receipt shapes follow job_contracts.R; no SQLite fixture exists.
  run_dir <- file.path(root, "logs", "runs", "run-123")
  manifest <- file.path(run_dir, "jobs", "contract-456", "job-manifest.json")
  write_json_atomic(list(schema_version = "brain-gnomes-job-manifest-v1",
    run_id = "run-123", contract_id = "contract-456",
    logical_work_unit = list(unit_key = "postproc:01", attempt = 2L), artifacts = list()), manifest)
  write_json_atomic(list(schema_version = "brain-gnomes-job-runtime-receipt-v1",
    job_id = "567", state = "started", enforcement = "passed"),
    file.path(dirname(manifest), "runtime-receipt.json"))
  write_json_atomic(list(request = list(stages = list("postprocess")),
    configuration = list(snapshot_file = "config.yaml", resolved = list(secret = "excluded"))),
    file.path(run_dir, "provenance.json"))
  withr::local_envvar(BG_JOB_MANIFEST = manifest, SLURM_ARRAY_TASK_ID = "3")
  original_checksum <- unname(tools::md5sum(fixture$bold))
  output <- postprocess_subject(fixture$bold, fixture$cfg)
  expect_true(file.exists(fixture$bold))
  expect_identical(unname(tools::md5sum(fixture$bold)), original_checksum)
  record <- read_derivative_provenance(output)$BrainGnomes
  expect_identical(record$Operations[[1]]$Name, "copy_input")
  expect_identical(record$Status, "completed")
  expect_identical(record$Execution$RunID, "run-123")
  expect_identical(record$Execution$Attempt, 2L)
  expect_identical(record$Execution$SchedulerTask$SLURM_ARRAY_TASK_ID, "3")
  expect_null(record$Execution$Run$Configuration$resolved)
  expect_true(file.exists(file.path(dirname(output), record$AttemptFile)))
  expect_identical(record$SourceFiles[[1]]$checksum, original_checksum)
  expect_match(record$MethodsText, "fMRIPrep test-version", fixed = TRUE)
  expect_true(record$HumanReviewRequired)
  writeLines("unrelated addition", file.path(derivative_provenance_paths(output)$directory, "unrelated.txt"))
  exported <- export_derivative_provenance(output, bundle,
    datalad = list(dataset_id = "example-id", commit = "example-commit"))
  expect_true(all(file.exists(unlist(exported))))
  expect_false(any(basename(list.files(bundle, recursive = TRUE)) == "unrelated.txt"))
  description <- jsonlite::read_json(exported$dataset_description)
  expect_identical(description$DatasetType, "derivative")
  expect_identical(description$GeneratedBy[[1]]$Name, "BrainGnomes")
  copied <- read_derivative_provenance(exported$derivative)
  expect_type(copied$Sources, "list") # One source must still be a JSON array.
  expect_match(copied$Sources[[1]], "^bids:source1:")
  expect_identical(copied$BrainGnomes$ExternalReferences$Verification, "user_supplied_not_verified")
  expect_length(copied$BrainGnomes$ExportedAttempts, 1)
  expect_match(paste(readLines(exported$bibliography), collapse = "\n"), "@misc{supplied", fixed = TRUE)
  expect_match(paste(readLines(exported$bibliography), collapse = "\n"), "10.1038/s41592-018-0235-4", fixed = TRUE)
  assets <- copied$BrainGnomes$Assets
  expect_true(any(vapply(assets, function(x) grepl("sub-01/figures/report.svg$", x$File), logical(1))))
  expect_true(any(vapply(assets, function(x) grepl("sub-01/figures/object.svg$", x$File), logical(1))))
  unlink(root, recursive = TRUE)
  expect_identical(read_derivative_provenance(exported$derivative)$BrainGnomes$RecordID, record$RecordID)
  expect_silent(derivative_provenance_check_companions(exported$derivative, copied))
  expect_true(all(vapply(assets, function(x) file.exists(file.path(bundle, x$File)), logical(1))))
  reexport <- export_derivative_provenance(exported$derivative, file.path(bundle, "second-copy"))
  offline_metadata <- read_derivative_provenance(reexport$derivative)
  expect_null(offline_metadata$Sources)
  expect_identical(offline_metadata$BrainGnomes$ExternalReferences, copied$BrainGnomes$ExternalReferences)
})

test_that("confound records reflect the written design and resolved censor vector", {
  root <- tempfile("resolved-confounds-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"), BG_JOB_MANIFEST = "")
  cfg <- fixture$cfg
  cfg$confound_regression <- list(enable = TRUE, prefix = "r",
    columns = "trans_[xy]", noproc_columns = "white_matter")
  cfg$scrubbing <- modifyList(cfg$scrubbing, list(enable = TRUE, apply = TRUE,
    expression = c(fd = "framewise_displacement > 0.5")))
  output <- postprocess_subject(fixture$bold, cfg)
  record <- read_derivative_provenance(output)$BrainGnomes
  confounds <- record$Resolved$Confounds
  expect_identical(unlist(confounds$RegressorColumns), c("intercept", "trans_x", "white_matter"))
  expect_identical(unlist(confounds$DroppedConstantColumns), "trans_y")
  expect_identical(unlist(record$Resolved$Censor$CensoredIndices), 1L)
  expect_identical(length(record$Resolved$Censor$RetainedIndices), 29L)
  expect_identical(record$Resolved$Censor$InputVolumes, 30L)
  expect_true(file.exists(record$Resolved$Censor$File$path))
  expect_identical(vapply(record$Operations, `[[`, character(1), "Status"), c("executed", "executed"))
  expect_match(record$MethodsText, "intercept, trans_x, white_matter", fixed = TRUE)
  expect_false(grepl("trans_y", record$MethodsText, fixed = TRUE))
  expect_identical(as.integer(dim(RNifti::readNifti(output))[4]), 29L)
  design_file <- sub("_bold.nii.gz", "_regressors.tsv", output, fixed = TRUE)
  expect_identical(ncol(data.table::fread(design_file, header = FALSE)), 3L)
})

test_that("reuse preserves its producer and failed replacement cannot claim completion", {
  root <- tempfile("provenance-states-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"), BG_JOB_MANIFEST = "")
  output <- postprocess_subject(fixture$bold, fixture$cfg)
  original_json <- readLines(derivative_provenance_paths(output)$json)
  cfg <- fixture$cfg
  cfg$overwrite <- FALSE
  cfg$tr <- 2
  expect_identical(postprocess_subject(fixture$bold, cfg), output)
  expect_identical(readLines(derivative_provenance_paths(output)$json), original_json)
  attempts <- read_derivative_attempts(output)
  reuse <- Filter(function(x) identical(x$Status, "reused"), attempts)[[1]]
  expect_identical(reuse$Requested$tr, 2L)
  expect_identical(reuse$ReusedRecordID, read_derivative_provenance(output)$BrainGnomes$RecordID)
  # Fail inside a real processing boundary without needing FSL or a scheduler.
  cfg$overwrite <- TRUE
  cfg$temporal_filter <- list(enable = TRUE, prefix = "f", method = "butterworth", high_pass_hz = 0.01)
  testthat::local_mocked_bindings(temporal_filter = function(...) stop("filter failure"))
  expect_error(postprocess_subject(fixture$bold, cfg), "filter failure")
  failed <- Filter(function(x) identical(x$Status, "failed"), read_derivative_attempts(output))[[1]]
  expect_identical(failed$Operations[[1]]$Status, "failed")
  expect_match(failed$Error, "filter failure")
  expect_true(file.exists(fixture$bold))
  expect_error(read_derivative_provenance(output))
})

test_that("legacy reuse reports unknown settings and stale records are rejected", {
  root <- tempfile("legacy-provenance-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"), BG_JOB_MANIFEST = "")
  output <- file.path(fixture$cfg$output_dir, sub("desc-preproc", "desc-clean", basename(fixture$bold)))
  file.copy(fixture$bold, output)
  cfg <- fixture$cfg
  cfg$overwrite <- FALSE
  postprocess_subject(fixture$bold, cfg)
  record <- read_derivative_provenance(output)$BrainGnomes
  expect_identical(record$Status, "unknown")
  expect_length(record$Operations, 0)
  expect_length(record$SourceFiles, 0)
  expect_null(record$Requested)
  expect_match(record$MethodsText, "generating settings and original sources are unknown", fixed = TRUE)
  cat("changed", file = output, append = TRUE)
  expect_error(read_derivative_provenance(output), "checksum")
  expect_error(export_derivative_provenance(output, file.path(root, "invalid")), "checksum")
})

test_that("ROI records carry atlas, censor alignment, connectivity, and upstream history", {
  root <- tempfile("roi-provenance-")
  bundle <- tempfile("roi-bundle-")
  on.exit(unlink(c(root, bundle), recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"), BG_JOB_MANIFEST = "")
  cfg <- fixture$cfg
  cfg$scrubbing <- modifyList(cfg$scrubbing, list(enable = TRUE, apply = TRUE,
    expression = c(fd = "framewise_displacement > 0.5")))
  output <- postprocess_subject(fixture$bold, cfg)
  dir.create(file.path(root, "roi"))
  roi <- extract_rois(output, fixture$atlas, out_dir = file.path(root, "roi"),
    cor_method = "pearson", rtoz = TRUE, save_diagnostics = TRUE, min_vox_per_roi = 1L)[[1]]
  expect_true(all(file.exists(roi$provenance)))
  record <- read_derivative_provenance(roi$timeseries)$BrainGnomes
  expect_identical(record$Resolved$Censor$Application, "already_applied_to_input")
  expect_identical(unlist(record$Resolved$Censor$OriginalVolumeIndices), 2:30)
  expect_identical(record$Resolved$OutputRows, 29L)
  expect_identical(unlist(record$Resolved$Atlas$Labels), 1:2)
  expect_false(record$Resolved$Atlas$Resampled)
  expect_identical(record$Resolved$Atlas$Original$checksum, unname(tools::md5sum(fixture$atlas)))
  expect_match(record$MethodsText, "Upstream recorded processing:", fixed = TRUE)
  correlation <- read_derivative_provenance(roi$correlation[[1]])$BrainGnomes
  expect_identical(correlation$Operations[[2]]$Parameters$method, "pearson")
  expect_true(correlation$Operations[[2]]$Parameters$fisher_z)
  diagnostics <- read_derivative_provenance(roi$diagnostics)$BrainGnomes
  expect_identical(diagnostics$Operations[[1]]$Name, "roi_diagnostics")
  expect_length(diagnostics$Operations, 1)
  exported <- export_derivative_provenance(roi$timeseries, bundle)
  unlink(root, recursive = TRUE)
  expect_identical(read_derivative_provenance(exported$derivative)$BrainGnomes$RecordID, record$RecordID)
  expect_match(paste(readLines(exported$bibliography), collapse = "\n"), "@misc{supplied", fixed = TRUE)
})

test_that("exports reject missing, altered, or escaping companions without publishing", {
  root <- tempfile("provenance-assets-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"), BG_JOB_MANIFEST = "")
  output <- postprocess_subject(fixture$bold, fixture$cfg)
  paths <- derivative_provenance_paths(output)
  expect_error(export_derivative_provenance(output, root), "already exists")
  expect_error(export_derivative_provenance(output, file.path(root, "bad"), datalad = list(unknown = "x")), "datalad")
  methods <- file.path(paths$directory, "methods.md")
  original_methods <- readLines(methods)
  writeLines("unreviewed replacement", methods)
  expect_error(export_derivative_provenance(output, file.path(root, "bad")), "checksum")
  expect_false(dir.exists(file.path(root, "bad")))
  writeLines(original_methods, methods)
  metadata <- read_derivative_provenance(output)
  metadata$BrainGnomes$Assets[[1]]$File <- "../../outside.txt"
  write_json_atomic(metadata, paths$json)
  expect_error(export_derivative_provenance(output, file.path(root, "bad")), "Missing or unsafe")
  unlink(methods)
  expect_error(export_derivative_provenance(output, file.path(root, "bad")), "Missing or unsafe")
})

test_that("skipped AROMA preserves its input and does not acquire an execution citation", {
  root <- tempfile("skipped-aroma-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"), BG_JOB_MANIFEST = "")
  cfg <- fixture$cfg
  cfg$apply_aroma$enable <- TRUE
  expect_warning(output <- postprocess_subject(fixture$bold, cfg), "Cannot find mixing file")
  expect_true(file.exists(fixture$bold))
  record <- read_derivative_provenance(output)$BrainGnomes
  expect_identical(record$Operations[[1]]$Status, "skipped")
  expect_match(record$MethodsText, "apply_aroma was skipped", fixed = TRUE)
  expect_false(any(grepl("BrainGnomes_AROMA_2015", unlist(record$BibliographyParts), fixed = TRUE)))
})

test_that("validation failure retains executed evidence without publishing a successful image", {
  root <- tempfile("provenance-validation-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"), BG_JOB_MANIFEST = "")
  cfg <- fixture$cfg
  cfg$temporal_filter <- list(enable = TRUE, prefix = "f", method = "butterworth", high_pass_hz = 0.01)
  cfg$validate_postproc_steps <- TRUE
  cfg$stop_on_failed_validation <- TRUE
  #' Copy an image for a deliberately failing validation check
  #' @param in_file,out_file Input and output paths.
  #' @param ... Ignored filtering arguments.
  #' @return Copied output path.
  #' @noRd
  copy_filter <- function(in_file, out_file, ...) {
    file.copy(in_file, out_file)
    out_file
  }
  testthat::local_mocked_bindings(temporal_filter = copy_filter,
    validate_temporal_filter = function(...) structure(FALSE, message = "replay mismatch"))
  expect_error(suppressWarnings(postprocess_subject(fixture$bold, cfg)), "validation")
  output <- file.path(cfg$output_dir, sub("desc-preproc", "desc-clean", basename(fixture$bold)))
  expect_false(file.exists(output))
  expect_false(file.exists(derivative_provenance_paths(output)$json))
  attempt <- read_derivative_attempts(output)[[1]]
  expect_identical(attempt$Status, "failed")
  expect_identical(attempt$Operations[[1]]$Status, "executed")
  expect_identical(attempt$Validation[[1]]$status, "failed")
})

test_that("repeated operations and intermediate reuse retain distinct evidence", {
  root <- tempfile("reused-intermediate-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"), BG_JOB_MANIFEST = "")
  cfg <- fixture$cfg
  cfg$overwrite <- FALSE
  cfg$force_processing_order <- TRUE
  cfg$processing_steps <- c("temporal_filter", "temporal_filter")
  cfg$temporal_filter <- list(enable = TRUE, prefix = "f", method = "butterworth", high_pass_hz = 0.01)
  intermediate <- file.path(cfg$output_dir,
    sub("desc-preproc", "desc-fClean", basename(fixture$bold)))
  file.copy(fixture$bold, intermediate)
  #' Copy a final image while preserving the reused intermediate
  #' @param in_file,out_file Input and output paths.
  #' @param ... Ignored filtering arguments.
  #' @return Copied output path.
  #' @noRd
  copy_filter <- function(in_file, out_file, ...) {
    file.copy(in_file, out_file)
    out_file
  }
  testthat::local_mocked_bindings(temporal_filter = copy_filter)
  output <- postprocess_subject(fixture$bold, cfg)
  operations <- read_derivative_provenance(output)$BrainGnomes$Operations
  expect_length(operations, 2)
  expect_identical(operations[[1]]$Status, "reused")
  expect_identical(operations[[1]]$ParameterEvidence, "requested_only")
  expect_identical(operations[[2]]$Status, "executed")
  expect_true(file.exists(intermediate))
})

test_that("uncomputed connectivity and all-missing matrices are described accurately", {
  root <- tempfile("missing-connectivity-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  dir.create(file.path(root, "roi"))
  withr::local_envvar(BG_JOB_MANIFEST = "")
  # Deliberately require more voxels than either region contains.
  expect_warning(missing <- extract_rois(fixture$bold, fixture$atlas,
    out_dir = file.path(root, "roi"), min_vox_per_roi = 100L,
    cor_method = "pearson", rtoz = TRUE)[[1]], "all-NA connectivity")
  record <- read_derivative_provenance(missing$correlation[[1]])$BrainGnomes
  expect_true(record$Operations[[2]]$Parameters$all_missing)
  expect_false(record$Operations[[2]]$Parameters$fisher_z)
  expect_match(record$MethodsText, "all-missing connectivity", fixed = TRUE)
  # With insufficient timepoints, only the skipped attempt is emitted for
  # connectivity; an existing result is not reattributed to this request.
  values <- RNifti::readNifti(fixture$bold)[, , , 1:10]
  RNifti::writeNifti(values, fixture$bold)
  expect_warning(short <- extract_rois(fixture$bold, fixture$atlas,
    out_dir = file.path(root, "roi"), min_vox_per_roi = 1L,
    cor_method = "pearson", overwrite = TRUE)[[1]], "Only 10 timepoints")
  expect_null(short$correlation)
  attempts <- read_derivative_attempts(missing$correlation[[1]])
  skipped <- Filter(function(x) identical(x$Status, "skipped"), attempts)
  expect_length(skipped, 1)
  expect_identical(tail(skipped[[1]]$Operations, 1)[[1]]$Status, "skipped")
})

test_that("PSC provenance publishes compact metadata instead of a NIfTI map", {
  root <- tempfile("psc-provenance-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  RNifti::writeNifti(array(1000, c(8, 8, 4, 30)), fixture$bold)
  fixture$cfg$intensity_normalize <- list(
    enable = TRUE, prefix = "n", mode = "voxel_psc", target = 100
  )
  fixture$cfg$validate_postproc_steps <- TRUE
  fixture$cfg$stop_on_failed_validation <- TRUE
  withr::local_envvar(log_file = file.path(root, "logs", "subject.log"))
  output <- with_mocked_bindings(
    postprocess_subject(fixture$bold, fixture$cfg),
    automask = function(img, outfile, ...) {
      RNifti::writeNifti(array(1L, c(8, 8, 4)), outfile)
      outfile
    },
    intensity_normalize = function(in_file, out_file, scale_file, ...) {
      pre <- RNifti::readNifti(in_file)
      scale <- as.numeric(RNifti::readNifti(scale_file))
      RNifti::writeNifti(pre * scale, out_file)
      out_file
    }
  )
  sidecar <- derivative_provenance_paths(output)$json
  record <- read_derivative_provenance(output)$BrainGnomes
  expect_identical(record$Status, "completed")
  reference <- record$Resolved$IntensityReference
  expect_null(reference$scale_map)
  expect_null(reference$automask_file)
  expect_true(file.exists(file.path(dirname(output), reference$scale_file)))
  expect_true(file.exists(file.path(dirname(output), reference$core_file)))
  expect_equal(unlist(reference$scale_geometry$dim)[2:4], c(8, 8, 4))
  expect_lt(file.info(sidecar)$size, 30000)
  expect_identical(record$Validation[[1]]$status, "passed")

  # Even a direct caller's unsaved in-memory map must not enter provenance.
  reference$scale_map <- RNifti::asNifti(array(0.1, c(8, 8, 4)))
  compact <- intensity_reference_provenance(
    reference, file.path(dirname(output), reference$core_file),
    file.path(dirname(output), reference$sidecar_file),
    file.path(dirname(output), reference$scale_file)
  )
  expect_null(compact$scale_map)
  expect_no_error(jsonlite::toJSON(compact, auto_unbox = TRUE))
})

test_that("attempts and companion publication refuse image payloads and image assets", {
  root <- tempfile("guarded-provenance-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  output <- file.path(fixture$cfg$output_dir, "sub-01_desc-clean_bold.nii.gz")
  file.copy(fixture$bold, output)
  tracker <- derivative_provenance_new(output, fixture$bold, "postprocessing")
  tracker$record$Resolved$UnexpectedImage <- RNifti::readNifti(fixture$bold)
  for (status in c("completed", "failed")) {
    expect_error(derivative_provenance_attempt(tracker, status), "UnexpectedImage")
  }
  expect_error(derivative_provenance_publish(tracker), "UnexpectedImage")
  expect_false(dir.exists(derivative_provenance_paths(output)$directory))
  expect_false(file.exists(derivative_provenance_paths(output)$json))
  tracker$record$Resolved$UnexpectedImage <- NULL
  tracker$assets[["upstream/scale.nii.gz"]] <- fixture$bold
  expect_error(derivative_provenance_publish(tracker), "provenance assets")
  expect_false(dir.exists(derivative_provenance_paths(output)$directory))
  tracker$assets[["upstream/scale.nii.gz"]] <- NULL
  expect_no_error(derivative_provenance_publish(tracker))
  expect_no_error(derivative_provenance_attempt(tracker, "completed"))
  companions <- list.files(derivative_provenance_paths(output)$directory,
                          recursive = TRUE, full.names = TRUE)
  expect_false(any(grepl("\\.nii(\\.gz)?$", companions)))
  for (file in companions[grepl("\\.json$", companions)]) {
    expect_no_error(derivative_provenance_read_json(file))
  }
})

test_that("upstream JSON cannot reintroduce flattened or nested voxel payloads", {
  root <- tempfile("upstream-image-metadata-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- make_derivative_provenance_fixture(root)
  sidecar <- derivative_provenance_paths(fixture$bold)$json
  jsonlite::write_json(list(voxel_values = as.numeric(1:24)), sidecar)
  expect_error(derivative_provenance_new("output.nii.gz", fixture$bold,
               "postprocessing"), "voxel_values")
  jsonlite::write_json(list(Data = array(1:24, c(2, 3, 4))), sidecar)
  expect_error(derivative_provenance_new("output.nii.gz", fixture$bold,
               "postprocessing"), "metadata\\$Data")
})
