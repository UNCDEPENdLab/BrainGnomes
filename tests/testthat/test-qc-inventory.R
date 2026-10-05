# Build a minimal study with real filesystem evidence and an isolated database.
make_qc_fixture <- function(root = tempfile("qc study ")) {
  dirs <- file.path(root, c("bids", "fmriprep", "postproc", "rois", "mriqc", "logs"))
  for (path in dirs) dir.create(path, recursive = TRUE)
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)
  cfg <- structure(list(metadata = list(project_name = "QC example",
    project_directory = root, bids_directory = dirs[1], fmriprep_directory = dirs[2],
    postproc_directory = dirs[3], rois_directory = dirs[4], mriqc_directory = dirs[5],
    log_directory = dirs[6], sqlite_db = db),
    fmriprep = list(enable = TRUE), mriqc = list(enable = TRUE),
    postprocess = list(enable = TRUE, clean = list(bids_desc = "clean",
      input_regex = "desc:preproc suffix:bold", validate_postproc_steps = TRUE)),
    extract_rois = list(enable = TRUE, network = list(input_streams = "clean",
      atlases = file.path(root, "atlas_demo.nii.gz"), save_ts = TRUE,
      save_diagnostics = TRUE, correlation = list(method = "pearson")))),
    class = "bg_project_cfg")
  list(root = root, cfg = cfg, db = db)
}

# Write small evidence files; the collector must never load their image arrays.
write_qc_fixture_file <- function(path, text = "placeholder image") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, path)
  normalizePath(path, winslash = "/")
}

test_that("QC path identities normalize filesystem aliases", {
  root <- tempfile("qc-path-identity-")
  path <- write_qc_fixture_file(file.path(root, "nested", "artifact.txt"))
  on.exit(unlink(root, recursive = TRUE))
  alias <- file.path(root, "nested", ".", "artifact.txt")

  expect_identical(
    BrainGnomes:::qc_path_key(path),
    BrainGnomes:::qc_path_key(alias)
  )
})

test_that("QC path identities normalize missing targets and preserve sentinels", {
  root <- tempfile("qc-missing-identity-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  target <- file.path(root, "future", "artifact.nii.gz")
  alias <- file.path(root, ".", "future", "artifact.nii.gz")
  expect_identical(qc_path_key(c(alias, NA_character_, "")),
    c(qc_path_key(target), NA_character_, ""))
  expect_false(file.exists(target))
})

# Add one acquisition, matching derivative evidence, and an optional failed job.
populate_qc_fixture <- function(fixture, subject = "001", run = "01", failed = FALSE) {
  cfg <- fixture$cfg
  prefix <- paste0("sub-", subject, "_ses-A_task-rest_acq-fast_dir-AP_run-", run)
  raw <- write_qc_fixture_file(file.path(cfg$metadata$bids_directory,
    paste0("sub-", subject), "ses-A", "func", paste0(prefix, "_bold.nii.gz")))
  preproc <- write_qc_fixture_file(file.path(cfg$metadata$fmriprep_directory,
    paste0("sub-", subject), "ses-A", "func", paste0(prefix, "_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz")))
  confounds <- write_qc_fixture_file(qc_find_confounds(preproc),
    c("framewise_displacement\ttrans_x", "n/a\t0", "0.1\t1", "0.5\t2", "0.3\t3"))
  expected <- qc_expected_derivatives(cfg, preproc, new.env())
  expected <- expected[expected$sub_id == subject, , drop = FALSE]
  pp <- expected$derivative_file[expected$stage == "postprocess"]
  rois <- expected$derivative_file[expected$stage == "extract_rois"]
  if (!failed) {
    write_qc_fixture_file(pp)
    for (roi in rois) write_qc_fixture_file(roi, "roi1\troi2\n0.1\tNA")
  }
  censor <- write_qc_fixture_file(get_censor_file(as.list(extract_bids_info(pp))), c("1", "0", "1", "1"))
  audit <- file.path(cfg$metadata$log_directory, paste0("sub-", subject),
    sub("\\.nii.gz$", "_postproc-validation.json", basename(pp)))
  dir.create(dirname(audit), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(schema_version = "postproc-validation-v1",
    intended_final_file = pp, pipeline_completed = !failed,
    overall_status = if (failed) "failed" else "passed",
    checks = list(list(step = "apply_mask", status = if (failed) "failed" else "passed",
      message = "Mask replay result"))), audit, auto_unbox = TRUE)
  diag <- sub("_timeseries.tsv$", "_roidiagnostics.tsv", rois[grepl("_timeseries.tsv$", rois)])
  write_qc_fixture_file(diag, c("roi\tretained\tproportion_usable\texclusion_reason",
    "roi1\tTRUE\t0.95\tNA", "roi2\tFALSE\t0.02\tbelow_threshold"))
  report <- write_qc_fixture_file(file.path(cfg$metadata$fmriprep_directory,
    paste0("sub-", subject, ".html")), "<html>Evidence report</html>")
  mriqc <- file.path(cfg$metadata$mriqc_directory, paste0("sub-", subject), "ses-A",
    "func", paste0(prefix, "_bold.json"))
  dir.create(dirname(mriqc), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(fd_mean = 0.25, tsnr = 43.2, dvars_std = 1.1), mriqc, auto_unbox = TRUE)
  insert_tracked_job(fixture$db, paste0(subject, run), list(
    job_name = paste0("postprocess_clean_sub-", subject, "_ses-A"),
    sequence_id = "run-current", status = if (failed) "FAILED" else "COMPLETED",
    stage = "postprocess", stream = "clean", sub_id = subject, ses_id = "A"))
  list(raw = raw, preproc = preproc, pp = pp, rois = rois, audit = audit,
    confounds = confounds, censor = censor, diag = diag, report = report, mriqc = mriqc)
}

test_that("QC retains failed audits for missing outputs under symlink aliases", {
  skip_on_os("windows") # Creating directory symlinks requires extra privileges.
  root <- tempfile("qc-real-root-")
  dir.create(root)
  alias <- paste0(root, "-alias")
  on.exit(unlink(c(alias, root), recursive = TRUE))
  skip_if_not(file.symlink(root, alias), "Directory symlinks unavailable")
  fixture <- make_qc_fixture(alias)
  paths <- populate_qc_fixture(fixture, failed = TRUE)
  expect_false(file.exists(paths$pp))

  result <- collect_qc_inventory(fixture$cfg)
  expect_identical(subset(result$inventory, stage == "postprocess")$validation_status,
    "failed")
  expect_false(any(grepl("matching target", result$issues$message, fixed = TRUE)))

  # A different missing filename must remain distinct, even under the alias.
  audit <- jsonlite::read_json(paths$audit)
  audit$intended_final_file <- paste0(paths$pp, "-different")
  jsonlite::write_json(audit, paths$audit, auto_unbox = TRUE)
  unmatched <- collect_qc_inventory(fixture$cfg)
  expect_identical(subset(unmatched$inventory, stage == "postprocess")$validation_status,
    "invalid")
})

test_that("empty projects produce typed inventories and retain explicit expectations", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  result <- collect_qc_inventory(fixture$cfg)
  expect_s3_class(result, "bg_qc_inventory")
  expect_equal(nrow(result$inventory), 0L)
  expect_type(result$inventory$derivative_file, "character")
  expect_type(result$metrics$value, "double")
  empty_scope <- collect_qc_inventory(fixture$cfg, subjects = data.frame(sub_id = character()))
  expect_equal(nrow(empty_scope$workflow), 0L)
  result <- collect_qc_inventory(fixture$cfg, subjects = data.frame(sub_id = "007", ses_id = "baseline"))
  expect_equal(nrow(result$workflow), 4L)
  expect_true(all(result$workflow$status == "NOT_TRACKED"))
  expect_identical(unique(result$workflow$sub_id), "007")
  expect_output(print(result), "4 workflow units")
})

test_that("collection preserves identities, missing outputs, evidence, and metric definitions", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  populate_qc_fixture(fixture, "002", failed = TRUE)
  before <- unname(tools::md5sum(fixture$db))
  result <- collect_qc_inventory(fixture$cfg)
  expect_identical(unname(tools::md5sum(fixture$db)), before)
  expect_equal(nrow(result$acquisitions), 2L)
  expect_setequal(result$acquisitions$sub_id, c("001", "002"))
  expect_true(all(result$acquisitions$acquisition == "fast"))
  pp <- subset(result$inventory, stage == "postprocess")
  expect_identical(pp$output_status, c("available", "missing"))
  expect_identical(pp$validation_status, c("passed", "failed"))
  expect_true(pp$attention[2L])
  expect_equal(subset(result$metrics, metric == "mean_fd")$value, c(0.3, 0.3))
  expect_equal(subset(result$metrics, metric == "mean_fd")$n_missing, c(1L, 1L))
  expect_true(all(subset(result$metrics, metric == "censor_retained_percent")$value == 75))
  expect_true(all(subset(result$metrics, metric == "roi_retained_percent")$value == 50))
  expect_true(all(subset(result$metrics, metric == "mriqc_tsnr")$value == 43.2))
  expect_true(all(subset(result$inventory, stage == "extract_rois")$diagnostics_status == "roi_loss"))
  report_ids <- subset(result$evidence, path == paths$report)$record_id
  expect_true(all(result$inventory$sub_id[match(report_ids, result$inventory$record_id)] == "001"))
  expect_false(any(c("reviewer", "decision", "included") %in% names(result$inventory)))
})

test_that("malformed and absent evidence cannot become a QC pass", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  writeLines("{invalid json", paths$audit)
  writeLines(c("1", "9"), paths$censor)
  writeLines("wrong\tcolumns\n1\t2", paths$diag)
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(subset(result$inventory, stage == "postprocess")$validation_status, "invalid")
  expect_true(all(subset(result$inventory, stage == "extract_rois")$diagnostics_status == "invalid"))
  expect_false(any(result$metrics$metric == "censor_retained_percent"))
  expect_gte(nrow(result$issues), 3L)
  unlink(paths$audit)
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(subset(result$inventory, stage == "postprocess")$validation_status, "unavailable")
})

test_that("historical manifest associations do not imply production by the latest attempt", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  manifest <- capture_output_manifest(dirname(paths$pp), files = paths$pp)
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  DBI::dbExecute(con, "UPDATE job_tracking SET output_manifest = ?, time_submitted = '2026-09-01 10:00:00'", params = list(manifest))
  insert_tracked_job(fixture$db, "retry", list(job_name = "postprocess_clean_sub-001_ses-A",
    sequence_id = "run-retry", status = "FAILED", stage = "postprocess", stream = "clean", sub_id = "001", ses_id = "A"))
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00' WHERE job_id = 'retry'")
  DBI::dbDisconnect(con)
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(subset(result$workflow, stage == "postprocess")$status, "FAILED")
  expect_equal(subset(result$inventory, stage == "postprocess")$output_status, "available")
  expect_equal(subset(result$inventory, stage == "postprocess")$workflow_status, "FAILED")
  expect_equal(subset(result$inventory, stage == "postprocess")$workflow_scope, "participant_session")
  expect_false(result$manifest_files$is_current_attempt)
  expect_true(result$manifest_files$matches_recorded_file)
  writeLines("changed image bytes", paths$pp)
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(result$manifest_files$status, "changed")
  expect_equal(nrow(result$jobs), 2L)
})

test_that("TSV and RDS exports retain the same snapshot and protect prior snapshots", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture)
  result <- collect_qc_inventory(fixture$cfg)
  directory <- file.path(fixture$root, "export")
  expect_path_identical(write_qc_inventory(result, directory), directory)
  expect_identical(readRDS(file.path(directory, "inventory.rds")), result)
  observed <- data.table::fread(file.path(directory, "inventory.tsv"), colClasses = "character")
  expect_setequal(observed$record_id, result$inventory$record_id)
  expect_true(all(observed$sub_id == "001"))
  expect_error(write_qc_inventory(result, directory), "new or empty")
  metadata <- jsonlite::read_json(file.path(directory, "snapshot.json"))
  expect_equal(metadata$snapshot_id, result$metadata$snapshot_id)
})

test_that("interactive components handle empty data and escape report content", {
  skip_if_not_installed("reactable")
  skip_if_not_installed("plotly")
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  empty <- collect_qc_inventory(fixture$cfg)
  expect_s3_class(qc_dashboard_widgets(empty, fixture$root)$inventory, "reactable")
  populate_qc_fixture(fixture)
  result <- collect_qc_inventory(fixture$cfg)
  widgets <- qc_dashboard_widgets(result, fixture$root)
  expect_s3_class(widgets$inventory, "reactable")
  expect_s3_class(widgets$metric_plot, "plotly")
  link <- qc_evidence_links(data.frame(kind = "<script>bad</script>",
    path = file.path(fixture$root, "a & b.html"), exists = TRUE), fixture$root)
  markup <- as.character(link)
  expect_match(markup, "&lt;script&gt;", fixed = TRUE)
  expect_match(markup, "a%20%26%20b.html", fixed = TRUE)
})

test_that("raw identities and missing upstream work survive collection", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  for (echo in c("1", "2")) write_qc_fixture_file(file.path(
    fixture$cfg$metadata$bids_directory, "sub-010", "ses-B", "func",
    paste0("sub-010_ses-B_task-rest_acq-fast_rec-test_dir-PA_run-02_echo-", echo, "_part-mag_bold.nii.gz")))
  scope_path <- file.path(fixture$cfg$metadata$log_directory, "runs", "planned", "subjects.tsv")
  write_qc_fixture_file(scope_path, c("sub_id\tses_id", "009\tA"))
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(nrow(result$acquisitions), 2L)
  expect_setequal(result$acquisitions$echo, c("1", "2"))
  expect_true(all(result$acquisitions$part == "mag"))
  expect_setequal(result$workflow$sub_id, c("009", "010"))
  expect_equal(nrow(result$inventory), 0L)
  expect_true(all(result$workflow$status == "NOT_TRACKED"))
})

test_that("existing native products remain visible when upstream data are absent", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  unlink(paths$preproc)
  result <- collect_qc_inventory(fixture$cfg)
  expect_true(norm_path(paths$pp) %in% norm_path(result$inventory$derivative_file))
  expect_true(all(norm_path(paths$rois) %in% norm_path(result$inventory$derivative_file)))
  expect_true(all(is.na(result$inventory$source_file)))
  expect_true(all(result$inventory$expectation_basis == "discovered"))
})

test_that("shared stream destinations and malformed manifests are explicit issues", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture)
  fixture$cfg$postprocess$duplicate <- fixture$cfg$postprocess$clean
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  DBI::dbExecute(con, "UPDATE job_tracking SET output_manifest = ?", params = list(
    '{"output_dir":"/tmp","files":[{"path":"missing.nii.gz"}]}'))
  DBI::dbDisconnect(con)
  result <- collect_qc_inventory(fixture$cfg)
  expect_true(all(subset(result$inventory, stage == "postprocess")$association_status == "shared_destination"))
  expect_true(any(grepl("share this destination", result$issues$message)))
  expect_true(any(grepl("lacks a valid path", result$issues$message)))
})

test_that("Quarto can render a snapshot without a project or running R server", {
  skip_if(Sys.getenv("BRAINGNOMES_TEST_QUARTO") != "true", "Set BRAINGNOMES_TEST_QUARTO=true for real Quarto rendering")
  skip_if(!nzchar(Sys.which("quarto")), "Quarto is unavailable")
  for (package in c("reactable", "plotly", "htmltools", "htmlwidgets", "knitr", "rmarkdown")) skip_if_not_installed(package)
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  result <- collect_qc_inventory(fixture$cfg)
  html <- render_qc_dashboard(result, file.path(fixture$root, "dashboard"))
  expect_true(file.exists(html))
  expect_match(paste(readLines(html, warn = FALSE), collapse = "\n"), "reactable")
  expect_true(file.exists(file.path(dirname(html), "inventory.tsv")))
  expect_identical(readRDS(file.path(dirname(html), "inventory.rds")), result)
})

test_that("unknown atlas identities do not pool different diagnostic directories", {
  skip_if_not_installed("plotly")
  inventory <- data.frame(record_id = c("a", "b"), sub_id = c("001", "002"),
    ses_id = NA_character_, run = "01", stage = "extract_rois", stream = NA_character_,
    input_stream = NA_character_, atlas = NA_character_, task = "rest",
    space = "MNI", resolution = NA_character_)
  metrics <- data.frame(record_id = c("a", "b"), metric = "roi_retained_percent",
    source_file = c("/rois/atlasA/sub-001_roidiagnostics.tsv", "/rois/atlasB/sub-002_roidiagnostics.tsv"),
    value = c(90, 95), unit = "%", n_observations = 10L, n_missing = 0L)
  plot <- plotly::plotly_build(qc_metrics_plot(metrics, inventory))
  expect_length(plot$x$data, 2L)
})
