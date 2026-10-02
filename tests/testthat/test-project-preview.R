# Build a two-subject, two-session fixture for read-only preview assertions.
# Returns a configuration; the caller owns cleanup via its deferred handler.
make_preview_project <- function() {
  root <- tempfile("concrete-preview-")
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  cfg <- setup_project(project_name = "preview", project_directory = root, interactive = FALSE)
  for (id in c("01", "02")) for (session in c("1", "2")) {
    dir.create(file.path(cfg$metadata$bids_directory, paste0("sub-", id), paste0("ses-", session)), recursive = TRUE)
  }
  container <- file.path(root, "container.sif")
  file.create(container)
  cfg$fmriprep <- list(enable = TRUE, output_spaces = "MNI152NLin2009cAsym",
    fs_license_file = container, ncores = 2, memgb = 8, nhours = 1)
  cfg$compute_environment$fmriprep_container <- container
  cfg$compute_environment$fsl_container <- container
  cfg$postprocess <- list(enable = TRUE, clean = list(
    input_regex = "desc:preproc suffix:bold", bids_desc = "clean", ncores = 1, memgb = 4, nhours = 0.5))
  cfg
}

test_that("previews show exact units, resources, paths, and completion decisions", {
  cfg <- make_preview_project()
  marker <- file.path(cfg$metadata$log_directory, "sub-01", ".fmriprep_sub-01_complete")
  dir.create(dirname(marker))
  file.create(marker)
  before <- list.files(cfg$metadata$project_directory, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
  execution <- resolve_project_execution(cfg, c("fmriprep", "postprocess"))
  plan <- project_plan_from_execution(cfg, execution)
  work <- plan$preview$work
  expect_equal(nrow(work[work$stage == "fmriprep", ]), 2L)
  expect_equal(nrow(work[work$stage == "postprocess", ]), 4L)
  row <- work[work$stage == "fmriprep" & !is.na(work$sub_id) & work$sub_id == "01", ]
  expect_identical(row$action, "would_skip")
  expect_path_identical(row$completion_marker, marker)
  expect_path_identical(row$input_directory, file.path(cfg$metadata$bids_directory, "sub-01"))
  expect_identical(row$ncores, 2)
  expect_identical(row$memgb, 8)
  expect_identical(row$nhours, 1)
  expect_match(row$depends_on, "fsaverage_setup", fixed = TRUE)
  expect_identical(plan$preview$counts$would_submit, 5L)
  expect_identical(plan$preview$counts$would_skip, 1L)
  execution$force <- TRUE
  forced <- build_project_preview(cfg, execution, plan$jobs)
  expect_identical(forced$counts$would_skip, 0L)
  expect_identical(forced$counts$would_submit, 6L)
  expect_identical(list.files(cfg$metadata$project_directory, recursive = TRUE, all.files = TRUE, include.dirs = TRUE), before)
})

test_that("direct dry runs return concrete plans without creating destinations", {
  cfg <- make_preview_project()
  cfg$metadata$log_directory <- file.path(cfg$metadata$project_directory, "future", "logs")
  cfg$metadata$scratch_directory <- file.path(cfg$metadata$project_directory, "future", "scratch")
  local_mocked_bindings(
    setup_project_directories = function(...) stop("Preview attempted directory creation"),
    record_run_provenance = function(...) stop("Preview attempted provenance writes"),
    cluster_job_submit = function(...) stop("Preview attempted submission"),
    .package = "BrainGnomes"
  )
  plan <- run_project(cfg, steps = "fmriprep", dry_run = TRUE)
  expect_s3_class(plan, "bg_project_plan")
  expect_equal(nrow(plan$preview$work), 4L)
  expect_false(dir.exists(dirname(cfg$metadata$log_directory)))
  expect_false(file.exists(cfg$metadata$sqlite_db))
})

test_that("deferred previews do not treat partial subjects as the final scope", {
  cfg <- make_preview_project()
  cfg$flywheel_sync <- list(enable = TRUE, source_url = "fw://example/project/")
  cfg$metadata$flywheel_sync_directory <- file.path(cfg$metadata$project_directory, "future-dicoms")
  execution <- resolve_project_execution(cfg, c("flywheel_sync", "fmriprep"))
  plan <- project_plan_from_execution(cfg, execution)
  work <- plan$preview$work
  downstream <- work[work$stage == "fmriprep", ]
  expect_identical(downstream$action, "deferred")
  expect_true(is.na(downstream$sub_id))
  expect_true(is.na(plan$jobs$n_jobs[plan$jobs$stage == "fmriprep"]))
  expect_identical(plan$preview$counts$deferred, 1L)
  expect_identical(work$input_directory[work$stage == "flywheel_sync"], "fw://example/project/")
  expect_false(dir.exists(cfg$metadata$flywheel_sync_directory))
})

test_that("retry previews keep exact pairs and remain serializable", {
  cfg <- make_preview_project()
  units <- data.frame(stage = "postprocess", stream = "clean", sub_id = c("01", "02"), ses_id = c("1", "2"))
  attr(cfg, "retry_work_units") <- units
  execution <- resolve_project_execution(cfg, "postprocess", force = TRUE)
  plan <- project_plan_from_execution(cfg, execution)
  expect_identical(plan$preview$work[, names(units)], units)
  file <- file.path(cfg$metadata$project_directory, "preview.yaml")
  write_project_plan(plan, file)
  restored <- read_project_plan(file)
  expect_equal(restored$preview$work, plan$preview$work)
  output <- paste(capture.output(print_project_preview(plan$preview, limit = 1L)), collapse = "\n")
  expect_match(output, "1 more rows", fixed = TRUE)
  expect_match(output, "Output root:", fixed = TRUE)
  expect_match(output, "not an exact scheduler job list", fixed = TRUE)
})

test_that("large dry runs bound console rows but retain their full scope", {
  cfg <- make_preview_project()
  for (id in sprintf("%02d", 3:25)) {
    dir.create(file.path(cfg$metadata$bids_directory, paste0("sub-", id)))
  }
  output <- paste(capture.output(plan <- run_project(cfg, steps = "fmriprep", dry_run = TRUE)), collapse = "\n")
  expect_s3_class(plan, "bg_project_plan")
  expect_equal(plan$preview$counts$would_submit, 25L)
  expect_true("25" %in% plan$preview$work$sub_id)
  expect_match(output, "more subject/session rows", fixed = TRUE)
  expect_false(grepl("sub-25", output, fixed = TRUE))
})
