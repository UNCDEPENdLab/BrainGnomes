test_that("template directory normalization resolves parents without creating missing paths", {
  root <- tempfile("template-path-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  canonical <- normalizePath(root, winslash = "/", mustWork = TRUE)
  missing <- file.path(root, "future", "dicoms")
  expect_identical(normalize_template_directory(root), canonical)
  expect_identical(normalize_template_directory(missing), file.path(canonical, "future", "dicoms"))
  if (.Platform$OS.type == "windows") {
    expect_identical(normalize_template_directory(chartr("/", "\\", missing)),
      file.path(canonical, "future", "dicoms"))
  }
  withr::local_dir(root)
  expect_identical(normalize_template_directory("future/./dicoms"),
    file.path(canonical, "future", "dicoms"))
  expect_identical(normalize_template_directory("future/../dicoms"), file.path(canonical, "dicoms"))
  expect_false(dir.exists(file.path(root, "future")))
  expect_false(dir.exists(file.path(root, "dicoms")))
})

test_that("template clones rebase missing descendants through directory aliases", {
  root <- tempfile("template-alias-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  real <- file.path(root, "real")
  alias <- file.path(root, "alias")
  dir.create(real)
  skip_if_not(suppressWarnings(file.symlink(real, alias)), "Directory symlinks unavailable")

  cfg <- setup_project(project_name = "source", project_directory = file.path(real, "source"),
    interactive = FALSE)
  cfg$metadata$project_directory <- file.path(alias, "source")
  cfg$metadata$postproc_directory <- file.path(alias, "source", "future", "postproc")
  cfg$metadata$flywheel_sync_directory <- file.path(alias, "shared")
  dir.create(cfg$metadata$flywheel_sync_directory)
  cfg$metadata$dicom_directory <- file.path(alias, "shared", "incoming", "dicoms")
  cfg$flywheel_sync$enable <- TRUE
  before <- list.files(real, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)

  clone <- setup_project(project_name = "clone", project_directory = file.path(root, "clone"),
    template = cfg, interactive = FALSE)
  expect_path_identical(clone$metadata$postproc_directory,
    file.path(clone$metadata$project_directory, "future", "postproc"))
  expect_path_identical(clone$metadata$dicom_directory,
    file.path(clone$metadata$flywheel_sync_directory, "incoming", "dicoms"))
  expect_identical(list.files(real, recursive = TRUE, all.files = TRUE, include.dirs = TRUE), before)
  expect_false(dir.exists(cfg$metadata$dicom_directory))
  expect_false(dir.exists(cfg$metadata$postproc_directory))
})
