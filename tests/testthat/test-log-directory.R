test_that("setup_project_metadata preserves an external log directory", {
  root <- tempfile("log_dir_preserve_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  project_dir <- file.path(root, "project")
  templateflow_dir <- file.path(root, "templateflow")
  external_log_dir <- file.path(root, "shared_logs")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(templateflow_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(external_log_dir, recursive = TRUE, showWarnings = FALSE)

  scfg <- list(
    metadata = list(
      project_name = "testproj",
      project_directory = project_dir,
      templateflow_home = templateflow_dir,
      log_directory = external_log_dir
    )
  )
  class(scfg) <- "bg_project_cfg"

  out <- setup_project_metadata(scfg, fields = "metadata/postproc_directory")

  expect_identical(out$metadata$log_directory, external_log_dir)
  expect_identical(out$metadata$postproc_directory, file.path(project_dir, "data_postproc"))
})

test_that("setup_project_metadata supports editing log_directory directly", {
  root <- tempfile("log_dir_edit_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  project_dir <- file.path(root, "project")
  templateflow_dir <- file.path(root, "templateflow")
  edited_log_dir <- file.path(root, "logs_external")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(templateflow_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(edited_log_dir, recursive = TRUE, showWarnings = FALSE)

  scfg <- list(
    metadata = list(
      project_name = "testproj",
      project_directory = project_dir,
      templateflow_home = templateflow_dir,
      log_directory = file.path(project_dir, "logs")
    )
  )
  class(scfg) <- "bg_project_cfg"

  local_mocked_bindings(
    prompt_directory = function(...) edited_log_dir,
    .package = "BrainGnomes"
  )

  out <- setup_project_metadata(scfg, fields = "metadata/log_directory")

  expect_identical(out$metadata$log_directory, edited_log_dir)
})

test_that("setup_project_metadata preserves explicit sqlite_db", {
  root <- tempfile("sqlite_preserve_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  project_dir <- file.path(root, "project")
  templateflow_dir <- file.path(root, "templateflow")
  sqlite_db <- file.path(root, "SB_W3_preprocessing.sqlite")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(templateflow_dir, recursive = TRUE, showWarnings = FALSE)

  scfg <- list(
    metadata = list(
      project_name = "DSN_SB_preprocessing_W3",
      project_directory = project_dir,
      templateflow_home = templateflow_dir,
      log_directory = file.path(project_dir, "logs"),
      sqlite_db = sqlite_db
    )
  )
  class(scfg) <- "bg_project_cfg"

  out <- setup_project_metadata(scfg, fields = "metadata/postproc_directory")

  expect_identical(out$metadata$sqlite_db, sqlite_db)
})

test_that("setup_project_metadata supports editing sqlite_db directly", {
  root <- tempfile("sqlite_edit_")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  project_dir <- file.path(root, "project")
  templateflow_dir <- file.path(root, "templateflow")
  edited_sqlite_db <- file.path(root, "edited_tracking.sqlite")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(templateflow_dir, recursive = TRUE, showWarnings = FALSE)

  scfg <- list(
    metadata = list(
      project_name = "testproj",
      project_directory = project_dir,
      templateflow_home = templateflow_dir,
      log_directory = file.path(project_dir, "logs"),
      sqlite_db = file.path(project_dir, "testproj.sqlite")
    )
  )
  class(scfg) <- "bg_project_cfg"

  local_mocked_bindings(
    prompt_input = function(...) edited_sqlite_db,
    .package = "BrainGnomes"
  )

  out <- setup_project_metadata(scfg, fields = "metadata/sqlite_db")

  expect_identical(out$metadata$sqlite_db, edited_sqlite_db)
})
