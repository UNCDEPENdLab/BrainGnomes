test_that("project setup writes portable relative paths and loads absolute paths", {
  parent <- tempfile("project-paths-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  root <- file.path(parent, "portable")

  cfg <- setup_project(
    project_name = "portable",
    project_directory = root,
    interactive = FALSE
  )
  yaml_path <- file.path(root, "project_config.yaml")
  raw <- yaml::read_yaml(yaml_path)

  expect_identical(raw$metadata$project_directory, ".")
  expect_identical(raw$metadata$log_directory, "logs")
  expect_identical(raw$metadata$postproc_directory, "data_postproc")
  expect_identical(raw$metadata$sqlite_db, "portable.sqlite")

  elsewhere <- tempfile("project-path-cwd-")
  dir.create(elsewhere)
  withr::local_dir(elsewhere)
  loaded <- load_project(yaml_path, validate = FALSE)

  expect_path_identical(loaded$metadata$project_directory, root)
  expect_path_identical(loaded$metadata$log_directory, file.path(root, "logs"))
  expect_path_identical(
    loaded$metadata$postproc_directory,
    file.path(root, "data_postproc")
  )
  expect_path_identical(
    loaded$metadata$sqlite_db,
    file.path(root, "portable.sqlite"),
    mustWork = FALSE
  )
})

test_that("relative external resources resolve from the project root", {
  parent <- tempfile("project-external-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  root <- file.path(parent, "project")
  shared <- file.path(parent, "shared")
  dir.create(root)
  dir.create(shared)
  dir.create(file.path(shared, "bids"))
  container <- file.path(shared, "fsl.sif")
  file.create(container)
  yaml_path <- file.path(root, "project_config.yaml")
  yaml::write_yaml(list(
    metadata = list(
      project_name = "external",
      project_directory = ".",
      log_directory = "logs",
      bids_directory = "../shared/bids",
      postproc_directory = "data_postproc",
      sqlite_db = "tracking.sqlite"
    ),
    compute_environment = list(fsl_container = "../shared/fsl.sif"),
    postprocess = list(
      enable = TRUE,
      default = list(apply_mask = list(mask_file = "template"))
    )
  ), yaml_path)

  cfg <- load_project(yaml_path, validate = FALSE)
  expect_path_identical(cfg$metadata$bids_directory, file.path(shared, "bids"))
  expect_path_identical(cfg$compute_environment$fsl_container, container)
  expect_identical(cfg$postprocess$default$apply_mask$mask_file, "template")

  cfg <- write_project_config(cfg, overwrite = TRUE)
  saved <- yaml::read_yaml(yaml_path)
  expect_identical(saved$metadata$log_directory, "logs")
  expect_path_identical(saved$metadata$bids_directory, file.path(shared, "bids"))
  expect_path_identical(saved$compute_environment$fsl_container, container)
})

test_that("absolute nonexistent external paths retain one root separator", {
  # Use the native drive root on Windows rather than a POSIX-only rooted path.
  root <- if (.Platform$OS.type == "windows") {
    substr(normalizePath(tempdir(), winslash = "/"), 1L, 3L)
  } else "/"
  path <- paste0(root, "external-assets/atlas-that-does-not-exist.nii.gz")
  expect_identical(BrainGnomes:::normalize_project_path(path), path)
})

test_that("missing configurations report the canonical directory", {
  root <- tempfile("missing-config-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  expect_error(load_project(file.path(root, "."), validate = FALSE),
    paste0("Cannot find file: ", normalizePath(root, winslash = "/")),
    fixed = TRUE)
})

test_that("a duplicated relative project root fails during loading", {
  parent <- tempfile("project-duplicate-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  root <- file.path(parent, "pp_entrypoints")
  dir.create(root)
  yaml_path <- file.path(root, "project_config.yaml")
  yaml::write_yaml(list(
    metadata = list(
      project_name = "pp_entrypoints",
      project_directory = "pp_entrypoints",
      log_directory = "pp_entrypoints/logs"
    )
  ), yaml_path)

  expect_error(
    load_project(yaml_path, validate = FALSE),
    "use `project_directory: .`",
    fixed = TRUE
  )
})

test_that("scheduler handoff rejects unresolved relative operational paths", {
  expect_error(
    BrainGnomes:::assert_absolute_scheduler_paths(
      c(out_dir = "relative/output"), NULL
    ),
    "out_dir=relative/output",
    fixed = TRUE
  )
  expect_true(BrainGnomes:::assert_absolute_scheduler_paths(
    c(out_dir = normalizePath(tempdir(), winslash = "/")), NULL
  ))
})

test_that("the field editor does not offer project_directory", {
  root <- tempfile("project-editor-")
  cfg <- setup_project(
    project_name = "editor",
    project_directory = root,
    interactive = FALSE
  )
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  menus <- list()
  calls <- 0L
  local_mocked_bindings(
    select_list_safe = function(choices, ...) {
      calls <<- calls + 1L
      menus[[calls]] <<- choices
      if (calls == 1L) return("General")
      if (calls == 2L) return(character())
      "Quit & Save"
    },
    .package = "BrainGnomes"
  )

  edit_project(root)
  expect_false(any(grepl("^project_directory ", menus[[2L]])))
})
