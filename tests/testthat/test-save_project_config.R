test_that("load_project records the YAML source path", {
  tmp_dir <- tempfile("bg_cfg_load_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  yaml_path <- file.path(tmp_dir, "custom_config.yaml")
  yaml::write_yaml(list(a = 1), yaml_path)

  cfg <- load_project(yaml_path, validate = FALSE)
  expect_identical(attr(cfg, "yaml_file"), normalizePath(yaml_path, winslash = "/", mustWork = TRUE))
})

test_that("save_project_config uses stored YAML path", {
  tmp_dir <- tempfile("bg_cfg_save_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  project_dir <- file.path(tmp_dir, "project")
  dir.create(project_dir)

  yaml_path <- file.path(tmp_dir, "custom.yaml")
  scfg <- structure(list(metadata = list(project_directory = project_dir)), class = "bg_project_cfg")
  attr(scfg, "yaml_file") <- yaml_path

  result <- save_project_config(scfg)
  expect_true(file.exists(yaml_path))
  expect_identical(
    normalizePath(attr(result, "yaml_file"), winslash = "/", mustWork = TRUE),
    normalizePath(yaml_path, winslash = "/", mustWork = TRUE)
  )
})

test_that("save_project_config updates YAML path when file argument supplied", {
  tmp_dir <- tempfile("bg_cfg_save_arg_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  project_dir <- file.path(tmp_dir, "project")
  dir.create(project_dir)

  new_yaml <- file.path(tmp_dir, "other.yaml")
  scfg <- structure(list(metadata = list(project_directory = project_dir)), class = "bg_project_cfg")

  result <- save_project_config(scfg, file = new_yaml)
  expect_true(file.exists(new_yaml))
  expect_identical(
    normalizePath(attr(result, "yaml_file"), winslash = "/", mustWork = TRUE),
    normalizePath(new_yaml, winslash = "/", mustWork = TRUE)
  )
})
