test_that("CLI initialization offers guided setup or prompt-free cwd creation", {
  root <- tempfile("cli current directory ")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))

  missing_name <- run_brain_gnomes_cli(c("init", "--overwrite"), wd = root)
  # The test subprocess has no TTY, so guided setup cannot read answers. Reaching
  # the TTY guard confirms that the command entered setup instead of rejecting
  # the omitted name as a usage error.
  expect_identical(missing_name$status, 1L)
  expect_match(paste(missing_name$output, collapse = "\n"), "BrainGnomes project setup", fixed = TRUE)
  expect_match(paste(missing_name$stderr, collapse = "\n"), "TTY-connected Rscript session", fixed = TRUE)
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)

  created <- run_brain_gnomes_cli(c("init", "cwd_demo"), wd = root)
  expect_identical(created$status, 0L, info = paste(created$stderr, collapse = "\n"))
  path <- file.path(root, "project_config.yaml")
  expect_identical(yaml::read_yaml(path)$metadata$project_name, "cwd_demo")
  checksum <- tools::md5sum(path)
  protected <- run_brain_gnomes_cli(c("setup_project", "replacement"), wd = root)
  expect_identical(protected$status, 1L)
  expect_identical(tools::md5sum(path), checksum)
  overwritten <- run_brain_gnomes_cli(c("setup_project", "replacement", "--overwrite"), wd = root)
  expect_identical(overwritten$status, 0L, info = paste(overwritten$stderr, collapse = "\n"))
  expect_identical(yaml::read_yaml(path)$metadata$project_name, "replacement")
})

test_that("omitted project paths preserve usage checks before any project access", {
  root <- tempfile("cli empty directory ")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  commands <- c("config show", "config validate", "config edit", "edit_project",
    "doctor", "plan", "run", "run_project", "validate-bids", "status", "logs",
    "provenance", "diagnose", "retry", "cancel")
  for (command in commands) {
    result <- run_brain_gnomes_cli(c(strsplit(command, " ", fixed = TRUE)[[1]],
      "--definitely-unknown"), wd = root)
    expect_identical(result$status, 2L, info = command)
    expect_match(paste(result$stderr, collapse = "\n"), "Unknown option", fixed = TRUE, info = command)
  }
  for (command in c("retry", "cancel")) {
    result <- run_brain_gnomes_cli(c(command, "--run=latest"), wd = root)
    expect_identical(result$status, 2L)
    expect_match(paste(result$stderr, collapse = "\n"), "requires --dry-run or --yes", fixed = TRUE)
  }
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("CLI defaults never discover a project in a parent directory", {
  root <- tempfile("cli parent project ")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  cfg <- setup_project(project_name = "parent", project_directory = root, interactive = FALSE)
  child <- file.path(root, "child")
  dir.create(child)
  for (command in c("config show", "config edit", "edit_project", "plan", "run",
                    "validate-bids", "status", "logs", "provenance", "diagnose")) {
    result <- run_brain_gnomes_cli(strsplit(command, " ", fixed = TRUE)[[1]], wd = child)
    expect_identical(result$status, 1L, info = command)
    # load_project() and the lifecycle adapter phrase missing configurations
    # differently; both must fail on this directory, not load the parent.
    output <- paste(result$output, collapse = "\n")
    expect_match(output, "No project_config.yaml found in project directory|Cannot find file:", info = command)
    expect_match(output, normalizePath(child, winslash = "/"), fixed = TRUE, info = command)
  }
  expect_length(list.files(child, all.files = TRUE, no.. = TRUE), 0L)
})
