# Create a configured project with real recorded failures and logs but no
# scheduler interaction. Returns the configuration; cleanup belongs to caller.
make_json_cli_project <- function() {
  root <- tempfile("json project ")
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  cfg <- setup_project(project_name = "json", project_directory = root, interactive = FALSE)
  for (id in c("01", "02")) dir.create(file.path(cfg$metadata$bids_directory, paste0("sub-", id)))
  container <- file.path(root, "container.sif")
  license <- file.path(root, "license.txt")
  file.create(container, license)
  cfg$fmriprep <- list(enable = TRUE, output_spaces = "MNI152NLin2009cAsym",
    fs_license_file = license, memgb = 8, nhours = 1, ncores = 2)
  cfg$compute_environment$fmriprep_container <- container
  log <- file.path(cfg$metadata$log_directory, "failure_jobid-2001.out")
  writeLines("Human log text, not JSON", log)
  insert_tracked_job(cfg$metadata$sqlite_db, "2001", list(
    job_name = "fmriprep_sub-01", sequence_id = "failed-run", stage = "fmriprep",
    sub_id = "01", status = "FAILED", n_nodes = 1, n_cpus = 2, scheduler = "slurm", stdout_log = log))
  insert_tracked_job(cfg$metadata$sqlite_db, "2002", list(
    job_name = "fmriprep_sub-02", sequence_id = "active-run", stage = "fmriprep",
    sub_id = "02", status = "QUEUED", n_nodes = 1, n_cpus = 2, scheduler = "slurm"))
  cfg <- write_project_config(cfg, overwrite = TRUE)
  execution <- resolve_project_execution(cfg, "fmriprep")
  record_run_provenance(cfg, "failed-run", execution)
  cfg
}

test_that("machine-readable CLI results parse from the entire stdout stream", {
  cfg <- make_json_cli_project()
  root <- cfg$metadata$project_directory
  cases <- list(
    show = c("config", "show", root),
    validate = c("config", "validate", root),
    doctor = c("doctor", root, "--steps=fmriprep"),
    plan = c("plan", root, "--steps=fmriprep"),
    run = c("run", root, "--steps=fmriprep", "--dry-run"),
    status = c("status", root, "--view=jobs"),
    diagnose = c("diagnose", root, "--run=failed-run"),
    retry = c("retry", root, "--run=failed-run", "--dry-run"),
    cancel = c("cancel", root, "--run=active-run", "--dry-run"),
    provenance = c("provenance", root, "--run=failed-run"),
    logs = c("logs", root, "--run=failed-run", "--tail=1")
  )
  before <- list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
  for (mode in c("explicit", "cwd")) {
    for (name in names(cases)) {
      args <- cases[[name]]
      if (mode == "cwd") args <- args[args != root]
      res <- run_brain_gnomes_cli(c(args, "--format=json"), wd = root)
      expect_true(res$status %in% if (name == "doctor") c(0L, 1L) else 0L,
        info = paste(mode, name, paste(res$stderr, collapse = "\n")))
      json <- paste(res$stdout, collapse = "\n")
      expect_true(jsonlite::validate(json), info = paste(mode, name, json))
      parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
      if (name %in% c("run", "plan", "retry")) {
        expect_true(length(parsed$preview$work) > 0L, info = name)
      }
      if (name == "run") expect_match(paste(res$stderr, collapse = "\n"), "Dry run enabled", fixed = TRUE)
      if (name == "logs") expect_match(paste(res$stderr, collapse = "\n"), "Human log text, not JSON", fixed = TRUE)
      if (name == "cancel") expect_identical(parsed[[1L]]$status, "would_cancel")
    }
  }
  expect_identical(list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = TRUE), before)
  human <- run_brain_gnomes_cli(c("run", root, "--steps=fmriprep", "--dry-run"))
  expect_identical(human$status, 0L)
  expect_equal(sum(grepl("Concrete work preview", human$output, fixed = TRUE)), 1L)
})

test_that("current-directory CLI runs retain first options, aliases, and explicit YAML selection", {
  cfg <- make_json_cli_project()
  root <- cfg$metadata$project_directory
  dir.create(file.path(cfg$metadata$bids_directory, "sub-selected"))
  for (command in c("run", "run_project", "plan")) {
    result <- run_brain_gnomes_cli(c(command, "--steps", "fmriprep",
      "--subject-filter=selected", if (command != "plan") "--dry-run", "--format=json"), wd = root)
    expect_identical(result$status, 0L, info = paste(result$stderr, collapse = "\n"))
    plan <- jsonlite::fromJSON(paste(result$stdout, collapse = "\n"))
    expect_identical(plan$request$steps, "fmriprep")
    expect_identical(plan$request$subject_filter, "selected")
    expect_identical(plan$subjects$sub_id, "selected")
  }

  alternate <- file.path(root, "alternate config.yaml")
  contents <- as.list(cfg)
  contents$metadata$project_name <- "alternate"
  yaml::write_yaml(contents, alternate)
  explicit <- run_brain_gnomes_cli(c("config", "show", alternate, "--format=json"), wd = root)
  expect_identical(explicit$status, 0L)
  expect_identical(jsonlite::fromJSON(paste(explicit$stdout, collapse = "\n"))$metadata$project_name,
    "alternate")
  default <- run_brain_gnomes_cli(c("config", "show", "--format=json"), wd = root)
  expect_identical(default$status, 0L)
  expect_identical(jsonlite::fromJSON(paste(default$stdout, collapse = "\n"))$metadata$project_name,
    "json")
})

test_that("machine formats fail before interactive or unsupported operations", {
  cases <- list(
    c("run", "missing-project", "--steps=fmriprep", "--format=invalid"),
    c("run", "missing-project", "--steps=fmriprep", "--format=NULL"),
    c("run", "missing-project", "--steps=fmriprep", "--format=csv"),
    c("run", "missing-project", "--format=json"),
    c("status", "missing-project", "--watch", "--format=json"),
    c("status", "missing-project", "--watch", "--format=csv"),
    c("diagnose", "missing-project", "--interactive", "--format=json")
  )
  for (args in cases) {
    res <- run_brain_gnomes_cli(args)
    expect_identical(res$status, 2L, info = paste(args, collapse = " "))
    expect_length(res$stdout, 0L)
    expect_match(paste(res$stderr, collapse = "\n"), "Usage error:", fixed = TRUE)
  }
  error <- run_brain_gnomes_cli(c("plan", "missing-project", "--format=json"))
  expect_identical(error$status, 1L)
  expect_length(error$stdout, 0L)
  expect_match(paste(error$stderr, collapse = "\n"), "Error:", fixed = TRUE)
})

test_that("empty and single-row machine results stay parseable", {
  cfg <- make_json_cli_project()
  root <- cfg$metadata$project_directory
  empty <- run_brain_gnomes_cli(c("status", root, "--sub-id=sub-01", "--ses-id=absent", "--view=jobs", "--format=json"))
  expect_identical(empty$status, 0L, info = paste(empty$stderr, collapse = "\n"))
  expect_identical(jsonlite::fromJSON(paste(empty$stdout, collapse = "\n"), simplifyVector = FALSE), list())
  single <- run_brain_gnomes_cli(c("status", root, "--run=failed-run", "--view=jobs", "--format=json"))
  expect_identical(single$status, 0L)
  expect_length(jsonlite::fromJSON(paste(single$stdout, collapse = "\n"), simplifyVector = FALSE), 1L)
  csv <- run_brain_gnomes_cli(c("status", root, "--view=jobs", "--format=csv"))
  expect_identical(csv$status, 0L)
  expect_equal(nrow(utils::read.csv(text = paste(csv$stdout, collapse = "\n"))), 2L)
})

test_that("late execution errors discard buffered machine output", {
  cfg <- make_json_cli_project()
  result <- run_brain_gnomes_cli(c("logs", cfg$metadata$project_directory,
    "--run=failed-run", "--tail=not-a-number", "--format=json"))
  expect_identical(result$status, 1L)
  expect_length(result$stdout, 0L)
  expect_match(paste(result$stderr, collapse = "\n"), "Error:", fixed = TRUE)
})

test_that("validation failures retain a parseable report and a nonzero status", {
  cfg <- make_json_cli_project()
  path <- attr(cfg, "yaml_file")
  contents <- yaml::read_yaml(path)
  contents$metadata$project_name <- NULL
  yaml::write_yaml(contents, path)
  result <- run_brain_gnomes_cli(c("config", "validate", path, "--format=json"))
  expect_identical(result$status, 1L)
  expect_true(jsonlite::validate(paste(result$stdout, collapse = "\n")))
  report <- jsonlite::fromJSON(paste(result$stdout, collapse = "\n"))
  expect_false(report$valid)
})

test_that("CLI selected validation, plans, dry runs and submission reject the same errors", {
  cfg <- make_json_cli_project()
  cfg$fmriprep$fs_license_file <- file.path(cfg$metadata$project_directory, "missing-license.txt")
  cfg$fmriprep$ncores <- -1
  cfg <- write_project_config(cfg, overwrite = TRUE)
  root <- cfg$metadata$project_directory
  before <- list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
  validation <- run_brain_gnomes_cli(c("config", "validate", root, "--steps=fmriprep", "--format=json"))
  expect_identical(validation$status, 1L)
  report <- jsonlite::fromJSON(paste(validation$stdout, collapse = "\n"))
  expect_false(report$valid)
  expect_setequal(report$issues$field, c("fmriprep/fs_license_file", "fmriprep/ncores"))
  for (args in list(c("plan", root), c("run", root, "--dry-run"), c("run", root))) {
    result <- run_brain_gnomes_cli(c(args, "--steps=fmriprep", "--format=json"))
    expect_identical(result$status, 1L)
    expect_length(result$stdout, 0L)
    expect_match(paste(result$stderr, collapse = "\n"), "fmriprep/fs_license_file", fixed = TRUE)
    expect_match(paste(result$stderr, collapse = "\n"), "fmriprep/ncores", fixed = TRUE)
  }
  exploratory <- run_brain_gnomes_cli(c("plan", root, "--steps=fmriprep", "--allow-invalid", "--format=json"))
  expect_identical(exploratory$status, 0L)
  expect_false(jsonlite::fromJSON(paste(exploratory$stdout, collapse = "\n"))$validation$valid)
  expect_identical(list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = TRUE), before)
})
