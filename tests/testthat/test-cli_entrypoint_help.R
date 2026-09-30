run_brain_gnomes_cli <- function(args = character()) {
  script <- system.file("BrainGnomes", package = "BrainGnomes")
  if (!nzchar(script)) {
    script <- normalizePath(file.path("inst", "BrainGnomes"), mustWork = TRUE)
  }

  out <- suppressWarnings(system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(script, args),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = out)
}

test_that("BrainGnomes --help prints global help", {
  res <- run_brain_gnomes_cli("--help")
  expect_equal(res$status, 0L)
  expect_true(any(grepl("^Usage: BrainGnomes <command> \\[options\\]$", res$output)))
  expect_true(any(grepl("^Typical workflow:$", res$output)))
  expect_true(any(grepl("^  setup_project <project_name>", res$output)))
  expect_true(any(grepl("^  run_project <project_directory", res$output)))
  expect_true(any(grepl("^Optional inspection and automation:$", res$output)))
  expect_true(any(grepl("^  doctor <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("^  plan <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("^  validate-bids <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("^  provenance <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("^  retry <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("Use 'BrainGnomes help <command>'", res$output, fixed = TRUE)))
  expect_true(any(grepl("Config, doctor, and plan are optional", res$output, fixed = TRUE)))
  expect_false(any(grepl("--steps=.*bids_validation", res$output)))
})

test_that("BrainGnomes help run_project prints command help", {
  res <- run_brain_gnomes_cli(c("help", "run_project"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl("^Usage: BrainGnomes run_project <project_directory\\|config\\.yaml\\|plan\\.yaml> \\[options\\]$", res$output)))
  expect_true(any(grepl("^Alias: BrainGnomes run <project_directory", res$output)))
  expect_true(any(grepl("^Options:$", res$output)))
  expect_true(any(grepl("schema_version brain-gnomes-plan-v1", res$output, fixed = TRUE)))
  expect_true(any(grepl("standard path", res$output, fixed = TRUE)))
  expect_true(any(grepl("same stages, streams, scope", res$output, fixed = TRUE)))
})

test_that("BrainGnomes run_project --help prints command help", {
  res <- run_brain_gnomes_cli(c("run_project", "--help"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl("^Usage: BrainGnomes run_project <project_directory\\|config\\.yaml\\|plan\\.yaml> \\[options\\]$", res$output)))
  expect_true(any(grepl("^  --debug", res$output)))
  expect_true(any(grepl("^  --force", res$output)))
  expect_true(any(grepl("^  --dry-run", res$output)))
  expect_true(any(grepl("^  --extract-streams=<streams>", res$output)))
  expect_true(any(grepl("^  --log-level=<level>", res$output)))
  expect_false(any(grepl("--steps=.*bids_validation", res$output)))
})

test_that("run_project CLI forwards selected extraction streams", {
  captured <- NULL
  cfg <- structure(list(metadata = list(project_name = "cli-test")), class = "bg_project_cfg")

  local_mocked_bindings(
    load_project = function(input) {
      expect_identical(input, "/proj/example")
      cfg
    },
    run_project = function(scfg, ...) {
      captured <<- c(list(scfg = scfg), list(...))
      TRUE
    },
    .package = "BrainGnomes"
  )

  cli_args <- parse_cli_args(c(
    "--steps=extract_rois",
    "--extract_streams=rest task",
    "--dry-run"
  ))
  expect_true(BrainGnomes:::run_project_cli("/proj/example", cli_args))
  captured_cfg <- captured$scfg
  attr(captured_cfg, "provenance_context") <- NULL
  expect_identical(captured_cfg, cfg)
  expect_identical(
    attr(captured$scfg, "provenance_context")$interface,
    "cli"
  )
  expect_identical(captured$steps, "extract_rois")
  expect_identical(captured$extract_streams, c("rest", "task"))
  expect_true(captured$dry_run)
})

test_that("BrainGnomes status --help prints command help", {
  res <- run_brain_gnomes_cli(c("status", "--help"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl("^Usage: BrainGnomes status <project_directory\\|config\\.yaml> \\[options\\]$", res$output)))
  expect_true(any(grepl("^  --sub-id=<id>", res$output)))
  expect_true(any(grepl("^  --summary", res$output)))
  expect_true(any(grepl("^  --run=<id\\|latest>", res$output)))
  expect_true(any(grepl("^  --view=<view>", res$output)))
  expect_true(any(grepl("^  --watch", res$output)))
})

test_that("BrainGnomes lifecycle commands have command-specific help", {
  for (command in c("init", "config", "doctor", "plan", "validate-bids", "provenance", "logs", "diagnose", "retry", "cancel")) {
    res <- run_brain_gnomes_cli(c(command, "--help"))
    expect_equal(res$status, 0L, info = command)
    displayed_command <- if (command == "init") "setup_project" else command
    expect_true(
      any(grepl(paste0("^Usage: BrainGnomes ", displayed_command), res$output)),
      info = command
    )
  }
})

test_that("BrainGnomes provenance help describes the complete record", {
  res <- run_brain_gnomes_cli(c("provenance", "--help"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl(
    "one run's request and realized work",
    res$output, fixed = TRUE
  )))
  expect_true(any(grepl("job manifests", res$output, fixed = TRUE)))
  expect_true(any(grepl("runtime receipts", res$output, fixed = TRUE)))
  expect_true(any(grepl("^  --run=<id\\|latest>", res$output)))
  expect_true(any(grepl("^  --format=table\\|json", res$output)))
})

test_that("BrainGnomes diagnose help distinguishes guided and prompt-free use", {
  res <- run_brain_gnomes_cli(c("diagnose", "--help"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl("newest attempt for each work unit", res$output, fixed = TRUE)))
  expect_true(any(grepl("guided dependency and log browser", res$output, fixed = TRUE)))
  expect_true(any(grepl("--subject-id=<id>", res$output, fixed = TRUE)))
  expect_true(any(grepl("--job-id=<id>", res$output, fixed = TRUE)))
})

test_that("BrainGnomes status help exposes focused active-job inspection", {
  res <- run_brain_gnomes_cli(c("status", "--help"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl("every resolution", res$output, fixed = TRUE)))
  expect_true(any(grepl("--refresh", res$output, fixed = TRUE)))
  expect_true(any(grepl("active, reconciliation", res$output, fixed = TRUE)))
})

test_that("inspection command help makes optional use cases explicit", {
  expected <- list(
    config = c("Optional inspection tooling", "not required before run_project"),
    doctor = c("Optional comprehensive preflight", "not required before run_project"),
    plan = c("Optional inspection/persistence", "not required before a direct run")
  )
  for (command in names(expected)) {
    res <- run_brain_gnomes_cli(c(command, "--help"))
    expect_equal(res$status, 0L, info = command)
    for (phrase in expected[[command]]) {
      expect_true(any(grepl(phrase, res$output, fixed = TRUE)), info = paste(command, phrase))
    }
  }

  retry <- run_brain_gnomes_cli(c("retry", "--help"))
  expect_equal(retry$status, 0L)
  expect_true(any(grepl("separate new run", retry$output, fixed = TRUE)))
  expect_true(any(grepl("original run is unchanged", retry$output, fixed = TRUE)))
  expect_true(any(grepl("blocked by an earlier failure", retry$output, fixed = TRUE)))

  cancel <- run_brain_gnomes_cli(c("cancel", "--help"))
  expect_equal(cancel$status, 0L)
  expect_true(any(grepl("project data and outputs are not deleted", cancel$output, fixed = TRUE)))
})

test_that("BrainGnomes rejects unknown options before project access", {
  res <- run_brain_gnomes_cli(c("doctor", "/does/not/exist", "--definitely-unknown"))
  expect_equal(res$status, 2L)
  expect_true(any(grepl("Unknown option", res$output, fixed = TRUE)))
})

test_that("BrainGnomes init and config validate support a headless first project", {
  init_help <- run_brain_gnomes_cli(c("init", "--help"))
  expect_equal(init_help$status, 0L)
  expect_false(any(grepl("--non-interactive", init_help$output, fixed = TRUE)))

  installed_interface <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(paste0(
      "cat(all(c('project_name', 'project_directory', 'interactive') %in% ",
      "names(formals(BrainGnomes::setup_project))))"
    ))),
    stdout = TRUE, stderr = FALSE
  ))
  skip_if_not(identical(installed_interface, "TRUE"),
    "installed package predates non-interactive setup_project")
  root <- tempfile("cli-init-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  init <- run_brain_gnomes_cli(c("init", "cli_demo", root))
  expect_equal(init$status, 0L, info = paste(init$output, collapse = "\n"))
  expect_true(file.exists(file.path(root, "project_config.yaml")))
  expect_true(any(grepl("Saved project configuration to:", init$output, fixed = TRUE)))
  expect_false(any(grepl("requires an interactive session", init$output, fixed = TRUE)))
  expect_false(any(grepl("Not a TTY", init$output, fixed = TRUE)))

  validation <- run_brain_gnomes_cli(c(
    "config", "validate", file.path(root, "project_config.yaml"), "--format=json"
  ))
  expect_equal(validation$status, 0L, info = paste(validation$output, collapse = "\n"))
  parsed <- jsonlite::fromJSON(paste(validation$output, collapse = "\n"))
  expect_true(parsed$valid)
})

test_that("BrainGnomes cli_project is an unknown command", {
  res <- run_brain_gnomes_cli("cli_project")
  expect_equal(res$status, 2L)
  expect_true(any(grepl("^Unknown command: cli_project$", res$output)))
  expect_true(any(grepl("^Usage: BrainGnomes <command> \\[options\\]$", res$output)))
})

test_that("installed CLI templates isolate paths unless sharing is explicit", {
  installed_interface <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote("cat('reuse_template_paths' %in% names(formals(BrainGnomes::setup_project)))")),
    stdout = TRUE, stderr = FALSE
  ))
  skip_if_not(identical(installed_interface, "TRUE"),
    "installed package predates template path isolation")
  root <- tempfile("cli-template-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original <- file.path(root, "original")
  expect_equal(run_brain_gnomes_cli(c("init", "original", shQuote(original)))$status, 0L)
  template <- file.path(original, "project_config.yaml")
  source <- yaml::read_yaml(template)
  for (reuse in c(FALSE, TRUE)) {
    destination <- file.path(root, if (reuse) "shared" else "isolated")
    result <- run_brain_gnomes_cli(c("init", "copy", shQuote(destination),
      shQuote(paste0("--template=", template)), if (reuse) "--reuse-template-paths"))
    expect_equal(result$status, 0L, info = paste(result$output, collapse = "\n"))
    clone <- yaml::read_yaml(file.path(destination, "project_config.yaml"))
    for (field in c("bids_directory", "fmriprep_directory", "postproc_directory",
        "rois_directory", "log_directory", "scratch_directory", "sqlite_db")) {
      expected <- if (reuse) source$metadata[[field]] else file.path(
        normalizePath(destination, winslash = "/"),
        if (field == "sqlite_db") "copy.sqlite" else basename(source$metadata[[field]])
      )
      expect_path_identical(clone$metadata[[field]], expected, mustWork = field != "sqlite_db")
    }
  }
})

test_that("BrainGnomes without args prints help and exits nonzero", {
  res <- run_brain_gnomes_cli()
  expect_equal(res$status, 1L)
  expect_true(any(grepl("^Usage: BrainGnomes <command> \\[options\\]$", res$output)))
})
