# Create an isolated tracking database; cleanup belongs to the calling test.
make_submission_fixture <- function() {
  root <- tempfile("submission-durability-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE), envir = parent.frame())
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)
  tracking <- list(job_name = "postprocess_default_sub-01", sequence_id = "run-one",
                   stage = "postprocess", stream = "default", sub_id = "01",
                   job_role = "subject", unit_key = "subject::01::::postprocess::default",
                   attempt = 1L)
  list(root = root, db = db, tracking = tracking)
}

# Seal metadata without a runtime copy for focused transaction tests.
register_submission_fixture <- function(fixture, tracking = fixture$tracking) {
  contract <- allocate_job_contract(fixture$db, tracking)
  manifest <- write_job_manifest(contract, "true", "sbatch", "/fake/sbatch", "",
                                 character(), FALSE, NULL, "afterok", "sbatch", tracking)
  tracking$contract_id <- manifest$contract_id
  tracking$job_manifest_path <- manifest$path
  tracking$job_manifest_checksum <- manifest$checksum
  register_job_submission(fixture$db, contract, tracking)
  list(contract = contract, tracking = tracking)
}

test_that("attempt registration precedes IDs and binding preserves worker state", {
  fixture <- make_submission_fixture()
  attempt <- register_submission_fixture(fixture)
  records <- read_job_submission_attempts(fixture$db)
  expect_identical(records$submission_state, "PREPARED")
  expect_true(is.na(records$scheduler_job_id))
  expect_identical(nrow(get_tracked_job_status(sequence_id = "run-one", sqlite_db = fixture$db)), 0L)
  set_job_submission_state(fixture$db, attempt$contract$contract_id, "SUBMITTING")
  expect_identical(bind_job_submission(fixture$db, attempt$contract$contract_id, "810"), "810")
  update_tracked_job_status(fixture$db, "810", "COMPLETED", strict = TRUE)
  completed <- get_tracked_job_status("810", sqlite_db = fixture$db)
  bind_job_submission(fixture$db, attempt$contract$contract_id, "810")
  insert_tracked_job(fixture$db, "810", attempt$tracking)
  set_job_submission_state(fixture$db, attempt$contract$contract_id, "UNKNOWN")
  expect_identical(get_tracked_job_status("810", sqlite_db = fixture$db)$status, "COMPLETED")
  expect_identical(get_tracked_job_status("810", sqlite_db = fixture$db)$time_ended, completed$time_ended)
  expect_identical(read_job_submission_attempts(fixture$db)$submission_state, "ACCEPTED")
  expect_error(bind_job_submission(fixture$db, attempt$contract$contract_id, "811"), "conflicts")
  expect_identical(next_job_contract_attempt(fixture$db, fixture$tracking$unit_key, "run-two"), 2L)
})

test_that("uncertain attempts prevent duplicates but not distinct child roles", {
  fixture <- make_submission_fixture()
  attempt <- register_submission_fixture(fixture)
  set_job_submission_state(fixture$db, attempt$contract$contract_id, "UNKNOWN")
  later <- fixture$tracking
  later$sequence_id <- "run-two"
  expect_error(register_submission_fixture(fixture, later), "unconfirmed")
  expect_identical(nrow(read_job_submission_attempts(fixture$db)), 1L)
  expect_identical(next_job_contract_attempt(fixture$db, later$unit_key, "run-two"), 2L)
  later$job_role <- "array"
  child <- register_submission_fixture(fixture, later)
  expect_identical(nrow(read_job_submission_attempts(fixture$db)), 2L)
  expect_identical(bind_job_submission(fixture$db, child$contract$contract_id, "812[3].server"), "812[].server")
  bind_job_submission(fixture$db, child$contract$contract_id, "812[4].server")
  expect_identical(get_tracked_job_status("812[].server", sqlite_db = fixture$db)$job_role, "array")
  # A worker's independent binding resolves a lost submitter acknowledgement.
  bind_job_submission(fixture$db, attempt$contract$contract_id, "813")
  expect_no_error(register_submission_fixture(fixture, fixture$tracking))
})

test_that("prepared submitters cannot both enter the scheduler for active work", {
  fixture <- make_submission_fixture()
  first <- register_submission_fixture(fixture)
  second <- register_submission_fixture(fixture)
  set_job_submission_state(fixture$db, first$contract$contract_id, "SUBMITTING")
  expect_error(set_job_submission_state(fixture$db, second$contract$contract_id, "SUBMITTING"), "duplicate")
  bind_job_submission(fixture$db, first$contract$contract_id, "814")
  expect_error(set_job_submission_state(fixture$db, second$contract$contract_id, "SUBMITTING"), "active")
  con <- job_submission_connect(fixture$db)
  expect_identical(DBI::dbGetQuery(con, "PRAGMA synchronous")$synchronous, 2L)
  DBI::dbDisconnect(con)
  update_tracked_job_status(fixture$db, "814", "FAILED", strict = TRUE)
  expect_no_error(set_job_submission_state(fixture$db, second$contract$contract_id, "SUBMITTING"))
})

test_that("unbound submissions are visible in SQLite-first inspection", {
  fixture <- make_submission_fixture()
  attempt <- register_submission_fixture(fixture)
  set_job_submission_state(fixture$db, attempt$contract$contract_id, "UNKNOWN")
  cfg <- structure(list(metadata = list(project_name = "durability",
                    project_directory = fixture$root, sqlite_db = fixture$db)), class = "bg_project_cfg")
  inspected <- inspect_project(cfg, run_id = "latest", subject_id = "01")
  expect_identical(inspected$run_id, "run-one")
  expect_identical(inspected$overview$overall_status, "UNCONFIRMED_SUBMISSIONS")
  expect_identical(inspected$overview$n_unconfirmed_submissions, 1L)
  expect_identical(inspected$submissions$contract_id, attempt$contract$contract_id)
  expect_identical(nrow(inspected$jobs), 0L)
  expect_false("tracking_metadata" %in% names(inspected$submissions))
  expect_no_error(inspect_project(cfg, run_id = "run-one"))
  printed <- capture.output(print(inspected), type = "message")
  expect_true(any(grepl("Unconfirmed submissions", printed, fixed = TRUE)))
  expect_identical(read_job_submission_attempts(fixture$db)$submission_state, "UNKNOWN")
  con <- job_submission_connect(fixture$db)
  DBI::dbExecute(con, "UPDATE job_submission_attempts SET prepared_at = '2026-10-07T18:00:00Z'")
  DBI::dbDisconnect(con)
  insert_tracked_job(fixture$db, "815", list(job_name = "mriqc_sub-02", sequence_id = "run-old", status = "COMPLETED"))
  con <- job_submission_connect(fixture$db)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-10-07 12:00:00'")
  DBI::dbDisconnect(con)
  expect_identical(inspect_project(cfg, run_id = "latest")$run_id, "run-one")
})

test_that("an unavailable executable is rejected before launch, not an uncertain submission", {
  skip_on_os("windows")
  fixture <- make_submission_fixture()
  local_mocked_bindings(Sys.which = function(command) setNames("", command),
                        system2 = function(...) stop("Scheduler must not be invoked"), .package = "base")
  expect_error(cluster_job_submit("true", scheduler = "slurm", echo = FALSE, fail_on_error = TRUE,
                                  tracking_sqlite_db = fixture$db), "executable is unavailable")
  records <- read_job_submission_attempts(fixture$db)
  expect_identical(records$submission_state, "REJECTED")
  expect_true(is.na(records$submitted_at))
  expect_true(startsWith(records$unit_key, "command::"))
  expect_error(cluster_job_submit("true", scheduler = "slurm", echo = FALSE, fail_on_error = TRUE,
                                  tracking_sqlite_db = fixture$db), "executable is unavailable")
  expect_identical(nrow(read_job_submission_attempts(fixture$db)), 2L)
  expect_error(cluster_job_submit("true", scheduler = "slurm", echo = FALSE, fail_on_error = TRUE,
                                  tracking_sqlite_db = fixture$db,
                                  tracking_args = list(stage = "fmriprep", sub_id = "01")), "executable is unavailable")
  records <- read_job_submission_attempts(fixture$db)
  expect_true("subject::01::::fmriprep::" %in% records$unit_key)
  cfg <- structure(list(metadata = list(project_name = "durability",
                    project_directory = fixture$root, sqlite_db = fixture$db)), class = "bg_project_cfg")
  expect_identical(inspect_project(cfg)$overview$overall_status, "SUBMISSION_FAILED")
})

test_that("registration failure prevents scheduler invocation", {
  skip_on_os("windows")
  fixture <- make_submission_fixture()
  local_mocked_bindings(register_job_submission = function(...) stop("Cannot persist prepared attempt"),
                        .package = "BrainGnomes")
  local_mocked_bindings(system2 = function(...) stop("Scheduler must not be invoked"),
                        .package = "base")
  expect_error(cluster_job_submit("true", scheduler = "slurm", echo = FALSE, fail_on_error = TRUE,
                                  tracking_sqlite_db = fixture$db, tracking_args = fixture$tracking),
               "Cannot persist prepared attempt")
  expect_identical(nrow(read_job_submission_attempts(fixture$db)), 0L)
})

test_that("tracked local workers use the prepared UUID and their own process ID", {
  skip_on_os("windows")
  skip_if(Sys.which("bash") == "" || Sys.which("md5sum") == "")
  fixture <- make_submission_fixture()
  script <- file.path(fixture$root, "worker.R")
  writeLines('BrainGnomes::update_tracked_job_status(Sys.getenv("sqlite_db"), Sys.getenv("BG_WORKER_JOB_ID"), "COMPLETED", strict = TRUE)', script)
  withr::local_envvar(c(SLURM_JOB_ID = "999999", SLURM_ARRAY_JOB_ID = "999998", SLURM_ARRAY_TASK_ID = "3",
                       PBS_JOBID = "999997.server", PBS_ARRAYID = "4", PBS_ARRAY_INDEX = "4"))
  job <- cluster_job_submit(script, scheduler = "local", echo = FALSE, fail_on_error = TRUE,
                            tracking_sqlite_db = fixture$db, tracking_args = fixture$tracking)
  deadline <- Sys.time() + 30
  repeat {
    row <- get_tracked_job_status(as.character(job), sqlite_db = fixture$db)
    if (row$status == "COMPLETED" || Sys.time() > deadline) break
    Sys.sleep(0.1)
  }
  expect_identical(row$status, "COMPLETED")
  expect_identical(read_job_submission_attempts(fixture$db)$scheduler_job_id, as.character(job))
})

test_that("bootstrap failure receipts are published even when R cannot execute", {
  skip_on_os("windows")
  skip_if(Sys.which("bash") == "" || Sys.which("md5sum") == "")
  fixture <- make_submission_fixture()
  contract <- allocate_job_contract(fixture$db, fixture$tracking)
  execution <- prepare_job_execution(contract, "true", "sbatch", character())
  Sys.chmod(execution$script, "0644")
  code <- readLines(execution$script)
  code <- gsub(file.path(R.home("bin"), "Rscript"), file.path(fixture$root, "missing-Rscript"), code, fixed = TRUE)
  writeLines(code, execution$script)
  withr::local_envvar(c(SLURM_JOB_ID = "840", SLURM_ARRAY_JOB_ID = NA, SLURM_ARRAY_TASK_ID = NA,
                       PBS_JOBID = NA, PBS_ARRAYID = NA, PBS_ARRAY_INDEX = NA))
  # R treats exit 127 specially when capturing stdout: some versions throw
  # cmdError even though Bash ran and published the receipt successfully.
  result <- suppressWarnings(tryCatch(
    system2("bash", shQuote(execution$script), stdout = TRUE, stderr = TRUE),
    error = function(error) error
  ))
  if (inherits(result, "error")) {
    expect_match(conditionMessage(result), "error in running command")
  } else {
    expect_identical(attr(result, "status"), 127L)
  }
  receipt <- file.path(execution$bootstrap_receipts, "failure-allocation.tsv")
  expect_true(file.exists(receipt))
  fields <- strsplit(readLines(receipt), "\t", fixed = TRUE)[[1L]]
  expect_identical(fields, c("brain-gnomes-bootstrap-v1", contract$contract_id, "840", "allocation", "127"))
})

test_that("scheduler acknowledgement parsing requires one unambiguous ID", {
  expect_identical(parse_job_submission_id(c("notice", "Submitted batch job 123"), "sbatch"), "123")
  expect_identical(parse_job_submission_id("123;cluster", "sbatch"), "123")
  expect_identical(parse_job_submission_id("123[].server", "qsub"), "123[].server")
  expect_null(parse_job_submission_id(c("Submitted batch job 123", "124"), "sbatch"))
  expect_null(parse_job_submission_id("warning 123", "qsub"))
})

test_that("a fast worker can complete before scheduler submission returns", {
  skip_on_os("windows")
  skip_if(Sys.which("bash") == "" || Sys.which("md5sum") == "")
  fixture <- make_submission_fixture()
  script <- file.path(fixture$root, "worker.R")
  marker <- file.path(fixture$root, "namespace.txt")
  writeLines(c(
    'BrainGnomes::update_tracked_job_status(Sys.getenv("sqlite_db"), Sys.getenv("SLURM_JOB_ID"), "STARTED", strict = TRUE)',
    paste0('writeLines(getNamespaceInfo(asNamespace("BrainGnomes"), "path"), ', dQuote(marker, FALSE), ')'),
    'BrainGnomes::update_tracked_job_status(Sys.getenv("sqlite_db"), Sys.getenv("SLURM_JOB_ID"), "COMPLETED", strict = TRUE)'
  ), script)
  fake <- file.path(fixture$root, "sbatch")
  writeLines(c("#!/bin/bash", 'for arg; do worker="$arg"; done',
               paste(shQuote(file.path(R.home("bin"), "Rscript")), "--vanilla -e", shQuote(paste0(
                 'con <- DBI::dbConnect(RSQLite::SQLite(), ', dQuote(fixture$db, FALSE), '); ',
                 'attempt <- DBI::dbGetQuery(con, "SELECT * FROM job_submission_attempts"); ',
                 'DBI::dbDisconnect(con); stopifnot(nrow(attempt) == 1L, ',
                 'attempt$submission_state == "SUBMITTING", is.na(attempt$scheduler_job_id))'
               )), "|| exit $?"),
               paste("printf '%s\\n' 'stop(\"mutable original\")' >", shQuote(script)),
               paste('SLURM_JOB_ID=820 bash "$worker" >', shQuote(file.path(fixture$root, "worker.log")), '2>&1 || exit $?'),
               "echo 'Submitted batch job 820'"), fake)
  Sys.chmod(fake, "0755")
  withr::local_envvar(PATH = paste(fixture$root, Sys.getenv("PATH"), sep = .Platform$path.sep))
  withr::local_envvar(c(SLURM_ARRAY_JOB_ID = NA, SLURM_ARRAY_TASK_ID = NA, PBS_JOBID = NA, PBS_ARRAYID = NA, PBS_ARRAY_INDEX = NA))
  job <- cluster_job_submit(script, scheduler = "slurm", echo = FALSE, fail_on_error = TRUE,
                            tracking_sqlite_db = fixture$db, tracking_args = fixture$tracking)
  expect_identical(as.character(job), "820")
  row <- get_tracked_job_status("820", sqlite_db = fixture$db)
  expect_identical(row$status, "COMPLETED")
  expect_false(is.na(row$time_started))
  expect_false(is.na(row$time_ended))
  attempt <- read_job_submission_attempts(fixture$db)
  expect_identical(attempt$submission_state, "ACCEPTED")
  expect_true(file.exists(file.path(dirname(attempt$manifest_path), "submission-ack.json")))
  expect_true(startsWith(readLines(marker), attempt$runtime_directory))
  expect_false(grepl("mutable original", paste(readLines(file.path(fixture$root, "worker.log")), collapse = "\n")))
})

test_that("TORQUE array workers bind the root before acknowledgement with ancestry intact", {
  skip_on_os("windows")
  skip_if(Sys.which("bash") == "" || Sys.which("md5sum") == "")
  fixture <- make_submission_fixture()
  insert_tracked_job(fixture$db, "823.server", fixture$tracking)
  script <- file.path(fixture$root, "worker.R")
  writeLines('BrainGnomes::update_tracked_job_status(Sys.getenv("sqlite_db"), Sys.getenv("PBS_JOBID"), "STARTED", strict = TRUE)', script)
  fake <- file.path(fixture$root, "qsub")
  writeLines(c("#!/bin/bash", 'for arg; do worker="$arg"; done',
               paste('PBS_JOBID="824[2].server" PBS_ARRAYID=2 bash "$worker" >',
                     shQuote(file.path(fixture$root, "worker.log")), '2>&1 || exit $?'),
               "echo '824[].server'"), fake)
  Sys.chmod(fake, "0755")
  withr::local_envvar(PATH = paste(fixture$root, Sys.getenv("PATH"), sep = .Platform$path.sep))
  withr::local_envvar(c(SLURM_JOB_ID = NA, SLURM_ARRAY_JOB_ID = NA, SLURM_ARRAY_TASK_ID = NA,
                       PBS_JOBID = NA, PBS_ARRAYID = NA, PBS_ARRAY_INDEX = NA))
  tracking <- fixture$tracking
  tracking$job_role <- "array"
  tracking$parent_job_id <- "823.server"
  tracking$child_level <- 2L
  job <- cluster_job_submit(script, scheduler = "torque", sched_args = "-t 0-3%2", echo = FALSE,
                            fail_on_error = TRUE, tracking_sqlite_db = fixture$db, tracking_args = tracking)
  expect_identical(as.character(job), "824[].server")
  row <- get_tracked_job_status("824[].server", sqlite_db = fixture$db)
  expect_identical(row$status, "STARTED")
  expect_identical(row$parent_id, get_tracked_job_status("823.server", sqlite_db = fixture$db)$id)
  expect_identical(row$child_level, 2L)
  expect_identical(row$job_role, "array")
  expect_identical(read_job_submission_attempts(fixture$db)$submission_state, "ACCEPTED")
})

test_that("sealed shell payloads and helpers survive original file replacement", {
  skip_on_os("windows")
  skip_if(Sys.which("bash") == "" || Sys.which("md5sum") == "")
  fixture <- make_submission_fixture()
  helpers <- file.path(fixture$root, "mutable-helpers")
  dir.create(helpers)
  helper <- file.path(helpers, "shell_functions")
  writeLines('sealed_helper() { printf "sealed\\n"; }', helper)
  Sys.chmod(helper, "0600")
  script <- file.path(fixture$root, "worker.sh")
  marker <- file.path(fixture$root, "result.txt")
  writeLines(c("#!/bin/bash", "#SBATCH --time=00:01:00", 'source "$pkg_dir/shell_functions"',
               paste("sealed_helper >", shQuote(marker)), "#SBATCH --this-was-not-a-directive"), script)
  contract <- allocate_job_contract(fixture$db, fixture$tracking)
  withr::local_envvar(pkg_dir = helpers)
  execution <- prepare_job_execution(contract, script, "sbatch", c(pkg_dir = NA_character_))
  sealed_mode <- as.integer(file.info(file.path(execution$runtime$helpers, "shell_functions"))$mode)
  expect_identical(bitwAnd(sealed_mode, as.integer(as.octmode("0077"))), 0L)
  expect_false(identical(execution$script, execution$payload))
  wrapper <- readLines(execution$script)
  expect_true("#SBATCH --time=00:01:00" %in% wrapper)
  expect_false(any(grepl("this-was-not-a-directive", wrapper, fixed = TRUE)))
  writeLines("exit 83", helper)
  writeLines("exit 84", script)
  withr::local_envvar(c(SLURM_ARRAY_JOB_ID = NA, SLURM_ARRAY_TASK_ID = NA, PBS_JOBID = NA, PBS_ARRAYID = NA, PBS_ARRAY_INDEX = NA))
  result <- system2("bash", shQuote(execution$script), stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_identical(readLines(marker), "sealed")
  reused <- prepare_job_runtime_bundle(allocate_job_contract(fixture$db, fixture$tracking),
                                       list(BG_RUNTIME_BUNDLE = execution$runtime$directory))
  expect_identical(reused$directory, execution$runtime$directory)
})

test_that("bootstrap failure persists without loading the BrainGnomes namespace", {
  skip_on_os("windows")
  skip_if(Sys.which("bash") == "" || Sys.which("md5sum") == "")
  fixture <- make_submission_fixture()
  script <- file.path(fixture$root, "payload.sh")
  writeLines(c("#!/bin/bash", "exit 0"), script)
  contract <- allocate_job_contract(fixture$db, fixture$tracking)
  execution <- prepare_job_execution(contract, script, "sbatch", c(sqlite_db = fixture$db))
  tracking <- fixture$tracking
  tracking$runtime <- execution$runtime
  tracking$execution_payload <- execution$payload
  manifest <- write_job_manifest(contract, execution$script, "sbatch", "/fake/sbatch", "",
                                 execution$environment, FALSE, NULL, "afterok", "sbatch", tracking)
  tracking$job_manifest_path <- manifest$path
  tracking$job_manifest_checksum <- manifest$checksum
  register_job_submission(fixture$db, contract, tracking)
  set_job_submission_state(fixture$db, contract$contract_id, "SUBMITTING")
  # Corrupt only this test-owned snapshot: the independent bridge still works.
  helper <- file.path(execution$runtime$helpers, "shell_functions")
  unlink(helper)
  withr::local_envvar(c(SLURM_JOB_ID = "830", SLURM_ARRAY_JOB_ID = NA, SLURM_ARRAY_TASK_ID = NA,
                       PBS_JOBID = NA, PBS_ARRAYID = NA, PBS_ARRAY_INDEX = NA))
  result <- suppressWarnings(system2("bash", shQuote(execution$script), stdout = TRUE, stderr = TRUE))
  expect_identical(attr(result, "status"), 1L)
  row <- get_tracked_job_status("830", sqlite_db = fixture$db)
  expect_identical(row$status, "FAILED")
  expect_identical(row$failure_category, "BOOTSTRAP")
  expect_identical(row$exit_code, 1L)
  receipt <- read_job_submission_attempts(fixture$db)$bootstrap_failure_path
  expect_true(file.exists(receipt))
  expect_match(readLines(receipt), contract$contract_id, fixed = TRUE)
  # An array task's failure receipt must not prematurely finalize its root.
  bind_job_submission(fixture$db, contract$contract_id, "830")
  update_tracked_job_status(fixture$db, "830", "STARTED", strict = TRUE)
  record_job_bootstrap_failure(fixture$db, contract$contract_id, "830", 1L, receipt, array_task = TRUE)
  expect_identical(get_tracked_job_status("830", sqlite_db = fixture$db)$status, "STARTED")
})

test_that("failed acknowledgement is durable and does not permit a duplicate launch", {
  skip_on_os("windows")
  fixture <- make_submission_fixture()
  script <- file.path(fixture$root, "worker.sh")
  writeLines(c("#!/bin/bash", "exit 0"), script)
  fake <- file.path(fixture$root, "sbatch")
  counter <- file.path(fixture$root, "invocations.txt")
  writeLines(c("#!/bin/bash", paste("echo invoked >>", shQuote(counter)), "echo 'no confirmed job ID'"), fake)
  Sys.chmod(fake, "0755")
  withr::local_envvar(PATH = paste(fixture$root, Sys.getenv("PATH"), sep = .Platform$path.sep))
  submit <- function() cluster_job_submit(script, scheduler = "slurm", echo = FALSE, fail_on_error = TRUE,
                                          tracking_sqlite_db = fixture$db, tracking_args = fixture$tracking)
  expect_error(submit(), "Job submission failed")
  records <- read_job_submission_attempts(fixture$db)
  expect_identical(records$submission_state, "UNKNOWN")
  expect_true(is.na(records$scheduler_job_id))
  expect_true(file.exists(file.path(dirname(records$manifest_path), "submission.stdout")))
  expect_error(submit(), "unconfirmed")
  expect_length(readLines(counter), 1L)
})

test_that("dynamic postprocessing arrays use the pre-registered submission path", {
  root <- system.file(package = "BrainGnomes")
  if (dir.exists(file.path(root, "inst"))) root <- file.path(root, "inst")
  for (extension in c("sbatch", "pbs")) {
    lines <- readLines(file.path(root, "hpc_scripts", paste0("postprocess_subject.", extension)))
    expect_true(any(grepl('"${pkg_dir}/submit_job.R"', lines, fixed = TRUE)))
    expect_false(any(grepl("array_jid=$(sbatch", lines, fixed = TRUE)))
    expect_false(any(grepl("array_jid=$(qsub", lines, fixed = TRUE)))
    expect_true(any(grepl('--child_level "2"', lines, fixed = TRUE)))
  }
})

test_that("the child submission CLI retains options, ancestry, and the inherited bundle", {
  skip_on_os("windows")
  fixture <- make_submission_fixture()
  insert_tracked_job(fixture$db, "850", fixture$tracking)
  script <- file.path(fixture$root, "array.sh")
  writeLines(c("#!/bin/bash", "exit 0"), script)
  execution <- prepare_job_execution(allocate_job_contract(fixture$db, fixture$tracking), script, "sbatch", character())
  fake <- file.path(fixture$root, "sbatch")
  writeLines(c("#!/bin/sh", "echo 'Submitted batch job 851'"), fake)
  Sys.chmod(fake, "0755")
  runtime <- execution$runtime
  withr::local_envvar(c(PATH = paste(fixture$root, Sys.getenv("PATH"), sep = .Platform$path.sep),
                       BG_RUNTIME_BUNDLE = runtime$directory, BG_RUNTIME_LIBRARY = runtime$library,
                       BG_RUNTIME_SOURCE = if (is.null(runtime$source)) "" else runtime$source,
                       BG_RUNTIME_LOADER = runtime$loader, pkg_dir = runtime$helpers,
                       R_LIBS_USER = execution$environment[["R_LIBS_USER"]]))
  options <- "--time=01:02:03 --mem=2G"
  args <- c(shQuote(file.path(runtime$helpers, "submit_job.R")),
            "--script", shQuote(script), "--scheduler", "slurm",
            "--sqlite_db", shQuote(fixture$db), "--sequence_id", "run-one",
            "--scheduler_options", shQuote(options), "--array", "0-3%2",
            "--job_name", "postprocess_default_array", "--stage", "postprocess",
            "--stream", "default", "--sub_id", "01", "--job_role", "array",
            "--parent_job_id", "850", "--child_level", "2")
  output <- system2(file.path(R.home("bin"), "Rscript"), args, stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true("851" %in% output)
  row <- get_tracked_job_status("851", sqlite_db = fixture$db)
  expect_identical(row$scheduler_options, options)
  expect_identical(row$child_level, 2L)
  expect_identical(row$parent_id, get_tracked_job_status("850", sqlite_db = fixture$db)$id)
  expect_identical(read_job_submission_attempts(fixture$db)$runtime_directory, runtime$directory)
})
