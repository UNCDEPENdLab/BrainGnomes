test_that("scheduler status normalization remains scheduler-aware", {
  expect_identical(
    normalize_scheduler_job_status(c("S", "T", "Z", "C"), "local"),
    c("RUNNING", "SUSPENDED", "FAILED", "COMPLETED")
  )
  expect_identical(
    normalize_scheduler_job_status(
      c("PENDING", "RUNNING+", "COMPLETED", "OUT_OF_MEMORY"), "slurm"
    ),
    c("QUEUED", "RUNNING", "COMPLETED", "FAILED")
  )
})

test_that("scheduler_job_status delegates to existing scheduler queries", {
  calls <- 0L
  local_mocked_bindings(
    local_job_status = function(job_ids = NULL, user = NULL, ...) {
      calls <<- calls + 1L
      data.frame(
        PID = as.integer(job_ids), STAT = c("S", "C"),
        stringsAsFactors = FALSE
      )
    },
    .package = "BrainGnomes"
  )

  status <- scheduler_job_status(c("10", "11"), scheduler = "sh")
  expect_equal(calls, 1L)
  expect_identical(status$scheduler, c("local", "local"))
  expect_identical(status$scheduler_status, c("RUNNING", "COMPLETED"))
  expect_identical(status$scheduler_raw_status, c("S", "C"))
})

test_that("wait_for_job consumes the scheduler-neutral status adapter", {
  calls <- 0L
  local_mocked_bindings(
    scheduler_job_status = function(job_ids, scheduler = "local", user = NULL) {
      calls <<- calls + 1L
      data.frame(
        job_id = as.character(job_ids), scheduler = "slurm",
        scheduler_status = "COMPLETED", scheduler_raw_status = "COMPLETED",
        query_detail = NA_character_, stringsAsFactors = FALSE
      )
    },
    .package = "BrainGnomes"
  )

  expect_true(wait_for_job("123", scheduler = "slurm"))
  expect_equal(calls, 1L)
})

test_that("Slurm arrays aggregate task states without confusing steps or prefixes", {
  records <- data.frame(
    JobID = c("123_0", "123_1", "123_1.batch", "1234_0", "9", "123"),
    State = c("COMPLETED", "COMPLETED", "FAILED", "FAILED", "TIMEOUT", "PENDING"),
    Submit = "2026-10-05T12:00:00",
    Start = c("2026-10-05T12:01:00", "2026-10-05T12:02:00", rep("Unknown", 4)),
    End = c("2026-10-05T13:00:00", "2026-10-05T14:00:00", rep("Unknown", 4)),
    ExitCode = "0:0"
  )
  status <- aggregate_slurm_job_status(records, c("9", "123", "123_1", "absent"))
  expect_identical(status$JobID, c("9", "123", "123_1", "absent"))
  expect_identical(status$State, c("TIMEOUT", "COMPLETED", "COMPLETED", "MISSING"))
  expect_identical(status$Start[[2L]], "2026-10-05T12:01:00")
  expect_identical(status$End[[2L]], "2026-10-05T14:00:00")
  expect_match(status$QueryDetail[[2L]], "2 array tasks: COMPLETED=2", fixed = TRUE)
  expect_true(is.na(status$ExitCode[[2L]]))
  expect_equal(nrow(aggregate_slurm_job_status(records, character())), 0L)
})

test_that("array lifecycle waits for active tasks and never hides uncertainty", {
  cases <- list(
    c("COMPLETED", "FAILED"), c("FAILED", "RUNNING"),
    c("FAILED", "PENDING"), c("COMPLETED", "CANCELLED by 123"),
    c("COMPLETED", "SUSPENDED"), c("COMPLETED", "new-state"),
    c("COMPLETED", NA_character_), c("COMPLETED", "UNAVAILABLE")
  )
  expected <- c("FAILED", "RUNNING", "QUEUED", "CANCELLED", "SUSPENDED",
                "UNKNOWN", "MISSING", "UNAVAILABLE")
  for (index in seq_along(cases)) {
    records <- data.frame(JobID = c("22_0", "22_1"), State = cases[[index]],
                           End = c("2026-10-05T13:00:00", "2026-10-05T14:00:00"))
    status <- aggregate_slurm_job_status(records, "22")
    expect_identical(status$State, expected[[index]])
    if (!expected[[index]] %in% c("FAILED", "CANCELLED")) expect_true(is.na(status$End))
  }
})

test_that("sacct expands arrays and preserves long character identifiers", {
  seen_args <- NULL
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      expect_identical(command, "sacct")
      seen_args <<- args
      c("JobID|State", "123456789012345_0|COMPLETED", "123456789012345_1|COMPLETED")
    }, .package = "base"
  )
  status <- slurm_job_status("123456789012345", user = "alice")
  expect_identical(status$JobID, "123456789012345")
  expect_identical(status$State, "COMPLETED")
  expect_true("--array" %in% seen_args)
  expect_true("-X" %in% seen_args)
  expect_true(any(grepl("jobid%100", seen_args, fixed = TRUE)))
  seen_args <- NULL
  expect_equal(nrow(slurm_job_status(character())), 0L)
  expect_null(seen_args)
})

test_that("the affected ten-task Slurm array is completed, not missing", {
  records <- data.frame(JobID = paste0("3845511_", 0:9), State = "COMPLETED")
  status <- aggregate_slurm_job_status(records, "3845511")
  expect_identical(status$State, "COMPLETED")
  expect_identical(status$QueryDetail, "Observed 10 array tasks: COMPLETED=10")
})

test_that("scheduler errors differ from successful empty accounting queries", {
  response <- structure("accounting unavailable", status = 1L)
  local_mocked_bindings(system2 = function(...) response,
                        Sys.which = function(...) "mock-command", .package = "base")
  failed <- scheduler_job_status("123", "slurm")
  expect_identical(failed$scheduler_status, "UNAVAILABLE")
  expect_match(failed$query_detail, "sacct query failed")
  response <- "garbage"
  expect_identical(scheduler_job_status("123", "slurm")$scheduler_status, "UNAVAILABLE")
  response <- character()
  expect_identical(scheduler_job_status("123", "slurm")$scheduler_status, "MISSING")
  response <- "JobID|State"
  expect_identical(scheduler_job_status("123", "slurm")$scheduler_status, "MISSING")
})

test_that("TORQUE requires exit status for success and preserves expired jobs", {
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      if (command == "qselect") {
        expect_true(any(grepl("alice", args, fixed = TRUE)))
        return(switch(tail(args, 1L), QWH = "1.server", ERT = "2.server",
                      C = c("3.server", "4.server", "5.server", "6.server")))
      }
      expect_identical(command, "qstat")
      if (any(grepl("3.server", args, fixed = TRUE))) return("    exit_status = 0")
      if (any(grepl("4.server", args, fixed = TRUE))) return("    Exit_status = 271")
      if (any(grepl("5.server", args, fixed = TRUE))) return("    job_state = C")
      structure("Unknown Job Id", status = 1L)
    }, .package = "base"
  )
  status <- torque_job_status(paste0(1:7, ".server"), user = "alice")
  expect_identical(status$State, c("QUEUED", "RUNNING", "COMPLETED", "FAILED",
                                  "UNKNOWN", "UNKNOWN", "MISSING"))
  expect_match(status$QueryDetail[[4L]], "exit_status=271", fixed = TRUE)
  expect_equal(nrow(torque_job_status(character())), 0L)
})

test_that("each TORQUE list query must succeed", {
  failing_state <- "QWH"
  local_mocked_bindings(system2 = function(command, args, ...) {
    if (tail(args, 1L) == failing_state) return(structure("server down", status = 1L))
    character()
  }, Sys.which = function(...) "mock-command", .package = "base")
  for (state in c("QWH", "ERT", "C")) {
    failing_state <- state
    status <- scheduler_job_status("123.server", "torque")
    expect_identical(status$scheduler_status, "UNAVAILABLE")
    expect_match(status$query_detail, "qselect query failed")
  }
  failing_state <- "none"
  expect_identical(scheduler_job_status("123.server", "torque")$scheduler_status, "MISSING")
})

test_that("scheduler adapter retains array details", {
  local_mocked_bindings(
    Sys.which = function(...) "mock-command", .package = "base"
  )
  local_mocked_bindings(slurm_job_status = function(...) {
    data.frame(JobID = "123", State = "COMPLETED", QueryDetail = "Observed 10 array tasks: COMPLETED=10")
  }, .package = "BrainGnomes")
  status <- scheduler_job_status("123", "slurm")
  expect_identical(status$scheduler_status, "COMPLETED")
  expect_match(status$query_detail, "10 array tasks", fixed = TRUE)
})

test_that("expired scheduler records cannot make wait_for_job succeed", {
  local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
    data.frame(scheduler_status = "MISSING")
  }, .package = "BrainGnomes")
  expect_false(wait_for_job("expired.server", scheduler = "torque", max_wait = 1,
                            repolling_interval = 0.1, stop_on_timeout = FALSE))
})

test_that("waiting for an array includes active tasks before reporting failures", {
  calls <- 0L
  local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
    calls <<- calls + 1L
    records <- data.frame(JobID = c("123_0", "123_1"),
                           State = c("FAILED", if (calls == 1L) "RUNNING" else "COMPLETED"))
    data.frame(scheduler_status = aggregate_slurm_job_status(records, job_ids)$State)
  }, .package = "BrainGnomes")
  capture.output(expect_false(wait_for_job("123", scheduler = "slurm", repolling_interval = 0.1)))
  expect_equal(calls, 2L)
})

test_that("waiting retains confirmed terminal states when accounting expires", {
  calls <- 0L
  local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
    calls <<- calls + 1L
    if (calls == 1L) {
      expect_identical(job_ids, c("1.server", "2.server"))
      return(data.frame(scheduler_status = c("COMPLETED", "RUNNING")))
    }
    # The first job's record has expired; it must not be queried again or
    # replaced by MISSING after a positively confirmed successful exit.
    expect_identical(job_ids, "2.server")
    data.frame(scheduler_status = "COMPLETED")
  }, .package = "BrainGnomes")
  expect_true(wait_for_job(c("1.server", "2.server"), scheduler = "torque", repolling_interval = 0.1))
  expect_equal(calls, 2L)
})
