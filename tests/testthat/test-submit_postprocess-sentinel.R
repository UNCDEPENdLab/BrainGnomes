test_that("submit_postprocess builds sentinel sched args from the selected stream", {
  tmp <- tempfile("postprocess_sentinel_")
  dir.create(tmp)
  log_dir <- file.path(tmp, "logs")
  dir.create(log_dir)
  dir.create(file.path(log_dir, "sub-01"), recursive = TRUE)

  scfg <- list(
    metadata = list(
      fmriprep_directory = file.path(tmp, "fmriprep"),
      postproc_directory = file.path(tmp, "postproc"),
      scratch_directory = file.path(tmp, "scratch"),
      log_directory = log_dir,
      project_name = "test-project"
    ),
    compute_environment = list(
      scheduler = "slurm",
      fsl_container = file.path(tmp, "fsl.sif")
    ),
    postprocess = list(
      enable = TRUE,
      stream1 = list(
        memgb = 48,
        nhours = 8,
        ncores = 3,
        cli_options = "",
        sched_args = "",
        input_regex = "desc:preproc suffix:bold",
        bids_desc = "postproc",
        keep_intermediates = FALSE,
        overwrite = FALSE,
        tr = 2,
        max_concurrent_images = 4L
      )
    )
  )
  class(scfg) <- "bg_project_cfg"

  calls <- list()
  local_mocked_bindings(
    cluster_job_submit = function(script, scheduler = "slurm", sched_args = NULL,
                                  env_variables = NULL, wait_jobs = NULL, wait_signal = "afterok",
                                  echo = TRUE, tracking_sqlite_db = NULL, tracking_args = list(), ...) {
      idx <- length(calls) + 1L
      calls[[idx]] <<- list(
        script = script,
        scheduler = scheduler,
        sched_args = sched_args,
        env_variables = env_variables,
        wait_jobs = wait_jobs,
        wait_signal = wait_signal,
        tracking_args = tracking_args
      )
      if (idx == 1L) "12345" else "67890"
    },
    get_job_script = function(scfg = NULL, job_name, subject_suffix = TRUE) {
      paste0(job_name, if (isTRUE(subject_suffix)) "_subject" else "", ".sbatch")
    },
    log_submission_command = function(...) invisible(NULL)
  )

  job_id <- submit_postprocess(
    scfg = scfg,
    sub_dir = file.path(tmp, "bids", "sub-01"),
    sub_id = "01",
    ses_id = NULL,
    env_variables = c(
      stdout_log = file.path(log_dir, "sub-01", "postprocess_stream1_jobid-%j.out"),
      stderr_log = file.path(log_dir, "sub-01", "postprocess_stream1_jobid-%j.err")
    ),
    sched_script = "postprocess_subject.sbatch",
    sched_args = "--time=08:00:00 --mem=48g -n 3",
    parent_ids = NULL,
    lg = NULL,
    pp_stream = "stream1",
    tracking_sqlite_db = NULL,
    tracking_args = list(
      job_name = "postprocess_stream1",
      n_cpus = 3,
      wall_time = "08:00:00",
      mem_total = 48,
      scheduler = "slurm",
      scheduler_options = "--time=08:00:00 --mem=48g -n 3"
    )
  )

  expect_identical(job_id, "67890")
  expect_length(calls, 2L)
  expect_match(calls[[2]]$sched_args, "--time=00:15:00")
  expect_match(calls[[2]]$sched_args, "--mem=2g")
  expect_match(calls[[2]]$sched_args, "-n 1", fixed = TRUE)
  expect_identical(calls[[2]]$tracking_args$job_name, "postprocess_stream1_sentinel")
  expect_identical(calls[[2]]$tracking_args$wall_time, "00:15:00")
  expect_identical(calls[[2]]$tracking_args$mem_total, 2L)
  expect_identical(calls[[2]]$tracking_args$n_cpus, 1L)
  expect_identical(calls[[2]]$tracking_args$scheduler_options, calls[[2]]$sched_args)
})
