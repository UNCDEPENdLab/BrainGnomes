test_that("get_job_sched_args formats torque arguments", {
  scfg <- list(
    compute_environment = list(scheduler = "torque"),
    myjob = list(ncores = 4, nhours = 2, memgb = 8, sched_args = NULL)
  )
  class(scfg) <- c(class(scfg), "bg_project_cfg")
  result <- get_job_sched_args(scfg, "myjob")
  expect_equal(
    result,
    glue::glue(
      "-l nodes=1:ppn=4 -l walltime=02:00:00 -l mem=8 -N myjob",
      .trim = TRUE, .sep = " ", .null = NULL
    )
  )
})

test_that("get_job_sched_args uses debug settings", {
  scfg <- list(
    compute_environment = list(scheduler = "slurm"),
    debug = TRUE,
    myjob = list(ncores = 4, nhours = 2, memgb = 8, sched_args = NULL)
  )
  class(scfg) <- c(class(scfg), "bg_project_cfg")
  result <- get_job_sched_args(scfg, "myjob")
  expect_equal(
    result,
    glue::glue(
      "-N 1 -n 1 --time=00:06:00 --mem=4g --job_name=myjob",
      .trim = TRUE, .sep = " ", .null = NULL
    )
  )
})

test_that("get_job_sched_args formats slurm arguments", {
  scfg <- list(
    compute_environment = list(scheduler = "slurm"),
    myjob = list(ncores = 4, nhours = 2, memgb = 8, sched_args = NULL)
  )
  class(scfg) <- c(class(scfg), "bg_project_cfg")
  result <- get_job_sched_args(scfg, "myjob")
  expect_equal(
    result,
    glue::glue(
      "-N 1 -n 4 --time=02:00:00 --mem=8g --job_name=myjob",
      .trim = TRUE, .sep = " ", .null = NULL
    )
  )
})
