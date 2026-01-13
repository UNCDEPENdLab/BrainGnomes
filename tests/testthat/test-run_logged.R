setup_test_logger <- function(name) {
  base_logger <- lgr::get_logger(name)
  base_logger$config(NULL)
  lg <- lgr::get_logger_glue(name)
  buffer <- lgr::AppenderBuffer$new()
  lg$set_propagate(FALSE)
  lg$set_threshold("trace")
  lg$set_appenders(list(buffer))
  list(logger = lg, buffer = buffer)
}

test_that("run_logged logs argument previews and returns values", {
  logger_ctx <- setup_test_logger("BrainGnomes.test.run_logged.success")
  lg <- logger_ctx$logger
  buffer <- logger_ctx$buffer
  on.exit(lg$config(NULL), add = TRUE)

  vec_vals <- c(0.12345, 20.9876, 0.000456789, 12345.6)
  result <- run_logged(
    function(alpha, vec) alpha + sum(vec),
    alpha = 5.6789,
    vec = vec_vals,
    fun_label = "add_numbers",
    logger = lg
  )

  expect_equal(result, 5.6789 + sum(vec_vals))
  expect_equal(buffer$data$level, c(400, 500))
  expect_match(
    buffer$data$msg[[1]],
    "Running add_numbers\\(alpha=5.68, vec=c\\(0.123, 21, 0.000457, \\.\\.\\. len=4\\)\\)"
  )
  expect_match(buffer$data$msg[[2]], "add_numbers completed in")
})

test_that("run_logged logs errors and propagates failures", {
  logger_ctx <- setup_test_logger("BrainGnomes.test.run_logged.error")
  lg <- logger_ctx$logger
  buffer <- logger_ctx$buffer
  on.exit(lg$config(NULL), add = TRUE)

  bad_fun <- function(alpha) {
    stop(glue::glue("boom {alpha}"))
  }

  expect_error(
    suppressWarnings(
      run_logged(
        bad_fun,
        alpha = 2,
        fun_label = "fail_step",
        logger = lg
      )
    ),
    "boom 2"
  )

  expect_equal(buffer$data$level, c(400, 200))
  expect_match(buffer$data$msg[[2]], "Error running fail_step: boom 2")
})

test_that("run_logged emits stack traces when DEBUG logging is enabled", {
  logger_ctx <- setup_test_logger("BrainGnomes.test.run_logged.stack")
  lg <- logger_ctx$logger
  buffer <- logger_ctx$buffer
  on.exit(lg$config(NULL), add = TRUE)
  old_option <- options("BrainGnomes.log_level")
  on.exit(do.call(options, old_option), add = TRUE)
  options(BrainGnomes.log_level = "DEBUG")

  expect_error(
    suppressWarnings(
      run_logged(
        function() stop("kaboom"),
        fun_label = "trace_me",
        logger = lg
      )
    ),
    "kaboom"
  )

  expect_equal(buffer$data$level[1:2], c(400, 200))
  expect_true(any(grepl("^Stack trace for trace_me", buffer$data$msg)))
  expect_true(any(buffer$data$level == 500))
})
