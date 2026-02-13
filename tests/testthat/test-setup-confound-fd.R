test_that("maybe_add_framewise_displacement can add unfiltered FD to noproc columns", {
  ppcfg <- list(
    confound_calculate = list(
      enable = TRUE,
      columns = "white_matter",
      noproc_columns = NULL
    ),
    motion_filter = list(enable = TRUE)
  )

  responses <- c(TRUE, FALSE, FALSE)
  idx <- 0L
  local_mocked_bindings(
    prompt_input = function(...) {
      idx <<- idx + 1L
      responses[[idx]]
    },
    .package = "BrainGnomes"
  )

  out <- maybe_add_framewise_displacement(ppcfg)
  expect_equal(idx, 3L)
  expect_true("white_matter" %in% out$confound_calculate$columns)
  expect_true("framewise_displacement_unfiltered" %in% out$confound_calculate$noproc_columns)
  expect_false("framewise_displacement" %in% out$confound_calculate$noproc_columns)
})

test_that("maybe_add_framewise_displacement can add filtered FD to processed columns", {
  ppcfg <- list(
    confound_calculate = list(
      enable = TRUE,
      columns = "white_matter",
      noproc_columns = NULL
    ),
    motion_filter = list(enable = TRUE)
  )

  responses <- c(TRUE, TRUE, TRUE)
  idx <- 0L
  local_mocked_bindings(
    prompt_input = function(...) {
      idx <<- idx + 1L
      responses[[idx]]
    },
    .package = "BrainGnomes"
  )

  out <- maybe_add_framewise_displacement(ppcfg)
  expect_equal(idx, 3L)
  expect_true("framewise_displacement" %in% out$confound_calculate$columns)
  expect_false("framewise_displacement_unfiltered" %in% out$confound_calculate$columns)
  expect_null(out$confound_calculate$noproc_columns)
})

test_that("maybe_add_framewise_displacement skips prompts when FD already requested", {
  ppcfg <- list(
    confound_calculate = list(
      enable = TRUE,
      columns = "framewise_displacement",
      noproc_columns = NULL
    ),
    motion_filter = list(enable = TRUE)
  )

  local_mocked_bindings(
    prompt_input = function(...) stop("prompt_input should not be called"),
    .package = "BrainGnomes"
  )

  out <- maybe_add_framewise_displacement(ppcfg)
  expect_identical(out$confound_calculate$columns, "framewise_displacement")
})
