test_that("maybe_add_framewise_displacement adds FD to noproc columns", {
  ppcfg <- list(
    confound_calculate = list(
      enable = TRUE,
      columns = "white_matter",
      noproc_columns = NULL
    ),
    motion_filter = list(enable = TRUE)
  )

  responses <- c(TRUE, FALSE)
  idx <- 0L
  local_mocked_bindings(
    prompt_input = function(...) {
      idx <<- idx + 1L
      responses[[idx]]
    },
    .package = "BrainGnomes"
  )

  out <- maybe_add_framewise_displacement(ppcfg)
  expect_equal(idx, 2L)
  expect_true("white_matter" %in% out$confound_calculate$columns)
  expect_true("framewise_displacement" %in% out$confound_calculate$noproc_columns)
  expect_false("framewise_displacement_unfiltered" %in% out$confound_calculate$noproc_columns)
})

test_that("maybe_add_framewise_displacement can add FD to processed columns", {
  ppcfg <- list(
    confound_calculate = list(
      enable = TRUE,
      columns = "white_matter",
      noproc_columns = NULL
    ),
    motion_filter = list(enable = TRUE)
  )

  responses <- c(TRUE, TRUE)
  idx <- 0L
  local_mocked_bindings(
    prompt_input = function(...) {
      idx <<- idx + 1L
      responses[[idx]]
    },
    .package = "BrainGnomes"
  )

  out <- maybe_add_framewise_displacement(ppcfg)
  expect_equal(idx, 2L)
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

test_that("postprocess stream setup persists motion decision after adding FD", {
  ppcfg <- list(
    confound_calculate = list(
      enable = TRUE,
      columns = "white_matter",
      noproc_columns = NULL
    ),
    confound_regression = list(enable = FALSE),
    scrubbing = list(enable = FALSE)
  )
  answers <- c(TRUE, FALSE, FALSE)
  prompts <- character()

  local_mocked_bindings(
    prompt_input = function(prompt, ...) {
      prompts <<- c(prompts, prompt)
      answer <- answers[[1L]]
      answers <<- answers[-1L]
      answer
    },
    setup_postprocess_globals = function(ppcfg, ...) ppcfg,
    setup_job = function(scfg, ...) scfg,
    setup_apply_mask = function(ppcfg, ...) ppcfg,
    setup_spatial_smooth = function(ppcfg, ...) ppcfg,
    setup_apply_aroma = function(ppcfg, ...) ppcfg,
    setup_temporal_filter = function(ppcfg, ...) ppcfg,
    setup_intensity_normalization = function(ppcfg, ...) ppcfg,
    setup_confound_calculate = function(ppcfg, ...) ppcfg,
    setup_scrubbing = function(ppcfg, ...) ppcfg,
    setup_confound_regression = function(ppcfg, ...) ppcfg,
    setup_postprocess_steps = function(ppcfg, ...) ppcfg,
    .package = "BrainGnomes"
  )

  scfg <- structure(
    list(postprocess = list(enable = TRUE, default = ppcfg)),
    class = "bg_project_cfg"
  )
  out <- setup_postprocess_stream(scfg, stream_name = "default")

  expect_identical(
    prompts,
    c(
      "Add framewise_displacement to postprocessed confounds?",
      "Apply BOLD-matched processing to framewise_displacement?",
      "Filter motion parameters before computing FD?"
    )
  )
  expect_identical(
    out$postprocess$default$confound_calculate$noproc_columns,
    "framewise_displacement"
  )
  expect_false(out$postprocess$default$motion_filter$enable)
  expect_length(answers, 0L)
})
