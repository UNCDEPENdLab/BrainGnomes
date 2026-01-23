make_base_postprocess_cfg <- function() {
  list(
    input_regex = "regex:.*",
    bids_desc = "desc",
    keep_intermediates = FALSE,
    overwrite = FALSE,
    tr = 2,
    temporal_filter = list(enable = FALSE),
    spatial_smooth = list(enable = FALSE),
    intensity_normalize = list(enable = FALSE),
    confound_calculate = list(enable = FALSE),
    scrubbing = list(enable = FALSE),
    confound_regression = list(enable = FALSE),
    apply_mask = list(enable = FALSE),
    apply_aroma = list(enable = FALSE),
    force_processing_order = FALSE
  )
}

make_motion_confounds <- function(n_time = 50L) {
  data.frame(
    rot_x = sin(seq_len(n_time) / 4),
    rot_y = cos(seq_len(n_time) / 5),
    rot_z = sin(seq_len(n_time) / 6),
    trans_x = seq(0.05, 0.15, length.out = n_time),
    trans_y = seq(-0.1, 0.1, length.out = n_time),
    trans_z = seq(0.2, -0.2, length.out = n_time)
  )
}

test_that("validate_postprocess_config_single accepts motion filter band", {
  cfg <- make_base_postprocess_cfg()
  cfg$motion_filter <- list(
    enable = TRUE,
    filter_type = "notch",
    bandstop_min_bpm = 12,
    bandstop_max_bpm = 20
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_false(any(grepl("postprocess/motion_filter", res$gaps, fixed = TRUE)))
})

test_that("validate_postprocess_config_single flags inverted motion filter band", {
  cfg <- make_base_postprocess_cfg()
  cfg$motion_filter <- list(
    enable = TRUE,
    filter_type = "notch",
    bandstop_min_bpm = 20,
    bandstop_max_bpm = 15
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_true("postprocess/motion_filter/bandstop_min_bpm" %in% res$gaps)
  expect_true("postprocess/motion_filter/bandstop_max_bpm" %in% res$gaps)
})

test_that("motion-related confounds trigger motion_filter enable gap", {
  cfg <- make_base_postprocess_cfg()
  cfg$confound_regression <- list(
    enable = TRUE,
    columns = c("rot_x"),
    noproc_columns = character(),
    prefix = "r"
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_true("postprocess/motion_filter/enable" %in% res$gaps)
})

test_that("validate_postprocess_config_single accepts low-pass motion filter", {
  cfg <- make_base_postprocess_cfg()
  cfg$motion_filter <- list(
    enable = TRUE,
    filter_type = "lowpass",
    lowpass_bpm = 6,
    filter_order = 4
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_false(any(grepl("postprocess/motion_filter", res$gaps, fixed = TRUE)))
})

test_that("filter_confounds adjusts notch frequencies above Nyquist", {
  conf_df <- make_motion_confounds(60L)
  motion_cols <- c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z")
  warns <- character(0)
  filtered <- withCallingHandlers(
    filter_confounds(
      confounds_df = conf_df,
      tr = 1,
      filter_type = "notch",
      bandstop_min_bpm = 70,
      bandstop_max_bpm = 80,
      columns = motion_cols,
      lg = lgr::get_logger_glue("BrainGnomes.test.motion")
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("above Nyquist frequency", warns)))
  expect_false(any(grepl("skipping motion filtering", warns)))
  expect_false(isTRUE(all.equal(filtered$rot_x, conf_df$rot_x)))
})

test_that("filter_confounds skips notch filter when adjusted band is invalid", {
  conf_df <- make_motion_confounds(40L)
  motion_cols <- c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z")
  warns <- character(0)
  filtered <- withCallingHandlers(
    filter_confounds(
      confounds_df = conf_df,
      tr = 4,
      filter_type = "notch",
      bandstop_min_bpm = 12,
      bandstop_max_bpm = 18,
      columns = motion_cols,
      lg = lgr::get_logger_glue("BrainGnomes.test.motion")
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("Adjusted notch band is invalid", warns)))
  expect_true(isTRUE(all.equal(filtered$rot_x, conf_df$rot_x)))
})

test_that("filter_confounds adjusts low-pass cutoff above Nyquist", {
  conf_df <- make_motion_confounds(60L)
  motion_cols <- c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z")
  warns <- character(0)
  filtered <- withCallingHandlers(
    filter_confounds(
      confounds_df = conf_df,
      tr = 1,
      filter_type = "lowpass",
      low_pass_hz = 0.6,
      columns = motion_cols,
      lg = lgr::get_logger_glue("BrainGnomes.test.motion")
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("above Nyquist frequency", warns)))
  expect_false(any(grepl("skipping motion filtering", warns)))
  expect_false(isTRUE(all.equal(filtered$rot_x, conf_df$rot_x)))
})
