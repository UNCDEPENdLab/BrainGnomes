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

test_that("validate_postprocess_config_single accepts motion filter band", {
  cfg <- make_base_postprocess_cfg()
  cfg$motion_filter <- list(
    enable = TRUE,
    band_stop_min = 12,
    band_stop_max = 20
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_false(any(grepl("postprocess/motion_filter", res$gaps, fixed = TRUE)))
})

test_that("validate_postprocess_config_single flags inverted motion filter band", {
  cfg <- make_base_postprocess_cfg()
  cfg$motion_filter <- list(
    enable = TRUE,
    band_stop_min = 20,
    band_stop_max = 15
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_true("postprocess/motion_filter/band_stop_min" %in% res$gaps)
  expect_true("postprocess/motion_filter/band_stop_max" %in% res$gaps)
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
