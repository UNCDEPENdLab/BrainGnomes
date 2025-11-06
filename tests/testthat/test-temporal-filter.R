test_that("temporal_filter skips high-pass when cutoff omitted", {
  commands <- character()
  with_mocked_bindings({
    temporal_filter(
      in_file = "fake_input.nii.gz",
      out_file = "fake_output.nii.gz",
      low_pass_hz = 0.1,
      high_pass_hz = NULL,
      tr = 2,
      method = "fslmaths"
    )
  }, run_fsl_command = function(args, ...) {
    commands <<- c(commands, args)
    invisible(NULL)
  })

  bptf_cmd <- commands[grepl("-bptf", commands, fixed = TRUE)]
  expect_length(bptf_cmd, 1)
  tokens <- strsplit(bptf_cmd, " ")[[1]]
  idx <- which(tokens == "-bptf")
  expect_length(idx, 1)
  hp <- tokens[idx + 1]
  lp <- tokens[idx + 2]
  expect_identical(hp, "-1")
  expect_gt(suppressWarnings(as.numeric(lp)), 0)
})

test_that("temporal_filter skips low-pass when cutoff omitted", {
  commands <- character()
  with_mocked_bindings({
    temporal_filter(
      in_file = "fake_input.nii.gz",
      out_file = "fake_output.nii.gz",
      low_pass_hz = NULL,
      high_pass_hz = 0.01,
      tr = 2,
      method = "fslmaths"
    )
  }, run_fsl_command = function(args, ...) {
    commands <<- c(commands, args)
    invisible(NULL)
  })

  bptf_cmd <- commands[grepl("-bptf", commands, fixed = TRUE)]
  expect_length(bptf_cmd, 1)
  tokens <- strsplit(bptf_cmd, " ")[[1]]
  idx <- which(tokens == "-bptf")
  expect_length(idx, 1)
  hp <- suppressWarnings(as.numeric(tokens[idx + 1]))
  lp <- tokens[idx + 2]
  expect_gt(hp, 0)
  expect_identical(lp, "-1")
})

test_that("validate_postprocess_config_single accepts band-pass ordering", {
  cfg <- list(
    input_regex = "regex:.*",
    bids_desc = "desc",
    keep_intermediates = FALSE,
    overwrite = FALSE,
    tr = 2,
    temporal_filter = list(
      enable = TRUE,
      low_pass_hz = 0.09,
      high_pass_hz = 0.008,
      prefix = "f",
      method = "fslmaths"
    ),
    spatial_smooth = list(enable = FALSE),
    intensity_normalize = list(enable = FALSE),
    confound_calculate = list(enable = FALSE),
    scrubbing = list(enable = FALSE),
    confound_regression = list(enable = FALSE),
    apply_mask = list(enable = FALSE),
    apply_aroma = list(enable = FALSE),
    force_processing_order = FALSE
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_false(any(grepl("postprocess/temporal_filter", res$gaps, fixed = TRUE)))
})

test_that("validate_postprocess_config_single flags reversed band-pass", {
  cfg <- list(
    input_regex = "regex:.*",
    bids_desc = "desc",
    keep_intermediates = FALSE,
    overwrite = FALSE,
    tr = 2,
    temporal_filter = list(
      enable = TRUE,
      low_pass_hz = 0.05,
      high_pass_hz = 0.2,
      prefix = "f",
      method = "fslmaths"
    ),
    spatial_smooth = list(enable = FALSE),
    intensity_normalize = list(enable = FALSE),
    confound_calculate = list(enable = FALSE),
    scrubbing = list(enable = FALSE),
    confound_regression = list(enable = FALSE),
    apply_mask = list(enable = FALSE),
    apply_aroma = list(enable = FALSE),
    force_processing_order = FALSE
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_true("postprocess/temporal_filter/low_pass_hz" %in% res$gaps)
  expect_true("postprocess/temporal_filter/high_pass_hz" %in% res$gaps)
})
