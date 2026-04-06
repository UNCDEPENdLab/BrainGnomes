test_that("validate_char treats sentinel empty string values as NA_character_", {
  expect_identical(validate_char(""), NA_character_)
  expect_identical(validate_char(".na.character"), NA_character_)
  expect_identical(validate_char(".na"), NA_character_)
  expect_identical(validate_char(NA_character_), NA_character_)
  expect_identical(validate_char(NULL), NA_character_)
  expect_identical(validate_char(list()), NA_character_)
  expect_identical(validate_char(character(0)), NA_character_)
  expect_identical(validate_char(NA), NA_character_)
})

test_that("validate_char preserves non-empty strings", {
  expect_identical(validate_char("--output-spaces MNI152NLin2009cAsym"), "--output-spaces MNI152NLin2009cAsym")
  expect_identical(validate_char("  hello  "), "hello")
})

test_that("validate_char respects empty_value = NULL", {
  expect_null(validate_char("", empty_value = NULL))
  expect_null(validate_char(".na.character", empty_value = NULL))
  expect_null(validate_char(NULL, empty_value = NULL))
  expect_null(validate_char(list(), empty_value = NULL))
  expect_null(validate_char(NA, empty_value = NULL))
  expect_identical(validate_char("MNI152NLin2009cAsym", empty_value = NULL), "MNI152NLin2009cAsym")
})

test_that("validate_char handles multi-element vectors", {
  expect_identical(validate_char(c("a", "b")), c("a", "b"))
  # NAs and sentinels are stripped from vectors
  expect_identical(validate_char(c("a", NA, ".na.character", "b")), c("a", "b"))
  # all-empty vector collapses
  expect_identical(validate_char(c("", NA)), NA_character_)
})

test_that("validate_postprocess_config_single normalizes sentinel noproc_columns to NULL", {
  # Minimal config that exercises the confound_calculate and confound_regression

  # noproc_columns validation paths. YAML ".na.character" arrives as NA_character_
  # which must be normalised to NULL (not flagged as a validation gap).
  cfg <- list(
    input_regex = "regex: .*_bold\\.nii\\.gz$",
    bids_desc = "test",
    keep_intermediates = FALSE,
    overwrite = FALSE,
    tr = 1.0,
    confound_calculate = list(
      enable = TRUE,
      demean = TRUE,
      columns = "csf",
      noproc_columns = NA_character_ # simulates .na.character from YAML
    ),
    confound_regression = list(
      enable = TRUE,
      columns = "csf",
      noproc_columns = NA_character_,
      prefix = "r"
    )
  )

  res <- validate_postprocess_config_single(cfg, cfg_name = "test", quiet = TRUE)
  expect_null(res$postprocess$confound_calculate$noproc_columns)
  expect_null(res$postprocess$confound_regression$noproc_columns)
  expect_false("postprocess/confound_calculate/noproc_columns" %in% res$gaps)
  expect_false("postprocess/confound_regression/noproc_columns" %in% res$gaps)
})
