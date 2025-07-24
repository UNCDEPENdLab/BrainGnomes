library(testthat)

test_that("scrub_timepoints removes timepoints and updates confounds", {
  skip_if_not_installed("RNifti")
  library(RNifti)
  dims <- c(2, 2, 2, 5)
  arr <- array(seq_len(prod(dims)), dim = dims)
  infile <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), infile)

  conf_df <- data.frame(a = 1:5, b = 6:10)
  conf_file <- tempfile(fileext = ".tsv")
  data.table::fwrite(conf_df, conf_file, sep = "\t", col.names = FALSE)

  censor_file <- tempfile()
  writeLines(c("1", "0", "1", "0", "1"), censor_file)

  out <- scrub_timepoints(infile, censor_file,
                          confound_files = conf_file,
                          prefix = "s", lg = lgr::get_logger_glue())
  res <- RNifti::readNifti(out)
  expect_equal(dim(res)[4], 3)

  conf_new <- data.table::fread(conf_file, header = FALSE)
  expect_equal(nrow(conf_new), 3)
  expect_equal(conf_new[[1]], c(1, 3, 5))
  expect_equal(conf_new[[2]], c(6, 8, 10))
})


test_that("scrub_timepoints interpolates confounds when requested", {
  skip_if_not_installed("RNifti")
  library(RNifti)
  dims <- c(1, 1, 1, 5)
  arr <- array(rnorm(5), dim = dims)
  infile <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), infile)

  conf_df <- data.frame(a = 1:5)
  conf_file <- tempfile(fileext = ".tsv")
  data.table::fwrite(conf_df, conf_file, sep = "\t", col.names = FALSE)

  censor_file <- tempfile()
  writeLines(c("1", "0", "0", "1", "1"), censor_file)

  out <- scrub_timepoints(infile, censor_file,
                          confound_files = conf_file, interpolate = TRUE,
                          prefix = "s", lg = lgr::get_logger_glue())
  conf_new <- data.table::fread(conf_file, header = FALSE)[[1]]
  good_idx <- c(1, 4, 5)
  sfun <- splinefun(good_idx, conf_df$a[good_idx], method = "natural")
  expected <- conf_df$a
  expected[c(2,3)] <- sfun(c(2,3))
  expect_equal(conf_new, expected)
})
