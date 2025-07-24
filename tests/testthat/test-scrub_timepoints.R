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


test_that("scrub_interpolate updates confounds with nearest-neighbor edges", {
  skip_if_not_installed("RNifti")
  library(RNifti)
  dims <- c(1, 1, 1, 6)
  arr <- array(rnorm(6), dim = dims)
  infile <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), infile)

  conf_df <- data.frame(a = 1:6)
  conf_file <- tempfile(fileext = ".tsv")
  data.table::fwrite(conf_df, conf_file, sep = "\t", col.names = FALSE)

  censor_file <- tempfile()
  writeLines(c("0", "1", "1", "1", "1", "0"), censor_file)

  out <- scrub_interpolate(infile, censor_file,
                           confound_files = conf_file,
                           prefix = "s", lg = lgr::get_logger_glue())
  conf_new <- data.table::fread(conf_file, header = FALSE)[[1]]
  expected <- conf_df$a
  expected[1] <- conf_df$a[2]
  expected[6] <- conf_df$a[5]
  expect_equal(conf_new, expected)
})

