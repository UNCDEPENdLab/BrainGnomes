test_that("extract_rois creates timeseries and correlations", {
  skip_if_not_installed("RNifti")
  library(RNifti)
  tmpdir <- tempdir()
  arr <- array(rnorm(3*3*3*5), dim = c(3,3,3,5))
  bold_file <- file.path(tmpdir, "sub-01_task-test_desc-test_bold.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), bold_file)

  atlas_arr <- array(0, c(3,3,3))
  atlas_arr[1:2,1:2,1:2] <- 1
  atlas_arr[3,3,3] <- 2
  atlas_file <- file.path(tmpdir, "Schaefer2.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(atlas_arr), atlas_file)

  res <- extract_rois(bold_file, atlas_files = atlas_file, out_dir = tmpdir,
                      cor_method = "kendall", roi_reduce = "median")
  atlas_name <- names(res)[1]
  ts_file <- res[[atlas_name]]$timeseries
  corr_file <- res[[atlas_name]]$correlation[["kendall"]]
  expect_true(file.exists(ts_file))
  expect_true(file.exists(corr_file))
  ts_df <- data.table::fread(ts_file)
  expect_true("roi1" %in% names(ts_df))
})
