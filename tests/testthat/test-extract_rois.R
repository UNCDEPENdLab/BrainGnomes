test_that("extract_rois creates timeseries and correlations", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("corpcor")
  skip_if_not_installed("data.table")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")
  library(RNifti)
  tmpdir <- tempdir()
  arr <- array(rnorm(3*3*3*25), dim = c(3,3,3,25))
  bold_file <- file.path(tmpdir, "sub-01_task-test_desc-test_bold.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), bold_file)

  atlas_arr <- array(0, c(3,3,3))
  atlas_arr[1:2,1:2,1:2] <- 1
  atlas_arr[3,3,3] <- 2
  atlas_file <- file.path(tmpdir, "Schaefer2.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(atlas_arr), atlas_file)

  res <- extract_rois(bold_file, atlas_files = atlas_file, out_dir = tmpdir,
                      cor_method = "cor.shrink", roi_reduce = "median")
  atlas_name <- names(res)[1]
  ts_file <- res[[atlas_name]]$timeseries
  corr_file <- res[[atlas_name]]$correlation[["cor.shrink"]]
  expect_true(file.exists(ts_file))
  expect_true(file.exists(corr_file))
  cmat <- as.matrix(read.delim(corr_file, check.names = FALSE))
  expect_equal(dim(cmat), c(2, 2))
  expect_true(all(is.na(cmat[2, ])) && all(is.na(cmat[, 2])))
})

test_that("extract_rois respects percentage-based minimum voxel thresholds", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")
  skip_if_not_installed("data.table")
  tmpdir <- tempdir()

  dims <- c(4, 4, 4, 25)
  arr <- array(rnorm(prod(dims)), dim = dims)

  # Introduce constant time series for two ROI2 voxels so they fail the usable-voxel check
  arr[3, 3, 3, ] <- 0
  arr[4, 4, 4, ] <- 0

  bold_file <- file.path(tmpdir, "sub-02_task-test_desc-test_bold.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), bold_file)

  atlas_arr <- array(0, dims[1:3])
  atlas_arr[1:2, 1:2, 1:2] <- 1  # ROI1 (8 voxels)
  atlas_arr[3:4, 3:4, 3:4] <- 2  # ROI2 (8 voxels)
  atlas_file <- file.path(tmpdir, "SchaeferPct.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(atlas_arr), atlas_file)

  # Mask excludes three voxels from ROI1, leaving five voxels available
  mask_arr <- array(0L, dims[1:3])
  mask_arr[atlas_arr > 0] <- 1L
  mask_arr[1, 1, 2] <- 0L
  mask_arr[2, 1, 2] <- 0L
  mask_arr[1, 2, 2] <- 0L
  mask_file <- file.path(tmpdir, "brain_mask_pct.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(mask_arr), mask_file)

  out_dir <- file.path(tmpdir, "pct-threshold")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  expect_warning(
    res_pct <- extract_rois(
      bold_file = bold_file,
      atlas_files = atlas_file,
      out_dir = out_dir,
      mask_file = mask_file,
      min_vox_per_roi = "80%",
      cor_method = "pearson",
      roi_reduce = "mean",
      overwrite = TRUE,
      save_ts = TRUE
    ),
    regexp = "creating an empty file"
  )

  atlas_name <- names(res_pct)[1]
  ts_file <- res_pct[[atlas_name]]$timeseries
  ts_pct <- read.delim(ts_file, check.names = FALSE)
  expect_true(all(is.na(ts_pct$roi1)))
  expect_true(all(is.na(ts_pct$roi2)))

  res_frac <- extract_rois(
    bold_file = bold_file,
    atlas_files = atlas_file,
    out_dir = out_dir,
    mask_file = mask_file,
    min_vox_per_roi = 0.5,
    cor_method = "pearson",
    roi_reduce = "mean",
    overwrite = TRUE,
    save_ts = TRUE
  )

  ts_file_frac <- res_frac[[atlas_name]]$timeseries
  ts_frac <- read.delim(ts_file_frac, check.names = FALSE)
  expect_true(any(!is.na(ts_frac$roi1)))
  expect_true(any(!is.na(ts_frac$roi2)))
})

test_that("extract_rois brain mask prunes atlas voxels outside mask", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")
  skip_if_not_installed("data.table")

  tmpdir <- tempfile("extract-mask-")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

  dims <- c(3, 3, 3, 20)
  arr <- array(rnorm(prod(dims)), dim = dims)
  bold_file <- file.path(tmpdir, "sub-03_task-test_desc-test_bold.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), bold_file)

  atlas_arr <- array(0L, dims[1:3])
  atlas_arr[2, 2, 2] <- 1L
  atlas_arr[3, 3, 3] <- 2L
  atlas_file <- file.path(tmpdir, "SchaeferMaskPrune.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(atlas_arr), atlas_file)

  mask_arr <- array(0L, dims[1:3])
  mask_arr[2, 2, 2] <- 1L
  mask_file <- file.path(tmpdir, "brain_mask_pruned.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(mask_arr), mask_file)

  out_dir <- file.path(tmpdir, "mask-pruned-output")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res <- extract_rois(
    bold_file = bold_file,
    atlas_files = atlas_file,
    out_dir = out_dir,
    mask_file = mask_file,
    cor_method = "pearson",
    roi_reduce = "mean",
    min_vox_per_roi = 1,
    overwrite = TRUE,
    save_ts = TRUE
  )

  atlas_name <- names(res)[1]
  ts_file <- res[[atlas_name]]$timeseries
  cor_file <- res[[atlas_name]]$correlation[["pearson"]]

  expect_true(file.exists(ts_file))
  expect_true(file.exists(cor_file))

  ts_df <- read.delim(ts_file, check.names = FALSE)
  expect_named(ts_df, c("volume", "roi1"))
  expect_false("roi2" %in% colnames(ts_df))

  cor_mat <- as.matrix(read.delim(cor_file, check.names = FALSE))
  expect_equal(dim(cor_mat), c(1, 1))
})
