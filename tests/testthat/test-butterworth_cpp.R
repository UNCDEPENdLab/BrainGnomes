test_that("butterworth_filter_4d matches signal::filtfilt and filtfilt_cpp", {
  skip_if_not_installed("signal")
  skip_if_not_installed("RNifti")
  
  
  # Parameters
  dims <- c(5, 5, 5, 100)
  n_vox <- prod(dims[1:3])
  n_time <- dims[4]
  low_hz <- 0.008
  high_hz <- 0.1
  tr <- 1.2
  fs <- 1 / tr
  filt_order <- 2L
  temp_file <- tempfile(fileext = ".nii.gz")
  
  # Simulate temporal structure
  time <- seq(0, by = tr, length.out = n_time)
  drift <- outer(rep(1, n_vox), 0.02 * time)
  sin1 <- outer(rep(1, n_vox), sin(2 * pi * 0.01 * time))
  sin2 <- outer(rep(1, n_vox), sin(2 * pi * 0.05 * time))
  noise <- matrix(rnorm(n_vox * n_time, sd = 0.1), n_vox, n_time)
  signal <- drift + sin1 + sin2 + noise
  img_data <- array(signal, dim = dims)
  
  # Write to NIfTI
  img <- asNifti(img_data)
  RNifti::writeNifti(img, temp_file)
  
  # Filter and compare
  outfile <- tempfile(fileext = ".nii.gz")
  result <- butterworth_filter_4d(
    infile = temp_file,
    tr = tr,
    low_hz = low_hz,
    high_hz = high_hz,
    outfile = outfile,
    internal = FALSE,
    order = filt_order, padtype = "constant", use_zi = TRUE
  )
  
  result_data <- as.array(result)
  
  # Pull a few random voxels and compare to signal::filtfilt
  b_a <- signal::butter(filt_order, c(low_hz, high_hz) / (fs / 2), type = "pass")
  
  set.seed(123)
  voxel_indices <- replicate(5, {
    x <- sample(dims[1], 1)
    y <- sample(dims[2], 1)
    z <- sample(dims[3], 1)
    c(x, y, z)
  }, simplify = FALSE)
  
  for (idx in voxel_indices) {
    x <- idx[1]; y <- idx[2]; z <- idx[3]
    ts_original <- img_data[x, y, z, ]
    ts_r <- signal::filtfilt(b_a, ts_original) # using signal package -- uses zero padding and no zi, so not identical
    ts_cpp <- filtfilt_cpp(ts_original, b = b_a$b, a = b_a$a, padtype = "constant", use_zi = TRUE)
    ts_out <- result_data[x, y, z, ] # voxel in generated 4d file
    
    expect_equal(ts_cpp, ts_out, tolerance = 1e-6) # voxelwise wrapper versus direct C++ call -- should be exact
    expect_gt(cor(ts_cpp, ts_r), expected = 0.90) # we can only expect approximate match due to the difference in implementation
  }
})