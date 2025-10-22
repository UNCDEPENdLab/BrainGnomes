test_that("lmfit_residuals_4d removes effects of nuisance regressors", {
  skip_if_not_installed("RNifti")
  library(RNifti)
  
  # Create synthetic 4D data: 5x5x5 voxels, 50 timepoints
  dims <- c(5, 5, 5, 50)
  n_vox <- prod(dims[1:3])
  n_time <- dims[4]
  temp_file <- tempfile(fileext = ".nii.gz")
  
  # Simulate a simple linear trend in every voxel with some noise
  set.seed(1001)
  trend <- seq(0, 1, length.out = n_time)
  voxel_intercepts <- runif(n_vox, min = 100, max = 200)  # known intercept per voxel
  # simulate deterministic linear trend and unique intercept in each voxel
  signal <- outer(voxel_intercepts, rep(1, n_time)) + outer(rep(1, n_vox), trend) + rnorm(n_vox * n_time, sd = 0.1)
  img_data <- array(signal, dim = dims)
  img <- asNifti(img_data)
  writeNifti(img, temp_file)
  
  # Design matrix with intercept and trend
  X <- cbind(1, trend)
  
  # Censor 10 random timepoints
  censor_vec <- rep(TRUE, n_time)
  censor_vec[sample(n_time, 10)] <- FALSE
  
  # ---- Test full nuisance removal (intercept + trend) ----
  residual_file <- tempfile(fileext = ".nii.gz")
  residual <- lmfit_residuals_4d(
    infile = temp_file,
    X = X,
    include_rows = censor_vec,
    add_intercept = FALSE,
    outfile = residual_file
  )
  
  expect_s3_class(residual, "niftiImage")
  expect_equal(dim(residual), dims)
  
  # Residuals should have approx. mean 0
  res_data <- as.array(residual)
  voxel_means <- apply(res_data, 1:3, function(x) mean(x[censor_vec]))
  expect_true(all(abs(voxel_means) < 1e-2))
  
  # ---- Compare to base R lm() for a few voxels ----
  set.seed(42)
  random_voxels <- matrix(sample(n_vox, 3), ncol = 1)  # linear indices
  ijk_coords <- arrayInd(random_voxels, .dim = dims[1:3])
  
  for (v in seq_len(nrow(ijk_coords))) {
    idx <- ijk_coords[v, ]
    ts_data <- img_data[idx[1], idx[2], idx[3], ]
    ts_resid <- res_data[idx[1], idx[2], idx[3], ]
    
    # Fit lm using uncensored timepoints
    df <- data.frame(y = ts_data[censor_vec], trend = trend[censor_vec])
    lm_fit <- lm(y ~ trend, data = df)
    coefs <- coef(lm_fit)
    
    # Predict across full time series
    y_hat <- coefs[1] + coefs[2] * trend
    resid_expected <- ts_data - y_hat
    
    # Compare to residual from Rcpp
    expect_equal(ts_resid, resid_expected, tolerance = 1e-6)
  }
  
  # ---- Test partial nuisance removal (remove only trend) ----
  residual_partial_file <- tempfile(fileext = ".nii.gz")
  residual_partial <- lmfit_residuals_4d(
    infile = temp_file,
    X = X,
    include_rows = censor_vec,
    add_intercept = FALSE,
    remove_cols = 2L,
    outfile = residual_partial_file
  )
  
  expect_s3_class(residual_partial, "niftiImage")
  expect_equal(dim(residual_partial), dims)
  
  # Extract residuals and compute voxelwise means over uncensored timepoints
  res_partial_data <- as.array(residual_partial)
  voxel_means_partial <- apply(res_partial_data, 1:3, function(x) mean(x[censor_vec]))
  
  # Compare to known intercepts
  intercepts_array <- array(voxel_intercepts, dim = dims[1:3])
  delta <- voxel_means_partial - intercepts_array
  expect_true(all(abs(delta) < 0.1))  # Loose tolerance due to added noise
  
  # And should not contain linear trend anymore
  corrs <- apply(res_partial_data, 1:3, function(ts) cor(ts[censor_vec], trend[censor_vec]))
  expect_true(mean(abs(corrs)) < 0.001)
})

test_that("lmfit_residuals_4d falls back to pivoted QR when qr_econ fails", {
  skip_if_not_installed("RNifti")
  library(RNifti)
  
  dims <- c(3, 3, 3, 20)
  n_vox <- prod(dims[1:3])
  n_time <- dims[4]
  temp_file <- tempfile(fileext = ".nii.gz")
  
  # Generate simple signal with noise
  set.seed(123)
  base_signal <- outer(rep(100, n_vox), rep(1, n_time)) + rnorm(n_vox * n_time, sd = 0.1)
  img_data <- array(base_signal, dim = dims)
  img <- asNifti(img_data)
  writeNifti(img, temp_file)
  
  # Create a rank-deficient design matrix
  trend <- seq(-1, 1, length.out = n_time)
  drift <- trend * 2
  collinear <- trend + drift  # Still perfectly collinear
  X <- cbind(1, trend, trend, drift, collinear)  # rank-deficient (3 cols are linearly dependent)
  
  # Include all timepoints
  include_rows <- rep(TRUE, n_time)
  
  # Run the function — expect fallback to pivoted QR
  output_file <- tempfile(fileext = ".nii.gz")
  result <- lmfit_residuals_4d(
    infile = temp_file,
    X = X,
    include_rows = include_rows,
    add_intercept = FALSE,
    outfile = output_file
  )
  
  expect_s3_class(result, "niftiImage")
  expect_equal(dim(result), dims)
  
  # Optional: validate against lm() for a single voxel
  ts_data <- img_data[1, 1, 1, ]
  df <- data.frame(y = ts_data, trend = trend, drift = drift, collinear = collinear)
  lm_fit <- lm(y ~ trend + drift + collinear, data = df)
  y_hat <- predict(lm_fit)
  resid_expected <- unname(ts_data - y_hat)
  
  resid_cpp <- as.array(result)[1, 1, 1, ]
  expect_equal(resid_cpp, resid_expected, tolerance = 1e-6)
})