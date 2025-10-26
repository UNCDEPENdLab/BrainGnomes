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
  expect_true(all(abs(voxel_means) < 1e-3))
  
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
    regress_cols = 2L,
    exclusive = TRUE,
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

test_that("exclusive regression requires regress_cols", {
  skip_if_not_installed("RNifti")
  library(RNifti)

  dims <- c(2, 2, 2, 10)
  temp_file <- tempfile(fileext = ".nii.gz")
  img <- asNifti(array(runif(prod(dims)), dim = dims))
  writeNifti(img, temp_file)

  X <- cbind(1, seq_len(dims[4]))

  expect_error(
    lmfit_residuals_4d(
      infile = temp_file,
      X = X,
      add_intercept = FALSE,
      regress_cols = NULL,
      exclusive = TRUE
    ),
    "exclusive = TRUE requires regress_cols"
  )
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
  
  
  # Run the function — expect fallback to pivoted QR
  output_file <- tempfile(fileext = ".nii.gz")
  expect_warning(result <- lmfit_residuals_4d(
    infile = temp_file,
    X = X,
    add_intercept = FALSE,
    outfile = output_file
  ), regexp = "Design matrix appears rank deficient")
  
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

test_that("lmfit_residuals_4d defaults to all timepoints when include_rows is omitted", {
  skip_if_not_installed("RNifti")
  library(RNifti)

  dims <- c(2, 2, 2, 12)
  n_vox <- prod(dims[1:3])
  n_time <- dims[4]
  temp_file <- tempfile(fileext = ".nii.gz")

  set.seed(2024)
  trend <- seq(-0.5, 0.5, length.out = n_time)
  intercepts <- runif(n_vox, 50, 150)
  signal <- outer(intercepts, rep(1, n_time)) + outer(rep(1, n_vox), trend) + rnorm(n_vox * n_time, sd = 0.05)
  img <- asNifti(array(signal, dim = dims))
  writeNifti(img, temp_file)

  X <- cbind(1, trend)

  residual_with <- lmfit_residuals_4d(
    infile = temp_file,
    X = X,
    include_rows = rep(TRUE, n_time),
    add_intercept = FALSE
  )

  residual_default <- lmfit_residuals_4d(
    infile = temp_file,
    X = X,
    add_intercept = FALSE
  )

  expect_equal(as.vector(residual_default), as.vector(residual_with))
})

test_that("lmfit_residuals_4d matches R's lm() for multiple regression", {
  set.seed(123)
  skip_if_not_installed("MASS")
  skip_if_not_installed("RNifti")
  library(MASS)
  library(RNifti)
  
  ## --- simulate correlated regressors ---
  n <- 200
  Sigma <- matrix(0.3, 3, 3)
  diag(Sigma) <- 1
  X <- mvrnorm(n, mu = rep(0, 3), Sigma = Sigma)
  colnames(X) <- c("X1", "X2", "X3")
  
  ## --- generate Y according to model Y = 1.5 + .2*X1 + .2*X2 + .2*X3 + noise ---
  Y <- 1.5 + 0.2 * X[, 1] + 0.2 * X[, 2] + 0.2 * X[, 3] + rnorm(n, sd = 0.5)
  
  ## --- write to minimal 4D nifti (1×1×1×n) ---
  img <- RNifti::asNifti(array(Y, dim = c(1, 1, 1, n)))
  tmpfile <- tempfile(fileext = ".nii")
  RNifti::writeNifti(img, tmpfile)
  
  ## --- full model: regress all 3 predictors ---
  res_cpp_full <- lmfit_residuals_4d(
    infile = tmpfile,
    X = X,
    add_intercept = TRUE
  )
  
  ## --- R’s lm() baseline for full model ---
  fit_r_full <- lm(Y ~ X)
  res_r_full <- unname(residuals(fit_r_full))
  
  ## --- compare (allowing for floating-point rounding) ---
  cpp_res_full <- as.numeric(res_cpp_full[1, 1, 1, ])
  expect_equal(cpp_res_full, res_r_full, tolerance = 1e-6)
  
  ## --- partial regression: only X1 and X2 (exclusive = TRUE) ---
  res_cpp_partial <- lmfit_residuals_4d(
    infile = tmpfile,
    X = X,
    regress_cols = 1:2,
    exclusive = TRUE,
    add_intercept = TRUE
  )
  
  ## --- R’s lm() baseline for Y ~ X1 + X2 ---
  fit_r_partial <- lm(Y ~ X[, 1] + X[, 2])
  res_r_partial <- unname(residuals(fit_r_partial))
  
  cpp_res_partial <- as.numeric(res_cpp_partial[1, 1, 1, ])
  expect_equal(cpp_res_partial, res_r_partial, tolerance = 1e-6)
  
  # partial regression without intercept
  res_cpp_partial_noint <- lmfit_residuals_4d(
    infile = tmpfile,
    X = X,
    regress_cols = 1:2,
    exclusive = TRUE,
    add_intercept = FALSE
  )
  
  fit_r_partial_noint <- lm(Y ~ -1 + X[, 1] + X[, 2])
  res_r_partial_noint <- unname(residuals(fit_r_partial_noint))
  
  cpp_res_partial_noint <- as.numeric(res_cpp_partial_noint[1, 1, 1, ])
  expect_equal(cpp_res_partial_noint, res_r_partial_noint, tolerance = 1e-6)
})
