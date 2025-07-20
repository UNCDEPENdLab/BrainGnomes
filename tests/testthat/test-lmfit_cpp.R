test_that("lmfit_residuals_4d removes effects of nuisance regressors", {
  skip_if_not_installed("RNifti")
  library(RNifti)
  
  # Create synthetic 4D data: 5x5x5 voxels, 50 timepoints
  dims <- c(5, 5, 5, 50)
  n_vox <- prod(dims[1:3])
  n_time <- dims[4]
  temp_file <- tempfile(fileext = ".nii.gz")
  
  # Simulate a simple linear trend in every voxel with some noise
  trend <- seq(0, 1, length.out = n_time)
  signal <- outer(rep(1, n_vox), trend) + rnorm(n_vox * n_time, sd = 0.1)
  img_data <- array(signal, dim = dims)
  img <- asNifti(img_data)
  writeNifti(img, temp_file)
  
  # Create design matrix with intercept and trend
  X <- cbind(1, trend)
  
  # Censor 10 random timepoints
  set.seed(123)
  censor_vec <- rep(TRUE, n_time)
  censor_vec[sample(n_time, 10)] <- FALSE
  
  # Run denoising
  residual_file <- tempfile(fileext = ".nii.gz")
  residual <- lmfit_residuals_4d(
    infile = temp_file,
    X = X,
    include_rows = censor_vec,
    add_intercept = FALSE,
    outfile = residual_file,
  )
  
  expect_s3_class(residual, "niftiImage")
  expect_equal(dim(residual), dims)
  
  # Residuals should have approx. mean 0
  res_data <- as.array(residual)
  voxel_means <- apply(res_data, 1:3, function(x) mean(x[censor_vec]))
  expect_true(all(abs(voxel_means) < 1e-2))
})