test_that("natural_spline_interp matches R's splinefun", {
  set.seed(123)
  x <- c(0, 1, 2, 3, 4, 5)
  y <- sin(x)
  xout <- seq(0, 5, by = 0.1)

  y_cpp <- natural_spline_interp(x, y, xout)
  spline_fn <- splinefun(x, y, method = "natural")
  y_r <- spline_fn(xout)

  expect_equal(y_cpp, y_r, tolerance = 1e-6)
})

test_that("natural_spline_4d matches voxelwise splinefun", {
  set.seed(1)
  dims <- c(5, 5, 5, 20)
  arr <- array(rnorm(prod(dims)), dim = dims)
  tmpnif <- tempfile(fileext=".nii.gz")
  RNifti::writeNifti(asNifti(arr), file = tmpnif)

  time_to_interp <- c(5, 10, 15, 20)  # 1-based timepoints
  arr_interp <- natural_spline_4d(tmpnif, t_interpolate = time_to_interp, edge_nn = FALSE)
  arr_interp <- array(arr_interp, dim = dims)

  # Check a few random voxels against splinefun interpolation
  for (i in 1:3) {
    coords <- sample(1:5, 3)
    ts_orig <- arr[coords[1], coords[2], coords[3], ]

    valid_t <- setdiff(1:20, time_to_interp)
    ts_valid <- ts_orig[valid_t]

    spline_fn <- splinefun(valid_t, ts_valid, method = "natural")
    ts_expected <- ts_orig
    ts_expected[time_to_interp] <- spline_fn(time_to_interp)

    ts_cpp <- arr_interp[coords[1], coords[2], coords[3], ]
    expect_equal(ts_cpp, ts_expected, tolerance = 1e-6)
  }
  
  unlink(tmpnif)
})


test_that("natural_spline_interp_4d errors on invalid interpolation timepoints", {
  set.seed(1)
  dims <- c(5, 5, 5, 20)
  arr <- array(rnorm(prod(dims)), dim = dims)
  
  tmpnif <- tempfile(fileext=".nii.gz")
  RNifti::writeNifti(asNifti(arr), file = tmpnif)
  
  expect_error(natural_spline_4d(tmpnif, -1, edge_nn = FALSE))
  expect_error(natural_spline_4d(tmpnif, 21, edge_nn = FALSE))
  
  unlink(tmpnif)
})