# Write a tiny synthetic 4D image; return its path and remove it after the test.
# Values are repeated at each spatial voxel to make numerical checks explicit.
release_test_image <- function(values, datatype = "FLOAT64") {
  file <- tempfile(fileext = ".nii.gz")
  withr::defer(unlink(file), envir = parent.frame())
  image <- array(rep(values, each = 8L), c(2L, 2L, 2L, length(values)))
  RNifti::writeNifti(image, file, datatype = datatype)
  file
}

test_that("interpolation replaces spikes when retained samples are constant", {
  file <- release_test_image(c(900, 5, 5, -900, 5, 5, 800))
  for (edge_nn in c(FALSE, TRUE)) {
    result <- natural_spline_4d(file, c(1L, 4L, 7L), edge_nn = edge_nn)
    expect_equal(as.numeric(result), rep(5, 56L))
  }
})

test_that("regression applies the fitted model to censored and constant series", {
  file <- release_test_image(c(5, 5, 900, 5, 5))
  include <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
  result <- lmfit_residuals_4d(file, matrix(1, 5, 1), include_rows = include)
  expect_equal(as.numeric(result[1, 1, 1, ]), c(0, 0, 895, 0, 0))
  expect_equal(as.numeric(lmfit_residuals_mat(matrix(c(5, 5, 900, 5, 5), 5, 1),
    matrix(1, 5, 1), include_rows = include)), c(0, 0, 895, 0, 0))

  file <- release_test_image(rep(5, 5))
  design <- matrix(seq_len(5), ncol = 1)
  expected <- stats::lm.fit(design, rep(5, 5))$residuals
  result <- lmfit_residuals_4d(file, design)
  expect_equal(as.numeric(result[1, 1, 1, ]), expected, tolerance = 1e-6)
  expect_equal(as.numeric(lmfit_residuals_mat(matrix(5, 5, 1), design)), expected,
    tolerance = 1e-6)

  result <- lmfit_residuals_4d(file, cbind(1, design), regress_cols = 2L)
  expect_equal(as.numeric(result[1, 1, 1, ]), rep(5, 5), tolerance = 1e-6)

  result <- lmfit_residuals_4d(file, design, set_mean = 100)
  expect_equal(mean(result[1, 1, 1, ]), 100, tolerance = 1e-6)
  expect_equal(mean(lmfit_residuals_mat(matrix(5, 5, 1), design, set_mean = 100)), 100)
})

test_that("integer native outputs preserve storage type within header quantization", {
  values <- c(1, 4, 0, 8, 2, 9, 5, 7, 1, 6)
  for (datatype in c("INT16", "UINT8")) {
    file <- release_test_image(values, datatype)
    input_datatype <- RNifti::niftiHeader(file)$datatype
    out <- tempfile(fileext = ".nii.gz")
    withr::defer(unlink(out))
    operations <- list(
      spline = function() natural_spline_4d(file, 3L, outfile = out),
      regression = function() lmfit_residuals_4d(file, matrix(1, 10, 1), outfile = out),
      filter = function() butterworth_filter_cpp(file, c(0.5, 0.5), c(1, 0),
        padlen = 0L, outfile = out)
    )
    for (operation in operations) {
      result <- operation()
      saved <- RNifti::readNifti(out)
      header <- RNifti::niftiHeader(out)
      # RNifti may choose unit spacing for in-range values. Bound the decoding
      # error by half the stored spacing, plus decoding arithmetic rounding.
      spacing <- if (header$scl_slope == 0) 1 else abs(header$scl_slope)
      rounding <- 8 * .Machine$double.eps * max(1e-38, abs(as.numeric(result)), abs(header$scl_inter))
      expect_lte(max(abs(as.numeric(saved) - as.numeric(result))), spacing / 2 + rounding)
      expect_identical(header$datatype, input_datatype)
      expect_true(all(is.finite(saved)))
    }
  }
})

test_that("scaled integer inputs are decoded before processing and retain output type", {
  file <- tempfile(fileext = ".nii.gz")
  out <- tempfile(fileext = ".nii.gz")
  withr::defer(unlink(c(file, out)))
  raw <- c(100, 125, 0, 175, 200, 225, 250, 275, 300, 325)
  RNifti::writeNifti(array(rep(raw, each = 8L), c(2, 2, 2, 10)), file,
    datatype = "INT16")
  scaled <- RNifti::updateNifti(RNifti::readNifti(file, internal = TRUE),
    list(scl_slope = 0.01, scl_inter = -1.25), datatype = "INT16")
  RNifti::writeNifti(scaled, file, datatype = "INT16")
  input_header <- RNifti::niftiHeader(file)
  expect_identical(input_header$datatype, 4L)
  expect_equal(input_header$scl_slope, 0.01, tolerance = 1e-6)
  expect_equal(input_header$scl_inter, -1.25)
  decoded <- RNifti::readNifti(file)[1, 1, 1, ]
  expect_equal(as.numeric(decoded), raw * 0.01 - 1.25, tolerance = 1e-6)
  expected_spline <- decoded
  expected_spline[3] <- stats::splinefun((1:10)[-3], decoded[-3], method = "natural")(3)
  expected <- list(spline = expected_spline, regression = decoded - mean(decoded),
    filter = 4 * decoded)
  operations <- list(
    spline = function() natural_spline_4d(file, 3L, outfile = out),
    regression = function() lmfit_residuals_4d(file, matrix(1, 10, 1), outfile = out),
    filter = function() butterworth_filter_cpp(file, 2, 1, padlen = 0L,
      demean = FALSE, outfile = out)
  )
  for (name in names(operations)) {
    result <- operations[[name]]()
    expect_equal(as.numeric(result[1, 1, 1, ]), as.numeric(expected[[name]]), tolerance = 1e-6)
    header <- RNifti::niftiHeader(out)
    expect_identical(header$datatype, input_header$datatype)
    spacing <- if (header$scl_slope == 0) 1 else abs(header$scl_slope)
    rounding <- 8 * .Machine$double.eps * max(1e-38, abs(as.numeric(result)), abs(header$scl_inter))
    expect_lte(max(abs(as.numeric(RNifti::readNifti(out)) - as.numeric(result))),
      spacing / 2 + rounding)
  }
})

test_that("filtering supports scalar gains and demeans nonzero constant voxels", {
  for (use_zi in c(FALSE, TRUE)) {
    expect_equal(filtfilt_cpp(1:5, b = 2, a = 1, padlen = 0L, use_zi = use_zi),
      4 * seq_len(5))
  }
  file <- release_test_image(rep(5, 10))
  expect_equal(as.numeric(butterworth_filter_cpp(file, 1, 1, padlen = 0L)), rep(0, 80L))
  expect_equal(as.numeric(butterworth_filter_cpp(file, 2, 1, padlen = 0L, demean = FALSE)),
    rep(20, 80L))
})

test_that("native filters reject invalid arguments even for zero images", {
  file <- release_test_image(rep(0, 10))
  invalid <- list(
    list(b = numeric(), a = 1), list(b = 1, a = numeric()),
    list(b = NA_real_, a = 1), list(b = 1, a = Inf),
    list(b = 1, a = 0), list(b = 1, a = 2),
    list(b = 1, a = 1, padlen = -2L),
    list(b = 1, a = 1, padtype = "invalid", padlen = 0L),
    list(b = 1, a = 1, padlen = 10L)
  )
  for (args in invalid) {
    expect_error(do.call(filtfilt_cpp, c(list(x = seq_len(10)), args)))
    expect_error(do.call(butterworth_filter_cpp, c(list(infile = file), args)))
  }
})

test_that("native regression rejects missing selections and invalid designs", {
  file <- release_test_image(seq_len(5))
  expect_error(lmfit_residuals_4d(file, matrix(c(1, 2, NA, 4, 5), 5, 1)), "X must contain only finite")
  expect_error(lmfit_residuals_4d(file, matrix(1, 5, 1),
    include_rows = c(TRUE, TRUE, NA, TRUE, TRUE)), "include_rows must not contain missing")
  expect_error(lmfit_residuals_4d(file, matrix(1, 5, 1), set_mean = Inf), "set_mean must be finite")
  expect_error(lmfit_residuals_4d(file, matrix(1, 5, 1), regress_cols = NA_integer_),
    "positive, non-missing column indices")
})

test_that("native time series helpers reject images with extra dimensions", {
  file <- tempfile(fileext = ".nii.gz")
  withr::defer(unlink(file))
  RNifti::writeNifti(array(1, c(2, 2, 2, 5, 2)), file)
  expect_error(natural_spline_4d(file, 3L), "must be 4D")
  expect_error(lmfit_residuals_4d(file, matrix(1, 5, 1)), "must be 4D")
  expect_error(butterworth_filter_cpp(file, 1, 1), "must be 4D")
  expect_error(remove_nifti_volumes(file, 3L, tempfile(fileext = ".nii.gz")), "must be 4D")
})

test_that("quantile selection excludes non-finite voxels only through a mask", {
  file <- tempfile(fileext = ".nii.gz")
  mask <- tempfile(fileext = ".nii.gz")
  withr::defer(unlink(c(file, mask)))
  values <- array(seq_len(8), c(2, 2, 2))
  for (invalid in c(NA_real_, NaN, Inf, -Inf)) {
    values[1] <- invalid
    RNifti::writeNifti(values, file, datatype = "FLOAT64")
    expect_error(image_quantile(file), "only finite values")
    RNifti::writeNifti(array(c(0, rep(1, 7)), c(2, 2, 2)), mask)
    expect_equal(unname(image_quantile(file, mask)), 5)
  }
  expect_error(natural_spline_interp(1:3, 1:3, NA_real_), "xout must contain only finite")
})
