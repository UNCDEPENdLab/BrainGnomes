#' Read the actual storage scaling, including NIfTI-2 double header fields
#' @param file Written NIfTI path, compressed or uncompressed.
#' @return RNifti header fields with the actual on-disk slope and intercept.
#' @noRd
nifti_storage_test_header <- function(file) {
  header <- RNifti::niftiHeader(file)
  if (unname(RNifti::niftiVersion(file)) == 2L) {
    # RNifti's R header accessor returns a NIfTI-1 representation even for
    # NIfTI-2. Read its double scaling fields directly to avoid narrowed values.
    connection <- gzfile(file, "rb")
    on.exit(close(connection))
    bytes <- readBin(connection, "raw", n = 192L)
    native <- readBin(bytes[1:4], "integer", n = 1L, size = 4L) == 540L
    endian <- if (native) .Platform$endian else
      if (.Platform$endian == "little") "big" else "little"
    header$scl_slope <- readBin(bytes[177:184], "double", n = 1L, size = 8L, endian = endian)
    header$scl_inter <- readBin(bytes[185:192], "double", n = 1L, size = 8L, endian = endian)
  }
  header
}

#' Check decoded output against the computed values and its storage resolution
#' @param file Written NIfTI path.
#' @param values Expected processed values.
#' @param datatype Expected integer storage type name.
#' @return Expectations for datatype, finiteness, quantization, and resolution.
#' @noRd
expect_integer_nifti_roundtrip <- function(file, values, datatype) {
  types <- c(INT8 = 256L, UINT8 = 2L, INT16 = 4L, UINT16 = 512L,
    INT32 = 8L, UINT32 = 768L, INT64 = 1024L, UINT64 = 1280L)
  bits <- c(INT8 = 8, UINT8 = 8, INT16 = 16, UINT16 = 16,
    INT32 = 32, UINT32 = 32, INT64 = 64, UINT64 = 64)
  header <- nifti_storage_test_header(file)
  decoded <- as.numeric(RNifti::readNifti(file))
  values <- as.numeric(values)
  expect_identical(header$datatype, unname(types[datatype]))
  expect_true(all(is.finite(decoded)))
  expect_true(is.finite(header$scl_slope) && header$scl_slope > 0)
  expect_true(is.finite(header$scl_inter))
  spacing <- abs(header$scl_slope)
  rounding <- 8 * .Machine$double.eps * max(.Machine$double.xmin, abs(values), abs(header$scl_inter))
  expect_lte(max(abs(decoded - values)), spacing / 2 + rounding)
  span <- diff(range(values))
  if (span > 0) {
    levels <- 2^bits[datatype] - 1 - as.integer(datatype == "INT32")
    # Reject a coarse grid that merely makes a large decoding error permissible.
    # Allow binary-grid alignment and NIfTI-1's minimum representable slope.
    header_minimum <- if (unname(RNifti::niftiVersion(file)) == 1L) 2^-149 else
      .Machine$double.xmin * .Machine$double.eps
    expect_lte(spacing, max(4 * span / levels, header_minimum) * (1 + 1e-6))
  }
  if (any(values == 0)) expect_true(all(decoded[values == 0] == 0))
}

test_that("scaled output survives range expansion across integer types and NIfTI versions", {
  types <- c("INT8", "UINT8", "INT16", "UINT16", "INT32", "UINT32", "INT64", "UINT64")
  for (version in c(1L, 2L)) {
    for (datatype in types) {
      input <- tempfile(fileext = ".nii.gz")
      output <- tempfile(fileext = ".nii.gz")
      withr::defer(unlink(c(input, output)))
      raw <- c(4, 1:8, 4)
      RNifti::writeNifti(array(rep(raw, each = 8L), c(2, 2, 2, 10)), input,
        datatype = datatype, version = version)
      scaled <- RNifti::updateNifti(RNifti::readNifti(input, internal = TRUE),
        list(scl_slope = 0.125, scl_inter = -0.5), datatype = datatype)
      RNifti::writeNifti(scaled, input, datatype = datatype, version = version)
      decoded <- as.numeric(RNifti::readNifti(input)[1, 1, 1, ])
      expect_equal(decoded, raw * 0.125 - 0.5)
      gain <- if (datatype %in% c("INT64", "UINT64")) 1e12 else 1e6
      spline <- decoded
      spline[c(1, 10)] <- c(-0.5, 0.625)
      expected <- list(spline = spline, regression = decoded - mean(decoded),
        shifted = decoded - mean(decoded) + 1e6, gain = decoded * gain^2)
      operations <- list(
        spline = function() natural_spline_4d(input, c(1L, 10L), outfile = output),
        regression = function() lmfit_residuals_4d(input, matrix(1, 10, 1), outfile = output),
        shifted = function() lmfit_residuals_4d(input, matrix(1, 10, 1),
          set_mean = 1e6, outfile = output),
        gain = function() butterworth_filter_cpp(input, gain, 1, padlen = 0L,
          demean = FALSE, outfile = output)
      )
      for (name in names(operations)) {
        result <- operations[[name]]()
        expect_equal(as.numeric(result[1, 1, 1, ]), expected[[name]], tolerance = 1e-6)
        expect_integer_nifti_roundtrip(output, result, datatype)
        expect_identical(unname(RNifti::niftiVersion(output)), version)
      }
    }
  }
})

test_that("zero and fractional constant outputs do not lose their scaling", {
  for (datatype in c("INT8", "UINT8", "INT16", "UINT16", "INT32", "UINT32", "INT64", "UINT64")) {
    input <- tempfile(fileext = ".nii.gz")
    output <- tempfile(fileext = ".nii.gz")
    withr::defer(unlink(c(input, output)))
    RNifti::writeNifti(array(3, c(2, 2, 2, 10)), input, datatype = datatype)
    scaled <- RNifti::updateNifti(RNifti::readNifti(input, internal = TRUE),
      list(scl_slope = 0.125, scl_inter = -0.5), datatype = datatype)
    RNifti::writeNifti(scaled, input, datatype = datatype)
    result <- butterworth_filter_cpp(input, 1, 1, padlen = 0L,
      demean = FALSE, outfile = output)
    expect_equal(as.numeric(result), rep(-0.125, 80))
    expect_integer_nifti_roundtrip(output, result, datatype)
    expect_identical(as.numeric(RNifti::readNifti(output)), rep(-0.125, 80))
    result <- lmfit_residuals_4d(input, matrix(1, 10, 1), outfile = output)
    expect_integer_nifti_roundtrip(output, result, datatype)
    expect_equal(as.numeric(RNifti::readNifti(output)), rep(0, 80), tolerance = 1e-7)
  }
})

test_that("very small and large integer outputs remain finite and bounded", {
  for (datatype in c("INT16", "UINT16", "INT64", "UINT64")) {
    for (gain in c(1e-20, 1e14)) {
      input <- tempfile(fileext = ".nii.gz")
      output <- tempfile(fileext = ".nii.gz")
      withr::defer(unlink(c(input, output)))
      raw <- c(0, 1:9)
      RNifti::writeNifti(array(rep(raw, each = 8L), c(2, 2, 2, 10)), input,
        datatype = datatype)
      result <- butterworth_filter_cpp(input, gain, 1, padlen = 0L,
        demean = FALSE, outfile = output)
      expect_true(all(is.finite(result)))
      expect_integer_nifti_roundtrip(output, result, datatype)
    }
  }
})

test_that("non-finite processed values cannot silently enter integer output", {
  input <- tempfile(fileext = ".nii.gz")
  output <- tempfile(fileext = ".nii.gz")
  withr::defer(unlink(c(input, output)))
  RNifti::writeNifti(array(1, c(2, 2, 2, 10)), input, datatype = "INT16")
  expect_error(butterworth_filter_cpp(input, 1e200, 1, padlen = 0L,
    demean = FALSE, outfile = output), "non-finite processed values")
  expect_false(file.exists(output))
})

test_that("floating-point outputs retain their input storage type", {
  for (datatype in c("FLOAT32", "FLOAT64")) {
    input <- tempfile(fileext = ".nii.gz")
    output <- tempfile(fileext = ".nii.gz")
    withr::defer(unlink(c(input, output)))
    RNifti::writeNifti(array(rep(seq_len(10) / 7, each = 8L), c(2, 2, 2, 10)),
      input, datatype = datatype)
    original <- RNifti::niftiHeader(input)$datatype
    result <- lmfit_residuals_4d(input, matrix(1, 10, 1), outfile = output)
    expect_identical(RNifti::niftiHeader(output)$datatype, original)
    expect_identical(as.numeric(RNifti::readNifti(output)), as.numeric(result))
  }
})

test_that("FLOAT64 operations retain precision beyond FLOAT32", {
  input <- tempfile(fileext = ".nii.gz")
  output <- tempfile(fileext = ".nii.gz")
  withr::defer(unlink(c(input, output)))
  values <- 1 + seq_len(10) * 1e-8
  RNifti::writeNifti(array(rep(values, each = 8L), c(2, 2, 2, 10)), input,
    datatype = "FLOAT64")
  spline <- values
  spline[3] <- stats::splinefun((1:10)[-3], values[-3], method = "natural")(3)
  expected <- list(spline = spline, regression = values - mean(values), filter = values)
  operations <- list(
    spline = function() natural_spline_4d(input, 3L, outfile = output),
    regression = function() lmfit_residuals_4d(input, matrix(1, 10, 1), outfile = output),
    filter = function() butterworth_filter_cpp(input, 1, 1, padlen = 0L,
      demean = FALSE, outfile = output)
  )
  for (name in names(operations)) {
    result <- operations[[name]]()
    expect_lte(max(abs(as.numeric(result[1, 1, 1, ]) - expected[[name]])), 1e-12)
    expect_identical(as.numeric(RNifti::readNifti(output)), as.numeric(result))
  }
  RNifti::writeNifti(array(rep(values * 1e100, each = 8L), c(2, 2, 2, 10)), input,
    datatype = "FLOAT64")
  result <- butterworth_filter_cpp(input, 1, 1, padlen = 0L, demean = FALSE,
    outfile = output)
  expect_true(all(is.finite(result)))
  expect_identical(as.numeric(result[1, 1, 1, ]), values * 1e100)
})

test_that("integer working buffers retain small differences at large offsets", {
  for (datatype in c("INT16", "UINT16", "INT32", "UINT32", "INT64", "UINT64")) {
    input <- tempfile(fileext = ".nii.gz")
    output <- tempfile(fileext = ".nii.gz")
    withr::defer(unlink(c(input, output)))
    wide <- datatype %in% c("INT32", "UINT32", "INT64", "UINT64")
    raw <- seq_len(10) + if (wide) 2^24 else 0
    RNifti::writeNifti(array(rep(raw, each = 8L), c(2, 2, 2, 10)), input,
      datatype = datatype)
    if (!wide) {
      scaled <- RNifti::updateNifti(RNifti::readNifti(input, internal = TRUE),
        list(scl_slope = 1e-6, scl_inter = 1000), datatype = datatype)
      RNifti::writeNifti(scaled, input, datatype = datatype)
    }
    values <- as.numeric(RNifti::readNifti(input)[1, 1, 1, ])
    spline <- values
    spline[3] <- stats::splinefun((1:10)[-3], values[-3], method = "natural")(3)
    expected <- list(spline = spline, regression = values - mean(values), filter = values)
    operations <- list(
      spline = function() natural_spline_4d(input, 3L, outfile = output),
      regression = function() lmfit_residuals_4d(input, matrix(1, 10, 1), outfile = output),
      filter = function() butterworth_filter_cpp(input, 1, 1, padlen = 0L,
        demean = FALSE, outfile = output)
    )
    for (name in names(operations)) {
      result <- operations[[name]]()
      expect_lte(max(abs(as.numeric(result[1, 1, 1, ]) - expected[[name]])), if (wide) 1e-7 else 1e-11)
      expect_integer_nifti_roundtrip(output, result, datatype)
    }
  }
})

test_that("NIfTI-2 integer scaling supports values beyond the FLOAT32 range", {
  for (datatype in c("INT16", "UINT16")) {
    for (scale in c(1e-100, 1e100)) {
      input <- tempfile(fileext = ".nii")
      output <- tempfile(fileext = ".nii.gz")
      withr::defer(unlink(c(input, output)))
      raw <- 0:9
      RNifti::writeNifti(array(rep(raw, each = 8L), c(2, 2, 2, 10)), input,
        datatype = datatype, version = 2L)
      # updateNifti also uses NIfTI-1 header fields. Patch the two double fields
      # of this native-endian NIfTI-2 fixture to retain the intended extreme scale.
      connection <- file(input, "r+b")
      seek(connection, where = 176L, rw = "write")
      writeBin(c(scale, 0), connection, size = 8L, endian = .Platform$endian)
      close(connection)
      values <- as.numeric(RNifti::readNifti(input)[1, 1, 1, ])
      expect_lte(max(abs(values / scale - raw)), 1e-14)
      spline <- values
      spline[3] <- stats::splinefun((1:10)[-3], values[-3], method = "natural")(3)
      expected <- list(spline = spline, regression = values - mean(values), filter = values)
      operations <- list(
        spline = function() natural_spline_4d(input, 3L, outfile = output),
        regression = function() lmfit_residuals_4d(input, matrix(1, 10, 1), outfile = output),
        filter = function() butterworth_filter_cpp(input, 1, 1, padlen = 0L,
          demean = FALSE, outfile = output)
      )
      for (name in names(operations)) {
        result <- operations[[name]]()
        expect_lte(max(abs((as.numeric(result[1, 1, 1, ]) - expected[[name]]) / scale)), 1e-12)
        expect_integer_nifti_roundtrip(output, result, datatype)
      }
    }
  }
})
