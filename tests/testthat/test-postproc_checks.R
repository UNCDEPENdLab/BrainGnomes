# --- helpers shared across tests ----------------------------------------------

#' Write a synthetic 4D NIfTI with optional pixdim
write_synth_4d <- function(arr, path, vox_mm = c(2, 2, 2)) {
  nii <- RNifti::asNifti(arr)
  n_dim <- length(dim(arr))
  pixdim(nii) <- if (n_dim == 4L) c(vox_mm, 1) else vox_mm
  RNifti::writeNifti(nii, path)
  invisible(path)
}

#' Write a synthetic 3D NIfTI mask
write_synth_mask <- function(arr, path, vox_mm = c(2, 2, 2)) {
  nii <- RNifti::asNifti(arr)
  pixdim(nii) <- vox_mm
  RNifti::writeNifti(nii, path)
  invisible(path)
}

# --- validate_apply_mask ------------------------------------------------------

test_that("validate_apply_mask passes when masking is correct", {
  skip_if_not_installed("RNifti")

  set.seed(101)
  nx <- 6; ny <- 6; nz <- 4; nt <- 20

  # spherical mask: 1 inside, 0 outside

  mask <- array(0L, dim = c(nx, ny, nz))
  for (i in 1:nx) for (j in 1:ny) for (k in 1:nz) {
    if (sqrt((i - 3.5)^2 + (j - 3.5)^2 + (k - 2.5)^2) <= 2.5) mask[i, j, k] <- 1L
  }

  # data: nonzero only where mask == 1
  data4d <- array(0, dim = c(nx, ny, nz, nt))
  for (t in 1:nt) {
    data4d[, , , t] <- mask * (runif(nx * ny * nz, 50, 200))
  }

  mask_file <- tempfile(fileext = ".nii.gz")
  data_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, data_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_4d(data4d, data_file)

  result <- validate_apply_mask(mask_file, data_file)
  expect_true(result)
  expect_equal(attr(result, "external_violations"), 0L)
})

test_that("validate_apply_mask fails when outside-mask voxels have signal", {
  skip_if_not_installed("RNifti")

  set.seed(102)
  nx <- 6; ny <- 6; nz <- 4; nt <- 20

  mask <- array(0L, dim = c(nx, ny, nz))
  mask[2:5, 2:5, 2:3] <- 1L

  # deliberately leak signal outside the mask
  data4d <- array(0, dim = c(nx, ny, nz, nt))
  for (t in 1:nt) data4d[, , , t] <- mask * runif(nx * ny * nz, 50, 200)
  data4d[1, 1, 1, ] <- 999 # outside mask

  mask_file <- tempfile(fileext = ".nii.gz")
  data_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, data_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_4d(data4d, data_file)

  result <- validate_apply_mask(mask_file, data_file)
  expect_false(result)
  expect_gt(attr(result, "external_violations"), 0L)
})

test_that("validate_apply_mask handles 3D input gracefully", {
  skip_if_not_installed("RNifti")

  mask <- array(1L, dim = c(4, 4, 4))
  data3d <- array(runif(64, 10, 100), dim = c(4, 4, 4))

  mask_file <- tempfile(fileext = ".nii.gz")
  data_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, data_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_mask(data3d, data_file) # 3D, not 4D

  # should not error thanks to the 3D guard
  result <- validate_apply_mask(mask_file, data_file)
  expect_true(is.logical(result))
})

# --- validate_intensity_normalize ---------------------------------------------

test_that("validate_intensity_normalize passes when median matches target", {
  skip_if_not_installed("RNifti")

  set.seed(201)
  nx <- 6; ny <- 6; nz <- 4; nt <- 30
  target <- 10000

  mask <- array(1L, dim = c(nx, ny, nz))
  # create data whose in-mask global median == target
  raw <- array(rnorm(nx * ny * nz * nt, mean = 500, sd = 50), dim = c(nx, ny, nz, nt))
  mask_logical <- (mask == 1L)
  n_vox <- prod(nx, ny, nz)
  raw_mat <- array(raw, dim = c(n_vox, nt))
  vals <- as.vector(raw_mat[as.vector(mask_logical), ])
  gmed <- median(vals)
  scaled <- raw * (target / gmed) # rescale so global median == target

  mask_file <- tempfile(fileext = ".nii.gz")
  data_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, data_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_4d(scaled, data_file)

  result <- validate_intensity_normalize(data_file, mask_file, target = target, tolerance = 1)
  expect_true(result)
  expect_true(abs(attr(result, "details")$global_median - target) <= 1)
})

test_that("validate_intensity_normalize fails when median is far from target", {
  skip_if_not_installed("RNifti")

  set.seed(202)
  nx <- 6; ny <- 6; nz <- 4; nt <- 30

  mask <- array(1L, dim = c(nx, ny, nz))
  data4d <- array(rnorm(nx * ny * nz * nt, mean = 500, sd = 50), dim = c(nx, ny, nz, nt))

  mask_file <- tempfile(fileext = ".nii.gz")
  data_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, data_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_4d(data4d, data_file)

  result <- validate_intensity_normalize(data_file, mask_file, target = 10000, tolerance = 0)
  expect_false(result)
})

test_that("validate_intensity_normalize rejects dimension mismatch", {
  skip_if_not_installed("RNifti")

  mask <- array(1L, dim = c(4, 4, 4))
  data4d <- array(runif(5 * 5 * 5 * 10, 100, 200), dim = c(5, 5, 5, 10))

  mask_file <- tempfile(fileext = ".nii.gz")
  data_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, data_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_4d(data4d, data_file)

  result <- validate_intensity_normalize(data_file, mask_file, target = 100, tolerance = 0)
  expect_false(result)
  expect_match(attr(result, "message"), "mismatch", ignore.case = TRUE)
})

# --- validate_scrub_timepoints ------------------------------------------------

test_that("validate_scrub_timepoints passes when correct TRs are removed", {
  skip_if_not_installed("RNifti")

  set.seed(301)
  nx <- 4; ny <- 4; nz <- 4; nt <- 40

  pre <- array(rnorm(nx * ny * nz * nt), dim = c(nx, ny, nz, nt))
  censor <- rep(1L, nt)
  censor[c(5, 10, 15, 20)] <- 0L # remove 4 TRs
  keep <- which(censor == 1L)
  post <- pre[, , , keep, drop = FALSE]

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)

  result <- validate_scrub_timepoints(pre_file, post_file, censor_vec = censor)
  expect_true(result)
  expect_equal(attr(result, "details")$n_removed, 4L)
})

test_that("validate_scrub_timepoints fails when wrong number of TRs", {
  skip_if_not_installed("RNifti")

  set.seed(302)
  nx <- 4; ny <- 4; nz <- 4; nt <- 40

  pre <- array(rnorm(nx * ny * nz * nt), dim = c(nx, ny, nz, nt))
  censor <- rep(1L, nt)
  censor[c(5, 10)] <- 0L # says remove 2
  # but post has wrong number of TRs (remove 5 instead)
  post <- pre[, , , 1:33, drop = FALSE]

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)

  result <- validate_scrub_timepoints(pre_file, post_file, censor_vec = censor)
  expect_false(result)
})

test_that("validate_scrub_timepoints fails on censor length mismatch", {
  skip_if_not_installed("RNifti")

  pre <- array(1, dim = c(4, 4, 4, 20))
  post <- array(1, dim = c(4, 4, 4, 18))

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)

  # censor length doesn't match pre T
  result <- validate_scrub_timepoints(pre_file, post_file, censor_vec = rep(1L, 10))
  expect_false(result)
  expect_match(attr(result, "message"), "Censor length")
})

# --- validate_scrub_interpolate -----------------------------------------------

test_that("validate_scrub_interpolate passes when interpolated TRs are finite", {
  skip_if_not_installed("RNifti")

  set.seed(401)
  nx <- 4; ny <- 4; nz <- 4; nt <- 30

  pre <- array(rnorm(nx * ny * nz * nt, mean = 100, sd = 10), dim = c(nx, ny, nz, nt))
  post <- pre # pretend interpolation just kept the data for simplicity
  censor <- rep(1L, nt)
  censor[c(8, 16, 24)] <- 0L

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  censor_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(pre_file, post_file, censor_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)
  writeLines(as.character(censor), censor_file)

  result <- validate_scrub_interpolate(pre_file, post_file, censor_file)
  expect_true(result)
  expect_equal(attr(result, "details")$n_interpolated, 3L)
})

test_that("validate_scrub_interpolate fails on dimension mismatch", {
  skip_if_not_installed("RNifti")

  pre <- array(1, dim = c(4, 4, 4, 20))
  post <- array(1, dim = c(4, 4, 3, 20)) # different z

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  censor_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(pre_file, post_file, censor_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)
  writeLines(as.character(rep(1L, 20)), censor_file)

  result <- validate_scrub_interpolate(pre_file, post_file, censor_file)
  expect_false(result)
  expect_match(attr(result, "message"), "dimensions differ")
})

test_that("validate_scrub_interpolate trivially passes with no censored TRs", {
  skip_if_not_installed("RNifti")

  data4d <- array(runif(4^3 * 20, 10, 100), dim = c(4, 4, 4, 20))
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  censor_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(pre_file, post_file, censor_file)), add = TRUE)
  write_synth_4d(data4d, pre_file)
  write_synth_4d(data4d, post_file)
  writeLines(as.character(rep(1L, 20)), censor_file)

  result <- validate_scrub_interpolate(pre_file, post_file, censor_file)
  expect_true(result)
  expect_match(attr(result, "message"), "No censored timepoints")
})

# --- validate_apply_aroma (replay) --------------------------------------------

test_that("validate_apply_aroma passes on consistent replay", {
  skip_if_not_installed("RNifti")

  set.seed(501)
  nx <- 5; ny <- 5; nz <- 5; nt <- 50
  n_comp <- 10
  noise_ics <- c(2, 5, 7)

  # mixing matrix
  mixing <- matrix(rnorm(nt * n_comp), nrow = nt, ncol = n_comp)
  mixing_file <- tempfile(fileext = ".txt")

  # write as plain text (no header), tab-separated
  write.table(mixing, mixing_file, row.names = FALSE, col.names = FALSE, sep = "\t")

  # create 4D BOLD with signal embedded from mixing matrix
  voxel_weights <- matrix(rnorm(prod(nx, ny, nz) * n_comp), nrow = prod(nx, ny, nz))
  signal_mat <- voxel_weights %*% t(mixing) + rnorm(prod(nx, ny, nz) * nt, sd = 0.5)
  pre_arr <- array(signal_mat, dim = c(nx, ny, nz, nt))

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mixing_file)), add = TRUE)
  write_synth_4d(pre_arr, pre_file)

  # apply AROMA (nonaggressive) via the same function the validator will replay
  lmfit_residuals_4d(
    infile = pre_file,
    X = mixing,
    include_rows = rep(TRUE, nt),
    add_intercept = FALSE,
    outfile = post_file,
    internal = FALSE,
    preserve_mean = FALSE,
    set_mean = 0.0,
    regress_cols = noise_ics,
    exclusive = FALSE # nonaggressive = TRUE → exclusive = FALSE
  )

  result <- validate_apply_aroma(pre_file, post_file, mixing_file,
    noise_ics = noise_ics, nonaggressive = TRUE
  )
  expect_true(result)
  expect_lt(attr(result, "details")$max_abs_diff, 0.05)
})

test_that("validate_apply_aroma skips when no noise ICs", {
  skip_if_not_installed("RNifti")

  pre_arr <- array(rnorm(4^3 * 20), dim = c(4, 4, 4, 20))
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mixing_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(pre_file, post_file, mixing_file)), add = TRUE)
  write_synth_4d(pre_arr, pre_file)
  write_synth_4d(pre_arr, post_file)
  write.table(matrix(rnorm(20 * 5), 20, 5), mixing_file,
    row.names = FALSE, col.names = FALSE, sep = "\t"
  )

  result <- validate_apply_aroma(pre_file, post_file, mixing_file,
    noise_ics = integer(0), nonaggressive = TRUE
  )
  expect_true(result)
  expect_match(attr(result, "message"), "No noise ICs")
})

# --- validate_confound_regression (replay) ------------------------------------

test_that("validate_confound_regression passes on consistent replay", {
  skip_if_not_installed("RNifti")

  set.seed(601)
  nx <- 5; ny <- 5; nz <- 5; nt <- 50

  # confound regressors
  n_reg <- 3
  Xmat <- matrix(rnorm(nt * n_reg), nrow = nt)
  regress_file <- tempfile(fileext = ".tsv")
  write.table(Xmat, regress_file, row.names = FALSE, col.names = FALSE, sep = "\t")

  # generate pre data
  voxel_betas <- matrix(rnorm(prod(nx, ny, nz) * n_reg), nrow = prod(nx, ny, nz))
  signal_mat <- voxel_betas %*% t(Xmat) + rnorm(prod(nx, ny, nz) * nt, sd = 1)
  pre_arr <- array(signal_mat, dim = c(nx, ny, nz, nt))

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, regress_file)), add = TRUE)
  write_synth_4d(pre_arr, pre_file)

  # apply confound regression the same way postprocess_subject does
  lmfit_residuals_4d(
    infile = pre_file,
    X = Xmat,
    include_rows = rep(TRUE, nt),
    add_intercept = FALSE,
    outfile = post_file,
    internal = FALSE,
    preserve_mean = TRUE,
    set_mean = 0.0,
    regress_cols = NULL,
    exclusive = FALSE
  )

  result <- validate_confound_regression(pre_file, post_file, to_regress = regress_file)
  expect_true(result)
  expect_lt(attr(result, "details")$max_abs_diff, 0.05)
})

# --- validate_spatial_smooth --------------------------------------------------

test_that("validate_spatial_smooth passes when post FWHM exceeds pre FWHM", {
  skip_if_not_installed("RNifti")

  set.seed(701)
  nx <- 16; ny <- 16; nz <- 8; nt <- 10
  vox_mm <- c(2, 2, 2)

  mask <- array(0L, dim = c(nx, ny, nz))
  mask[3:(nx - 2), 3:(ny - 2), 2:(nz - 1)] <- 1L

  # pre: random noise within mask
  pre <- array(0, dim = c(nx, ny, nz, nt))
  for (t in seq_len(nt)) {
    pre[, , , t] <- mask * rnorm(nx * ny * nz, mean = 1000, sd = 100)
  }

  # post: Gaussian-smoothed version (approximate via repeated averaging)
  # Use a simple 3D box-car average as a proxy for smoothing
  smooth_volume <- function(vol, mask) {
    out <- vol
    for (i in 2:(nx - 1)) for (j in 2:(ny - 1)) for (k in 2:(nz - 1)) {
      if (mask[i, j, k] == 1L) {
        neighbourhood <- vol[(i - 1):(i + 1), (j - 1):(j + 1), (k - 1):(k + 1)]
        out[i, j, k] <- mean(neighbourhood)
      }
    }
    out * mask
  }

  post <- array(0, dim = c(nx, ny, nz, nt))
  for (t in seq_len(nt)) {
    smoothed <- pre[, , , t]
    for (pass in 1:3) smoothed <- smooth_volume(smoothed, mask) # 3 passes
    post[, , , t] <- smoothed
  }

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(pre, pre_file, vox_mm = vox_mm)
  write_synth_4d(post, post_file, vox_mm = vox_mm)
  write_synth_mask(mask, mask_file, vox_mm = vox_mm)

  # use a generous tolerance since box-car smoothing doesn't match calibration exactly
  result <- validate_spatial_smooth(pre_file, post_file, mask_file, fwhm_mm = 6,
    smoother = "gaussian", used_mask = TRUE, tolerance_mm = 5)
  expect_true(is.logical(result))
  details <- attr(result, "details")
  expect_gt(details$post_fwhm_mm, details$pre_fwhm_mm)
  expect_true("delta_expected_mm" %in% names(details))
  expect_true("delta_diff_mm" %in% names(details))
})

test_that("validate_spatial_smooth calibration returns expected structure", {
  skip_if_not_installed("RNifti")

  set.seed(702)
  nx <- 16; ny <- 16; nz <- 8; nt <- 5
  vox_mm <- c(2, 2, 2)

  mask <- array(0L, dim = c(nx, ny, nz))
  mask[3:(nx - 2), 3:(ny - 2), 2:(nz - 1)] <- 1L

  pre <- array(0, dim = c(nx, ny, nz, nt))
  for (t in seq_len(nt)) pre[, , , t] <- mask * rnorm(nx * ny * nz, mean = 1000, sd = 100)
  post <- pre # no actual smoothing — should likely fail calibration

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(pre, pre_file, vox_mm = vox_mm)
  write_synth_4d(post, post_file, vox_mm = vox_mm)
  write_synth_mask(mask, mask_file, vox_mm = vox_mm)

  result <- validate_spatial_smooth(pre_file, post_file, mask_file, fwhm_mm = 6,
    smoother = "susan", used_mask = TRUE, tolerance_mm = 0.5)
  # no smoothing applied, calibration expects ~5 mm delta, so this should fail
  expect_false(result)
  details <- attr(result, "details")
  expect_equal(details$smoother, "susan")
  expect_true(details$used_mask)
  expect_true(is.finite(details$delta_expected_mm))
})

test_that("validate_spatial_smooth falls back to directional check without fwhm_mm", {
  skip_if_not_installed("RNifti")

  set.seed(703)
  nx <- 10; ny <- 10; nz <- 4; nt <- 5
  mask <- array(1L, dim = c(nx, ny, nz))
  data4d <- array(rnorm(nx * ny * nz * nt, mean = 500, sd = 100), dim = c(nx, ny, nz, nt))

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(data4d, pre_file)
  write_synth_4d(data4d, post_file)
  write_synth_mask(mask, mask_file)

  result <- validate_spatial_smooth(pre_file, post_file, mask_file, fwhm_mm = NA_real_)
  expect_true(is.logical(result))
  expect_match(attr(result, "message"), "directional check only")
  # no calibration fields expected
  expect_null(attr(result, "details")$delta_expected_mm)
})

test_that("validate_spatial_smooth fails when pre/post dims mismatch", {
  skip_if_not_installed("RNifti")

  pre <- array(1, dim = c(8, 8, 4, 10))
  post <- array(1, dim = c(8, 8, 3, 10)) # different z
  mask <- array(1L, dim = c(8, 8, 4))

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)
  write_synth_mask(mask, mask_file)

  result <- validate_spatial_smooth(pre_file, post_file, mask_file, fwhm_mm = 6)
  expect_false(result)
  expect_match(attr(result, "message"), "mismatch", ignore.case = TRUE)
})

# --- calibration helpers ------------------------------------------------------

test_that(".pp_predict_calibration linear model works", {
  model <- list(type = "linear", coeffs = c(-2.942579, 1.198781))
  # kernel=6 -> -2.942579 + 1.198781*6 = 4.250107
  expect_equal(.pp_predict_calibration(model, 6), -2.942579 + 1.198781 * 6, tolerance = 1e-6)
})

test_that(".pp_predict_calibration poly model works", {
  model <- list(type = "poly", coeffs = c(-3.6270403, 1.4369376, -0.03108286))
  # kernel=6 -> -3.6270403 + 1.4369376*6 + -0.03108286*36
  expected <- -3.6270403 + 1.4369376 * 6 + (-0.03108286) * 36
  expect_equal(.pp_predict_calibration(model, 6), expected, tolerance = 1e-6)
})

test_that(".pp_select_calibration selects correct model", {
  m <- .pp_select_calibration("susan", used_mask = TRUE)
  expect_equal(m$type, "poly")
  expect_length(m$coeffs, 3)

  m2 <- .pp_select_calibration("gaussian", used_mask = FALSE)
  expect_equal(m2$type, "linear")
  expect_length(m2$coeffs, 2)
})

test_that(".pp_select_calibration warns on unknown smoother", {
  expect_warning(
    m <- .pp_select_calibration("unknown_smoother", used_mask = TRUE),
    "No calibration table"
  )
  # should fall back to gaussian
  expect_equal(m$type, "linear")
})

# --- validate_temporal_filter -------------------------------------------------

test_that("validate_temporal_filter passes on properly bandpass-filtered data", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("multitaper")
  skip_if_not_installed("signal")

  set.seed(801)
  nx <- 6; ny <- 6; nz <- 4; nt <- 200
  tr <- 0.8 # seconds
  nyquist <- 1 / (2 * tr) # 0.625 Hz

  # passband: 0.01 - 0.1 Hz
  hp <- 0.01; lp <- 0.1

  mask <- array(0L, dim = c(nx, ny, nz))
  mask[2:5, 2:5, 2:3] <- 1L
  mask_idx <- which(mask == 1L)

  # pre: white noise + sine at 0.05 Hz (in passband) + sine at 0.3 Hz (stopband)
  t_sec <- seq(0, by = tr, length.out = nt)
  pre <- array(0, dim = c(nx, ny, nz, nt))
  for (idx in mask_idx) {
    coords <- arrayInd(idx, dim(mask))
    ts_in <- sin(2 * pi * 0.05 * t_sec) * 10 +
      sin(2 * pi * 0.3 * t_sec) * 10 +
      rnorm(nt, sd = 2)
    pre[coords[1], coords[2], coords[3], ] <- ts_in + 500
  }

  # post: apply Butterworth bandpass filter to each voxel
  bf <- signal::butter(3, c(hp, lp) / nyquist, type = "pass")
  post <- pre
  for (idx in mask_idx) {
    coords <- arrayInd(idx, dim(mask))
    ts_raw <- pre[coords[1], coords[2], coords[3], ]
    ts_dm <- ts_raw - mean(ts_raw)
    ts_filt <- signal::filtfilt(bf, ts_dm) + mean(ts_raw)
    post[coords[1], coords[2], coords[3], ] <- ts_filt
  }

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)
  write_synth_mask(mask, mask_file)

  result <- validate_temporal_filter(
    pre_file = pre_file,
    post_file = post_file,
    tr = tr,
    band_low_hz = hp,
    band_high_hz = lp,
    mask_file = mask_file,
    n_voxels = 10L
  )
  expect_true(result)
  details <- attr(result, "details")
  # outside-band power should be reduced
  expect_gt(details$avg_reduction_outside_db, 0)
  # passband should not lose much power
  expect_true(is.na(details$avg_passband_change_db) || details$avg_passband_change_db > -3)
})

test_that("validate_temporal_filter fails when pre and post have different T", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("multitaper")
  skip_if_not_installed("signal")

  pre <- array(rnorm(4^3 * 50, mean = 500), dim = c(4, 4, 4, 50))
  post <- array(rnorm(4^3 * 40, mean = 500), dim = c(4, 4, 4, 40))

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)

  result <- validate_temporal_filter(pre_file, post_file, tr = 1.0,
    band_low_hz = 0.01, band_high_hz = 0.1
  )
  expect_false(result)
  expect_match(attr(result, "message"), "same number of timepoints")
})

test_that("validate_temporal_filter returns FALSE when mask/image dims mismatch", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("multitaper")
  skip_if_not_installed("signal")

  pre <- array(rnorm(4^3 * 30, mean = 500), dim = c(4, 4, 4, 30))
  mask <- array(1L, dim = c(5, 5, 5)) # wrong dims
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(pre, post_file)
  write_synth_mask(mask, mask_file)

  result <- validate_temporal_filter(pre_file, post_file, tr = 1.0,
    band_low_hz = 0.01, band_high_hz = 0.1, mask_file = mask_file
  )
  expect_false(result)
  expect_match(attr(result, "message"), "dimensions", ignore.case = TRUE)
})

# --- helper: .pp_is_valid_series ----------------------------------------------

test_that(".pp_is_valid_series detects constant and invalid series", {
  expect_true(.pp_is_valid_series(rnorm(50)))
  expect_false(.pp_is_valid_series(rep(5, 50)))
  expect_false(.pp_is_valid_series(c(1, 2, NA, 4)))
  expect_false(.pp_is_valid_series(c(1, Inf, 3)))
})

# --- helper: .pp_power_multitaper ---------------------------------------------

test_that(".pp_power_multitaper returns expected frequency structure", {
  skip_if_not_installed("multitaper")
  skip_if_not_installed("signal")

  set.seed(901)
  dt <- 0.8 # TR
  nt <- 200
  t_sec <- seq(0, by = dt, length.out = nt)

  # pure sine at 0.05 Hz
  y <- sin(2 * pi * 0.05 * t_sec) + rnorm(nt, sd = 0.1)
  spec <- .pp_power_multitaper(y, dt = dt)

  expect_s3_class(spec, "data.frame")
  expect_true(all(c("f", "power") %in% names(spec)))
  expect_true(nrow(spec) > 0)

  # peak should be near 0.05 Hz
  peak_freq <- spec$f[which.max(spec$power)]
  expect_true(abs(peak_freq - 0.05) < 0.02)
})

test_that(".pp_power_multitaper errors on constant series", {
  skip_if_not_installed("multitaper")

  spec <- .pp_power_multitaper(rep(0, 50), dt = 1)
  expect_equal(nrow(spec), 0) # returns empty data.frame for constant series
})

# --- helper: .pp_mtm_bandpower ------------------------------------------------

test_that(".pp_mtm_bandpower computes band power for defined bands", {
  skip_if_not_installed("multitaper")

  set.seed(902)
  dt <- 0.8
  nt <- 200
  t_sec <- seq(0, by = dt, length.out = nt)

  # signal with power concentrated in low frequencies
  y <- sin(2 * pi * 0.05 * t_sec) * 10 + rnorm(nt, sd = 1)

  bands <- data.frame(
    low = c(0.01, 0.2),
    high = c(0.1, 0.5),
    label = c("low", "high")
  )

  bp <- .pp_mtm_bandpower(y, dt = dt, bands = bands, detrend = "linear")
  expect_s3_class(bp, "data.frame")
  expect_equal(nrow(bp), 2)
  expect_true(all(c("label", "power_linear", "relative_power") %in% names(bp)))

  # low band should have more power than high band
  low_pwr <- bp$power_linear[bp$label == "low"]
  high_pwr <- bp$power_linear[bp$label == "high"]
  expect_gt(low_pwr, high_pwr)
})

# --- helper: estimate_classic_fwhm -------------------------------------------

test_that("estimate_classic_fwhm returns larger FWHM for smoother data", {
  skip_if_not_installed("RNifti")

  set.seed(903)
  nx <- 16; ny <- 16; nz <- 8; nt <- 5
  vox_mm <- c(2, 2, 2)
  mask <- array(1L, dim = c(nx, ny, nz))
  mask[1, , ] <- 0L; mask[nx, , ] <- 0L
  mask[, 1, ] <- 0L; mask[, ny, ] <- 0L
  mask[, , 1] <- 0L; mask[, , nz] <- 0L

  # noisy (unsmoothed)
  noisy <- array(0, dim = c(nx, ny, nz, nt))
  for (t in 1:nt) noisy[, , , t] <- mask * rnorm(nx * ny * nz, mean = 1000, sd = 200)

  # smoothed (box-car average)
  smooth_vol <- function(vol) {
    out <- vol
    for (i in 2:(nx - 1)) for (j in 2:(ny - 1)) for (k in 2:(nz - 1)) {
      out[i, j, k] <- mean(vol[(i - 1):(i + 1), (j - 1):(j + 1), (k - 1):(k + 1)])
    }
    out
  }
  smoothed <- array(0, dim = c(nx, ny, nz, nt))
  for (t in 1:nt) {
    v <- noisy[, , , t]
    for (p in 1:3) v <- smooth_vol(v)
    smoothed[, , , t] <- v * mask
  }

  mask_logical <- mask == 1L
  fwhm_noisy <- estimate_classic_fwhm(noisy, mask_logical, vox_mm)$geom
  fwhm_smooth <- estimate_classic_fwhm(smoothed, mask_logical, vox_mm)$geom

  expect_true(is.finite(fwhm_noisy))
  expect_true(is.finite(fwhm_smooth))
  expect_gt(fwhm_smooth, fwhm_noisy)
})
