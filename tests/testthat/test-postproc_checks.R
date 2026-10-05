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

#' Write a synthetic NIfTI with explicit, matching qform and sform transforms
write_synth_xform <- function(arr, path, vox_mm = c(2, 2, 2),
                              offset_mm = c(0, 0, 0)) {
  nii <- RNifti::asNifti(arr)
  n_dim <- length(dim(arr))
  pixdim(nii) <- if (n_dim == 4L) c(vox_mm, 1) else vox_mm
  affine <- diag(c(vox_mm, 1))
  affine[1:3, 4] <- offset_mm
  attr(affine, "code") <- 4L
  nii <- RNifti::`qform<-`(nii, affine)
  nii <- RNifti::`sform<-`(nii, affine)
  RNifti::writeNifti(nii, path)
  invisible(path)
}

test_that("spatial-grid comparison detects changed qform and sform", {
  arr <- array(rnorm(3 * 4 * 2 * 6), dim = c(3, 4, 2, 6))
  reference_file <- tempfile(fileext = ".nii.gz")
  matching_file <- tempfile(fileext = ".nii.gz")
  shifted_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(reference_file, matching_file, shifted_file)), add = TRUE)
  write_synth_xform(arr, reference_file, offset_mm = c(-8, 4, 12))
  write_synth_xform(arr, matching_file, offset_mm = c(-8, 4, 12))
  write_synth_xform(arr, shifted_file, offset_mm = c(-3, 4, 12))

  expect_true(pp_compare_nifti_grid(reference_file, matching_file)$passed)
  shifted <- pp_compare_nifti_grid(reference_file, shifted_file)
  expect_false(shifted$passed)
  expect_true(any(grepl("transform", shifted$mismatch_reasons)))
  expect_equal(shifted$max_qform_difference, 5)
  expect_equal(shifted$max_sform_difference, 5)
})

test_that("postprocessing validators reject correct values on a shifted grid", {
  dims <- c(3L, 3L, 2L, 8L)
  pre <- array(rnorm(prod(dims)), dim = dims)
  mask <- array(1L, dim = dims[1:3])
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_xform(pre, pre_file)
  write_synth_xform(pre, post_file, offset_mm = c(10, 0, 0))
  write_synth_xform(mask, mask_file)

  result <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_false(result)
  expect_match(attr(result, "message"), "grid mismatch")
  expect_true(
    "qform transform" %in%
      attr(result, "details")$spatial_grid$mismatch_reasons
  )

  write_synth_xform(pre, post_file)
  write_synth_xform(mask, mask_file, offset_mm = c(0, -6, 0))
  shifted_mask <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_false(shifted_mask)
  expect_match(attr(shifted_mask, "message"), "pre/mask spatial")
})

# --- validate_apply_mask ------------------------------------------------------

test_that("distributed volume selection is deterministic and includes endpoints", {
  expected <- as.integer(round(seq.int(1, 1000, length.out = 32)))
  selected <- pp_distributed_volume_indices(1000L, 32L)

  expect_identical(selected, expected)
  expect_length(selected, 32L)
  expect_identical(selected[1], 1L)
  expect_identical(selected[32], 1000L)
  expect_identical(anyDuplicated(selected), 0L)
  expect_true(all(diff(selected) > 0L))
  expect_identical(pp_distributed_volume_indices(20L, 32L), 1:20)
  expect_identical(pp_distributed_volume_indices(20L, Inf), 1:20)
})

test_that("validate_apply_mask passes when masking is correct", {
  skip_if_not_installed("RNifti")

  set.seed(101)
  nx <- 6; ny <- 6; nz <- 4; nt <- 20

  # spherical mask: 1 inside, 0 outside

  mask <- array(0L, dim = c(nx, ny, nz))
  for (i in 1:nx) for (j in 1:ny) for (k in 1:nz) {
    if (sqrt((i - 3.5)^2 + (j - 3.5)^2 + (k - 2.5)^2) <= 2.5) mask[i, j, k] <- 1L
  }

  pre <- array(runif(nx * ny * nz * nt, 50, 200),
               dim = c(nx, ny, nz, nt))
  post <- pre * array(rep(mask, nt), dim = dim(pre))

  mask_file <- tempfile(fileext = ".nii.gz")
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, pre_file, post_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)

  result <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_true(result)
  expect_equal(attr(result, "external_violations"), 0L)
  expect_equal(attr(result, "details")$n_mismatched, 0L)
  expect_identical(attr(result, "details")$volumes_compared, 20L)
  expect_identical(attr(result, "details")$total_volumes, 20L)
  expect_identical(attr(result, "details")$volume_indices, 1:20)
  expect_identical(attr(result, "details")$volume_sampling, "all")
  expect_match(attr(result, "message"), "Exact mask replay \\(20/20 volumes")
})

test_that("validate_apply_mask samples 32 distributed volumes by default", {
  expect_identical(formals(validate_apply_mask)$max_volumes, 32L)
  expect_identical(formals(validate_apply_mask)$absolute_tolerance, 1e-4)

  nx <- 4; ny <- 4; nz <- 3; nt <- 41
  mask <- array(0L, dim = c(nx, ny, nz))
  mask[2:3, 2:3, 2] <- 1L
  pre <- array(seq_len(nx * ny * nz * nt), dim = c(nx, ny, nz, nt))
  correct <- pre * array(rep(mask, nt), dim = dim(pre))
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(correct, post_file)
  write_synth_mask(mask, mask_file)

  result <- validate_apply_mask(pre_file, post_file, mask_file)
  details <- attr(result, "details")
  expect_true(result)
  expect_identical(details$volumes_compared, 32L)
  expect_identical(details$total_volumes, 41L)
  expect_identical(details$volume_sampling, "distributed")
  expect_length(details$volume_indices, 32L)
  expect_identical(details$volume_indices[1], 1L)
  expect_identical(details$volume_indices[32], 41L)
  expect_true(details$includes_first_volume)
  expect_true(details$includes_last_volume)
  expect_match(attr(result, "message"), "Distributed mask replay \\(32/41 volumes")

  unsampled <- setdiff(seq_len(nt), details$volume_indices)[1]
  altered <- correct
  altered[1, 1, 1, unsampled] <- 999
  write_synth_4d(altered, post_file)
  expect_true(validate_apply_mask(pre_file, post_file, mask_file))
  exhaustive <- validate_apply_mask(
    pre_file, post_file, mask_file, max_volumes = Inf
  )
  expect_false(exhaustive)
  expect_identical(attr(exhaustive, "details")$volumes_compared, 41L)
  expect_identical(attr(exhaustive, "details")$volume_sampling, "all")

  altered <- correct
  altered[1, 1, 1, nt] <- 999
  write_synth_4d(altered, post_file)
  endpoint_result <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_false(endpoint_result)
  expect_gt(attr(endpoint_result, "external_violations"), 0L)
})

test_that("validate_apply_mask accommodates scaled-int16 float output noise", {
  dims <- c(4L, 4L, 3L, 8L)
  mask <- array(0L, dim = dims[1:3])
  mask[2:3, 2:3, 2] <- 1L
  pre <- array(0, dim = dims)
  pre[2:3, 2:3, 2, ] <- rep(c(0.023, 0.4, 1.2, 2.7), dims[4])
  correct <- pre * array(rep(mask, dims[4]), dim = dims)
  rounded <- correct
  rounded[2, 2, 2, ] <- rounded[2, 2, 2, ] + 3e-5

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(rounded, post_file)
  write_synth_mask(mask, mask_file)

  result <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_true(result)
  expect_identical(attr(result, "details")$absolute_tolerance, 1e-4)
  expect_identical(attr(result, "details")$tolerance, 1e-5)
  expect_match(attr(result, "message"), "atol 0.0001, rtol 1e-05")

  strict <- validate_apply_mask(
    pre_file, post_file, mask_file, absolute_tolerance = 1e-5
  )
  expect_false(strict)
  expect_gt(attr(strict, "details")$n_mismatched, 0L)

  corrupted <- correct
  corrupted[2, 2, 2, 1] <- corrupted[2, 2, 2, 1] + 2e-4
  write_synth_4d(corrupted, post_file)
  expect_false(validate_apply_mask(pre_file, post_file, mask_file))

  outside_leak <- correct
  outside_leak[1, 1, 1, 1] <- 3e-5
  write_synth_4d(outside_leak, post_file)
  outside_result <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_false(outside_result)
  expect_identical(attr(outside_result, "external_violations"), 1L)
})

test_that("validate_apply_mask fails when outside-mask voxels have signal", {
  skip_if_not_installed("RNifti")

  set.seed(102)
  nx <- 6; ny <- 6; nz <- 4; nt <- 20

  mask <- array(0L, dim = c(nx, ny, nz))
  mask[2:5, 2:5, 2:3] <- 1L

  pre <- array(runif(nx * ny * nz * nt, 50, 200),
               dim = c(nx, ny, nz, nt))
  post <- pre * array(rep(mask, nt), dim = dim(pre))
  post[1, 1, 1, ] <- 999 # outside mask

  mask_file <- tempfile(fileext = ".nii.gz")
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, pre_file, post_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)

  result <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_false(result)
  expect_gt(attr(result, "external_violations"), 0L)
})

test_that("validate_apply_mask handles 3D input gracefully", {
  skip_if_not_installed("RNifti")

  mask <- array(1L, dim = c(4, 4, 4))
  pre3d <- array(runif(64, 10, 100), dim = c(4, 4, 4))
  post3d <- pre3d

  mask_file <- tempfile(fileext = ".nii.gz")
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(mask_file, pre_file, post_file)), add = TRUE)
  write_synth_mask(mask, mask_file)
  write_synth_mask(pre3d, pre_file)
  write_synth_mask(post3d, post_file)

  # should not error thanks to the 3D guard
  result <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_true(result)
})

test_that("validate_apply_mask rejects all-zero or altered in-mask output", {
  mask <- array(0L, dim = c(4, 4, 3))
  mask[2:3, 2:3, 2] <- 1L
  pre <- array(seq_len(4 * 4 * 3 * 8), dim = c(4, 4, 3, 8))
  correct <- pre * array(rep(mask, 8), dim = dim(pre))
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_mask(mask, mask_file)

  write_synth_4d(array(0, dim = dim(pre)), post_file)
  expect_false(validate_apply_mask(pre_file, post_file, mask_file))

  altered <- correct
  altered[2, 2, 2, 4] <- altered[2, 2, 2, 4] + 10
  write_synth_4d(altered, post_file)
  result <- validate_apply_mask(pre_file, post_file, mask_file)
  expect_false(result)
  expect_gt(attr(result, "details")$n_mismatched, 0L)
})

# --- validate_intensity_normalize ---------------------------------------------

test_that("validate_intensity_normalize passes for a frozen factor", {
  skip_if_not_installed("RNifti")

  nx <- 6; ny <- 6; nz <- 4; nt <- 30
  target <- 10000
  reference_location <- 5000
  factor <- target / reference_location
  raw <- array(reference_location, dim = c(nx, ny, nz, nt))

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  core_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, core_file)), add = TRUE)
  write_synth_4d(raw, pre_file)
  write_synth_4d(raw * factor, post_file)
  write_synth_mask(array(1L, dim = c(nx, ny, nz)), core_file)

  result <- validate_intensity_normalize(
    pre_file, post_file,
    reference_location = reference_location,
    target = target, scale_factor = factor,
    core_file = core_file, include_frames = rep(TRUE, nt), tolerance = 1e-5
  )
  expect_true(result)
  expect_equal(attr(result, "details")$expected_target, target)
  expect_equal(attr(result, "details")$observed_target, target)
})

test_that("validate_intensity_normalize detects an incorrectly applied factor", {
  skip_if_not_installed("RNifti")

  set.seed(202)
  nx <- 6; ny <- 6; nz <- 4; nt <- 30
  raw <- array(rnorm(nx * ny * nz * nt), dim = c(nx, ny, nz, nt))
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file)), add = TRUE)
  write_synth_4d(raw, pre_file)
  write_synth_4d(raw * 3, post_file)

  result <- validate_intensity_normalize(
    pre_file, post_file,
    reference_location = 5000, target = 10000,
    scale_factor = 2, tolerance = 1e-5
  )
  expect_false(result)
})

test_that("validate_intensity_normalize verifies denominator-guarded PSC maps", {
  skip_if_not_installed("RNifti")

  nx <- 3; ny <- 2; nz <- 2; nt <- 25
  raw <- array(seq_len(nx * ny * nz * nt), dim = c(nx, ny, nz, nt))
  scale <- array(seq(0.05, 0.16, length.out = nx * ny * nz),
                 dim = c(nx, ny, nz))
  scaled <- raw * array(rep(as.vector(scale), nt), dim = dim(raw))
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  scale_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, scale_file)), add = TRUE)
  write_synth_4d(raw, pre_file)
  write_synth_4d(scaled, post_file)
  write_synth_mask(scale, scale_file)

  result <- validate_intensity_normalize(
    pre_file, post_file, mode = "voxel_psc",
    reference_location = 1000, target = 100,
    scale_file = scale_file, tolerance = 1e-5
  )

  expect_true(result)
  expect_equal(attr(result, "details")$mode, "voxel_psc")
})

test_that("validate_intensity_normalize rejects pre/post dimension mismatch", {
  skip_if_not_installed("RNifti")

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file)), add = TRUE)
  write_synth_4d(array(1, dim = c(4, 4, 4, 10)), pre_file)
  write_synth_4d(array(2, dim = c(5, 5, 5, 10)), post_file)

  result <- validate_intensity_normalize(
    pre_file, post_file,
    reference_location = 50, target = 100,
    scale_factor = 2, tolerance = 1e-5
  )
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

test_that("validate_scrub_timepoints rejects the wrong retained volumes", {
  nx <- 3; ny <- 3; nz <- 2; nt <- 8
  pre <- array(seq_len(nx * ny * nz * nt), dim = c(nx, ny, nz, nt))
  censor <- c(1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L)
  # Correct output length, but these are the first six rather than censor == 1.
  wrong <- pre[, , , seq_len(sum(censor)), drop = FALSE]
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(wrong, post_file)

  result <- validate_scrub_timepoints(pre_file, post_file, censor)
  expect_false(result)
  expect_gt(attr(result, "details")$n_mismatched, 0L)
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

test_that("scrub validators reject nonbinary censor values", {
  pre <- array(rnorm(3 * 3 * 2 * 8), dim = c(3, 3, 2, 8))
  post <- pre[, , , 1:7, drop = FALSE]
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  censor_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(pre_file, post_file, censor_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  write_synth_4d(post, post_file)
  invalid <- c(1L, 2L, 1L, 1L, 0L, 1L, 1L, 1L)
  writeLines(as.character(invalid), censor_file)

  dropped <- validate_scrub_timepoints(pre_file, post_file, invalid)
  expect_false(dropped)
  expect_match(attr(dropped, "message"), "binary")

  write_synth_4d(pre, post_file)
  interpolated <- validate_scrub_interpolate(
    pre_file, post_file, censor_file, n_sample = 5L
  )
  expect_false(interpolated)
  expect_match(attr(interpolated, "message"), "binary")
})

# --- validate_scrub_interpolate -----------------------------------------------

test_that("validate_scrub_interpolate passes when interpolated TRs are finite", {
  skip_if_not_installed("RNifti")

  set.seed(401)
  nx <- 4; ny <- 4; nz <- 4; nt <- 30

  pre <- array(rnorm(nx * ny * nz * nt, mean = 100, sd = 10), dim = c(nx, ny, nz, nt))
  censor <- rep(1L, nt)
  censor[c(8, 16, 24)] <- 0L

  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  censor_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(pre_file, post_file, censor_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  writeLines(as.character(censor), censor_file)
  natural_spline_4d(
    pre_file, t_interpolate = which(censor == 0L), edge_nn = TRUE,
    outfile = post_file, internal = TRUE
  )

  result <- validate_scrub_interpolate(
    pre_file, post_file, censor_file, n_sample = 20L
  )
  expect_true(result)
  expect_equal(attr(result, "details")$n_interpolated, 3L)
  expect_equal(attr(result, "details")$retained_mismatches, 0L)
  expect_equal(attr(result, "details")$spline_mismatches, 0L)
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
  expect_match(attr(result, "message"), "mismatch")
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
  expect_equal(attr(result, "details")$n_interpolated, 0L)
  expect_equal(attr(result, "details")$retained_mismatches, 0L)
})

test_that("validate_scrub_interpolate rejects arbitrary output and retained changes", {
  set.seed(402)
  dims <- c(4, 4, 3, 20)
  pre <- array(rnorm(prod(dims), mean = 500, sd = 20), dim = dims)
  censor <- rep(1L, dims[4])
  censor[c(1, 8, 20)] <- 0L
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  censor_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(pre_file, post_file, censor_file)), add = TRUE)
  write_synth_4d(pre, pre_file)
  writeLines(as.character(censor), censor_file)

  write_synth_4d(array(42, dim = dims), post_file)
  arbitrary <- validate_scrub_interpolate(
    pre_file, post_file, censor_file, n_sample = 20L
  )
  expect_false(arbitrary)

  natural_spline_4d(
    pre_file, t_interpolate = which(censor == 0L), edge_nn = TRUE,
    outfile = post_file, internal = TRUE
  )
  corrupted <- RNifti::readNifti(post_file)
  corrupted[2, 2, 2, 10] <- corrupted[2, 2, 2, 10] + 5
  write_synth_4d(corrupted, post_file)
  retained_change <- validate_scrub_interpolate(
    pre_file, post_file, censor_file, n_sample = 20L
  )
  expect_false(retained_change)
  expect_gt(attr(retained_change, "details")$retained_mismatches, 0L)
})

# --- validate_apply_aroma (replay) --------------------------------------------

test_that("validate_apply_aroma replays the production AROMA specification", {
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
  signal_mat <- 1000 + voxel_weights %*% t(mixing) +
    rnorm(prod(nx, ny, nz) * nt, sd = 0.5)
  pre_arr <- array(signal_mat, dim = c(nx, ny, nz, nt))

  pre_file <- tempfile(fileext = ".nii.gz")
  post_files <- tempfile(pattern = c("nonaggressive-", "aggressive-"), fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_files, mixing_file)), add = TRUE)
  write_synth_4d(pre_arr, pre_file)

  # Use the production entry point: it adds an intercept and preserves each
  # voxel's temporal mean. The validator must replay those exact settings.
  for (i in seq_along(post_files)) {
    nonaggressive <- c(TRUE, FALSE)[[i]]
    apply_aroma(
      in_file = pre_file,
      out_file = post_files[[i]],
      mixing_file = mixing_file,
      noise_ics = noise_ics,
      overwrite = TRUE,
      nonaggressive = nonaggressive
    )

    result <- validate_apply_aroma(
      pre_file, post_files[[i]], mixing_file,
      noise_ics = noise_ics, nonaggressive = nonaggressive
    )
    expect_true(result)
    expect_lt(attr(result, "details")$max_abs_diff, 0.05)
  }
})

test_that("validate_apply_aroma verifies no-noise-IC output is unchanged", {
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
  expect_true(attr(result, "details")$no_op)
  expect_false(attr(result, "details")$skipped)

  altered <- pre_arr
  altered[2, 2, 2, 5] <- altered[2, 2, 2, 5] + 1
  write_synth_4d(altered, post_file)
  changed <- validate_apply_aroma(
    pre_file, post_file, mixing_file,
    noise_ics = integer(0), nonaggressive = TRUE
  )
  expect_false(changed)
  expect_gt(attr(changed, "details")$n_mismatched, 0L)
})

test_that("validate_apply_aroma fails when every requested IC is invalid", {
  nt <- 20L
  pre_arr <- array(rnorm(3^3 * nt), dim = c(3, 3, 3, nt))
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mixing_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(pre_file, post_file, mixing_file)), add = TRUE)
  write_synth_4d(pre_arr, pre_file)
  write_synth_4d(pre_arr, post_file)
  write.table(
    matrix(rnorm(nt * 3L), nt, 3L), mixing_file,
    row.names = FALSE, col.names = FALSE, sep = "\t"
  )

  result <- validate_apply_aroma(
    pre_file, post_file, mixing_file, noise_ics = c(4L, 8L)
  )
  expect_false(result)
  expect_match(attr(result, "message"), "No requested AROMA noise IC")
  expect_equal(attr(result, "details")$invalid_noise_ics, c(4L, 8L))
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
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file, regress_file)), add = TRUE)
  write_synth_4d(pre_arr, pre_file)
  write_synth_mask(array(1L, dim = c(nx, ny, nz)), mask_file)

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

  result <- validate_confound_regression(
    pre_file, post_file, to_regress = regress_file, mask_file = mask_file
  )
  expect_true(result)
  expect_lt(attr(result, "details")$max_abs_diff, 0.05)

  set.seed(99999)
  repeated <- validate_confound_regression(
    pre_file, post_file, to_regress = regress_file, mask_file = mask_file
  )
  expect_identical(
    attr(repeated, "details")$sampled_indices,
    attr(result, "details")$sampled_indices
  )
})

test_that("validate_confound_regression rejects invalid censor vectors", {
  skip_if_not_installed("RNifti")

  nt <- 12L
  dims <- c(3L, 3L, 3L, nt)
  Xmat <- matrix(rnorm(nt * 2L), nrow = nt)
  pre <- array(rnorm(prod(dims)), dim = dims)
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  regress_file <- tempfile(fileext = ".tsv")
  censor_file <- tempfile(fileext = ".txt")
  on.exit(
    unlink(c(pre_file, post_file, regress_file, censor_file)), add = TRUE
  )
  write_synth_4d(pre, pre_file)
  write_synth_4d(pre, post_file)
  write.table(
    Xmat, regress_file, row.names = FALSE, col.names = FALSE, sep = "\t"
  )
  writeLines(as.character(c(rep(1L, nt - 1L), 2L)), censor_file)

  result <- validate_confound_regression(
    pre_file, post_file, regress_file, censor_file = censor_file
  )
  expect_false(result)
  expect_match(attr(result, "message"), "binary 0/1")
  expect_equal(attr(result, "details")$n_sampled, 0L)
})

test_that("regression replay fails empty and corrupted post-step samples", {
  nt <- 30L
  dims <- c(4L, 4L, 3L, nt)
  X <- matrix(rnorm(nt * 3L), nrow = nt)
  design_file <- tempfile(fileext = ".txt")
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mixing_file <- tempfile(fileext = ".txt")
  on.exit(
    unlink(c(design_file, pre_file, post_file, mixing_file)), add = TRUE
  )
  write.table(X, design_file, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(X, mixing_file, row.names = FALSE, col.names = FALSE, sep = "\t")

  zeros <- array(0, dim = dims)
  write_synth_4d(zeros, pre_file)
  write_synth_4d(zeros, post_file)
  confound_empty <- validate_confound_regression(
    pre_file, post_file, design_file
  )
  aroma_empty <- validate_apply_aroma(
    pre_file, post_file, mixing_file, noise_ics = 1L
  )
  expect_false(confound_empty)
  expect_false(aroma_empty)
  expect_equal(attr(confound_empty, "details")$n_sampled, 0L)
  expect_equal(attr(aroma_empty, "details")$n_sampled, 0L)

  pre <- array(rnorm(prod(dims), mean = 1000, sd = 20), dim = dims)
  write_synth_4d(pre, pre_file)
  write_synth_4d(zeros, post_file)
  corrupted <- validate_confound_regression(
    pre_file, post_file, design_file, n_sample = 20L
  )
  expect_false(corrupted)
  expect_gt(
    attr(corrupted, "details")$n_unexpected_constant_post, 0L
  )

  lmfit_residuals_4d(
    infile = pre_file, X = X, include_rows = rep(TRUE, nt),
    outfile = post_file, internal = FALSE, preserve_mean = TRUE
  )
  nonfinite_post <- RNifti::readNifti(post_file)
  nonfinite_post[, , , 1L] <- NA_real_
  write_synth_4d(nonfinite_post, post_file)
  nonfinite <- validate_confound_regression(
    pre_file, post_file, design_file, n_sample = 20L
  )
  expect_false(nonfinite)
  expect_gt(attr(nonfinite, "details")$n_nonfinite_post, 0L)
})

test_that("spatial replay sampling scales with E2E-like matrix dimensions", {
  ellipsoid_candidates <- function(dims) {
    coords <- arrayInd(seq_len(prod(dims)), dims)
    normalized <- sweep(coords - 0.5, 2L, dims, FUN = "/")
    radius <- sweep(normalized - 0.5, 2L, c(0.42, 0.45, 0.40), FUN = "/")
    which(rowSums(radius^2) <= 1)
  }
  # 58 x 72 x 61 is the MNI grid in the fMRIPrep 25.2.5 E2E reference.
  # The second grid represents the same relative brain support at roughly
  # half the number of samples per dimension.
  fine_dims <- c(58L, 72L, 61L)
  coarse_dims <- c(29L, 36L, 31L)
  coarse <- pp_select_spatial_replay_voxels(
    ellipsoid_candidates(coarse_dims), coarse_dims, 100L
  )
  fine <- pp_select_spatial_replay_voxels(
    ellipsoid_candidates(fine_dims), fine_dims, 100L
  )

  expect_equal(length(coarse$indices), 100L)
  expect_equal(length(fine$indices), 100L)
  expect_equal(
    apply(coarse$normalized_coords, 2L, range),
    apply(fine$normalized_coords, 2L, range),
    tolerance = 0.06
  )
  coarse_quantiles <- apply(
    coarse$normalized_coords, 2L, stats::quantile,
    probs = c(0.1, 0.5, 0.9)
  )
  fine_quantiles <- apply(
    fine$normalized_coords, 2L, stats::quantile,
    probs = c(0.1, 0.5, 0.9)
  )
  expect_equal(coarse_quantiles, fine_quantiles, tolerance = 0.06)
  octant_count <- function(coords) {
    length(unique(
      (coords[, 1L] > 0.5) + 2L * (coords[, 2L] > 0.5) +
        4L * (coords[, 3L] > 0.5)
    ))
  }
  expect_equal(octant_count(coarse$normalized_coords), 8L)
  expect_equal(octant_count(fine$normalized_coords), 8L)
  coarse_cell_count <- function(coords) {
    cells <- pmin(floor(coords * 3), 2L)
    nrow(unique(cells))
  }
  expect_equal(coarse_cell_count(coarse$normalized_coords), 27L)
  expect_equal(coarse_cell_count(fine$normalized_coords), 27L)
})

# --- validate_spatial_smooth --------------------------------------------------

test_that("validate_spatial_smooth passes when post FWHM exceeds pre FWHM", {
  skip_if_not_installed("RNifti")

  set.seed(701)
  nx <- 16; ny <- 16; nz <- 8; nt <- 10
  vox_mm <- c(2.7, 2.7, 2.7)

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
  vox_mm <- c(2.7, 2.7, 2.7)

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
    smoother = "susan", used_mask = TRUE)
  # no smoothing applied, calibration expects ~5 mm delta, so this should fail
  expect_false(result)
  details <- attr(result, "details")
  expect_equal(details$smoother, "susan")
  expect_true(details$used_mask)
  expect_true(is.finite(details$delta_expected_mm))
  expect_true(is.finite(details$post_expected_mm))
  expect_equal(details$tolerance_mm, pp_select_calibration("susan", TRUE)$tolerance_mm)
  expect_equal(details$calibration_type, "quadrature_ratio_linear")
  expect_equal(
    details$calibration_model_version,
    "smoothness-calibration-v4-fullcontext-distributed96-k3-8"
  )
  expect_equal(details$calibration_estimator, "detrend_mad")
  expect_true(is.finite(details$calibration_gain))
  expect_false(details$calibration_extrapolated)
  expect_true(details$preprocessing$enabled)
  expect_identical(details$calibration_max_volumes, 96L)
  expect_identical(
    details$calibration_volume_sampling, "distributed_full_run"
  )
  expect_identical(details$calibration_smoothing_context, "full_run")
  expect_identical(details$volumes_used, as.integer(nt))
  expect_identical(details$volume_indices, seq_len(nt))
  expect_identical(details$volume_sampling, "all")

  expect_error(
    validate_spatial_smooth(
      pre_file, post_file, mask_file, fwhm_mm = 6,
      smoother = "susan", used_mask = TRUE, max_volumes = 95L
    ),
    "requires max_volumes=96"
  )

  extrapolated <- validate_spatial_smooth(
    pre_file, post_file, mask_file, fwhm_mm = 10,
    smoother = "susan", used_mask = TRUE, tolerance_mm = 100
  )
  extrapolated_details <- attr(extrapolated, "details")
  expect_false(extrapolated)
  expect_true(extrapolated_details$within_tolerance)
  expect_true(extrapolated_details$calibration_extrapolated)
  expect_true(extrapolated_details$exact_input_mask_calibration)
  expect_match(
    attr(extrapolated, "message"),
    "FAIL \\(outside calibration support\\)"
  )

  expect_warning(
    custom_result <- validate_spatial_smooth(
      pre_file, post_file, mask_file, fwhm_mm = 6,
      smoother = "susan", used_mask = TRUE, input_mask = "custom"
    ),
    "No exact smoothness calibration for input mask 'custom'"
  )
  custom_details <- attr(custom_result, "details")
  expect_false(custom_result)
  expect_identical(custom_details$calibration_input_mask, "template")
  expect_true(custom_details$calibration_input_mask_extrapolated)
  expect_false(custom_details$exact_input_mask_calibration)
  expect_match(
    attr(custom_result, "message"),
    "FAIL \\(no exact input-mask calibration\\)"
  )
  expect_true(custom_details$calibration_extrapolated)
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

  result <- validate_spatial_smooth(pre_file, post_file, mask_file,
    fwhm_mm = NA_real_, max_volumes = 3)
  expect_true(is.logical(result))
  expect_match(attr(result, "message"), "directional check only")
  expect_equal(attr(result, "details")$volumes_used, 3L)
  expect_equal(attr(result, "details")$total_volumes, nt)
  # no calibration fields expected
  expect_null(attr(result, "details")$delta_expected_mm)

  all_result <- validate_spatial_smooth(
    pre_file, post_file, mask_file, fwhm_mm = NA_real_
  )
  all_details <- attr(all_result, "details")
  expect_equal(all_details$volumes_used, nt)
  expect_identical(all_details$volume_indices, seq_len(nt))
  expect_identical(all_details$volume_sampling, "all")
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

test_that("pp_predict_calibration linear model works", {
  model <- list(type = "linear", coeffs = c(-2.942579, 1.198781))
  # kernel=6 -> -2.942579 + 1.198781*6 = 4.250107
  expect_equal(pp_predict_calibration(model, 6), -2.942579 + 1.198781 * 6, tolerance = 1e-6)
})

test_that("pp_predict_calibration poly model works", {
  model <- list(type = "poly", coeffs = c(-3.6270403, 1.4369376, -0.03108286))
  # kernel=6 -> -3.6270403 + 1.4369376*6 + -0.03108286*36
  expected <- -3.6270403 + 1.4369376 * 6 + (-0.03108286) * 36
  expect_equal(pp_predict_calibration(model, 6), expected, tolerance = 1e-6)
})

test_that("pp_predict_calibration conditions quadrature delta on baseline and resolution", {
  model <- list(type = "quadrature_ratio_linear", coeffs = c(1.2, -0.5))
  low_pre <- pp_predict_calibration(model, 6, pre_fwhm = 2, voxel_mm = c(2, 2, 2))
  high_pre <- pp_predict_calibration(model, 6, pre_fwhm = 6, voxel_mm = c(2, 2, 2))
  coarser <- pp_predict_calibration(model, 6, pre_fwhm = 2, voxel_mm = c(3, 3, 3))

  expect_gt(low_pre, high_pre)
  expect_lt(coarser, low_pre)
  expect_equal(pp_calibration_gain(model, 6, c(2, 2, 2)), 1.2 - 0.5 * 2 / 6)
})

test_that("pp_smoothness_volume_indices distributes or preserves volumes", {
  expect_identical(formals(pp_smoothness_volume_indices)$max_volumes, 96L)
  expect_identical(
    formals(pp_estimate_classic_smoothness_file)$max_volumes, 96L
  )
  expect_identical(formals(validate_spatial_smooth)$max_volumes, 96L)
  distributed <- pp_smoothness_volume_indices(800L)
  expect_length(distributed, 96L)
  expect_identical(distributed[c(1L, 96L)], c(1L, 800L))
  expect_true(all(diff(distributed) %in% c(8L, 9L)))
  expect_equal(pp_smoothness_volume_indices(5L, max_volumes = 3L), c(1L, 3L, 5L))
  expect_equal(pp_smoothness_volume_indices(5L, max_volumes = 10L), 1:5)
  expect_equal(pp_smoothness_volume_indices(5L), 1:5)
  expect_equal(pp_smoothness_volume_indices(5L, max_volumes = Inf), 1:5)
})

test_that("pp_mad_scale_matrix matches row-wise MAD scaling", {
  mat <- matrix(c(1, 2, 3, 3, 3, 3, 10, 12, 14), nrow = 3, byrow = TRUE)
  expected_mad <- apply(mat, 1L, stats::mad, constant = 1.4826, na.rm = TRUE)
  expected_mad[!is.finite(expected_mad) | expected_mad <= 1e-6] <- 1
  expect_equal(pp_mad_scale_matrix(mat), mat / expected_mad)
})

test_that("file-backed smoothness estimation matches full-array preparation", {
  skip_if_not_installed("RNifti")
  set.seed(20260828)
  image <- array(rnorm(8L * 7L * 5L * 20L), dim = c(8L, 7L, 5L, 20L))
  mask <- array(FALSE, dim = c(8L, 7L, 5L))
  mask[2:7, 2:6, 2:4] <- TRUE
  image_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(image_file), add = TRUE)
  RNifti::writeNifti(image, image_file)
  voxel_mm <- pp_pixdim_mm(image_file)

  specs <- list(
    raw = list(preprocess = FALSE, polydeg = NA_integer_, demean = FALSE, unif = FALSE),
    detrend = list(preprocess = TRUE, polydeg = 3L, demean = TRUE, unif = FALSE),
    detrend_mad = list(preprocess = TRUE, polydeg = 3L, demean = TRUE, unif = TRUE)
  )
  for (spec in specs) {
    expected_data <- if (isTRUE(spec$preprocess)) {
      pp_prepare_classic_smoothness(
        image, mask, polydeg = spec$polydeg,
        demean = spec$demean, unif = spec$unif
      )
    } else {
      image
    }
    expected <- estimate_classic_fwhm(expected_data, mask, voxel_mm)$geom
    observed <- pp_estimate_classic_smoothness_file(
      image_file, mask, max_volumes = Inf,
      preprocess = spec$preprocess, polydeg = spec$polydeg,
      demean = spec$demean, unif = spec$unif
    )$geom
    expect_equal(observed, expected, tolerance = 1e-12)

    volume_indices <- pp_smoothness_volume_indices(20L, max_volumes = 7L)
    subset_image <- image[, , , volume_indices, drop = FALSE]
    subset_expected_data <- if (isTRUE(spec$preprocess)) {
      pp_prepare_classic_smoothness(
        subset_image, mask, polydeg = spec$polydeg,
        demean = spec$demean, unif = spec$unif
      )
    } else {
      subset_image
    }
    subset_expected <- estimate_classic_fwhm(
      subset_expected_data, mask, voxel_mm
    )$geom
    subset_observed <- pp_estimate_classic_smoothness_file(
      image_file, mask, max_volumes = 7L,
      preprocess = spec$preprocess, polydeg = spec$polydeg,
      demean = spec$demean, unif = spec$unif
    )
    expect_equal(subset_observed$geom, subset_expected, tolerance = 1e-12)
    expect_identical(subset_observed$volume_indices, volume_indices)
    expect_identical(subset_observed$volume_sampling, "distributed")
  }
})

test_that("masked-matrix classic estimator exactly matches full-array details", {
  set.seed(20260828)
  image <- array(rnorm(8L * 7L * 6L * 11L), dim = c(8L, 7L, 6L, 11L))
  mask <- array(runif(8L * 7L * 6L) > 0.25, dim = c(8L, 7L, 6L))
  sparse_volume <- image[, , , 1L]
  sparse_volume[] <- NA_real_
  sparse_volume[which(mask)[seq_len(6L)]] <- rnorm(6L)
  image[, , , 1L] <- sparse_volume
  image[3L, 3L, 3L, 2L] <- NA_real_
  voxel_mm <- c(2.4, 2.7, 3.1)
  masked_matrix <- array(image, dim = c(prod(dim(mask)), dim(image)[[4L]]))[
    as.vector(mask), , drop = FALSE
  ]

  reference <- estimate_classic_fwhm(image, mask, voxel_mm)
  observed <- pp_estimate_classic_masked_matrix(
    masked_matrix, mask, voxel_mm
  )

  expect_equal(observed$per_axis, reference$per_axis, tolerance = 1e-12)
  expect_equal(observed$geom_axes, reference$geom_axes, tolerance = 1e-12)
  expect_equal(observed$geom, reference$geom, tolerance = 1e-12)
})

test_that("pp_select_calibration selects correct model", {
  m <- pp_select_calibration("susan", used_mask = TRUE)
  expect_equal(m$type, "quadrature_ratio_linear")
  expect_length(m$coeffs, 2)
  expect_equal(m$mode, "fsl_susan_mask")
  expect_true(m$preprocess)
  expect_equal(m$estimator, "detrend_mad")
  expect_equal(
    m$model_version,
    "smoothness-calibration-v4-fullcontext-distributed96-k3-8"
  )
  expect_identical(m$max_volumes, 96L)
  expect_identical(m$volume_sampling, "distributed_full_run")
  expect_identical(m$smoothing_context, "full_run")
  expect_identical(m$calibrated_input_mask, "none")
  expect_false(m$input_mask_extrapolated)

  m2 <- pp_select_calibration("gaussian", used_mask = FALSE)
  expect_equal(m2$type, "quadrature_ratio_linear")
  expect_length(m2$coeffs, 2)
  expect_equal(m2$mode, "afni_3dmerge")
  expect_equal(m2$estimator, "detrend_mad")
})

test_that("input-mask calibration fallback is explicitly extrapolated", {
  expect_warning(
    model <- pp_select_calibration(
      "susan", used_mask = TRUE, input_mask = "custom"
    ),
    "No exact smoothness calibration for input mask 'custom'"
  )
  expect_identical(model$calibrated_input_mask, "template")
  expect_true(model$input_mask_extrapolated)
})

test_that("nested input-mask calibrations select an exact mask-specific leaf", {
  nested <- pp_calibration_coeffs
  base_model <- nested$susan$classic$mask$none
  nested$susan$classic$mask <- list(
    none = modifyList(base_model, list(input_mask = "none", coeffs = c(1, -0.1))),
    fmriprep = modifyList(
      base_model, list(input_mask = "fmriprep", coeffs = c(1.1, -0.2))
    ),
    template = modifyList(
      base_model, list(input_mask = "template", coeffs = c(1.2, -0.3))
    )
  )
  testthat::local_mocked_bindings(
    pp_calibration_coeffs = nested,
    .package = "BrainGnomes"
  )

  model <- pp_select_calibration(
    "susan", used_mask = TRUE, input_mask = "template"
  )
  expect_equal(model$coeffs, c(1.2, -0.3))
  expect_identical(model$calibrated_input_mask, "template")
  expect_false(model$input_mask_extrapolated)
})

test_that("promoted SUSAN calibrations are exact reviewed mask-specific models", {
  expected <- list(
    none = list(
      coeffs = c(1.27820370989578, -0.520302740093367),
      tolerance = 0.7
    ),
    fmriprep = list(
      coeffs = c(1.23406155442799, -0.451054336264315),
      tolerance = 0.8
    ),
    template = list(
      coeffs = c(1.16494890257513, -0.381080413733339),
      tolerance = 0.6
    )
  )
  for (input_mask in names(expected)) {
    model <- pp_select_calibration(
      "susan", used_mask = TRUE, input_mask = input_mask
    )
    expect_identical(
      model$model_version,
      "smoothness-calibration-v4-fullcontext-distributed96-k3-8"
    )
    expect_identical(model$calibrated_input_mask, input_mask)
    expect_false(model$input_mask_extrapolated)
    expect_equal(model$coeffs, expected[[input_mask]]$coeffs, tolerance = 1e-14)
    expect_equal(model$tolerance_mm, expected[[input_mask]]$tolerance)
    expect_identical(model$estimator, "detrend_mad")
    expect_equal(model$kernel_range_mm, c(3, 8))
    expect_identical(model$max_volumes, 96L)
    expect_identical(model$volume_sampling, "distributed_full_run")
    expect_identical(model$smoothing_context, "full_run")
  }
})

test_that("smoothing input-mask classification is independent of threshold masking", {
  expect_identical(
    smoothness_input_mask_condition(character(), "template", "/tmp/template.nii.gz"),
    "none"
  )
  expect_identical(
    smoothness_input_mask_condition("apply_mask", "template", "/tmp/anything.nii.gz"),
    "template"
  )
  expect_identical(
    smoothness_input_mask_condition(
      "apply_mask", "/data/mask.nii.gz",
      "/data/sub-01_space-MNI_desc-brain_mask.nii.gz"
    ),
    "fmriprep"
  )
  expect_identical(
    smoothness_input_mask_condition(
      "apply_mask", "/data/mask.nii.gz",
      "/data/sub-01_space-MNI_desc-preproc_templatemask.nii.gz"
    ),
    "template"
  )
  expect_identical(
    smoothness_input_mask_condition(
      "apply_mask", "/data/custom_mask.nii.gz", "/data/custom_mask.nii.gz"
    ),
    "custom"
  )
})

test_that("calibrated estimator preparation is exact and cannot be overridden", {
  susan_model <- pp_select_calibration("susan", used_mask = TRUE)
  susan_preparation <- pp_calibration_preparation(susan_model)
  expect_identical(susan_preparation$estimator, "detrend_mad")
  expect_true(susan_preparation$preprocess)
  expect_true(susan_preparation$unif)

  detrended_model <- pp_select_calibration("gaussian", used_mask = FALSE)
  detrended <- pp_calibration_preparation(detrended_model)
  expect_identical(detrended$estimator, "detrend_mad")
  expect_true(detrended$preprocess)
  expect_identical(detrended$polydeg, 3L)
  expect_true(detrended$demean)
  expect_true(detrended$unif)

  expect_error(
    pp_calibration_preparation(susan_model, preprocess = FALSE),
    "incompatible override.*preprocess"
  )
  expect_error(
    pp_calibration_preparation(detrended_model, unif = FALSE),
    "incompatible override.*unif"
  )
})

test_that("pp_select_calibration warns on unknown smoother", {
  expect_warning(
    m <- pp_select_calibration("unknown_smoother", used_mask = TRUE),
    "No calibration table"
  )
  # should fall back to gaussian
  expect_equal(m$type, "quadrature_ratio_linear")
})

test_that("embedded smoothness models match the reviewed extdata table", {
  table_path <- system.file(
    "extdata", "spatial_smooth_calibration.csv",
    package = "BrainGnomes", mustWork = TRUE
  )
  calibration <- utils::read.csv(table_path, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(calibration))) {
    row <- calibration[i, ]
    model <- pp_select_calibration(
      row$smoother, row$used_mask, input_mask = row$input_mask
    )
    expect_equal(model$model_version, row$model_version)
    expect_identical(model$calibrated_input_mask, row$input_mask)
    expect_false(model$input_mask_extrapolated)
    expect_equal(model$mode, row$mode)
    expect_equal(model$type, row$model_type)
    expect_equal(model$coeffs, c(row$coefficient_0, row$coefficient_1), tolerance = 1e-8)
    expect_equal(model$tolerance_mm, row$tolerance_mm)
    expect_equal(model$kernel_range_mm, c(row$kernel_min_mm, row$kernel_max_mm))
    expect_equal(model$voxel_range_mm, c(row$voxel_min_mm, row$voxel_max_mm))
    if (!is.na(row$preprocess)) {
      expect_identical(model$preprocess, row$preprocess)
    }
    if (!is.na(row$estimator)) {
      expect_identical(model$estimator, row$estimator)
    }
    if (!is.na(row$volumes_per_run) && nzchar(row$volume_sampling)) {
      expect_identical(model$max_volumes, as.integer(row$volumes_per_run))
      expect_identical(model$volume_sampling, row$volume_sampling)
    }
    if (!is.na(row$smoothing_context) && nzchar(row$smoothing_context)) {
      expect_identical(model$smoothing_context, row$smoothing_context)
    }
  }
})

test_that("promoted SUSAN models retain independent-validation evidence", {
  validation_path <- system.file(
    "extdata", "spatial_smooth_calibration_validation.csv",
    package = "BrainGnomes", mustWork = TRUE
  )
  validation <- utils::read.csv(validation_path, stringsAsFactors = FALSE)
  promoted <- validation[
    validation$model_version ==
      "smoothness-calibration-v4-fullcontext-distributed96-k3-8",
  ]

  expect_equal(nrow(promoted), 3L)
  expect_setequal(promoted$input_mask, c("none", "fmriprep", "template"))
  expect_true(all(promoted$external_within_tolerance))
  expect_true(all(
    promoted$external_q95_abs_error_mm <= promoted$tolerance_mm
  ))
  expect_identical(promoted$n_external_subjects, rep(10L, 3L))
  expect_identical(promoted$fmriprep_version, rep("25.2.5", 3L))
  expect_identical(promoted$volumes_per_run, rep(96L, 3L))
  expect_identical(
    promoted$volume_sampling, rep("distributed_full_run", 3L)
  )
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
  expect_gte(details$fraction_voxels_outside_reduced, 0.8)
  # passband should not lose much power
  expect_true(is.na(details$avg_passband_change_db) || details$avg_passband_change_db > -3)
  expect_gte(details$fraction_voxels_passband_preserved, 0.8)

  set.seed(999999)
  repeated <- validate_temporal_filter(
    pre_file = pre_file, post_file = post_file, tr = tr,
    band_low_hz = hp, band_high_hz = lp, mask_file = mask_file,
    n_voxels = 10L
  )
  expect_true(repeated)
  expect_identical(
    attr(repeated, "details")$sampled_indices,
    details$sampled_indices
  )

  # Leave three deterministic samples entirely unfiltered. The aggregate mean
  # still shows substantial attenuation, but the per-voxel coverage gate must
  # detect that the requested filter was not applied consistently.
  partial <- post
  for (idx in details$sampled_indices[seq_len(3L)]) {
    coord <- arrayInd(idx, dim(mask))
    partial[coord[1], coord[2], coord[3], ] <-
      pre[coord[1], coord[2], coord[3], ]
  }
  partial_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(partial_file), add = TRUE)
  write_synth_4d(partial, partial_file)
  inconsistent <- validate_temporal_filter(
    pre_file = pre_file, post_file = partial_file, tr = tr,
    band_low_hz = hp, band_high_hz = lp, mask_file = mask_file,
    n_voxels = 10L
  )
  expect_false(inconsistent)
  expect_equal(
    attr(inconsistent, "details")$sampled_indices,
    details$sampled_indices
  )
  expect_lt(
    attr(inconsistent, "details")$fraction_voxels_outside_reduced, 0.8
  )
  expect_gt(attr(inconsistent, "details")$avg_reduction_outside_db, 0)

  ineffective <- validate_temporal_filter(
    pre_file = pre_file, post_file = post_file, tr = tr,
    band_low_hz = NA_real_, band_high_hz = Inf,
    mask_file = mask_file, n_voxels = 10L
  )
  expect_false(ineffective)
  expect_true(attr(ineffective, "details")$invalid_band_specification)
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

# --- helper: pp_is_valid_series ----------------------------------------------

test_that("pp_is_valid_series detects constant and invalid series", {
  expect_true(pp_is_valid_series(rnorm(50)))
  expect_false(pp_is_valid_series(rep(5, 50)))
  expect_false(pp_is_valid_series(c(1, 2, NA, 4)))
  expect_false(pp_is_valid_series(c(1, Inf, 3)))
})

# --- helper: pp_power_multitaper ---------------------------------------------

test_that("pp_power_multitaper returns expected frequency structure", {
  skip_if_not_installed("multitaper")
  skip_if_not_installed("signal")

  set.seed(901)
  dt <- 0.8 # TR
  nt <- 200
  t_sec <- seq(0, by = dt, length.out = nt)

  # pure sine at 0.05 Hz
  y <- sin(2 * pi * 0.05 * t_sec) + rnorm(nt, sd = 0.1)
  spec <- pp_power_multitaper(y, dt = dt)

  expect_s3_class(spec, "data.frame")
  expect_true(all(c("f", "power") %in% names(spec)))
  expect_true(nrow(spec) > 0)

  # peak should be near 0.05 Hz
  peak_freq <- spec$f[which.max(spec$power)]
  expect_true(abs(peak_freq - 0.05) < 0.02)
})

test_that("pp_power_multitaper smooths a 50-frame spectrum safely", {
  skip_if_not_installed("multitaper")
  skip_if_not_installed("signal")

  set.seed(902)
  y <- sin(seq_len(50L) / 4) + rnorm(50L, sd = 0.2)
  expect_no_error(spec <- pp_power_multitaper(y, dt = 0.635, smooth_psd = TRUE))
  expect_gt(nrow(spec), 0L)
  expect_true(all(is.finite(spec$power)))
})

test_that("pp_power_multitaper errors on constant series", {
  skip_if_not_installed("multitaper")

  spec <- pp_power_multitaper(rep(0, 50), dt = 1)
  expect_equal(nrow(spec), 0) # returns empty data.frame for constant series
})

# --- helper: pp_mtm_bandpower ------------------------------------------------

test_that("pp_mtm_bandpower computes band power for defined bands", {
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

  bp <- pp_mtm_bandpower(y, dt = dt, bands = bands, detrend = "linear")
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

test_that("external 2 mm smoothing support is confined to the validated context", {
  model <- pp_select_calibration("susan", TRUE, input_mask = "template")
  expect_identical(pp_calibration_support(model, 5, c(2, 2, 2)), "externally_validated")
  expect_identical(pp_calibration_support(model, 5, rep(2 + 1e-7, 3)), "externally_validated")
  expect_identical(pp_calibration_support(model, 5, rep(2.7, 3)), "interpolated")
  expect_identical(pp_calibration_support(model, 3, rep(2, 3)), "EXTRAPOLATED")
  expect_identical(pp_calibration_support(model, 8, rep(2, 3)), "EXTRAPOLATED")
  expect_identical(pp_calibration_support(model, 5, c(1, 2, 4)), "EXTRAPOLATED")
  expect_identical(pp_calibration_support(model, 5, rep(1.9, 3)), "EXTRAPOLATED")
  for (mask in c("none", "fmriprep")) {
    other <- pp_select_calibration("susan", TRUE, input_mask = mask)
    expect_identical(pp_calibration_support(other, 5, rep(2, 3)), "EXTRAPOLATED")
  }
  model$input_mask_extrapolated <- TRUE
  expect_identical(pp_calibration_support(model, 5, rep(2, 3)), "EXTRAPOLATED")
})

test_that("external smoothing support metadata agrees with the validation evidence", {
  path <- system.file("extdata", "spatial_smooth_calibration_support.csv", package = "BrainGnomes")
  evidence <- utils::read.csv(path, stringsAsFactors = FALSE)
  model <- pp_select_calibration("susan", TRUE, input_mask = "template")
  support <- model$external_support[[1L]]
  expect_identical(evidence$model_version, model$model_version)
  expect_identical(evidence$support_version, model$support_version)
  expect_equal(c(evidence$kernel_min_mm, evidence$kernel_max_mm), support$kernel_range_mm)
  expect_equal(c(evidence$voxel_min_mm, evidence$voxel_max_mm), support$voxel_range_mm)
  expect_equal(evidence$n_validation, support$n_validation)
  expect_equal(evidence$n_subjects, support$n_subjects)
  expect_equal(evidence$max_abs_error_mm, support$max_abs_error_mm)
  expect_lte(evidence$max_abs_error_mm, model$tolerance_mm)
})

test_that("externally supported smoothing still enforces the numerical tolerance", {
  pre_file <- tempfile(fileext = ".nii.gz")
  post_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(pre_file, post_file, mask_file)), add = TRUE)
  image <- RNifti::asNifti(array(1, c(2, 2, 2, 100)))
  RNifti::pixdim(image) <- c(2, 2, 2, 1)
  RNifti::writeNifti(image, pre_file)
  RNifti::writeNifti(image, post_file)
  RNifti::writeNifti(RNifti::asNifti(array(1, c(2, 2, 2)), reference = image), mask_file)
  post_fwhm <- 5.5877
  validate <- function() with_mocked_bindings(
    validate_spatial_smooth(pre_file, post_file, mask_file,
      fwhm_mm = 5, input_mask = "template"),
    pp_estimate_classic_smoothness_file = function(path, ...) list(
      geom = if (identical(path, pre_file)) 2.6135 else post_fwhm,
      volumes_used = 96L, total_volumes = 100L,
      volume_indices = pp_distributed_volume_indices(100L, 96L),
      volume_sampling = "distributed"
    )
  )
  result <- validate()
  expect_true(result)
  expect_identical(attr(result, "details")$calibration_support, "externally_validated")
  expect_false(attr(result, "details")$calibration_extrapolated)
  post_fwhm <- 7
  expect_false(validate())
})
