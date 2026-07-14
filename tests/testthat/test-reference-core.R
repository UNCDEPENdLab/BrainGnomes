make_reference_core_fixture <- function(scale = 1) {
  n_timepoints <- 60L
  n_voxels <- 16L
  time <- seq_len(n_timepoints)

  signal <- t(vapply(seq_len(n_voxels), function(voxel) {
    1000 + (0.5 + voxel / 10) * sin(time / 3)
  }, numeric(n_timepoints)))

  signal[2, ] <- 50 + sin(time / 3)                  # low baseline
  signal[3, ] <- 1000 + 300 * (-1)^time             # sustained noise
  signal[4, ] <- 1000 + sin(time / 3)               # isolated spikes
  signal[4, c(20, 40)] <- 1800
  signal[5, ] <- -100 + sin(time / 3)               # negative baseline
  signal[6, seq_len(50)] <- NA_real_                 # insufficient data
  signal <- signal * scale

  image <- RNifti::asNifti(array(signal, dim = c(4, 2, 2, n_timepoints)))
  mask_a <- array(1, dim = c(4, 2, 2))
  mask_b <- mask_a
  mask_b[7] <- 0                                     # mask disagreement

  list(
    image = image,
    mask_a = RNifti::asNifti(mask_a),
    mask_b = RNifti::asNifti(mask_b)
  )
}

test_that("derive_reference_core applies each conservative criterion", {
  fixture <- make_reference_core_fixture()

  result <- derive_reference_core(
    fixture$image,
    fixture$mask_a,
    fixture$mask_b,
    min_valid_frames = 20L
  )

  core <- as.vector(as.array(result$core))
  rejection <- as.vector(as.array(result$rejection_code))
  membership <- as.vector(as.array(result$mask_membership))

  expect_equal(dim(as.array(result$core)), c(4, 2, 2))
  expect_equal(core, c(1, 0, 0, 0, 0, 0, 0, rep(1, 9)))
  expect_equal(rejection[1:7], c(0, 8, 48, 32, 4, 2, 1))
  expect_true(all(rejection[8:16] == 0))
  expect_equal(membership, c(rep(3, 6), 1, rep(3, 9)))
  expect_true(is.finite(as.vector(as.array(result$baseline))[7]))

  expect_lt(
    as.vector(as.array(result$baseline))[2],
    result$thresholds$baseline_floor
  )
  expect_gt(
    as.vector(as.array(result$relative_noise_cv))[3],
    result$thresholds$relative_noise_threshold
  )
  expect_gt(
    as.vector(as.array(result$spike_fraction))[4],
    result$thresholds$max_spike_fraction
  )
  expect_equal(unname(result$counts[c("agreement", "core")]), c(15L, 10L))
  expect_equal(
    unname(result$counts[c("mask_a_only", "mask_b_only", "union")]),
    c(1L, 0L, 16L)
  )
  expect_equal(result$mask_metrics[["dice"]], 30 / 31)
  expect_equal(result$mask_metrics[["core_fraction_of_agreement"]], 10 / 15)
})

test_that("derive_reference_core supports an automask without a constraint", {
  fixture <- make_reference_core_fixture()

  result <- derive_reference_core(
    fixture$image,
    candidate_mask = fixture$mask_a,
    min_valid_frames = 20L
  )

  expect_false(result$used_constraint_mask)
  expect_equal(result$counts[["agreement"]], 16L)
  expect_equal(result$counts[["mask_a_only"]], 0L)
  expect_equal(result$mask_metrics[["dice"]], 1)
  expect_equal(as.vector(as.array(result$core))[7], 1)
})

test_that("relative criteria and the core are invariant to global gain", {
  original <- make_reference_core_fixture(scale = 1)
  rescaled <- make_reference_core_fixture(scale = 10)

  first <- derive_reference_core(
    original$image, original$mask_a, original$mask_b,
    min_valid_frames = 20L
  )
  second <- derive_reference_core(
    rescaled$image, rescaled$mask_a, rescaled$mask_b,
    min_valid_frames = 20L
  )

  expect_equal(
    as.vector(as.array(first$core)),
    as.vector(as.array(second$core))
  )
  expect_equal(
    as.vector(as.array(first$relative_noise_cv)),
    as.vector(as.array(second$relative_noise_cv)),
    tolerance = 1e-7
  )
  expect_equal(
    second$thresholds$baseline_floor,
    10 * first$thresholds$baseline_floor,
    tolerance = 1e-7
  )
})

test_that("temporal QA is retained for mask-disagreement voxels", {
  fixture <- make_reference_core_fixture()
  second_mask <- as.array(fixture$mask_b)
  second_mask[2] <- 0

  result <- derive_reference_core(
    fixture$image,
    fixture$mask_a,
    RNifti::asNifti(second_mask),
    min_valid_frames = 20L
  )

  membership <- as.vector(as.array(result$mask_membership))
  rejection <- as.vector(as.array(result$rejection_code))
  baseline <- as.vector(as.array(result$baseline))

  expect_equal(membership[2], 1)
  expect_true(is.finite(baseline[2]))
  expect_equal(bitwAnd(rejection[2], 1L), 1L)
  expect_equal(bitwAnd(rejection[2], 8L), 8L)
})

test_that("excluded volumes do not create or contribute spike transitions", {
  fixture <- make_reference_core_fixture()
  include_frames <- rep(TRUE, 60)
  include_frames[c(20, 40)] <- FALSE

  result <- derive_reference_core(
    fixture$image,
    fixture$mask_a,
    fixture$mask_b,
    include_frames = include_frames,
    min_valid_frames = 20L
  )

  expect_equal(as.vector(as.array(result$core))[4], 1)
  expect_equal(as.vector(as.array(result$spike_fraction))[4], 0)
})

test_that("derive_reference_core validates grids and frame selection", {
  fixture <- make_reference_core_fixture()

  expect_error(
    derive_reference_core(
      fixture$image,
      fixture$mask_a,
      RNifti::asNifti(array(1, dim = c(2, 2, 2))),
      min_valid_frames = 20L
    ),
    "dimensions and affine"
  )
  expect_error(
    derive_reference_core(
      fixture$image,
      fixture$mask_a,
      fixture$mask_b,
      include_frames = rep(TRUE, 59),
      min_valid_frames = 20L
    ),
    "one value per image volume"
  )
  expect_error(
    derive_reference_core(
      fixture$image,
      fixture$mask_a,
      fixture$mask_b,
      include_frames = c(rep(TRUE, 59), NA),
      min_valid_frames = 20L
    ),
    "cannot contain missing"
  )
})

test_that("derive_reference_core writes the final mask when requested", {
  fixture <- make_reference_core_fixture()
  outfile <- tempfile(fileext = ".nii.gz")

  result <- derive_reference_core(
    fixture$image,
    fixture$mask_a,
    fixture$mask_b,
    min_valid_frames = 20L,
    outfile = outfile
  )

  expect_true(file.exists(outfile))
  expect_equal(
    as.vector(as.array(RNifti::readNifti(outfile))),
    as.vector(as.array(result$core))
  )
})
