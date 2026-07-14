run_automask_for_test <- function(image, peels = 0L) {
  output <- NULL
  invisible(capture.output({
    output <- automask(
      RNifti::asNifti(image),
      outfile = "",
      NN = 1L,
      peels = peels,
      fill_holes = FALSE
    )
  }))
  as.array(output) > 0
}

test_that("automask clip estimation is not controlled by positive background", {
  image <- array(0.01, dim = c(30, 30, 30))
  expected <- array(FALSE, dim = dim(image))
  expected[11:20, 11:20, 11:20] <- TRUE
  image[expected] <- 100

  mask <- run_automask_for_test(image)

  expect_identical(mask, expected)
})

test_that("gradual clip levels preserve connected low-sensitivity brain", {
  dims <- c(20, 20, 20)
  x <- array(rep(seq_len(dims[1]), times = dims[2] * dims[3]), dims)
  y <- aperm(x, c(2, 1, 3))
  z <- aperm(x, c(2, 3, 1))
  expected <- ((x - 10.5)^2 / 36 +
               (y - 10.5)^2 / 49 +
               (z - 10.5)^2 / 36) <= 1

  image <- array(1, dims)
  image[expected & x <= 10] <- 40
  image[expected & x > 10] <- 100

  mask <- run_automask_for_test(image)

  expect_identical(mask, expected)
  expect_true(any(mask & x <= 10))
  expect_true(any(mask & x > 10))
})

test_that("automask thresholding is invariant to global gain", {
  image <- array(0.01, dim = c(20, 20, 20))
  image[6:15, 6:15, 6:15] <- 100

  expect_identical(
    run_automask_for_test(image),
    run_automask_for_test(image * 37)
  )
})

test_that("automask returns an empty mask when no positive signal exists", {
  expect_false(any(run_automask_for_test(array(0, dim = c(10, 10, 10)))))
})

test_that("AFNI-style peeling removes thin attachments without general dilation", {
  dims <- c(25, 25, 25)
  candidate <- array(FALSE, dims)
  candidate[8:16, 8:16, 8:16] <- TRUE
  candidate[17:21, 12, 12] <- TRUE
  image <- array(0.01, dims)
  image[candidate] <- 100

  expected <- candidate
  expected[17:21, 12, 12] <- FALSE
  for (x in c(8, 16))
    for (y in c(8, 16))
      for (z in c(8, 16)) expected[x, y, z] <- FALSE

  mask <- run_automask_for_test(image, peels = 1L)

  expect_identical(mask, expected)
  expect_false(any(mask & !candidate))
})

test_that("AFNI-style restoration does not fill voxels absent before peeling", {
  dims <- c(21, 21, 21)
  candidate <- array(FALSE, dims)
  candidate[6:16, 6:16, 6:16] <- TRUE
  candidate[11, 11, 11] <- FALSE
  image <- array(0.01, dims)
  image[candidate] <- 100

  mask <- run_automask_for_test(image, peels = 1L)

  expect_false(mask[11, 11, 11])
  expect_false(any(mask & !candidate))
  expect_true(mask[11, 11, 10])
})
