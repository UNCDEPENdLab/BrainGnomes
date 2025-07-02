library(testthat)
library(BrainGnomes)

test_that("compute_spike_regressors identifies spikes", {
  mot <- data.frame(fd = c(0.1, 0.6, 0.2, 0.7))
  spikes <- BrainGnomes:::compute_spike_regressors(mot, "fd > 0.5", lg = lgr::get_logger())
  expect_equal(nrow(spikes), 4)
  expect_equal(ncol(spikes), 2)
  expect_equal(colSums(spikes), c(1, 1))
})
