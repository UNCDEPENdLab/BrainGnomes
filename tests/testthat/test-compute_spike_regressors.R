test_that("compute_spike_regressors identifies spikes", {
  mot <- data.frame(framewise_displacement = c(0.1, 0.6, 0.2, 0.7))
  spikes <- compute_spike_regressors(mot, c("framewise_displacement > 0.5"), lg = lgr::get_logger_glue("BrainGnomes"))
  expect_equal(nrow(spikes), 4)
  expect_equal(ncol(spikes), 2)
  expect_equal(colSums(spikes), c(expr1_spike_1=1, expr1_spike_2=1))
})
