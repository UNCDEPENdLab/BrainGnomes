test_that("expand_confound_columns expands regex and angle brackets", {
  available <- c(
    "motion_param_1", "motion_param_2", "motion_param_3",
    "motion_param_25", "csf", "csf_derivative1", "csf_power2", "white_matter",
    "motion_parameter_1", "motion_parameter_12", "motion_parameter_0"
  )

  expect_equal(
    expand_confound_columns("motion_param_<1-3>", available),
    c("motion_param_1", "motion_param_2", "motion_param_3")
  )

  expect_equal(
    expand_confound_columns("motion_param_<1,25>", available),
    c("motion_param_1", "motion_param_25")
  )

  expect_setequal(
    expand_confound_columns("motion_parameter_[1-9]+", available),
    c("motion_parameter_1", "motion_parameter_12")
  )
  
  # ensure that regex expansion works when requested
  expect_setequal(
    expand_confound_columns("csf.*", available),
    c("csf", "csf_derivative1", "csf_power2")
  )
  
  # ensure that bad regexs return nothing
  expect_null(expand_confound_columns("csfsdflkj", available))
})
