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

test_that("expand_confound_columns expands motion/confound shortcuts", {
  available <- c(
    "csf", "csf_derivative1", "csf_derivative1_power2", "csf_power2",
    "global_signal", "global_signal_derivative1", "global_signal_derivative1_power2",
    "global_signal_power2",
    "rot_x", "rot_x_derivative1", "rot_x_derivative1_power2", "rot_x_power2",
    "rot_y", "rot_y_derivative1", "rot_y_derivative1_power2", "rot_y_power2",
    "rot_z", "rot_z_derivative1", "rot_z_derivative1_power2", "rot_z_power2",
    "trans_x", "trans_x_derivative1", "trans_x_derivative1_power2", "trans_x_power2",
    "trans_y", "trans_y_derivative1", "trans_y_derivative1_power2", "trans_y_power2",
    "trans_z", "trans_z_derivative1", "trans_z_derivative1_power2", "trans_z_power2",
    "white_matter", "white_matter_derivative1", "white_matter_derivative1_power2",
    "white_matter_power2"
  )

  expect_setequal(
    expand_confound_columns("6p", available),
    c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z")
  )

  expect_setequal(
    expand_confound_columns("12p", available),
    c(
      "rot_x", "rot_x_derivative1", "rot_y", "rot_y_derivative1",
      "rot_z", "rot_z_derivative1", "trans_x", "trans_x_derivative1",
      "trans_y", "trans_y_derivative1", "trans_z", "trans_z_derivative1"
    )
  )

  expect_setequal(
    expand_confound_columns("24p", available),
    c(
      "rot_x", "rot_x_derivative1", "rot_x_derivative1_power2", "rot_x_power2",
      "rot_y", "rot_y_derivative1", "rot_y_derivative1_power2", "rot_y_power2",
      "rot_z", "rot_z_derivative1", "rot_z_derivative1_power2", "rot_z_power2",
      "trans_x", "trans_x_derivative1", "trans_x_derivative1_power2", "trans_x_power2",
      "trans_y", "trans_y_derivative1", "trans_y_derivative1_power2", "trans_y_power2",
      "trans_z", "trans_z_derivative1", "trans_z_derivative1_power2", "trans_z_power2"
    )
  )

  expect_setequal(
    expand_confound_columns("36p", available),
    available
  )
})
