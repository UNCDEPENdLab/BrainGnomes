# Tests for calculate_motion_outliers()

# Helper to create a mock confounds file with known motion parameters
create_mock_confounds <- function(dir, filename, motion_data, fd_values = NULL) {
  filepath <- file.path(dir, filename)
  df <- as.data.frame(motion_data)
  if (!is.null(fd_values)) {
    df$framewise_displacement <- fd_values
  }
  data.table::fwrite(df, filepath, sep = "\t")
  filepath
}

test_that("calculate_motion_outliers computes correct FD from motion parameters", {
  skip_if_not_installed("data.table")


  tmp_dir <- tempfile("motion_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create motion data where FD can be computed manually

# Motion parameters: rot_x, rot_y, rot_z (radians), trans_x, trans_y, trans_z (mm)
  # FD = sum(abs(diff(rot))) * head_radius + sum(abs(diff(trans)))
  # With head_radius = 50mm (default)
  #
  # Row 1: baseline (FD = 0 by definition)
  # Row 2: rot_x changes by 0.01 rad -> 0.01 * 50 = 0.5mm rotation contribution
  # Row 3: trans_x changes by 0.3mm -> 0.3mm translation contribution
  # Row 4: rot_y changes by 0.02 rad, trans_z changes by 0.1mm -> 0.02*50 + 0.1 = 1.1mm
  # Row 5: no change -> FD = 0
  motion_data <- data.frame(
    rot_x = c(0, 0.01, 0.01, 0.01, 0.01),
    rot_y = c(0, 0, 0, 0.02, 0.02),
    rot_z = c(0, 0, 0, 0, 0),
    trans_x = c(0, 0, 0.3, 0.3, 0.3),
    trans_y = c(0, 0, 0, 0, 0),
    trans_z = c(0, 0, 0, 0.1, 0.1)
  )

  # Expected FD values (computed manually):
  # Row 1: 0 (first volume)
  # Row 2: |0.01-0|*50 + 0 = 0.5
  # Row 3: 0 + |0.3-0| = 0.3
  # Row 4: |0.02-0|*50 + |0.1-0| = 1.0 + 0.1 = 1.1
  # Row 5: 0
  expected_fd <- c(0, 0.5, 0.3, 1.1, 0)

  confounds_file <- create_mock_confounds(
    tmp_dir,
    "sub-01_task-rest_desc-confounds_timeseries.tsv",
    motion_data
  )

  result <- calculate_motion_outliers(
    confounds_files = confounds_file,
    thresholds = c(0.3, 0.5, 1.0)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)

  # Check FD statistics
  expect_equal(result$fd_max, max(expected_fd), tolerance = 1e-6)
  expect_equal(result$fd_mean, mean(expected_fd), tolerance = 1e-6)

  # Check outlier percentages
  # Threshold 0.3: values > 0.3 are 0.5, 1.1 -> 2/5 = 40%
  expect_equal(result$fd_gt_0p3, 40, tolerance = 1e-6)
  # Threshold 0.5: values > 0.5 are 1.1 -> 1/5 = 20%
  expect_equal(result$fd_gt_0p5, 20, tolerance = 1e-6)
  # Threshold 1.0: values > 1.0 are 1.1 -> 1/5 = 20%
  expect_equal(result$fd_gt_1, 20, tolerance = 1e-6)
})

test_that("calculate_motion_outliers uses existing FD column when present", {
  skip_if_not_installed("data.table")

  tmp_dir <- tempfile("motion_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Motion data (values don't matter since we provide FD directly)
  motion_data <- data.frame(
    rot_x = rep(0, 5),
    rot_y = rep(0, 5),
    rot_z = rep(0, 5),
    trans_x = rep(0, 5),
    trans_y = rep(0, 5),
    trans_z = rep(0, 5)
  )

  # Provide explicit FD values
  fd_values <- c(0.1, 0.2, 0.4, 0.6, 0.8)

  confounds_file <- create_mock_confounds(
    tmp_dir,
    "sub-02_task-rest_desc-confounds_timeseries.tsv",
    motion_data,
    fd_values = fd_values
  )

  result <- calculate_motion_outliers(
    confounds_files = confounds_file,
    thresholds = c(0.3, 0.5)
  )

  expect_equal(result$fd_max, 0.8, tolerance = 1e-6)
  expect_equal(result$fd_mean, mean(fd_values), tolerance = 1e-6)

  # Threshold 0.3: values > 0.3 are 0.4, 0.6, 0.8 -> 3/5 = 60%
  expect_equal(result$fd_gt_0p3, 60, tolerance = 1e-6)
  # Threshold 0.5: values > 0.5 are 0.6, 0.8 -> 2/5 = 40%
  expect_equal(result$fd_gt_0p5, 40, tolerance = 1e-6)
})

test_that("calculate_motion_outliers extracts BIDS info correctly", {
  skip_if_not_installed("data.table")

  tmp_dir <- tempfile("motion_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  motion_data <- data.frame(
    rot_x = c(0, 0),
    rot_y = c(0, 0),
    rot_z = c(0, 0),
    trans_x = c(0, 0),
    trans_y = c(0, 0),
    trans_z = c(0, 0),
    framewise_displacement = c(0.1, 0.2)
  )

  confounds_file <- file.path(tmp_dir, "sub-ABC_ses-01_task-rest_run-02_desc-confounds_timeseries.tsv")
  data.table::fwrite(motion_data, confounds_file, sep = "\t")

  result <- calculate_motion_outliers(
    confounds_files = confounds_file,
    thresholds = 0.5
  )

  expect_equal(result$subject, "ABC")
  expect_equal(result$session, "01")
  expect_equal(result$task, "rest")
  expect_equal(result$run, "02")
})

test_that("calculate_motion_outliers handles multiple files", {
  skip_if_not_installed("data.table")

  tmp_dir <- tempfile("motion_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create two confounds files with different FD values
  motion1 <- data.frame(
    rot_x = rep(0, 4), rot_y = rep(0, 4), rot_z = rep(0, 4),
    trans_x = rep(0, 4), trans_y = rep(0, 4), trans_z = rep(0, 4),
    framewise_displacement = c(0.1, 0.2, 0.3, 0.4)
  )
  motion2 <- data.frame(
    rot_x = rep(0, 4), rot_y = rep(0, 4), rot_z = rep(0, 4),
    trans_x = rep(0, 4), trans_y = rep(0, 4), trans_z = rep(0, 4),
    framewise_displacement = c(0.5, 0.6, 0.7, 0.8)
  )

  file1 <- file.path(tmp_dir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  file2 <- file.path(tmp_dir, "sub-02_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(motion1, file1, sep = "\t")
  data.table::fwrite(motion2, file2, sep = "\t")

  result <- calculate_motion_outliers(
    confounds_files = c(file1, file2),
    thresholds = 0.5
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result$subject, c("01", "02"))
  expect_equal(result$fd_max, c(0.4, 0.8), tolerance = 1e-6)
})

test_that("calculate_motion_outliers validates bandstop_max_bpm > bandstop_min_bpm", {
  skip_if_not_installed("data.table")

  tmp_dir <- tempfile("motion_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  motion_data <- data.frame(
    rot_x = c(0, 0.01), rot_y = c(0, 0), rot_z = c(0, 0),
    trans_x = c(0, 0), trans_y = c(0, 0), trans_z = c(0, 0),
    framewise_displacement = c(0.1, 0.2)
  )
  confounds_file <- file.path(tmp_dir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(motion_data, confounds_file, sep = "\t")

  expect_error(
    calculate_motion_outliers(
      confounds_files = confounds_file,
      thresholds = 0.3,
      include_filtered = TRUE,
      filter_method = "notch",
      tr = 2,
      bandstop_min_bpm = 20,
      bandstop_max_bpm = 15
    ),
    "bandstop_max_bpm.*must be greater than bandstop_min_bpm"
  )
})

test_that("calculate_motion_outliers lowpass filtering produces filtered FD columns", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("signal")

  tmp_dir <- tempfile("motion_lp_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  n <- 100L
  set.seed(42)
  # Motion with a mix of slow drift and fast oscillation
  t_seq <- seq_len(n)
  motion_data <- data.frame(
    rot_x   = 0.01 * sin(2 * pi * t_seq / 50) + 0.005 * sin(2 * pi * t_seq / 4),
    rot_y   = 0.005 * cos(2 * pi * t_seq / 40),
    rot_z   = 0.002 * sin(2 * pi * t_seq / 60),
    trans_x = 0.1 * sin(2 * pi * t_seq / 45) + 0.08 * sin(2 * pi * t_seq / 3),
    trans_y = 0.05 * cos(2 * pi * t_seq / 55),
    trans_z = 0.03 * sin(2 * pi * t_seq / 35)
  )
  confounds_file <- file.path(tmp_dir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(motion_data, confounds_file, sep = "\t")

  result <- calculate_motion_outliers(
    confounds_files = confounds_file,
    thresholds = 0.3,
    include_filtered = TRUE,
    filter_method = "lowpass",
    tr = 1,
    low_pass_hz = 0.1,
    filter_order = 2L
  )

  expect_equal(nrow(result), 1L)
  # Filtered columns should be present
  expect_true("fd_filt_max" %in% names(result))
  expect_true("fd_filt_mean" %in% names(result))
  expect_true("fd_filt_gt_0p3" %in% names(result))

  # Filtered FD should be non-negative and finite

  expect_true(is.finite(result$fd_filt_max))
  expect_true(result$fd_filt_max >= 0)
  expect_true(is.finite(result$fd_filt_mean))

  # Low-pass should reduce high-frequency motion, so filtered max FD should be
  # smaller than or equal to unfiltered (the fast oscillation is removed)
  expect_true(result$fd_filt_max <= result$fd_max + 1e-6)
})

test_that("calculate_motion_outliers writes output file with correct separator", {
  skip_if_not_installed("data.table")

  tmp_dir <- tempfile("motion_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  motion_data <- data.frame(
    rot_x = rep(0, 3), rot_y = rep(0, 3), rot_z = rep(0, 3),
    trans_x = rep(0, 3), trans_y = rep(0, 3), trans_z = rep(0, 3),
    framewise_displacement = c(0.1, 0.2, 0.3)
  )
  confounds_file <- file.path(tmp_dir, "sub-01_task-rest_desc-confounds_timeseries.tsv")
  data.table::fwrite(motion_data, confounds_file, sep = "\t")

  # Test TSV output
  tsv_out <- file.path(tmp_dir, "output.tsv")
  result_tsv <- calculate_motion_outliers(
    confounds_files = confounds_file,
    thresholds = 0.3,
    output_file = tsv_out
  )
  expect_true(file.exists(tsv_out))
  tsv_content <- readLines(tsv_out, n = 2)
  expect_true(grepl("\t", tsv_content[1]))  # Tab-separated

  # Test CSV output
  csv_out <- file.path(tmp_dir, "output.csv")
  result_csv <- calculate_motion_outliers(
    confounds_files = confounds_file,
    thresholds = 0.3,
    output_file = csv_out
  )
  expect_true(file.exists(csv_out))
  csv_content <- readLines(csv_out, n = 2)
  expect_true(grepl(",", csv_content[1]))  # Comma-separated
})

test_that("calculate_motion_outliers returns empty data.frame when no files found", {
  skip_if_not_installed("data.table")

  tmp_dir <- tempfile("empty_motion_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  expect_warning(
    result <- calculate_motion_outliers(
      input_dir = tmp_dir,
      thresholds = 0.3
    ),
    "No confounds files found"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_true("subject" %in% names(result))
  expect_true("fd_max" %in% names(result))
  expect_true("fd_mean" %in% names(result))
})

test_that("calculate_motion_outliers errors when no input source provided",
{
  expect_error(
    calculate_motion_outliers(thresholds = 0.3),
    "Provide one of confounds_files, input_dir, or scfg"
  )
})

test_that("calculate_motion_outliers errors on missing confounds files", {
  expect_error(
    calculate_motion_outliers(
      confounds_files = "/nonexistent/path/confounds.tsv",
      thresholds = 0.3
    ),
    "Confounds files not found"
  )
})
