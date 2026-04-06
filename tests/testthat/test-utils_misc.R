# tests for miscellaneous utility functions
# file_sans_ext removes common extensions

test_that("file_sans_ext removes image extensions", {
  expect_equal(file_sans_ext("test.nii.gz"), "test")
  expect_equal(file_sans_ext("path/data.nii"), "path/data")
  expect_true(is.na(file_sans_ext("file.txt2")))
})

# extract_bids_info extracts BIDS fields

test_that("extract_bids_info parses filename entities and reconstructs with construct_bids_filename", {
  files <- c(
    "sub-01_ses-02_task-memory_run-1_bold.nii.gz",
    "sub-02_task-rest_bold.nii.gz",
    "sub-03_task-emo1_dir-AP_run-02_space-MNIPediatricAsym_cohort-2_desc-preproc_bold.nii.gz"
  )
  df <- extract_bids_info(files)
  expect_equal(df$subject, c("01", "02", "03"))
  expect_equal(df$session[1], "02")
  expect_true(is.na(df$session[2]))
  expect_true(is.na(df$session[3]))
  expect_equal(df$task, c("memory", "rest", "emo1"))
  expect_equal(df$run[1], "1")
  expect_equal(df$direction[3], "AP")
  expect_equal(df$space[3], "MNIPediatricAsym")
  expect_equal(df$cohort[3], "2")
  

  # check that the reversal to filenames is perfect
  bids_fnames <- construct_bids_filename(df)
  expect_setequal(bids_fnames, files)
  
  # if we add a field, does it propagate into filesname as expected?
  df$description <- "test"
  expect_setequal(
    construct_bids_filename(df),
    c(
      "sub-01_ses-02_task-memory_run-1_desc-test_bold.nii.gz",
      "sub-02_task-rest_desc-test_bold.nii.gz",
      "sub-03_task-emo1_dir-AP_run-02_space-MNIPediatricAsym_cohort-2_desc-test_bold.nii.gz"
    )
  )
})

