# tests for miscellaneous utility functions

library(testthat)

# file_sans_ext removes common extensions

test_that("file_sans_ext removes image extensions", {
  expect_equal(file_sans_ext("test.nii.gz"), "test")
  expect_equal(file_sans_ext("path/data.nii"), "path/data")
  expect_true(is.na(file_sans_ext("file.txt2")))
})

# extract_bids_info extracts BIDS fields

test_that("extract_bids_info parses filename entities", {
  files <- c(
    "sub-01_ses-02_task-memory_run-1_bold.nii.gz",
    "sub-02_task-rest_bold.nii.gz"
  )
  df <- extract_bids_info(files)
  expect_equal(df$subject, c("01", "02"))
  expect_equal(df$session[1], "02")
  expect_true(is.na(df$session[2]))
  expect_equal(df$task, c("memory", "rest"))
  expect_equal(df$run[1], "1")
})

