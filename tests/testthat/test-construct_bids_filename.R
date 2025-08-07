test_that("construct_bids_regex avoids partial matches", {
  pattern <- construct_bids_regex("task:ridl desc:preproc suffix:bold")
  expect_false(grepl(pattern, "sub-01_task-ridlye_desc-preproc_bold.nii.gz"))
  expect_true(grepl(pattern, "sub-01_task-ridl_desc-preproc_bold.nii.gz"))
})

test_that("construct_bids_regex accepts raw regex via regex: syntax", {
  pattern <- construct_bids_regex("regex:sub-\\d+_task-ridl_.*_bold")
  expect_equal(pattern, "sub-\\d+_task-ridl_.*_bold")
  expect_true(grepl(pattern, "sub-01_task-ridl_desc-preproc_bold.nii.gz"))

test_that("construct_bids_filename accepts abbreviated entities", {
  df <- data.frame(
    sub = "01",
    task = "rest",
    rec = "abc",
    desc = "preproc",
    suffix = "bold",
    ext = ".nii.gz",
    stringsAsFactors = FALSE
  )
  expect_equal(
    construct_bids_filename(df),
    "sub-01_task-rest_rec-abc_desc-preproc_bold.nii.gz"
  )
})
