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
