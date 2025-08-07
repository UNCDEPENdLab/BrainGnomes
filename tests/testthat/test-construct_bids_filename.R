test_that("construct_bids_regex avoids partial matches", {
  pattern <- construct_bids_regex("task:ridl desc:preproc suffix:bold")
  expect_false(grepl(pattern, "sub-01_task-ridlye_desc-preproc_bold.nii.gz"))
  expect_true(grepl(pattern, "sub-01_task-ridl_desc-preproc_bold.nii.gz"))
})
