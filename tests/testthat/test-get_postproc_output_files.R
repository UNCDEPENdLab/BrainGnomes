test_that("get_postproc_output_files targets postprocessed outputs", {
  tmp_dir <- tempfile("bg_postproc_")
  dir.create(tmp_dir, recursive = TRUE)

  postproc <- file.path(
    tmp_dir,
    "sub-01_task-impressions_space-MNI152NLin2009cAsym_desc-postproc_clean_bold.nii.gz"
  )
  preproc <- file.path(
    tmp_dir,
    "sub-01_task-impressions_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz"
  )
  file.create(postproc)
  file.create(preproc)

  rx_with_desc <- "desc:preproc task:impressions space:MNI152NLin2009cAsym suffix:bold"
  res_with_desc <- get_postproc_output_files(tmp_dir, rx_with_desc, "postproc_clean")
  expect_true(postproc %in% res_with_desc)
  expect_false(preproc %in% res_with_desc)

  rx_no_desc <- "task:impressions space:MNI152NLin2009cAsym suffix:bold"
  res_no_desc <- get_postproc_output_files(tmp_dir, rx_no_desc, "postproc_clean")
  expect_true(postproc %in% res_no_desc)
  expect_false(preproc %in% res_no_desc)
})
