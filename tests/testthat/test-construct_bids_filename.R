test_that("construct_bids_regex avoids partial matches", {
  pattern <- construct_bids_regex("task:ridl desc:preproc suffix:bold")
  expect_false(grepl(pattern, "sub-01_task-ridlye_desc-preproc_bold.nii.gz"))
  expect_true(grepl(pattern, "sub-01_task-ridl_desc-preproc_bold.nii.gz"))
})

test_that("construct_bids_regex accepts missing suffix and ext", {
  pattern <- construct_bids_regex("task:ridl desc:preproc")
  expect_false(grepl(pattern, "sub-01_task-ridlye_desc-preproc_bold.nii.gz"))
  expect_true(grepl(pattern, "sub-01_task-ridl_desc-preproc_bold.nii.gz"))
  
  # turn off auto .nii.gz
  pattern <- construct_bids_regex("task:ridl desc:preproc", add_niigz_ext = FALSE)
  expect_true(grepl(pattern, "sub-01_task-ridl_desc-preproc_bold.tsv"))
})

test_that("construct_bids_regex accepts raw regex via regex: syntax", {
  pattern <- construct_bids_regex("regex:sub-\\d+_task-ridl_.*_bold")
  expect_equal(pattern, "sub-\\d+_task-ridl_.*_bold")
  expect_true(grepl(pattern, "sub-01_task-ridl_desc-preproc_bold.nii.gz"))
})


test_that("construct_bids_regex returns expected pattern for full specification", {
  spec <- "sub:01 ses:02 task:rest acq:highres run:1 space:MNI152NLin6Asym res:2 desc:preproc suffix:bold"
  pattern <- construct_bids_regex(spec)
  
  expected <- "^sub-01(_[^_]+)*_ses-02(_[^_]+)*_task-rest(_[^_]+)*_acq-highres(_[^_]+)*_run-1(_[^_]+)*_space-MNI152NLin6Asym(_[^_]+)*_res-2(_[^_]+)*_desc-preproc(_[^_]+)*_bold\\.nii(\\.gz)?$"
  expect_equal(pattern, expected)
  expect_true(grepl(pattern, "sub-01_ses-02_task-rest_acq-highres_run-1_space-MNI152NLin6Asym_res-2_desc-preproc_bold.nii.gz"))
  expect_true(grepl(pattern, "sub-01_interveningentity-100_ses-02_task-rest_acq-highres_run-1_space-MNI152NLin6Asym_res-2_desc-preproc_bold.nii.gz"))
  expect_false(grepl(pattern, "sub-01_ses-02_task-rest_acq-highres_run-1_space-MNI152NLin6Asym_res-2_desc-preproc_bold.tsv"))
})

test_that("construct_bids_regex works with non-NIfTI files when add_niigz_ext = FALSE", {
  spec <- "sub:02 suffix:events ext:.tsv"
  pattern <- construct_bids_regex(spec)
  
  expected <- "^sub-02(_[^_]+)*_events\\.tsv"
  expect_equal(pattern, expected)
})

test_that("construct_bids_regex returns pattern with generic suffix and nifti extension if unspecified", {
  spec <- "sub:03 task:memory"
  pattern <- construct_bids_regex(spec)
  
  expected <- "^sub-03(_[^_]+)*_task-memory(_[^_]+)*_.*\\.nii(\\.gz)?$"
  expect_equal(pattern, expected)
  expect_true(grepl(pattern, "sub-03_field-12_task-memory_another-10_suffix.nii.gz"))
  expect_true(grepl(pattern, "sub-03_field-12_task-memory_suffix.nii.gz"))
})

test_that("construct_bids_regex supports prefixed regex: passthrough", {
  spec <- "regex:^sub-[0-9]{2}_task-[a-z]+_bold\\.nii\\.gz$"
  pattern <- construct_bids_regex(spec)
  
  expect_equal(pattern, "^sub-[0-9]{2}_task-[a-z]+_bold\\.nii\\.gz$")
})

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
