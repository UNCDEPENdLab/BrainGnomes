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

test_that("construct_bids_filename preserves full and abbreviated correlation entities", {
  entities <- list(
    subject = "01",
    rois = "DemoAtlas",
    suffix = "connectivity",
    ext = ".tsv"
  )

  expect_identical(
    construct_bids_filename(c(entities, list(correlation = "pearson"))),
    "sub-01_rois-DemoAtlas_cor-pearson_connectivity.tsv"
  )
  expect_identical(
    construct_bids_filename(c(entities, list(cor = "spearman"))),
    "sub-01_rois-DemoAtlas_cor-spearman_connectivity.tsv"
  )
})

test_that("bids_camelcase treats periods, hyphens, and underscores as word boundaries", {
  expect_identical(bids_camelcase("cor.shrink"), "corShrink")
  expect_identical(bids_camelcase("partial.cor_rank-based"), "partialCorRankBased")
  expect_identical(bids_camelcase("method...version_2"), "methodVersion2")
  expect_identical(bids_camelcase("pearson"), "pearson")
})

test_that("construct_bids_regex supports cohort-qualified spaces", {
  pattern <- construct_bids_regex(
    "task:emo1 dir:AP space:MNIPediatricAsym cohort:2 desc:preproc suffix:bold"
  )

  expect_true(grepl(
    pattern,
    "sub-03_task-emo1_dir-AP_run-02_space-MNIPediatricAsym_cohort-2_desc-preproc_bold.nii.gz"
  ))
  expect_false(grepl(
    pattern,
    "sub-03_task-emo1_dir-AP_run-02_space-MNIPediatricAsym_desc-preproc_bold.nii.gz"
  ))
})

test_that("get_fmriprep_outputs resolves cohort-qualified bold and non-spatial derivatives", {
  tmp_dir <- tempfile("fmriprep_outputs_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  in_file <- file.path(
    tmp_dir,
    "sub-03_task-emo1_dir-AP_run-02_space-MNIPediatricAsym_cohort-2_desc-preproc_bold.nii.gz"
  )
  brain_mask <- file.path(
    tmp_dir,
    "sub-03_task-emo1_dir-AP_run-02_space-MNIPediatricAsym_cohort-2_desc-brain_mask.nii.gz"
  )
  confounds <- file.path(
    tmp_dir,
    "sub-03_task-emo1_dir-AP_run-02_desc-confounds_timeseries.tsv"
  )
  melodic_mix <- file.path(
    tmp_dir,
    "sub-03_task-emo1_dir-AP_run-02_res-2_desc-melodic_mixing.tsv"
  )

  file.create(in_file)
  file.create(brain_mask)
  file.create(confounds)
  file.create(melodic_mix)

  outputs <- get_fmriprep_outputs(in_file)

  expect_path_identical(outputs$bold, in_file)
  expect_path_identical(outputs$brain_mask, brain_mask)
  expect_path_identical(outputs$confounds, confounds)
  expect_path_identical(outputs$melodic_mix, melodic_mix)
  expect_identical(outputs$prefix, "sub-03_task-emo1_dir-AP_run-02")
})
test_that("BIDS parsing requires complete entities and preserves resolution labels", {
  file <- "sub-01_task-rest_res-iso2mm_desc-preproc_bold.nii.gz"
  parsed <- extract_bids_info(file)
  expect_identical(parsed$resolution, "iso2mm")
  expect_identical(construct_bids_filename(parsed), file)
  parsed <- extract_bids_info("prefixsub-01_task-rest_extra-space-MNI_bold.nii.gz")
  expect_true(is.na(parsed$subject))
  expect_true(is.na(parsed$space))
  expect_error(extract_bids_info(NA_character_), "filenames")
  expect_error(extract_bids_info(""), "filenames")
})
