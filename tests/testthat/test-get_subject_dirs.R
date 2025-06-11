
test_that("get_subject_dirs works with and without sessions", {
  # Setup temporary directory
  root <- tempfile("bids_test_")
  dir.create(root)

  # Create subject directories
  dir.create(file.path(root, "sub-001"))
  dir.create(file.path(root, "sub-002"))
  dir.create(file.path(root, "sub-003"))

  # Create session subdirectories for some subjects
  dir.create(file.path(root, "sub-001", "ses-01"))
  dir.create(file.path(root, "sub-001", "ses-02"))
  dir.create(file.path(root, "sub-002", "ses-01"))
  # sub-003 has no sessions

  # Test without sessions
  result_no_ses <- get_subject_dirs(
    root = root,
    sub_regex = "sub-[0-9]+",
    sub_id_match = "sub-([0-9]+)",
    full.names = FALSE
  )

  expect_equal(nrow(result_no_ses), 3)
  expect_true(all(is.na(result_no_ses$ses_id)))
  expect_true(all(is.na(result_no_ses$ses_dir)))
  expect_setequal(result_no_ses$sub_id, c("001", "002", "003"))

  # Test with sessions
  result_with_ses <- get_subject_dirs(
    root = root,
    sub_regex = "sub-[0-9]+",
    sub_id_match = "sub-([0-9]+)",
    ses_regex = "ses-[0-9]+",
    ses_id_match = "ses-([0-9]+)",
    full.names = FALSE
  )

  # Expected: sub-001 has 2 sessions, sub-002 has 1, sub-003 has none (but included)
  expect_equal(nrow(result_with_ses), 4)
  expect_setequal(result_with_ses$sub_id, c("001", "001", "002", "003"))
  expect_equal(result_with_ses$ses_id[result_with_ses$sub_id == "001"], c("01", "02"))
  expect_equal(result_with_ses$ses_id[result_with_ses$sub_id == "002"], "01")
  expect_true(is.na(result_with_ses$ses_id[result_with_ses$sub_id == "003"]))

  # Clean up
  unlink(root, recursive = TRUE)
})

