test_that("ensure_aroma_output_space adds default + aroma space when output_spaces blank", {
  scfg <- structure(list(
    aroma = list(enable = TRUE),
    fmriprep = list(output_spaces = "")
  ), class = "bg_project_cfg")

  out <- ensure_aroma_output_space(scfg, require_aroma = TRUE, verbose = FALSE)

  expect_equal(out$fmriprep$output_spaces, "MNI152NLin2009cAsym MNI152NLin6Asym:res-2")
  expect_true(isTRUE(out$fmriprep$auto_added_aroma_space))
})

test_that("ensure_aroma_output_space appends aroma space to existing spaces", {
  scfg <- structure(list(
    aroma = list(enable = TRUE),
    fmriprep = list(output_spaces = "MNI152NLin2009cAsym")
  ), class = "bg_project_cfg")

  out <- ensure_aroma_output_space(scfg, require_aroma = TRUE, verbose = FALSE)

  expect_equal(out$fmriprep$output_spaces, "MNI152NLin2009cAsym MNI152NLin6Asym:res-2")
  expect_true(isTRUE(out$fmriprep$auto_added_aroma_space))
})

test_that("ensure_aroma_output_space is a no-op when AROMA not required", {
  scfg <- structure(list(
    aroma = list(enable = FALSE),
    fmriprep = list(output_spaces = "")
  ), class = "bg_project_cfg")

  out <- ensure_aroma_output_space(scfg, require_aroma = FALSE, verbose = FALSE)

  expect_identical(out, scfg)
})
