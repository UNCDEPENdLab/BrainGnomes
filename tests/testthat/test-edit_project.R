test_that("edit_project toggles BIDS Validation enable", {
  scfg <- structure(list(
    bids_validation = list(enable = TRUE, outfile = "out.html")
  ), class = "bg_project_cfg")

  sel_mock <- local({
    i <- 0L
    function(choices, title = NULL, multiple = FALSE) {
      i <<- i + 1L
      if (i == 1L) {
        "BIDS Validation"
      } else if (i == 2L) {
        choices["enable"]
      } else {
        "Quit"
      }
    }
  })

  res <- with_mocked_bindings(
    edit_project(scfg),
    select_list_safe = sel_mock,
    prompt_input = function(...) FALSE,
    save_project_config = function(scfg) scfg
  )

  expect_false(res$bids_validation$enable)
})

test_that("edit_project toggles postprocess enable", {
  tmpf <- tempfile()
  file.create(tmpf)
  scfg <- structure(list(
    postprocess = list(enable = FALSE),
    metadata = list(
      fmriprep_directory = tempdir(),
      postproc_directory = tempdir()
    ),
    compute_environment = list(fsl_container = tmpf)
  ), class = "bg_project_cfg")

  sel_mock <- local({
    i <- 0L
    function(choices, title = NULL, multiple = FALSE) {
      i <<- i + 1L
      if (i == 1L) {
        "Postprocessing"
      } else {
        "Quit"
      }
    }
  })

  res <- with_mocked_bindings(
    edit_project(scfg),
    select_list_safe = sel_mock,
    prompt_input = function(...) TRUE,
    manage_postprocess_streams = function(scfg, allow_empty = TRUE) scfg,
    save_project_config = function(scfg) scfg
  )

  expect_true(res$postprocess$enable)
})

