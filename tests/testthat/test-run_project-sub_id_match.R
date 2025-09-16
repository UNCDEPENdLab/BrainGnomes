test_that("run_project uses custom sub_id_match for dicom dirs", {
  # Setup temporary directories
  tmp <- tempfile("proj_")
  dir.create(tmp)
  dicom_dir <- file.path(tmp, "dicoms"); dir.create(dicom_dir)
  bids_dir <- tempfile("bids_"); dir.create(bids_dir)
  fmriprep_dir <- tempfile("fmriprep_"); dir.create(fmriprep_dir)
  postproc_dir <- file.path(tmp, "postproc"); dir.create(postproc_dir)
  mriqc_dir <- file.path(tmp, "mriqc"); dir.create(mriqc_dir)
  log_dir <- file.path(tmp, "logs"); dir.create(log_dir)
  work_dir <- file.path(tmp, "work"); dir.create(work_dir)

  # Create a subject directory with letters and numbers
  dir.create(file.path(dicom_dir, "SB1134"))

  # Dummy files required by run_project
  heuristic_file <- file.path(tmp, "heuristic.py"); file.create(heuristic_file)
  container_file <- file.path(tmp, "heudiconv.sif"); file.create(container_file)

  scfg <- list(
    metadata = list(
      project_name = "test",
      project_directory = tmp,
      dicom_directory = dicom_dir,
      bids_directory = bids_dir,
      fmriprep_directory = fmriprep_dir,
      postproc_directory = postproc_dir,
      mriqc_directory = mriqc_dir,
      log_directory = log_dir,
      scratch_directory = work_dir
    ),
    bids_conversion = list(
      enable = TRUE,
      sub_regex = "SBJ*[0-9]+",
      sub_id_match = "(.+)",
      ses_regex = NA_character_,
      ses_id_match = NA_character_,
      heuristic_file = heuristic_file
    ),
    compute_environment = list(
      heudiconv_container = container_file,
      scheduler = "sh"
    )
  )
  class(scfg) <- "bg_project_cfg"

  captured <- NULL
  ns <- asNamespace("BrainGnomes")
  orig <- get("process_subject", envir = ns)
  unlockBinding("process_subject", ns)
  assign("process_subject", function(scfg, sub_cfg, steps, postprocess_streams = NULL, extract_streams = NULL, parent_ids = NULL) {
    captured <<- sub_cfg
    TRUE
  }, envir = ns)
  lockBinding("process_subject", ns)
  on.exit({
    unlockBinding("process_subject", ns)
    assign("process_subject", orig, envir = ns)
    lockBinding("process_subject", ns)
  })

  run_project(scfg, steps = "bids_conversion", debug = TRUE)

  expect_equal(captured$sub_id, "SB1134")
})
