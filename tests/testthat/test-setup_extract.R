test_that("interactive extraction setup retains masks for existing and new streams", {
  skip_if_not_installed("yaml")

  tmp <- tempfile("setup-extract-mask-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  mask_path <- file.path(tmp, "selected-mask.nii.gz")
  file.create(mask_path)

  responses <- c(mask_path, "fresh", mask_path)
  response_index <- 0L
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) {
      response_index <<- response_index + 1L
      responses[[response_index]]
    },
    .package = "BrainGnomes"
  )

  existing <- structure(list(
    metadata = list(project_directory = tmp),
    extract_rois = list(
      enable = TRUE,
      demo = list(mask_file = "old-mask.nii.gz")
    )
  ), class = "bg_project_cfg")

  updated_existing <- setup_extract_stream(
    existing,
    fields = "extract_rois/demo/mask_file",
    stream_name = "demo"
  )
  expect_identical(updated_existing$extract_rois$demo$mask_file, mask_path)
  existing_yaml <- file.path(tmp, "existing.yaml")
  save_project_config(updated_existing, file = existing_yaml)
  expect_identical(
    yaml::read_yaml(existing_yaml)$extract_rois$demo$mask_file,
    basename(mask_path)
  )

  new_stream <- structure(list(
    metadata = list(project_directory = tmp),
    extract_rois = list(enable = TRUE)
  ), class = "bg_project_cfg")

  updated_new <- setup_extract_stream(
    new_stream,
    fields = "extract_rois/fresh/mask_file",
    stream_name = "fresh"
  )
  expect_identical(updated_new$extract_rois$fresh$mask_file, mask_path)
  new_yaml <- file.path(tmp, "new.yaml")
  save_project_config(updated_new, file = new_yaml)
  expect_identical(
    yaml::read_yaml(new_yaml)$extract_rois$fresh$mask_file,
    basename(mask_path)
  )
  expect_identical(response_index, 3L)
})

test_that("interactive extraction setup normalizes intentionally empty masks", {
  response <- NA_character_
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) response,
    .package = "BrainGnomes"
  )

  cfg <- structure(list(
    extract_rois = list(
      enable = TRUE,
      demo = list(mask_file = "previous-mask.nii.gz")
    )
  ), class = "bg_project_cfg")

  for (empty_response in list(NA_character_, "", "   ", ".na.character")) {
    response <- empty_response
    cleared <- setup_extract_stream(
      cfg,
      fields = "extract_rois/demo/mask_file",
      stream_name = "demo"
    )
    expect_null(cleared$extract_rois$demo$mask_file)
  }
})

test_that("interactive extraction setup records ROI diagnostics preference", {
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) TRUE,
    .package = "BrainGnomes"
  )

  cfg <- structure(list(
    extract_rois = list(enable = TRUE, demo = list(save_diagnostics = FALSE))
  ), class = "bg_project_cfg")

  updated <- setup_extract_stream(
    cfg,
    fields = "extract_rois/demo/save_diagnostics",
    stream_name = "demo"
  )
  expect_true(updated$extract_rois$demo$save_diagnostics)
})

test_that("extraction validation defaults and checks ROI diagnostics preference", {
  tmp <- tempfile("validate-extract-diagnostics-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  atlas <- file.path(tmp, "atlas.nii.gz")
  file.create(atlas)

  config <- list(
    input_streams = "clean",
    atlases = atlas,
    roi_reduce = "mean",
    rtoz = FALSE,
    min_vox_per_roi = 1L
  )
  defaulted <- validate_extract_config_single(config, quiet = TRUE)
  expect_false(defaulted$extract_rois$save_diagnostics)
  expect_false("extract_rois/save_diagnostics" %in% defaulted$gaps)

  config$save_diagnostics <- "yes"
  invalid <- validate_extract_config_single(config, quiet = TRUE)
  expect_null(invalid$extract_rois$save_diagnostics)
  expect_true("extract_rois/save_diagnostics" %in% invalid$gaps)
})

test_that("interactive extraction setup records atlas resampling and space", {
  responses <- list(TRUE, "MNI152NLin2009cAsym")
  response_index <- 0L
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) {
      response_index <<- response_index + 1L
      responses[[response_index]]
    },
    .package = "BrainGnomes"
  )

  cfg <- structure(list(
    extract_rois = list(enable = TRUE, demo = list())
  ), class = "bg_project_cfg")
  updated <- setup_extract_stream(
    cfg,
    fields = "extract_rois/demo/allow_atlas_resampling",
    stream_name = "demo"
  )

  expect_true(updated$extract_rois$demo$allow_atlas_resampling)
  expect_identical(
    updated$extract_rois$demo$atlas_space,
    "MNI152NLin2009cAsym"
  )
  expect_identical(response_index, 2L)
})

test_that("interactive extraction setup does not request an inferable atlas space", {
  response_count <- 0L
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) {
      response_count <<- response_count + 1L
      if (response_count > 1L) stop("atlas_space should be inferred")
      TRUE
    },
    .package = "BrainGnomes"
  )

  cfg <- structure(list(
    extract_rois = list(enable = TRUE, demo = list(
      atlases = "space-MNI152NLin2009cAsym_atlas-Demo_dseg.nii.gz"
    ))
  ), class = "bg_project_cfg")
  updated <- setup_extract_stream(
    cfg,
    fields = "extract_rois/demo/allow_atlas_resampling",
    stream_name = "demo"
  )

  expect_true(updated$extract_rois$demo$allow_atlas_resampling)
  expect_null(updated$extract_rois$demo$atlas_space)
  expect_identical(response_count, 1L)
})

test_that("extraction validation defaults and checks atlas resampling", {
  tmp <- tempfile("validate-atlas-resampling-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  atlas <- file.path(tmp, "atlas.nii.gz")
  file.create(atlas)
  config <- list(
    input_streams = "clean",
    atlases = atlas,
    roi_reduce = "mean",
    rtoz = FALSE,
    min_vox_per_roi = 1L
  )

  defaulted <- validate_extract_config_single(config, quiet = TRUE)
  expect_false(defaulted$extract_rois$allow_atlas_resampling)
  expect_false("extract_rois/allow_atlas_resampling" %in% defaulted$gaps)

  config$allow_atlas_resampling <- TRUE
  missing_space <- validate_extract_config_single(config, quiet = TRUE)
  expect_true("extract_rois/atlas_space" %in% missing_space$gaps)

  config$atlas_space <- "MNI152NLin2009cAsym"
  valid <- validate_extract_config_single(config, quiet = TRUE)
  expect_true(valid$extract_rois$allow_atlas_resampling)
  expect_identical(valid$extract_rois$atlas_space, config$atlas_space)
  expect_false("extract_rois/atlas_space" %in% valid$gaps)

  bids_atlas <- file.path(
    tmp,
    "space-MNI152NLin2009cAsym_atlas-Demo_dseg.nii.gz"
  )
  file.create(bids_atlas)
  inferred_config <- config
  inferred_config$atlases <- bids_atlas
  inferred_config$atlas_space <- NULL
  inferred <- validate_extract_config_single(inferred_config, quiet = TRUE)
  expect_true(inferred$extract_rois$allow_atlas_resampling)
  expect_null(inferred$extract_rois$atlas_space)
  expect_false("extract_rois/atlas_space" %in% inferred$gaps)

  inferred_config$atlas_space <- "MNI152NLin6Asym"
  conflict <- validate_extract_config_single(inferred_config, quiet = TRUE)
  expect_null(conflict$extract_rois$atlas_space)
  expect_true("extract_rois/atlas_space" %in% conflict$gaps)

  config$allow_atlas_resampling <- "yes"
  invalid_flag <- validate_extract_config_single(config, quiet = TRUE)
  expect_null(invalid_flag$extract_rois$allow_atlas_resampling)
  expect_true(
    "extract_rois/allow_atlas_resampling" %in% invalid_flag$gaps
  )
})
