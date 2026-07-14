test_that("intensity_reference_frames automatically combines available exclusions", {
  image_file <- tempfile(fileext = ".nii.gz")
  confounds_file <- tempfile(fileext = ".tsv")
  censor_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(image_file, confounds_file, censor_file)), add = TRUE)

  image <- RNifti::asNifti(array(1000, dim = c(2, 2, 2, 25)))
  RNifti::writeNifti(image, image_file)
  confounds <- data.frame(
    non_steady_state_outlier00 = c(1, rep(0, 24)),
    non_steady_state_outlier01 = c(0, 1, rep(0, 23)),
    framewise_displacement = rep(0, 25)
  )
  data.table::fwrite(confounds, confounds_file, sep = "\t")
  writeLines(as.character(c(1, 1, 0, rep(1, 22))), censor_file)

  include <- intensity_reference_frames(
    image_file, confounds_file = confounds_file,
    censor_file = censor_file
  )

  expect_length(include, 25L)
  expect_false(any(include[1:3]))
  expect_true(all(include[4:25]))
})

test_that("prepare_intensity_reference uses the fixed internal policy", {
  image_file <- tempfile(fileext = ".nii.gz")
  calibration_file <- tempfile(fileext = ".nii.gz")
  automask_file <- tempfile(fileext = ".nii.gz")
  core_file <- tempfile(fileext = ".nii.gz")
  sidecar_file <- tempfile(fileext = ".json")
  on.exit(unlink(c(image_file, calibration_file, automask_file, core_file, sidecar_file)), add = TRUE)

  image <- RNifti::asNifti(array(5000, dim = c(8, 8, 4, 25)))
  RNifti::writeNifti(image, image_file)
  RNifti::writeNifti(
    RNifti::asNifti(array(4000, dim = c(8, 8, 4, 25))),
    calibration_file
  )
  recorded <- new.env(parent = emptyenv())

  result <- with_mocked_bindings(
    prepare_intensity_reference(
      image_file, target = 10000, calibration_file = calibration_file,
      calibration_steps = c("apply_mask", "spatial_smooth"),
      automask_file = automask_file,
      core_file = core_file,
      sidecar_file = sidecar_file
    ),
    automask = function(img, outfile, ...) {
      recorded$automask_args <- list(...)
      RNifti::writeNifti(
        RNifti::asNifti(array(1L, dim = c(8, 8, 4))), outfile
      )
      outfile
    },
    derive_reference_core = function(img, candidate_mask, include_frames,
                                     baseline_method, baseline_trim,
                                     baseline_floor_fraction,
                                     relative_noise_mad_cutoff,
                                     spike_mad_cutoff, max_spike_fraction,
                                     min_valid_frames, outfile, ...) {
      recorded$core_args <- as.list(environment())
      core <- array(0L, dim = c(8, 8, 4))
      core[seq_len(200)] <- 1L
      baseline <- array(5000, dim = c(8, 8, 4))
      RNifti::writeNifti(RNifti::asNifti(core), outfile)
      list(
        core = RNifti::asNifti(core),
        baseline = RNifti::asNifti(baseline),
        thresholds = list(baseline_floor = 1000),
        counts = c(agreement = 256L, core = 200L)
      )
    }
  )

  expect_equal(result$reference_location, 4000)
  expect_equal(result$scale_factor, 2.5)
  expect_equal(result$core_fraction, 200 / 256)
  expect_false(recorded$automask_args$fill_holes)
  expect_equal(recorded$automask_args$dilate_steps, 0L)
  expect_equal(recorded$core_args$baseline_floor_fraction, 0.20)
  expect_equal(recorded$core_args$relative_noise_mad_cutoff, 5.0)
  expect_true(file.exists(core_file))
  expect_true(file.exists(sidecar_file))
  sidecar <- jsonlite::fromJSON(sidecar_file)
  expect_equal(sidecar$method, "automask_reference_core_v1")
  expect_equal(sidecar$calibration_stage, "post_spatial_pre_temporal")
  expect_equal(sidecar$calibration_steps, c("apply_mask", "spatial_smooth"))
  expect_equal(sidecar$reference_location, 4000)
  expect_length(result$include_frames, 25L)
})

test_that("prepare_intensity_reference explains denominator-guarded PSC policy", {
  image_file <- tempfile(fileext = ".nii.gz")
  automask_file <- tempfile(fileext = ".nii.gz")
  core_file <- tempfile(fileext = ".nii.gz")
  scale_file <- tempfile(fileext = ".nii.gz")
  sidecar_file <- tempfile(fileext = ".json")
  on.exit(unlink(c(
    image_file, automask_file, core_file, scale_file, sidecar_file
  )), add = TRUE)
  image <- RNifti::asNifti(array(1000, dim = c(8, 8, 4, 25)))
  RNifti::writeNifti(image, image_file)
  log_entries <- list()

  result <- with_mocked_bindings(
    prepare_intensity_reference(
      image_file, target = 10000, mode = "voxel_psc",
      automask_file = automask_file, core_file = core_file,
      scale_file = scale_file, sidecar_file = sidecar_file,
      lg = structure(list(), class = "test_logger")
    ),
    automask = function(img, outfile, ...) {
      RNifti::writeNifti(
        RNifti::asNifti(array(1L, dim = c(8, 8, 4))), outfile
      )
      outfile
    },
    derive_reference_core = function(img, candidate_mask, outfile, ...) {
      core <- RNifti::asNifti(array(1L, dim = c(8, 8, 4)))
      RNifti::writeNifti(core, outfile)
      list(
        core = core,
        thresholds = list(baseline_floor = 200),
        counts = c(agreement = 256L, core = 256L)
      )
    },
    to_log = function(lg, level, message, ...) {
      log_entries[[length(log_entries) + 1L]] <<- list(
        level = level,
        message = as.character(message)
      )
    }
  )

  sidecar <- jsonlite::fromJSON(sidecar_file)
  expect_equal(result$target, 100)
  expect_null(result$scale_factor)
  expect_true(file.exists(scale_file))
  expect_equal(sidecar$normalization_mode, "voxel_psc")
  expect_equal(sidecar$normalization_method, "guarded_voxel_psc_v1")
  expect_equal(sidecar$psc_guard$denominator_floor, 200)
  expect_match(sidecar$psc_guard$definition, "No observation clipping or voxel masking")
  expect_match(sidecar$psc_guard$categories$ordinary_psc, "exact robust local PSC")
  expect_match(sidecar$psc_guard$categories$denominator_floor, "not exact local PSC")
  expect_match(sidecar$psc_guard$categories$insufficient_frames_fallback, "not exact local PSC")
  expect_match(sidecar$psc_guard$categories$invalid_baseline_fallback, "not exact local PSC")
  info_messages <- vapply(
    Filter(function(x) identical(x$level, "info"), log_entries),
    `[[`, character(1), "message"
  )
  debug_messages <- vapply(
    Filter(function(x) identical(x$level, "debug"), log_entries),
    `[[`, character(1), "message"
  )
  expect_true(any(grepl(
    "within conservative automask.*denominator floor=.*fallback=",
    info_messages
  )))
  expect_true(any(grepl(
    "across full image grid.*denominator floor=.*fallback=",
    debug_messages
  )))
})

test_that("intensity normalization order brackets spatial and temporal steps", {
  expect_invisible(.validate_intensity_normalization_order(c(
    "apply_mask", "spatial_smooth", "intensity_normalize",
    "apply_aroma", "temporal_filter", "confound_regression"
  )))
  expect_error(
    .validate_intensity_normalization_order(c(
      "apply_mask", "apply_aroma", "intensity_normalize"
    )),
    "after apply_mask/spatial_smooth"
  )
  expect_error(
    .validate_intensity_normalization_order(c(
      "intensity_normalize", "spatial_smooth", "confound_regression"
    )),
    "after apply_mask/spatial_smooth"
  )
})

test_that("intensity_normalize only applies the frozen factor", {
  command <- NULL

  with_mocked_bindings(
    intensity_normalize("input.nii.gz", "output.nii.gz", scale_factor = 2.5),
    run_fsl_command = function(cmd, ...) {
      command <<- as.character(cmd)
      invisible(NULL)
    }
  )

  expect_match(command, "-mul 2.5", fixed = TRUE)
  expect_false(grepl("fslstats|quantile|median", command, ignore.case = TRUE))
})

test_that("denominator-guarded voxel PSC uses a 3D multiplier map", {
  scale_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(scale_file), add = TRUE)
  RNifti::writeNifti(
    RNifti::asNifti(array(0.1, dim = c(2, 2, 2))), scale_file
  )
  command <- NULL

  with_mocked_bindings(
    intensity_normalize(
      "input.nii.gz", "output.nii.gz", mode = "voxel_psc",
      scale_file = scale_file
    ),
    run_fsl_command = function(cmd, ...) {
      command <<- as.character(cmd)
      invisible(NULL)
    }
  )

  expect_match(command, paste0("-mul ", file_sans_ext(scale_file)), fixed = TRUE)
  expect_false(grepl("-mas|fslstats|median", command, ignore.case = TRUE))
})

test_that("legacy global_median resolves to the canonical target", {
  expect_equal(
    resolve_intensity_normalization_target(list(global_median = 10000)),
    10000
  )
  expect_equal(
    resolve_intensity_normalization_target(list(target = 12000, global_median = 10000)),
    12000
  )
  expect_equal(resolve_intensity_normalization_mode(list()), "run_scalar")
  expect_equal(
    resolve_intensity_normalization_target(
      list(mode = "voxel_psc", target = 10000)
    ),
    100
  )
})

test_that("postprocess config validation migrates global_median to target", {
  result <- validate_postprocess_config_single(
    list(
      intensity_normalize = list(
        enable = TRUE, global_median = 9000, prefix = "n"
      )
    ),
    cfg_name = "legacy", quiet = TRUE
  )

  expect_equal(result$postprocess$intensity_normalize$target, 9000)
  expect_equal(result$postprocess$intensity_normalize$mode, "run_scalar")
  expect_false("postprocess/intensity_normalize/target" %in% result$gaps)
})

test_that("voxel PSC config does not require a scalar target", {
  result <- validate_postprocess_config_single(
    list(
      intensity_normalize = list(
        enable = TRUE, mode = "voxel_psc", prefix = "n"
      )
    ),
    cfg_name = "psc", quiet = TRUE
  )

  expect_equal(result$postprocess$intensity_normalize$mode, "voxel_psc")
  expect_false("postprocess/intensity_normalize/target" %in% result$gaps)
})

test_that("interactive setup centers the run-scalar versus PSC decision", {
  answers <- list(TRUE, "voxel_psc")
  answer_index <- 0L
  prompts <- list()

  result <- with_mocked_bindings(
    setup_intensity_normalization(list()),
    prompt_input = function(instruct = NULL, prompt, ...) {
      answer_index <<- answer_index + 1L
      prompts[[length(prompts) + 1L]] <<- list(
        prompt = prompt,
        instruct = as.character(instruct)
      )
      answers[[answer_index]]
    }
  )

  expect_true(result$intensity_normalize$enable)
  expect_identical(result$intensity_normalize$mode, "voxel_psc")
  mode_prompt <- Filter(
    function(x) identical(x$prompt, "Intensity-normalization mode"),
    prompts
  )[[1L]]$instruct
  expect_match(mode_prompt, "run_scalar")
  expect_match(mode_prompt, "voxel_psc")
  expect_match(mode_prompt, "percent signal change")
  expect_false(grepl("denominator|floor|fallback", mode_prompt, ignore.case = TRUE))
})

test_that("derive_voxel_psc_scale guards denominators without masking voxels", {
  nt <- 25L
  x <- array(1000, dim = c(2, 2, 2, nt))
  x[2, 1, 1, ] <- 100
  x[1, 2, 1, ] <- 0
  x[2, 2, 1, 1:10] <- 1000
  x[2, 2, 1, 11:25] <- NA_real_
  include <- rep(TRUE, nt)
  include[1] <- FALSE
  qa <- RNifti::asNifti(array(1L, dim = c(2, 2, 2)))
  out_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(out_file), add = TRUE)

  result <- derive_voxel_psc_scale(
    RNifti::asNifti(x), reference_location = 1000,
    qa_mask = qa, include_frames = include, outfile = out_file
  )
  scale <- as.array(result$scale)

  expect_equal(scale[1, 1, 1], 0.1)
  expect_equal(scale[2, 1, 1], 0.5)
  expect_equal(scale[1, 2, 1], 0.1)
  expect_equal(scale[2, 2, 1], 0.1)
  expect_equal(unname(result$grid_counts), c(5L, 1L, 1L, 1L))
  expect_equal(sum(result$qa_mask_counts[-1]), 8L)
  expect_true(file.exists(out_file))
  expect_equal(dim(RNifti::readNifti(out_file)), c(2, 2, 2))
})
