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

test_that("legacy global_median resolves to the canonical target", {
  expect_equal(
    resolve_intensity_normalization_target(list(global_median = 10000)),
    10000
  )
  expect_equal(
    resolve_intensity_normalization_target(list(target = 12000, global_median = 10000)),
    12000
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
  expect_false("postprocess/intensity_normalize/target" %in% result$gaps)
})
