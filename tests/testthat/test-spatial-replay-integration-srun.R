# Run inside an interactive allocation with the production AFNI/FSL runtime.
# Opt in with BG_RUN_SPATIAL_REPLAY_INTEGRATION=true and BG_TEST_FSL_IMAGE.
# This tests full-run outputs against distributed-volume replay, not an R mock.
test_that("spatial replay matches full SUSAN and Gaussian outputs on a compute node", {
  skip_if(!identical(tolower(Sys.getenv("BG_RUN_SPATIAL_REPLAY_INTEGRATION", "false")), "true"),
           "Set BG_RUN_SPATIAL_REPLAY_INTEGRATION=true inside a Slurm allocation.")
  skip_if(!nzchar(Sys.getenv("SLURM_JOB_ID")), "Run this integration test inside a Slurm allocation.")
  skip_on_os("windows")
  skip_if_not_installed("RNifti")
  for (program in c("3dBlurInMask", "3dmerge")) {
    skip_if(!nzchar(Sys.which(program)), paste("Missing production AFNI executable:", program))
  }
  fsl_image <- Sys.getenv("BG_TEST_FSL_IMAGE")
  skip_if(!nzchar(fsl_image) || !file.exists(fsl_image), "Set BG_TEST_FSL_IMAGE to the production FSL container.")
  scratch <- tempfile("spatial-replay-integration-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  set.seed(8642)
  dimensions <- c(12, 12, 8, 129)
  spacing <- c(4, 1.8, 1.8)
  kernel <- 1.5 * prod(spacing)^(1 / 3)
  mask <- array(0L, dimensions[1:3])
  mask[2:11, 2:11, 2:7] <- 1L
  values <- array(rnorm(prod(dimensions), 1000, 100), dimensions)
  for (volume in seq_len(dimensions[4])) {
    # Temporal intensity variation makes full-run brightness/mean parameters
    # matter: a temporal subset is not an interchangeable SUSAN reference.
    values[, , , volume] <- (values[, , , volume] + 8 * volume) * mask
  }
  image <- RNifti::asNifti(values)
  RNifti::pixdim(image) <- c(spacing, 2)
  header <- RNifti::niftiHeader(image)
  header$qform_code <- 1L
  header$sform_code <- 1L
  header$xyzt_units <- 10L
  header$srow_x <- c(spacing[1], 0, 0, 0)
  header$srow_y <- c(0, spacing[2], 0, 0)
  header$srow_z <- c(0, 0, spacing[3], 0)
  image <- RNifti::asNifti(values, reference = header)
  pre <- file.path(scratch, "pre.nii.gz")
  mask_file <- file.path(scratch, "mask.nii.gz")
  RNifti::writeNifti(image, pre, datatype = "float")
  RNifti::writeNifti(RNifti::asNifti(mask, reference = image), mask_file)
  for (context in c("susan", "gaussian_mask", "gaussian_nomask")) {
    post <- file.path(scratch, paste0(context, ".nii.gz"))
    used_mask <- context != "gaussian_nomask"
    smoother <- if (context == "susan") "susan" else "gaussian"
    if (smoother == "susan") {
      spatial_smooth(pre, post, kernel, brain_mask = mask_file,
                      overwrite = TRUE, fsl_img = fsl_image)
    } else {
      program <- if (used_mask) "3dBlurInMask" else "3dmerge"
      arguments <- if (used_mask) {
        c("-overwrite", "-input", pre, "-mask", mask_file, "-FWHM",
          format(kernel, digits = 17), "-prefix", post)
      } else {
        c("-overwrite", "-doall", "-1blur_fwhm", format(kernel, digits = 17),
          "-prefix", post, pre)
      }
      output <- system2(program, vapply(arguments, shQuote, character(1)),
                         stdout = TRUE, stderr = TRUE)
      expect_true(is.null(attr(output, "status")) || attr(output, "status") == 0L)
    }
    actual <- validate_spatial_smooth(pre, post, mask_file, kernel,
                                      smoother = smoother, used_mask = used_mask,
                                      validation_mode = "replay", fsl_img = fsl_image)
    expect_true(actual, info = paste(context, attr(actual, "message")))
    details <- attr(actual, "details")
    expect_identical(details$validation_method, "operator_replay")
    expect_equal(details$volumes_compared, 96L)
    expect_equal(details$total_volumes, 129L)
    expect_silent(assert_provenance_metadata(details))
    provenance <- file.path(scratch, paste0(context, ".json"))
    write_json_atomic(details, provenance)
    expect_lt(file.info(provenance)$size, 25000)
    wrong <- validate_spatial_smooth(pre, post, mask_file, 2 * kernel,
                                     smoother = smoother, used_mask = used_mask,
                                     validation_mode = "replay", fsl_img = fsl_image)
    expect_false(wrong)
    unchanged <- validate_spatial_smooth(pre, pre, mask_file, kernel,
                                         smoother = smoother, used_mask = used_mask,
                                         validation_mode = "replay", fsl_img = fsl_image)
    expect_false(unchanged)
  }
})
