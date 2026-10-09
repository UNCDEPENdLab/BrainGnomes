test_that("native imaging helpers omit RNifti reference-count diagnostics", {
  input <- tempfile(fileext = ".nii.gz")
  output <- tempfile(fileext = ".nii.gz")
  removed <- tempfile(fileext = ".nii.gz")
  withr::defer(unlink(c(input, output, removed)))
  signal <- RNifti::asNifti(array(rep(100 + seq_len(10), each = 64L), c(4, 4, 4, 10)))
  mask <- RNifti::asNifti(array(1, c(4, 4, 4)))
  RNifti::writeNifti(signal, input, datatype = "INT16")
  messages <- capture.output({
    interpolated <- natural_spline_4d(input, 3L, outfile = output)
    residuals <- lmfit_residuals_4d(input, matrix(1, 10, 1), outfile = output)
    filtered <- butterworth_filter_cpp(input, 1, 1, padlen = 0L, outfile = output)
    quantile <- image_quantile(input)
    remove_nifti_volumes(input, 3L, removed)
    brain_mask <- automask(signal, peels = 0L)
    location <- measure_reference_location(signal, mask, min_valid_frames = 5L)
    psc <- derive_voxel_psc_scale(signal, location$reference_location, mask,
      min_valid_frames = 5L)
    rm(interpolated, residuals, filtered, quantile, brain_mask, location, psc)
    invisible(gc())
  })
  expect_false(any(grepl(
    "Acquiring pointer|Releasing pointer|Releasing untracked object|Creating NiftiImage|Converting v[12] image",
    messages
  )))
})
