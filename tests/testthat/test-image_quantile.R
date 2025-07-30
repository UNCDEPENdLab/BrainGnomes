test_that("image_quantile computes correct quantiles in 3D and 4D with masking and zero exclusion", {
  skip_if_not_installed("RNifti")
  
  # --- Create 3D image with known values and some zeros
  arr3d <- array(1:125, dim = c(5, 5, 5))
  arr3d[1:10] <- 0  # insert zeros
  nifti3d <- RNifti::asNifti(arr3d)
  file3d <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(nifti3d, file3d)
  
  # --- Create mask (only include voxels > 60)
  mask_arr <- array(as.numeric(arr3d > 60), dim = dim(arr3d))
  mask_nifti <- RNifti::asNifti(mask_arr)
  mask_file <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(mask_nifti, mask_file)
  
  # --- Test all combinations of exclude_zero and use_mask for 3D
  combinations <- expand.grid(exclude_zero = c(TRUE, FALSE), use_mask = c(TRUE, FALSE))
  
  for (i in seq_len(nrow(combinations))) {
    ez <- combinations$exclude_zero[i]
    um <- combinations$use_mask[i]
    label <- paste0("3D - exclude_zero=", ez, ", use_mask=", um)
    
    expected <- arr3d
    if (um) expected <- expected[mask_arr == 1]
    if (ez) expected <- expected[expected != 0]
    
    q_val <- image_quantile(
      in_file = file3d,
      brain_mask = if (um) mask_file else NULL,
      quantiles = 0.5,
      exclude_zero = ez
    )
    
    expect_equal(as.numeric(q_val), as.numeric(quantile(expected, 0.5, names = FALSE)),
                 tolerance = 1e-6, label = label)
  }
  
  # --- Create 4D image (replicate 3D twice with an offset)
  arr4d <- array(c(arr3d, arr3d + 100), dim = c(5, 5, 5, 2))
  nifti4d <- RNifti::asNifti(arr4d)
  file4d <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(nifti4d, file4d)
  
  for (i in seq_len(nrow(combinations))) {
    ez <- combinations$exclude_zero[i]
    um <- combinations$use_mask[i]
    label <- paste0("4D - exclude_zero=", ez, ", use_mask=", um)
    
    expected <- as.vector(arr4d)
    if (um) {
      mask_vec <- rep(as.numeric(mask_arr > 0), 2)  # repeat for 2 volumes
      expected <- expected[mask_vec == 1]
    }
    if (ez) expected <- expected[expected != 0]
    
    q_val <- image_quantile(
      in_file = file4d,
      brain_mask = if (um) mask_file else NULL,
      quantiles = 0.5,
      exclude_zero = ez
    )
    
    expect_equal(as.numeric(q_val), as.numeric(quantile(expected, 0.5, names = FALSE)),
                 tolerance = 1e-6, label = label)
  }
})