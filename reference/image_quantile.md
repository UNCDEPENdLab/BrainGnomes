# Compute Quantiles from a 3D or 4D NIfTI Image

Computes one or more quantiles from a 3D or 4D NIfTI image. Optionally
applies a 3D brain mask and/or excludes zero-valued voxels. For 4D
images, the function pools over all timepoints.

## Arguments

- in_file:

  Path to the input 3D or 4D NIfTI image (.nii or .nii.gz).

- brain_mask:

  Optional path to a 3D NIfTI image used as a brain mask. Voxels with
  values \> 0.001 are retained. The mask must have the same spatial
  dimensions as the input image. If `R_NilValue`, no mask is used.

- quantiles:

  A numeric vector of probabilities in `[0, 1]` specifying which
  quantiles to compute (e.g., 0.5 for the median).

- exclude_zero:

  If `true`, zero-valued voxels in the image will be excluded from the
  quantile calculation.

## Value

A named numeric vector of quantiles. Names are formatted as percentage
strings (e.g., "50.00%").

## Details

- For 4D images, the mask (if used) is applied identically to all
  volumes.

- Quantile calculation uses partial sorting for performance (via
  `std::nth_element`).

- Throws an error if no voxels are valid after masking or zero
  exclusion.

## Examples

``` r
if (FALSE) { # \dontrun{
# Compute the median
image_quantile("bold.nii.gz", 0.5)

# With masking and zero exclusion
image_quantile("bold.nii.gz", "mask.nii.gz", c(0.25, 0.5, 0.75), exclude_zero=TRUE)
} # }
```
