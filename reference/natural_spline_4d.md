# Interpolate fMRI Time Series with Cubic Splines in a NIfTI File

This function performs voxelwise natural cubic spline interpolation over
the time dimension of a 4D NIfTI image. Timepoints to interpolate are
specified, and interpolation is applied independently to each voxel's
time series.

## Arguments

- infile:

  Character string. Path to the input 4D NIfTI file (e.g., BOLD fMRI
  data).

- t_interpolate:

  Integer vector (1-based). Specifies the timepoints (TRs) to
  interpolate. Timepoints outside the valid range `[1, T]` are ignored
  with a warning.

- edge_nn:

  Logical. If `TRUE`, extrapolated values at the edges of the time
  series are filled in using nearest-neighbor extrapolation instead of
  cubic splines.

- outfile:

  Character string (optional). If provided, the interpolated image will
  be written to this path. If omitted, the result is returned but not
  saved.

- internal:

  Logical. If FALSE (the default), an array of class "niftiImage",
  containing the image pixel or voxel values, will be returned. If TRUE,
  the return value will be an object of class "internalImage", which
  contains only minimal metadata about the image. Either way, the return
  value has an attribute which points to a C data structure containing
  the full image. Cf.
  [`RNifti::readNifti`](https://rdrr.io/pkg/RNifti/man/readNifti.html)

## Value

A `niftiImage` object with the same dimensions and metadata as the
input, with interpolated values inserted at the specified timepoints.

## Details

The function reads the NIfTI image from disk, performs interpolation in
memory, and optionally writes the result back to a new NIfTI file.

The interpolation is voxelwise and assumes column-major order. If a
voxel time series has fewer than three valid (non-interpolated)
timepoints, or is constant across time, it is skipped. Linear
extrapolation is used for timepoints outside the valid range if
`edge_nn = FALSE`, matching R's `splinefun` approach with natural
splines. If `edge_nn = TRUE`, nearest-neighbor extrapolation is used for
interpolation timepoints at the beginning or end of the timeseries,
potentially reducing extreme values in extrapolation.

This implementation uses RNifti's C++ API (`NiftiImage`,
`NiftiImageData`) for efficient memory access and file handling,
allowing the function to operate directly on NIfTI files.

## Examples

``` r
if (FALSE) { # \dontrun{
out_img <- natural_spline_4d(
  infile = "bold.nii.gz",
  t_interpolate = 91:95,
  outfile = "bold_interpolated.nii.gz",
  edge_nn = TRUE
)
} # }
```
