# Create an automatic brain mask from a NIfTI image (Rcpp implementation)

This function mimics AFNI's `3dAutomask` logic to generate a binary
brain mask from a 3D or 4D NIfTI image. If the input is 4D, the time
dimension is collapsed by computing the mean image across frames. A
robust clip level is then estimated, voxels above threshold are
retained, and morphological clean-up steps are applied (largest
component, peels, hole-filling, optional erosion/dilation, optional
superior–inferior cutoff).

## Arguments

- image:

  A `RNifti::NiftiImage` object containing a 3D or 4D volume or file
  path to a NIfTI object whose mask should be calculated

- outfile:

  Optional file path where the resulting mask should be saved as a NIfTI
  file. If `""` (default), no file is written.

- clfrac:

  Fraction of the robust intensity range used to set the clip level for
  initial thresholding. Default is 0.5.

- NN:

  Neighborhood connectivity used for the largest connected component
  search and optional morphology. Options are `1` (faces only,
  6-neighbor), `2` (faces+edges, 18-neighbor), or `3`
  (faces+edges+corners, 26-neighbor). Default is 2.

- erode_steps:

  Number of additional erosions to apply after main mask construction.
  Default is 0 (none).

- dilate_steps:

  Number of additional dilations to apply after main mask construction.
  Default is 0 (none).

- SIhh:

  Distance in millimeters below the most superior voxel of the mask to
  retain. Voxels inferior to this cutoff are set to zero. Default is 0
  (no cutoff).

- peels:

  Number of "peel/unpeel" operations (erode then dilate with NN2
  neighborhood) applied to remove thin protuberances. Default is 1,
  matching AFNI `3dAutomask`.

- fill_holes:

  Logical; if `TRUE`, interior holes in the mask are filled using NN=1
  connectivity. Default is TRUE.

## Value

A 3D RNifti mask object with dimensions `c(nx, ny, nz)` and values
`0/1`. If `outfile` is provided, the mask is also written to disk as an
unsigned 8-bit (`DT_UINT8`) NIfTI file.

## Details

The processing pipeline is as follows:

1.  Collapse 4D inputs to a 3D mean volume.

2.  Compute robust clip threshold and apply initial thresholding.

3.  Retain only the largest connected component (NN as specified).

4.  Apply AFNI-style peel/unpeel (`peels` times, NN2).

5.  Optionally fill interior holes.

6.  Apply user-specified erosion/dilation steps (NN as specified).

7.  Apply optional superior–inferior cutoff (`SIhh`).

## See also

[`readNifti`](https://rdrr.io/pkg/RNifti/man/readNifti.html), AFNI
`3dAutomask`

## Examples

``` r
if (FALSE) { # \dontrun{
  library(RNifti)
  nii <- readNifti("sub-01_task-rest_bold.nii.gz")
  mask <- automask_rcpp(nii, outfile = "sub-01_mask.nii.gz")
} # }
```
