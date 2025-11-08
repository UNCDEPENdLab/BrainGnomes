# Apply SUSAN-based spatial smoothing to a 4D fMRI image

Performs spatial smoothing using FSL's `susan` algorithm, which adapts
smoothing based on local image intensity structure. A smoothing kernel
defined by `fwhm_mm` is applied and the extents mask is re-applied
post-smoothing to constrain the result to original data extents.

## Usage

``` r
spatial_smooth(
  in_file,
  out_file,
  fwhm_mm = 6,
  brain_mask = NULL,
  overwrite = FALSE,
  lg = NULL,
  fsl_img = NULL
)
```

## Arguments

- in_file:

  Path to the input 4D NIfTI file.

- out_file:

  The full path for the file output by this step

- fwhm_mm:

  Full-width at half-maximum (FWHM) of the Gaussian kernel in
  millimeters.

- brain_mask:

  Optional brain mask to guide intensity thresholding. If `NULL`, the
  whole image is used.

- overwrite:

  Logical; whether to overwrite the output file if it already exists.

- lg:

  Optional lgr object used for logging messages

- fsl_img:

  Optional Singularity image to execute FSL commands in a containerized
  environment.

## Value

Path to the spatially smoothed output NIfTI file.

## Details

The SUSAN threshold is computed based on the 2nd and 50th percentiles of
intensity values. An extents mask is created prior to smoothing to
ensure no new voxels are introduced in the output.
