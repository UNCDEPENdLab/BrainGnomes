# Normalize global intensity of a 4D fMRI image

Rescales the intensity of a 4D NIfTI image so that the median voxel
intensity within a brain mask matches a specified global target. This
operation is commonly used to standardize signal across runs or
subjects.

## Usage

``` r
intensity_normalize(
  in_file,
  out_file,
  brain_mask = NULL,
  global_median = 10000,
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

- brain_mask:

  Optional path to a brain mask NIfTI file. If `NULL`, the entire image
  is used.

- global_median:

  Target median intensity value to normalize to (default is 10000).

- overwrite:

  Logical; whether to overwrite the output file if it exists.

- lg:

  Optional lgr object used for logging messages

- fsl_img:

  Optional Singularity image to execute FSL commands in a containerized
  environment.

## Value

Path to the intensity-normalized output NIfTI file.

## Details

The 50th percentile intensity is estimated using `fslstats`, and the
input image is rescaled using `fslmaths -mul`. If the output file exists
and `overwrite = FALSE`, the step is skipped.
