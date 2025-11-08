# Regress confound time series from a 4D fMRI image

Uses FSL's `fsl_glm` to remove nuisance regressors from a 4D NIfTI
image. The residuals from the regression are re-centered by adding back
the temporal mean of the original image.

## Usage

``` r
confound_regression(
  in_file,
  out_file,
  to_regress = NULL,
  censor_file = NULL,
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

- to_regress:

  Path to a text file containing nuisance regressors (one column per
  regressor).

- censor_file:

  An optional censor file (1s indicate volumes to keep) that is used to

- overwrite:

  Logical; whether to overwrite the output file if it already exists.

- lg:

  Optional lgr object used for logging messages

- fsl_img:

  Optional Singularity image to execute FSL commands in a containerized
  environment.

## Value

Path to the residualized output NIfTI file.

## Details

The regressors are converted to FSL's binary matrix format using
`Text2Vest`. The residuals are computed using `fsl_glm`, and the
temporal mean of the original image is added back to preserve baseline
signal intensity.
