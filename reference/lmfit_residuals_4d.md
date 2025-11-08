# Apply Confound Regression to 4D fMRI Data Using Voxelwise Linear Models

This function performs voxelwise confound regression by fitting a linear
model to a subset of timepoints (e.g., uncensored volumes) for each
voxel in a 4D NIfTI image. The fitted model is then applied to all
timepoints to obtain predicted values, and the residuals are returned as
a cleaned 4D image.

## Arguments

- infile:

  Path to a 4D NIfTI image file to denoise (e.g., functional data).

- X:

  A numeric matrix where rows correspond to timepoints and columns to
  nuisance regressors. Typically includes motion parameters,
  physiological noise, etc.

- include_rows:

  Optional logical vector identifying the timepoints used when
  estimating the model (e.g., uncensored volumes). If supplied it must
  have length `nrow(X)`; when `NULL`, all timepoints are used.

- add_intercept:

  Logical; if `TRUE`, adds an intercept column to the design matrix
  unless one is already present.

- outfile:

  Optional path to write the output residuals image. If empty, no file
  is written.

- internal:

  Logical; if `TRUE`, returns an internal RNifti pointer. Otherwise
  returns an R array.

- preserve_mean:

  Logical; if `TRUE`, constant time series will be left unchanged (not
  demeaned or recentered).

- set_mean:

  Optional numeric value; if specified, all residual time series will be
  shifted to have this mean (default is 0). Cannot be used in
  combination with `preserve_mean = TRUE`.

- regress_cols:

  Optional integer vector (1-based) indicating which columns of `X`
  should be regressed out. When omitted, all non-constant columns are
  removed unless `exclusive = TRUE`.

- exclusive:

  Logical; if `TRUE`, only the columns listed in `regress_cols` (and an
  intercept, if present) are used to estimate the model. This allows for
  partial regression that preserves other effects.

## Value

A residualized 4D NIfTI image, either as an in-memory array or RNifti
object (if `internal = TRUE`).

## Details

This approach mirrors the strategy used in the XCP-D pipeline, where
nuisance regressors are fit only to valid (non-censored) timepoints to
prevent bias, but the resulting model is applied to the full dataset
including censored timepoints (e.g., for continuity or interpolation).

Constant columns in the design matrix are automatically removed. If an
intercept column (all ones) is present, it is preserved. If
`add_intercept = TRUE`, an intercept column will be added (if not
present).

## References

Ciric, R. et al. (2018). Mitigating head motion artifact in functional
connectivity MRI. *Nature Protocols*.
https://xcp-d.readthedocs.io/en/latest/
https://dannyjameswilliams.co.uk/portfolios/sc2/rcpp/

## Examples

``` r
if (FALSE) { # \dontrun{
  X <- cbind(1, motion_params, compcor)
  residual_img <- lmfit_residuals_4d(
    infile = "func.nii.gz",
    X = X,
    include_rows = !censor_vector,
    add_intercept = TRUE,
    outfile = "residual.nii.gz",
    set_mean = 1000
  )
} # }
```
