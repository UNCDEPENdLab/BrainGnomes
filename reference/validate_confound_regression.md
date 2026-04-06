# Validate confound regression (voxel-sampling replay vs output)

Samples ~100 voxels and replays the regression via `lmfit_residuals_mat`
(`preserve_mean = TRUE`); passes if max abs diff \< 0.05.

## Usage

``` r
validate_confound_regression(
  pre_file,
  post_file,
  to_regress,
  censor_file = NULL,
  n_sample = 100L
)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `confound_regression`.

- post_file:

  Path to 4D BOLD after `confound_regression`.

- to_regress:

  Regressor TSV (no header).

- censor_file:

  Optional censor file (1 = keep TR).

- n_sample:

  Number of voxels to sample (default 100).

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details` (`max_abs_diff`).
