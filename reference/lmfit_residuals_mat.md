# Residualize matrix time series using lm()

Applies the same confound regression logic as `lmfit_residuals_4d` but
operates on an in-memory matrix rather than a NIfTI image on disk. Each
column of `Y` is treated as a separate time series, and nuisance
regressors supplied in `X` are regressed out using
[`stats::lm.fit()`](https://rdrr.io/r/stats/lmfit.html) on the
uncensored timepoints.

## Usage

``` r
lmfit_residuals_mat(
  Y,
  X,
  include_rows = NULL,
  add_intercept = FALSE,
  preserve_mean = FALSE,
  set_mean = 0,
  regress_cols = NULL,
  exclusive = FALSE
)
```

## Arguments

- Y:

  Numeric matrix of time series to residualize (rows = timepoints,
  columns = signals).

- X:

  Numeric design matrix with the same number of rows as `Y`.

- include_rows:

  Optional logical vector marking rows to use during fitting.

- add_intercept:

  Logical; add an intercept column when the design lacks one.

- preserve_mean:

  Logical; keep the original mean of uncensored timepoints.

- set_mean:

  Numeric; shift residuals so every column has this mean (ignored when
  `preserve_mean = TRUE`).

- regress_cols:

  Optional integer vector (1-based) selecting columns of `X` to regress
  out.

- exclusive:

  Logical; if `TRUE`, the fit only uses columns listed in `regress_cols`
  (and the intercept, if present).

## Value

A numeric matrix of residuals with the same dimensions as `Y`.

## Details

Constant columns in the design matrix are removed automatically (with
the first intercept column preserved). Optional censoring allows the fit
to use only valid rows while predictions are generated for the full
series.

## See also

lmfit_residuals_4d
