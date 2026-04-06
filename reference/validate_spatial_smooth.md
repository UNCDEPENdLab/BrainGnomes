# Validate spatial smoothing (classic FWHM pre vs post, calibration-corrected)

Measures the observed FWHM change using `estimate_classic_fwhm()` and
compares it to the calibration-predicted delta for the requested kernel
size. The calibration accounts for the fact that fMRI data are
non-Gaussian and the naive first-differences FWHM estimate has a
systematic bias that depends on smoother type and whether masking was
used.

## Usage

``` r
validate_spatial_smooth(
  pre_file,
  post_file,
  mask_file,
  fwhm_mm = NA_real_,
  smoother = "susan",
  used_mask = TRUE,
  tolerance_mm = 0.5
)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `spatial_smooth`.

- post_file:

  Path to 4D BOLD after `spatial_smooth`.

- mask_file:

  3D mask (same space as BOLD).

- fwhm_mm:

  Requested smoothing kernel FWHM in mm (`cfg$spatial_smooth$fwhm_mm`).

- smoother:

  Character; `"susan"` (default, matches
  [`spatial_smooth()`](https://uncdependlab.github.io/BrainGnomes/reference/spatial_smooth.md))
  or `"gaussian"`.

- used_mask:

  Logical; whether smoothing was performed inside a mask (default
  `TRUE`).

- tolerance_mm:

  Tolerance in mm for `|observed_delta - expected_delta|` (default 0.5).

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details` (pre/post/delta/expected_delta/diff
FWHM mm).
