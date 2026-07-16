# Validate spatial smoothing (classic FWHM pre vs post, calibration-corrected)

Measures the observed FWHM change using `estimate_classic_fwhm()` and
compares it to the calibration-predicted delta for the requested kernel
size. The calibration accounts for the fact that fMRI data are
non-Gaussian and the naive first-differences FWHM estimate has a
systematic bias that depends on smoother type and whether masking was
used. Before estimating FWHM, voxel time series are polynomial-detrended
and MAD-normalized to match the preprocessing used by the calibration
script.

## Usage

``` r
validate_spatial_smooth(
  pre_file,
  post_file,
  mask_file,
  fwhm_mm = NA_real_,
  smoother = "susan",
  used_mask = TRUE,
  tolerance_mm = NULL,
  preprocess = TRUE,
  polydeg = 3L,
  demean = TRUE,
  unif = TRUE,
  max_volumes = 300L
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

  Logical; whether a mask was used to calculate SUSAN's brightness
  threshold (default `TRUE`). SUSAN itself is not spatially restricted
  to this mask. For Gaussian calibration modes this instead
  distinguishes masked `3dBlurInMask` from unmasked `3dmerge`.

- tolerance_mm:

  Tolerance in mm for `|observed_post - expected_post|`. `NULL` (the
  default) uses the program/mask-specific cross-validation tolerance
  stored with the calibration model.

- preprocess:

  Logical; if `TRUE`, apply calibration-matched preprocessing.

- polydeg:

  Polynomial detrending degree used when `preprocess = TRUE`.

- demean:

  Logical; include mean removal in preprocessing.

- unif:

  Logical; normalize each voxel by temporal MAD in preprocessing.

- max_volumes:

  Maximum number of timepoints used for validation. The first
  `max_volumes` are used; `Inf` uses all volumes.

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details` (pre/post/delta/expected_delta/diff
FWHM mm).

## Details

Classic first-difference FWHM is used intentionally as a local
gradient-variance statistic. A mixed Gaussian-plus-exponential ACF can
describe fMRI's longer spatial tail, but its scalar half-height FWHM
does not obey Gaussian quadrature when a Gaussian kernel is added: the
fitted core and tail change differently. The real-BOLD calibration
therefore absorbs the non-Gaussian core behavior without treating the
full ACF as Gaussian.

The preprocessing is important because the classic first-difference FWHM
estimator is sensitive to non-smooth sources of spatial variance that
are not the target of the smoothing check. Slow voxelwise drifts, mean
offsets, and large between-voxel variance differences can inflate or
deflate the ratio of spatial-difference variance to total variance,
making the measured FWHM change disagree with the calibration even when
smoothing was applied correctly. The validation therefore mirrors
`local/smoothness_checks/3dSmoothnessChange.R`: it removes a low-order
polynomial trend from each in-mask voxel time series, removes the mean
when `demean = TRUE`, and scales each voxel by its temporal MAD when
`unif = TRUE`. This puts pre/post data on the same residualized,
variance- normalized scale used to derive the empirical calibration
coefficients.
