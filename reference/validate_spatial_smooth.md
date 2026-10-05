# Validate spatial smoothing (classic FWHM pre vs post, calibration-corrected)

Measures the observed FWHM change using `estimate_classic_fwhm()` and
compares it to the calibration-predicted delta for the requested kernel
size. The calibration accounts for the fact that fMRI data are
non-Gaussian and the naive first-differences FWHM estimate has a
systematic bias that depends on smoother type and whether masking was
used. The calibrated preprocessing mode is selected together with the
calibration model. The production masked-SUSAN calibration stores and
enforces the estimator preparation selected by cross-dataset validation;
the estimator is not chosen at validation time.

## Usage

``` r
validate_spatial_smooth(
  pre_file,
  post_file,
  mask_file,
  fwhm_mm = NA_real_,
  smoother = "susan",
  used_mask = TRUE,
  input_mask = "none",
  tolerance_mm = NULL,
  preprocess = NULL,
  polydeg = NULL,
  demean = NULL,
  unif = NULL,
  max_volumes = 96L
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
  [`spatial_smooth()`](https://hallquistlab.github.io/BrainGnomes/reference/spatial_smooth.md))
  or `"gaussian"`.

- used_mask:

  Logical; whether a mask was used to calculate SUSAN's brightness
  threshold (default `TRUE`). SUSAN itself is not spatially restricted
  to this mask. For Gaussian calibration modes this instead
  distinguishes masked `3dBlurInMask` from unmasked `3dmerge`.

- input_mask:

  Character input-mask condition: `"none"`, `"fmriprep"`, `"template"`,
  or `"custom"`. This describes masking already applied to the BOLD
  before smoothing, independently of `used_mask`. A model from a
  different input-mask condition is reported as an extrapolation but
  cannot pass validation, because masking materially changes the
  calibrated gain.

- tolerance_mm:

  Tolerance in mm for `|observed_post - expected_post|`. `NULL` (the
  default) uses the program/mask-specific cross-validation tolerance
  stored with the calibration model.

- preprocess:

  Logical or `NULL`. `NULL` (the default) uses the preprocessing mode
  recorded by the selected calibration model. An explicit value must
  match that model; without `fwhm_mm`, `TRUE` applies diagnostic
  detrending.

- polydeg:

  Optional polynomial detrending degree. `NULL` uses the model.

- demean:

  Optional logical mean-removal setting. `NULL` uses the model.

- unif:

  Optional logical temporal-MAD scaling setting. `NULL` uses the model.

- max_volumes:

  Maximum number of timepoints used for validation. Timepoints are
  deterministically distributed over the complete run; shorter runs use
  every timepoint, and `Inf` uses all volumes. A calibrated model may
  require its stored cap and reject a different override.

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

Optional preprocessing can remove a low-order polynomial trend, the
temporal mean, and voxelwise temporal scale before the classic estimate.
It is retained for diagnostic and legacy calibration use, but it must
match the selected calibration. `preprocess = NULL` enforces that
model-specific choice. Requests outside the model's fitted domain and
explicit external support are reported as extrapolations and cannot pass
validation. Isotropic 2 mm images use separately fitted models for 3–8
mm kernels: masked-threshold SUSAN with unmasked, fMRIPrep-masked, or
template-masked input, and Gaussian smoothing with or without an
automask on otherwise unmasked input. These models require
`max_volumes = 96` and the calibrated estimator preparation. The three
internal cohorts were resampled to 2 mm; ten held-out subjects had
genuine 2 mm fMRIPrep outputs. Each 2 mm error limit is established from
internal cohort-transfer checks, capped at 1 mm, and verified on the
held-out subjects. Coarser-resolution models retain their original
limits. An image with no measurable smoothness increase fails even when
its error falls within the calibrated tolerance. Diagnostic SUSAN
without a threshold mask has no isotropic 2 mm calibration.
