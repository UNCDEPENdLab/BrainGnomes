# Derive a conservative functional reference core from 4D BOLD data

Refines a functional candidate mask by removing voxels with an invalid
or very low robust baseline, extreme robust relative temporal noise, or
an excessive fraction of abrupt signal changes. An optional second mask
can be supplied as a subtractive constraint. This function never dilates
a mask or fills holes.

## Usage

``` r
derive_reference_core(
  img,
  candidate_mask,
  constraint_mask = NULL,
  include_frames = NULL,
  baseline_method = "trimmed_mean",
  baseline_trim = 0.1,
  baseline_floor_fraction = 0.2,
  relative_noise_mad_cutoff = 5,
  spike_mad_cutoff = 6,
  max_spike_fraction = 0.05,
  min_valid_frames = 20L,
  affine_tolerance = 1e-05,
  outfile = ""
)
```

## Arguments

- img:

  A 4D `RNifti::NiftiImage` object or path to a 4D BOLD NIfTI.
  Intensities must retain a positive baseline; do not pass demeaned or
  residualized data whose voxelwise means have been removed.

- candidate_mask:

  Required 3D functional candidate mask, normally from
  [`automask()`](https://uncdependlab.github.io/BrainGnomes/reference/automask.md)
  applied to the original positive-scale BOLD data with
  `fill_holes = FALSE` and `dilate_steps = 0`.

- constraint_mask:

  Optional 3D mask used only as a subtractive constraint. If `NULL` (the
  default), the candidate mask alone defines the population subject to
  temporal quality refinement.

- include_frames:

  Optional logical vector with one value per volume. `TRUE` marks
  steady-state, uncensored volumes eligible for estimation. Missing
  values are not allowed. If `NULL`, all volumes are eligible.

- baseline_method:

  Either `"trimmed_mean"` (default) or `"median"`.

- baseline_trim:

  Fraction removed from each tail when
  `baseline_method = "trimmed_mean"`. The default is 0.10.

- baseline_floor_fraction:

  Minimum baseline as a fraction of the spatial median baseline in the
  mask intersection. The default 0.20 is provisional and should be
  calibrated on representative data.

- relative_noise_mad_cutoff:

  Upper cutoff for log robust relative noise, expressed as spatial
  median plus this many scaled MADs. The default is 5.

- spike_mad_cutoff:

  A consecutive-volume change is called extreme when its absolute
  deviation from the median first difference exceeds this many scaled
  MADs. Both volumes bordering an extreme change are marked. Default is
  6.

- max_spike_fraction:

  Maximum fraction of eligible volumes marked by an extreme
  consecutive-volume change. Default is 0.05.

- min_valid_frames:

  Minimum number of finite eligible volumes required for a voxel. At
  least two consecutive finite differences are also required.

- affine_tolerance:

  Absolute tolerance used when comparing the active 4-by-4 NIfTI
  transforms of the image and masks.

- outfile:

  Optional path for the final uint8 reference-core mask. If `""`, no
  file is written.

## Value

A list containing 3D RNifti images `core`, `agreement_mask`,
`mask_membership`, `baseline`, `relative_noise_cv`, `spike_fraction`,
`finite_fraction`, and `rejection_code`, plus scalar `thresholds`, voxel
`counts`, and `mask_metrics`. `mask_membership` is coded 0 = neither
mask, 1 = mask A only, 2 = mask B only, and 3 = agreement. Temporal QA
maps are computed over the candidate-mask union when a constraint is
supplied so disagreement regions can be audited, but thresholds and the
final core are derived strictly from the effective candidate population.
With no constraint, `agreement_mask` is simply the candidate mask.
Rejection codes are additive bit flags: 1 = outside the effective
candidate population, 2 = insufficient finite/consecutive observations,
4 = nonfinite or nonpositive baseline, 8 = baseline below the relative
floor, 16 = extreme relative noise, and 32 = excessive spike fraction.

## Details

Robust relative noise is
`1.4826 * median(abs(diff(y))) / (sqrt(2) * baseline)`, calculated only
across consecutive eligible volumes. This median absolute
first-difference estimator remains sensitive to sustained alternating
noise, for which MAD around the median difference can collapse to zero.
Its spatial upper threshold is estimated on the log scale after the
baseline criteria have been applied. Spike fraction is the fraction of
eligible volumes adjoining an extreme first difference, with the spike
threshold based on MAD around the median difference. Threshold defaults
are intentionally conservative draft values and require empirical
calibration before pipeline use.
