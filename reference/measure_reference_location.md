# Measure a robust intensity location within a frozen reference core

Computes the same voxelwise temporal location and spatial median used
for intensity normalization, but does not redefine or refine the
supplied core. This supports measuring the denominator after spatial
preprocessing while keeping voxel and frame selection fixed from the
original positive-scale BOLD image.

## Usage

``` r
measure_reference_location(
  img,
  core_mask,
  include_frames = NULL,
  baseline_method = "trimmed_mean",
  baseline_trim = 0.1,
  min_valid_frames = 20L,
  affine_tolerance = 1e-05
)
```

## Arguments

- img:

  A 4D `RNifti::NiftiImage` object or path to a 4D BOLD NIfTI.

- core_mask:

  A 3D frozen reference-core mask on the same grid as `img`.

- include_frames:

  Optional logical vector with one value per volume.

- baseline_method:

  Either `"trimmed_mean"` (default) or `"median"`.

- baseline_trim:

  Fraction removed from each temporal tail for a trimmed mean. The
  default is 0.10.

- min_valid_frames:

  Minimum finite eligible observations per core voxel.

- affine_tolerance:

  Absolute tolerance for comparing NIfTI transforms.

## Value

A list with `reference_location`, core and usable voxel counts, usable
fraction, and eligible-frame count.
