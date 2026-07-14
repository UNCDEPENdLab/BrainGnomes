# Validate run-wise intensity normalization

Checks that the requested target and stored multiplier agree, that every
finite image value was multiplied by that same constant, and that image
dimensions and missing-value locations did not change. When the
reference mask is supplied, the function also remeasures the normalized
image to confirm that the requested target was reached at the point of
scaling.

## Usage

``` r
validate_intensity_normalize(
  pre_file,
  post_file,
  reference_location,
  target,
  scale_factor,
  core_file = NULL,
  include_frames = NULL,
  tolerance = 1e-05
)
```

## Arguments

- pre_file:

  Path to the 4D NIfTI image immediately before scaling.

- post_file:

  Path to the 4D NIfTI image immediately after scaling.

- reference_location:

  Positive run reference intensity measured after masking and smoothing
  and before temporal denoising.

- target:

  Desired value of the run reference intensity after scaling.

- scale_factor:

  Positive constant applied to every voxel and volume.

- core_file:

  Optional path to the fixed 3D reference-region mask. The BrainGnomes
  pipeline supplies this to verify the achieved target directly.

- include_frames:

  Optional logical vector with one value per volume. `TRUE` identifies a
  volume used to estimate the temporal baselines. The pipeline supplies
  the same vector used for `reference_location`.

- tolerance:

  Maximum allowed relative numerical error for each check.

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed). The
`message` attribute gives a readable summary. The `details` attribute
reports the target, multiplier, expected and remeasured reference
intensities, and relative multiplication errors.

## Details

`reference_location` is the run reference intensity before scaling: the
spatial median across reference voxels of their 10%-trimmed temporal
means. Therefore, `reference_location * scale_factor` should equal
`target`. If `core_file` is supplied, the function independently repeats
this two-stage calculation on `post_file`, using `include_frames` to
select the same baseline-estimation volumes.
