# Validate run-scalar or denominator-guarded voxelwise PSC normalization

Checks dimensions, finite-value locations, and exact application of
either a single positive multiplier or a positive 3D PSC multiplier map.
A guarded PSC map encodes ordinary `100 / local_baseline` factors plus
denominator-floor and run-reference fallback factors; guarding does not
imply observation clipping or voxel masking. For scalar normalization,
an optional reference mask also verifies the achieved target.

## Usage

``` r
validate_intensity_normalize(
  pre_file,
  post_file,
  reference_location,
  target,
  scale_factor = NULL,
  mode = "run_scalar",
  scale_file = NULL,
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

  Positive constant applied to every voxel and volume in `run_scalar`
  mode.

- mode:

  Either `"run_scalar"` or denominator-guarded `"voxel_psc"`.

- scale_file:

  Path to the 3D multiplier map used in `voxel_psc` mode.

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
`target`. If `core_file` is supplied in `run_scalar` mode, the function
independently repeats this two-stage calculation on `post_file`, using
`include_frames` to select the same baseline-estimation volumes. In
`voxel_psc` mode, `target` must be 100 and `scale_file` must be a finite
positive 3D map matching the spatial BOLD grid. No binary PSC validity
mask is expected or applied.
