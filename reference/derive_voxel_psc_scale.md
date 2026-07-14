# Derive a denominator-guarded voxelwise percent-signal-change scale map

"Guarded PSC" means that BrainGnomes protects the denominator used for
baseline-to-100 scaling. It calculates a 10%-trimmed temporal baseline
for every voxel from finite, steady-state, uncensored observations and
returns a positive 3D multiplier map. A reliable baseline receives
`target / baseline`. A positive baseline below
`baseline_floor_fraction * reference_location` is replaced by that
floor, which caps the multiplier. A baseline that cannot be identified
because it has too few observations or is nonfinite/nonpositive receives
the conservative run multiplier `target / reference_location`.

## Usage

``` r
derive_voxel_psc_scale(
  img,
  reference_location,
  qa_mask,
  include_frames = NULL,
  target = 100,
  baseline_trim = 0.1,
  baseline_floor_fraction = 0.2,
  min_valid_frames = 20L,
  affine_tolerance = 1e-05,
  outfile = ""
)
```

## Arguments

- img:

  A 4D `RNifti::NiftiImage` object or path to the BOLD image at the
  post-spatial, pre-temporal normalization point.

- reference_location:

  Finite positive spatial median of the robust baselines in the frozen
  intensity-reference core.

- qa_mask:

  A 3D conservative functional mask used only to stratify QA counts. It
  never changes the multiplier map.

- include_frames:

  Optional logical vector with one value per volume. `TRUE` marks a
  steady-state, uncensored volume eligible for baseline estimation. The
  multiplier is nevertheless applied to every volume.

- target:

  Voxelwise robust-baseline target. Use 100 for PSC units.

- baseline_trim:

  Fraction removed from each temporal tail. The default 0.10 removes 10%
  from each tail and averages the central 80%.

- baseline_floor_fraction:

  Positive denominator floor relative to `reference_location`. This
  bounds amplification in very low-signal voxels.

- min_valid_frames:

  Minimum finite eligible observations required for a voxel-specific
  denominator.

- affine_tolerance:

  Absolute tolerance for comparing NIfTI transforms.

- outfile:

  Optional path for the float32 3D multiplier map. If `""`, no file is
  written.

## Value

A list containing 3D RNifti images `scale`, `baseline`,
`effective_denominator`, and `guard_code`; named counts for the complete
grid and QA mask; and the target, reference multiplier, and denominator
floor. Guard codes are 0 = ordinary PSC (`target / local baseline`), 1 =
positive local baseline replaced by the denominator floor, 2 = too few
finite eligible frames and run-reference fallback, and 3 =
nonfinite/nonpositive baseline and run-reference fallback.

## Details

The guard acts only on the denominator. It does not clip individual BOLD
observations, impute a local baseline, create a validity mask, or remove
or zero any voxel. Spatial inclusion remains the responsibility of the
pipeline's separate `apply_mask` step. Voxels receiving the floor or
fallback are retained but do not have an exact local PSC interpretation.
