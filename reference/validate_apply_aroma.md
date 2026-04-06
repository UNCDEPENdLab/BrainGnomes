# Validate AROMA (voxel-sampling replay vs output)

Samples ~100 voxels and replays the AROMA regression via
`lmfit_residuals_mat`; passes if max abs diff \< 0.05 (or skips if no
noise ICs).

## Usage

``` r
validate_apply_aroma(
  pre_file,
  post_file,
  mixing_file,
  noise_ics,
  nonaggressive = TRUE,
  n_sample = 100L
)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `apply_aroma`.

- post_file:

  Path to 4D BOLD after `apply_aroma`.

- mixing_file:

  MELODIC mixing matrix (no header).

- noise_ics:

  Noise IC indices (1-based), same as pipeline.

- nonaggressive:

  Same as `apply_aroma`.

- n_sample:

  Number of voxels to sample (default 100).

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details`.
