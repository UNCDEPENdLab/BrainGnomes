# Resolve the configured intensity-normalization mode

Missing modes retain the historical run-wise scalar behavior. In the
denominator-guarded voxelwise PSC pathway, a reliable local baseline is
scaled to 100, very low positive baselines use a lower denominator
bound, and unidentified baselines use a conservative run-level fallback.
These guards do not clip observations or mask voxels. Both modes use the
same filename prefix and processing checkpoint; the mode is recorded in
normalization provenance rather than encoded in the prefix.

## Usage

``` r
resolve_intensity_normalization_mode(intensity_cfg)
```

## Arguments

- intensity_cfg:

  Intensity-normalization configuration list.

## Value

One of `"run_scalar"` or `"voxel_psc"`.
