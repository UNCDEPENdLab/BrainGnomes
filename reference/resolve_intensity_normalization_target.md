# Resolve the configured intensity-normalization target

The target is the desired spatial median across reference-region voxels
of their 10%-trimmed temporal means for `run_scalar`. `target` is the
canonical configuration field and `global_median` remains accepted for
backward compatibility. Denominator-guarded `voxel_psc` always uses 100:
reliable local baselines are scaled to 100, while floor or fallback
denominators prevent unstable division without clipping or masking data.
The fixed target is required for regression coefficients to be expressed
in approximate percent-signal-change units; any configured scalar target
is ignored in that mode.

## Usage

``` r
resolve_intensity_normalization_target(
  intensity_cfg,
  mode = resolve_intensity_normalization_mode(intensity_cfg)
)
```

## Arguments

- intensity_cfg:

  Intensity-normalization configuration list.

- mode:

  Optional resolved normalization mode.

## Value

A finite positive numeric scalar.
