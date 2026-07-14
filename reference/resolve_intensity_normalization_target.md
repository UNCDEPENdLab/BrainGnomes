# Resolve the configured intensity-normalization target

The target is the desired spatial median across reference-region voxels
of their 10%-trimmed temporal means at the point when scaling is
applied. `target` is the canonical configuration field. `global_median`
remains accepted for backward compatibility, although that older name is
misleading: the method does not target the median of all values in the
4D image.

## Usage

``` r
resolve_intensity_normalization_target(intensity_cfg)
```

## Arguments

- intensity_cfg:

  Intensity-normalization configuration list.

## Value

A finite positive numeric scalar.
