# Configure intensity normalization settings for postprocessing

Configures robust run-wise scalar or denominator-guarded voxelwise
percent-signal-change intensity normalization so runs share
interpretable units before temporal denoising.

## Usage

``` r
setup_intensity_normalization(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  Postprocessing configuration list, normally the `postprocess` section
  of a study configuration.

- fields:

  Character vector naming fields to prompt for. If `NULL`, the function
  prompts for any missing intensity-normalization settings.

## Value

The `ppcfg` list with its `intensity_normalize` settings updated.

## Details

BrainGnomes first selects a fixed set of stable, positive-signal
functional voxels from the input BOLD image. After masking and spatial
smoothing, it calculates a 10% trimmed temporal mean for each of these
reference voxels and takes the spatial median of those voxelwise
baselines. If that run reference intensity is `L`, `run_scalar`
multiplies the complete run by `target / L`. `voxel_psc` instead
calculates the same robust baseline at every voxel and applies a
denominator-guarded multiplier targeting 100. Here, "guarded" means a
reliable voxel uses `100 / local_baseline`, a very low positive baseline
uses a fixed lower denominator bound, and a baseline that is nonfinite,
nonpositive, or insufficiently observed uses the conservative run
multiplier `100 / L`. The guards prevent unstable division; they do not
clip BOLD observations, impute a baseline, apply the reference core as a
validity mask, or remove voxels. Floor and fallback voxels remain in the
output but are not exact local PSC.

Volumes identified as non-steady-state or marked for censoring are
omitted from both scalar and PSC baseline estimates, when matching
metadata are available. The resulting multiplier is nevertheless applied
to every volume. Both modes use the same user-specified prefix and occur
after masking/smoothing but before AROMA, interpolation, temporal
filtering, confound regression, or volume removal.

For `voxel_psc`, this placement defines percent change relative to each
voxel's smoothed baseline, rather than an average of pre-smoothing PSC
series. The distinction can matter near tissue boundaries or dropout,
where baselines differ across neighbors. Post-smoothing calibration uses
the same signal that enters modeling and avoids spatially spreading
large multipliers from low-baseline voxels. Users who need unsmoothed
voxelwise PSC should use a postprocessing stream with spatial smoothing
disabled.
