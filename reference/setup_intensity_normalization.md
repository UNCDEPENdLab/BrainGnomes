# Configure intensity normalization settings for postprocessing

Configures run-wise intensity normalization. BrainGnomes multiplies
every voxel and volume in a run by one positive constant so that runs
share a common intensity convention before temporal denoising.

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
baselines. If that run reference intensity is `L`, the complete run is
multiplied by `target / L`. Thus, `target` is not the whole-brain mean
or the median of all values in the final 4D image.

Volumes identified as non-steady-state or marked for censoring are
omitted when estimating the run reference intensity, when matching
metadata are available. The resulting multiplier is nevertheless applied
to every volume. Scaling occurs after masking and smoothing but before
AROMA, interpolation, temporal filtering, confound regression, or volume
removal.
