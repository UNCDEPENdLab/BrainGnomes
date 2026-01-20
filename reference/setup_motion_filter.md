# Configure optional motion parameter filtering

If motion parameters are used in scrubbing expressions or included among
the selected confound regressors, users can choose to filter the
rigid-body motion time series prior to downstream processing (either
with a notch/band-stop filter or a low-pass filter). This mirrors the
respiration filtering strategy used in tools such as xcp-d and helps
mitigate respiration-induced spikes in framewise displacement.

## Usage

``` r
setup_motion_filter(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  Optional vector of fields to prompt for.

## Value

Modified `ppcfg` with `$motion_filter` populated when applicable.
