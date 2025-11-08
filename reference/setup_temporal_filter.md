# Configure temporal filtering settings for postprocessing

This function configures the temporal filtering step in the
postprocessing pipeline. Temporal filtering removes unwanted frequency
components from the BOLD signal, such as slow drifts (via high-pass
filtering) or physiological noise (via low-pass filtering). This step is
often used to improve signal quality for subsequent statistical
analysis.

## Usage

``` r
setup_temporal_filter(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  A character vector of field names to prompt for. If `NULL`, all
  temporal filtering fields will be prompted.

## Value

A modified version of `ppcfg` with the `$temporal_filter` entry updated.
