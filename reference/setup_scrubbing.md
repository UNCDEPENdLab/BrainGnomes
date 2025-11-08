# Configure scrubbing of high-motion volumes

Generates spike regressors based on expressions evaluated on the
confounds file (e.g., "framewise_displacement \> 0.9" or "-1:1; dvars \>
1.5"). These regressors can later be used to censor volumes during
modeling.

## Usage

``` r
setup_scrubbing(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  Optional vector of fields to prompt for.

## Value

Modified `scfg` with `$postprocess$scrubbing` populated.
