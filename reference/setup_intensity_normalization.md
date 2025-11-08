# Configure intensity normalization settings for postprocessing

This function configures the intensity normalization step in the
postprocessing pipeline. Intensity normalization rescales the fMRI time
series so that the median signal across the entire 4D image reaches a
specified global value (e.g., 10,000). This step can help ensure
comparability across runs and subjects.

## Usage

``` r
setup_intensity_normalization(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  A character vector of field names to prompt for. If `NULL`, all
  intensity normalization fields will be prompted.

## Value

A modified version of `ppcfg` with the `$intensity_normalize` entry
updated.
