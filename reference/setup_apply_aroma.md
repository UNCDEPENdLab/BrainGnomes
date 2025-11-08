# Configure ICA-AROMA denoising application in postprocessing

This function configures the application of ICA-AROMA denoising to fMRI
data as part of postprocessing. ICA-AROMA (Pruim et al., 2015)
identifies and labels motion-related independent components (ICs) using
spatiotemporal features, and outputs regressors that can be used to
remove these components.

## Usage

``` r
setup_apply_aroma(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  A character vector of fields to prompt for. If `NULL`, all fields will
  be prompted.

## Value

A modified version of `ppcfg` with the `$apply_aroma` entry updated.

## Details

If enabled, this step applies the AROMA regressors to remove noise
components from the BOLD time series using either 'aggressive' or
'nonaggressive' regression. Nonaggressive denoising is recommended as it
preserves shared variance with signal components.

This step assumes that ICA-AROMA has already been run using a tool like
`fmripost-aroma`.
