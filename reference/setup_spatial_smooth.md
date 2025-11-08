# Configure spatial smoothing settings for fMRI postprocessing

This function configures the spatial smoothing step for postprocessing
of BOLD fMRI data. Spatial smoothing increases signal-to-noise ratio by
averaging nearby voxels and can improve the statistical properties of
the data, especially for group-level analyses.

## Usage

``` r
setup_spatial_smooth(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  A character vector of field names to prompt for. If `NULL`, all
  spatial smoothing fields will be prompted.

## Value

A modified version of `ppcfg` with the `$spatial_smooth` field
populated.

## Details

The user is asked whether they want to apply smoothing, and if so, to
specify the full width at half maximum (FWHM) of the Gaussian smoothing
kernel and a filename prefix.

If enabled, spatial smoothing is applied to the preprocessed BOLD data
using a Gaussian kernel with the user-specified FWHM in millimeters.
This can help improve sensitivity and inter-subject alignment,
especially in standard space. This is accomplished using FSL's
contrast-sensitive susan smoothing command.
