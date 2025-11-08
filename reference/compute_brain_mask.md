# Compute a loose brain mask from functional MRI data using FSL

Generates a brain mask from a functional image using a modified FSL
approach based on the 98-2 percentile intensity method. This method
combines BET skull-stripping with percentile thresholding and binary
dilation to produce a conservative mask.

## Usage

``` r
compute_brain_mask(in_file, lg = NULL, fsl_img = NULL)
```

## Arguments

- in_file:

  Path to the input 4D NIfTI functional image.

- lg:

  Optional lgr object used for logging messages

- fsl_img:

  Optional Singularity image to execute FSL commands in a containerized
  environment.

## Value

File path to the computed binary brain mask (not yet dilated). A dilated
version of the mask is also saved with a `_dil1x` suffix.

## Details

This function replicates the "98-2" heuristic used in FSL's featlib.tcl:
it computes the 2nd and 98th percentiles from a skull-stripped mean
image and thresholds at 10% above the 2nd percentile. A final mask is
formed by applying this threshold, binarizing, and performing one
dilation iteration.
