# Specify the postprocessing steps for a study

This function determines the sequence of postprocessing steps to be
applied after fMRIPrep. Steps are included based on whether the
corresponding `$enable` field is `TRUE` in the project configuration. If
the user opts to override the default order, they may manually specify a
custom sequence.

## Usage

``` r
setup_postprocess_steps(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  a character vector of fields to be prompted for. If `NULL`, all fields
  will be prompted for.

## Value

a modified version of `ppcfg` with the `$postprocess$processing_steps`
field populated

## Details

This function is used to set up the postprocessing steps for a study. It
prompts the user for the order of the processing steps and whether to
apply them. The order of the processing steps is important, particularly
because if we filter certain frequencies from the fMRI data, we must
filter any regressors that we later apply to the data – that is,
confounds and fMRI data must match in frequency content prior to
regression. See Hallquist, Hwang, & Luna (2013) or Lindquist (2019) for
details.
