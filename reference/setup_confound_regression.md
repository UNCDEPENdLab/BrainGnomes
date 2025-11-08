# Configure confound regression for postprocessing

This function configures voxelwise regression of nuisance confounds from
fMRI data. Confounds are typically drawn from the fMRIPrep confounds
file. Users can select confounds to be temporally filtered to match the
BOLD data (e.g., continuous-valued regressors) and those that should not
be filtered (e.g., binary spike regressors).

## Usage

``` r
setup_confound_regression(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  A character vector of fields to be prompted for. If `NULL`, all fields
  will be prompted.

## Value

A modified version of `ppcfg` with the `$confound_regression` entry
populated.

## Details

Regression is applied on a voxelwise basis. Filtered regressors
typically include motion parameters, CompCor components, DVARS, or
global signal. Unfiltered regressors usually include 0/1 indicators of
outlier volumes.
