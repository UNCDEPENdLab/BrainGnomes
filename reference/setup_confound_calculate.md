# Configure confound calculation for postprocessing

This function configures the generation of a confound file during
postprocessing. The resulting file includes nuisance regressors (e.g.,
motion parameters, CompCor components, DVARS, global signal) that may be
used during task-based modeling to account for noise without directly
altering the fMRI data.

## Usage

``` r
setup_confound_calculate(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  A character vector of fields to prompt for. If `NULL`, all relevant
  fields will be prompted.

## Value

A modified version of `ppcfg` with the `$confound_calculate` entry
updated.

## Details

Confounds can be filtered (e.g., with the same temporal filter as
applied to fMRI data) or left unfiltered. Filtered regressors should
typically include continuous-valued signals (e.g., a_comp_cor\_*, global
signal), while spike regressors or discrete values (e.g.,
motion_outlier*) should not be filtered.

This function only generates the confound regressors file. Actual
regression is handled separately.
