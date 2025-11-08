# Configure fMRIPrep preprocessing settings

This function sets up fMRIPrep job configuration, including scheduling
and resource parameters, output specifications, and the location of
required files such as the FreeSurfer license. It prompts the user
interactively (or selectively if `fields` is supplied) and modifies the
project configuration list (`scfg`) to include settings for running
fMRIPrep.

## Usage

``` r
setup_fmriprep(scfg = NULL, fields = NULL)
```

## Arguments

- scfg:

  A project configuration object, as produced by
  [`load_project()`](https://uncdependlab.github.io/BrainGnomes/reference/load_project.md)
  or
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md).

- fields:

  A character vector of fields to be prompted for. If `NULL`, all
  fMRIPrep fields will be prompted for.

## Value

A modified version of `scfg` with the `$fmriprep` entry populated.

## Details

fMRIPrep is a robust and standardized preprocessing pipeline for BOLD
and structural MRI data organized according to the BIDS standard. It
performs motion correction, susceptibility distortion correction, brain
extraction, spatial normalization, confound estimation, and other key
steps to prepare fMRI data for statistical analysis.

This function allows you to specify memory, number of cores, and maximum
runtime for fMRIPrep jobs, as well as fMRIPrep-specific options such as
output spaces and the FreeSurfer license file location.
