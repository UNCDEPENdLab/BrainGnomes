# Set up project metadata for an fMRI preprocessing study

Prompts the user to configure essential metadata fields for a
study-level configuration object. This includes directories for DICOM
inputs, BIDS-formatted outputs, fMRIPrep outputs, MRIQC reports,
TemplateFlow cache, and scratch space for intermediate files. It also
ensures required directories exist or offers to create them
interactively.

## Usage

``` r
setup_project_metadata(scfg = NULL, fields = NULL)
```

## Arguments

- scfg:

  A project configuration object created by
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md).

- fields:

  A character vector of metadata fields to prompt for (e.g.,
  `"metadata/project_name"`). If `NULL`, all missing or unset fields
  will be prompted.

## Value

A modified version of `scfg` with the `$metadata` field populated with
validated paths and project details.

## Details

The function is designed to be used during initial study setup, but can
also be used later to fill in missing metadata or revise selected
fields. If specific `fields` are provided, only those fields will be
prompted.
