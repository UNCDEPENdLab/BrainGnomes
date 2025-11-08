# Specify the MRIQC settings

Specify the MRIQC settings

## Usage

``` r
setup_mriqc(scfg, fields = NULL)
```

## Arguments

- scfg:

  A project configuration object, as produced by
  [`load_project()`](https://uncdependlab.github.io/BrainGnomes/reference/load_project.md)
  or
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md).

- fields:

  A character vector of fields to be prompted for. If `NULL`, all MRIQC
  fields will be prompted for.

## Value

A modified version of `scfg` with the `$mriqc` entry populated.
