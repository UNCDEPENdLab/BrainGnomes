# Configure flywheel sync settings

Sets up synchronization from a Flywheel instance prior to BIDS
conversion. Prompts for the Flywheel project URL, drop-off directory for
downloaded DICOMs, and a temporary directory used during transfer.
Standard job settings are also collected through `setup_job`.

## Usage

``` r
setup_flywheel_sync(scfg, fields = NULL)
```

## Arguments

- scfg:

  A project configuration object, as produced by
  [`load_project()`](https://uncdependlab.github.io/BrainGnomes/reference/load_project.md)
  or
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md).

- fields:

  A character vector of fields to be prompted for. If `NULL`, all
  Flywheel fields will be prompted for.

## Value

A modified version of `scfg` with the `$flywheel_sync` entry populated.
