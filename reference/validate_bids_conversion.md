# Validate the structure of a project configuration object

Validate the structure of a project configuration object

## Usage

``` r
validate_bids_conversion(scfg = list(), quiet = FALSE, selection = NULL)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

- quiet:

  Suppress validation messages where supported.

- selection:

  Optional resolved work, allowing selected upstream outputs to be
  absent.
