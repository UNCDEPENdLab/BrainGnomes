# Specify the BIDS conversion settings

Specify the BIDS conversion settings

## Usage

``` r
setup_bids_conversion(scfg, fields = NULL)
```

## Arguments

- scfg:

  a project configuration object, as produced by `load_project` or
  `setup_project`

- fields:

  a character vector of fields to be prompted for. If `NULL`, all fields
  will be prompted for.

## Value

a modified version of `scfg` with `$bids_conversion` populated
