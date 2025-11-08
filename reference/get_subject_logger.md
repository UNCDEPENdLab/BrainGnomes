# This function returns an lgr object corresponding to logging for a single subject directory

This function returns an lgr object corresponding to logging for a
single subject directory

## Usage

``` r
get_subject_logger(scfg, sub_id)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

- sub_id:

  The id of the subject whose logger we wish to access

## Value

a configured lgr object for logging subject processing messages
