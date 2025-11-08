# Run BIDS validation on the project BIDS directory

This helper submits the BIDS validator as a standalone job. It does not
run as part of
[`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
and can be invoked whenever validation of the project BIDS directory is
desired.

## Usage

``` r
run_bids_validation(scfg, outfile = NULL)
```

## Arguments

- scfg:

  A `bg_project_cfg` object returned by
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md)
  or
  [`load_project()`](https://uncdependlab.github.io/BrainGnomes/reference/load_project.md).

- outfile:

  The name of the HTML report to create in the BIDS directory. If
  `NULL`, the value stored in `scfg$bids_validation$outfile` is used.

## Value

The job id returned by the scheduler.

## Examples

``` r
if (FALSE) { # \dontrun{
  run_bids_validation(study_config, outfile = "bids_validator_output.html")
} # }
```
