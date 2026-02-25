# Run BIDS validation on the project BIDS directory

This helper submits the BIDS validator as a standalone job. It does not
run as part of
[`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
and can be invoked whenever validation of the project BIDS directory is
desired.

## Usage

``` r
run_bids_validation(scfg, outfile = NULL, wait_jobs = NULL, sequence_id = NULL)
```

## Arguments

- scfg:

  A `bg_project_cfg` object returned by
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md)
  or
  [`load_project()`](https://uncdependlab.github.io/BrainGnomes/reference/load_project.md).

- outfile:

  The output HTML report path for bids-validator. Relative paths are
  written under `scfg$metadata$log_directory` (to avoid contaminating
  the BIDS dataset); absolute paths are used as provided. If `NULL`, the
  value stored in `scfg$bids_validation$outfile` is used.

- wait_jobs:

  Optional character vector of upstream scheduler job IDs that must
  complete before this validation job starts.

- sequence_id:

  Optional sequence ID used for job tracking.

## Value

The job id returned by the scheduler.

## Examples

``` r
if (FALSE) { # \dontrun{
  run_bids_validation(study_config, outfile = "bids_validator_output.html")
} # }
```
