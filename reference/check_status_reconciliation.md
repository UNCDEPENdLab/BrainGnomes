# Check for discrepancies between DB status, manifest verification, and .complete files

Examines the job tracking database and compares against actual file
system state to identify subjects where completion status is
inconsistent across different verification methods.

## Usage

``` r
check_status_reconciliation(
  scfg,
  sub_ids = NULL,
  steps = c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess"),
  verbose = TRUE
)
```

## Arguments

- scfg:

  A project configuration object as produced by `load_project` or
  `setup_project`

- sub_ids:

  Character vector of subject IDs to check. If NULL, checks all
  subjects.

- steps:

  Character vector of step names to check. Default is all main steps.

- verbose:

  Logical. If TRUE, print detailed reconciliation information.

## Value

A data.frame with columns: sub_id, ses_id, step_name, db_status,
manifest_verified, complete_file_exists, fail_file_exists, discrepancy
(logical), and details.
