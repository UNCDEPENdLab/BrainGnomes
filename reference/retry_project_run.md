# Create a new run for failed work

A retry does not resume scheduler jobs in place and does not change the
original run. It creates a new
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
submission containing the exact failed or cancelled
subject/session/stage/stream combinations found in the source run. Array
tasks and sentinels resolve to their owning work unit; the whole unit is
retried, not individual files within it. Selected work is rerun even if
old completion markers would normally skip it. Successful combinations
are not added, and the new provenance record identifies the source run
even when a retry plan is saved and submitted later.

## Usage

``` r
retry_project_run(
  input = getwd(),
  run_id = "latest",
  include_blocked = FALSE,
  dry_run = TRUE
)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.
  Defaults to the current working directory.

- run_id:

  Source run ID returned by
  [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  or listed in `inspect_project(input)$runs`. An explicit ID is
  recommended for retry.

- include_blocked:

  Also include downstream jobs marked `FAILED_BY_EXT`. These jobs did
  not fail themselves; they could not run because an earlier required
  job failed. They are excluded by default.

- dry_run:

  If `TRUE`, return a plan without submitting jobs. If `FALSE`, submit
  the new run immediately.

## Value

A `bg_project_plan` for a dry run or `bg_project_run` after submission.

## Details

Required project setup jobs are listed separately in the plan. A
setup-only failure cannot determine the intended subject scope: include
blocked jobs or use
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
with an explicit selection. Ambiguous legacy records or missing inputs
cause an error instead of broadening the retry scope.

Preview with `dry_run = TRUE` before submitting. The preview returns a
plan and contacts no scheduler. With `dry_run = FALSE`, submission
begins immediately and the function returns the new run handle.

## See also

[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
to inspect the source failure and
[`get_run_provenance()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_provenance.md)
to compare the original and retry runs.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inspect and correct the failure before retrying.
diagnosis <- diagnose_project(
  scfg, run$run_id, interactive = FALSE
)

# Preview only; no jobs are submitted.
retry_plan <- retry_project_run(scfg, run$run_id, dry_run = TRUE)

# Submit a separate new run after reviewing the plan.
retry_run <- retry_project_run(scfg, run$run_id, dry_run = FALSE)
} # }
```
