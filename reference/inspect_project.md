# Inspect current project progress

Provides a compact, non-interactive view of tracked work from submission
through queueing, execution, completion, failure, or cancellation. By
default, the current state is integrated across runs by retaining the
most recent attempt for each project or subject-level work unit. Set
`run_id` to inspect only one submission.

## Usage

``` r
inspect_project(
  input = getwd(),
  run_id = NULL,
  subject_id = NULL,
  refresh = FALSE
)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.
  Defaults to the current working directory.

- run_id:

  Optional run ID. Use `"latest"` for the most recently recorded run, an
  explicit run ID for an older submission, or `NULL` (the default) for
  current project status across all runs.

- subject_id:

  Optional subject identifier, with or without the `sub-` prefix. When
  supplied, every returned resolution is restricted to that subject,
  including its run summaries.

- refresh:

  If `TRUE`, query the configured scheduler for jobs recorded as queued
  or running. The query is read-only: database values are retained and
  any differences are returned in `reconciliation`. The default,
  `FALSE`, relies only on the tracking database.

## Value

A `bg_project_inspection` object. Its `overview`, `stages`, `active`,
`reconciliation`, `subjects`, `subject_stages`, `runs`, `attempts`,
`submissions`, and `jobs` elements are data frames suitable for
programmatic queries.

## Details

The `overview` table contains one row for the selected scope. `stages`
and `subjects` aggregate its current work units; `active` reports
current queued and running jobs with elapsed time, requested wall time,
and health flags. When `refresh = TRUE`, `reconciliation` compares those
database states with the scheduler without modifying either source.
`submissions` exposes UUID attempts registered before scheduler
submission, including uncertain outcomes without a scheduler ID. Those
outcomes remain visible and prevent a project from being reported as
completed. `subject_stages` retains the stage and stream detail; `runs`
summarizes submissions; and `attempts` retains both current and
superseded logical attempts. `jobs` contains the underlying tracking
rows and marks the rows contributing to current project status with
`is_current_attempt`. Printing the object or its `jobs` component
deliberately omits long scheduler, path, and manifest fields, but those
columns remain available for ordinary data-frame access. Subject-wide
stages use `NA` for `ses_id`; stages that run separately by session
retain their session identifier.

## See also

[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for failure and log investigation.

## Examples

``` r
if (FALSE) { # \dontrun{
status <- inspect_project(scfg)
status
summary(status, by = "subject")
subject <- inspect_project(scfg, subject_id = "014")
subject$active

latest <- inspect_project(scfg, run_id = "latest", refresh = TRUE)
latest$reconciliation
subset(latest$jobs, lifecycle_status == "FAILED")
} # }
```
