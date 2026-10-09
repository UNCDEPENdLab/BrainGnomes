# Inspecting and Diagnosing Pipeline Runs

## Overview

BrainGnomes separates routine progress inspection from failure
diagnosis:

- [`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
  answers, “What is happening now?”
- [`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
  answers, “What failed, and where are its logs?”

Both functions use the project’s job-tracking database. Inspection is
database-only by default. An optional read-only refresh can compare
active records with the scheduler; inspection never submits work or
changes project files.

``` r

library(BrainGnomes)
scfg <- load_project("/project/my_study")
```

Both functions also accept the project directory or configuration YAML
path directly. If the project root is the current working directory, the
input can be omitted:

``` r

setwd("/project/my_study")
inspect_project()
diagnose_project() # guided browser in an interactive R session
```

Automatic discovery checks only the current directory. It does not
search parent directories, and reports a clear error if
`project_config.yaml` is not there.

## Inspect current project progress

The default inspection integrates all tracked runs:

``` r

status <- inspect_project(scfg)
status
```

The printed dashboard gives overall counts, stage/stream progress, and a
bounded list of subjects that are active or need attention. Long
tracking fields such as scheduler options, paths, and manifests remain
available in the returned object but are not printed automatically.

The object contains ordinary data frames at several resolutions:

``` r

status$overview
status$active
status$reconciliation
status$stages
status$subjects
status$subject_stages
status$runs
status$attempts
status$jobs
```

The same tables are available through
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r

summary(status)                         # one-row overview
summary(status, by = "stage")
summary(status, by = "subject")
summary(status, by = "subject_stage")
summary(status, by = "active")
```

Because these are data frames, ordinary R queries work without another
API:

``` r

subset(status$subjects, status != "COMPLETED")
subset(status$subject_stages, sub_id == "540296")
subset(status$jobs, lifecycle_status %in% c("RUNNING", "QUEUED"))
```

### Focus on one subject

Use `subject_id` with or without the `sub-` prefix to restrict every
resolution, including the run summaries and active-job table:

``` r

subject_status <- inspect_project(scfg, subject_id = "540294")
subject_status$overview
subject_status$runs
subject_status$subject_stages
subject_status$active
```

This is a true inspection scope rather than a display filter, so the
returned counts and summaries all describe that subject.

### Check active jobs against the scheduler

`status$active` is available without contacting the scheduler. It
reports database status, time in the current state, requested wall time,
and flags a running record that has exceeded that request. To compare
queued and running database records with their current scheduler states,
request a refresh:

``` r

refreshed <- inspect_project(scfg, refresh = TRUE)
refreshed$active
refreshed$reconciliation
```

The refresh is read-only. It preserves the database status, places the
scheduler observation beside it, and flags unconfirmed or mismatched
records without updating tracking data. Internally this uses the same
scheduler query path as
[`wait_for_job()`](https://hallquistlab.github.io/BrainGnomes/reference/wait_for_job.md),
including the existing Slurm, TORQUE, and local process status
functions.

### How status is integrated across runs

Each subject/session/stage/stream combination is treated as a logical
work unit. Related controller, array, and sentinel jobs are grouped into
that unit. For current project status, BrainGnomes uses the **newest
attempt** for every unit rather than the best status ever observed.

Consequently, a successful retry supersedes an older failure, while a
failed forced rerun is not hidden by an older success. Work omitted from
a later partial retry retains its newest earlier state. Historical
attempts remain available in `status$attempts` and `status$runs`.

Project-wide inspection describes work represented in the tracking
database. It does not invent a status for configured work that has never
been submitted.

### Interpreting statuses

Raw database statuses are retained in `status$jobs$status`. Summarized
views use friendlier lifecycle labels:

- `COMPLETED`: all jobs in the work unit completed.
- `RUNNING`: at least one job has started and no failure takes
  precedence.
- `QUEUED`: submitted work has not started.
- `FAILED`: the work itself failed.
- `BLOCKED`: the database records `FAILED_BY_EXT`, meaning required
  upstream work failed.
- `CANCELLED`: the work was cancelled.
- `UNKNOWN`: the recorded state is missing or unrecognized.

Counts are always retained alongside the convenience status, so mixed
states remain visible.

## Choose the run you mean

Use the run table to find historical IDs:

``` r

status$runs
run_id <- status$runs$run_id[[1L]]
```

For retry or cancellation, an explicit run ID is safer than `"latest"`
because another submission could become the newest run.

## Inspect one run without prompts

Supply `"latest"` or an explicit ID to restrict every inspection view to
one submission:

``` r

latest <- inspect_project(scfg, run_id = "latest")
selected <- inspect_project(scfg, run_id = run_id)

selected$overview
selected$subjects
selected$subject_stages
selected$jobs
```

The older
[`get_project_runs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_runs.md)
and
[`get_run_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_jobs.md)
functions remain as deprecated compatibility wrappers. New code should
use `$runs` and `$jobs` from
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md).

## Diagnose failures

Routine monitoring belongs in
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md).
When current work has failed, call
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
from an interactive R session to open the guided dependency and log
browser:

``` r

diagnose_project(scfg)
```

The browser begins with unresolved failures grouped by stage and stream.
A single-job group opens that job directly; a repeated problem first
narrows to the affected subjects. Subject, run, and job selections are
retained during the drill-down, so the browser never returns to a full
run-wide job list unless you explicitly choose that broader view.

If you already know the subject or scheduler job ID, start at that
scope:

``` r

diagnose_project(scfg, subject_id = "540294")
diagnose_project(scfg, job_id = "66273010")
```

Scripts, tests, and reports are non-interactive, so the same call
returns a structured diagnosis there. Use `interactive = FALSE`
explicitly when you want that result in an interactive R session:

``` r

diagnosis <- diagnose_project(scfg, interactive = FALSE)
diagnosis$failures
diagnosis$logs
```

An existing inspection snapshot can be reused:

``` r

diagnosis <- diagnose_project(status, interactive = FALSE)
```

For a historical post-mortem, select the run explicitly:

``` r

diagnosis <- diagnose_project(
  scfg, run_id = run_id, interactive = FALSE
)
```

Pass `interactive = TRUE` when code must open the guided browser
regardless of how R was started:

``` r

diagnose_project(scfg, run_id = run_id, interactive = TRUE)
```

[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
is the single diagnosis entry point for both the guided browser and
structured, non-interactive results.

## Inspect run provenance

Before deciding what to rerun, inspect the settings and scope that
produced the selected run:

``` r

provenance <- get_run_provenance(scfg, run_id)
provenance$request
provenance$execution$subjects
provenance$configuration$snapshot_file
```

The provenance bundle records the resolved stages and subjects,
configuration, resources, software, host, and fingerprints of
execution-driving files.

## Retry failed work after correcting the cause

A retry creates a **new run**. It does not restart scheduler jobs in
place and does not change the original run. Always preview it first:

``` r

retry_plan <- retry_project_run(scfg, run_id, dry_run = TRUE)
retry_plan$jobs
retry_plan$subjects
```

After correcting the underlying cause, submit the retry explicitly:

``` r

retry_run <- retry_project_run(scfg, run_id, dry_run = FALSE)
```

Jobs marked `FAILED_BY_EXT` were blocked by an earlier failure and are
excluded by default. Include them when appropriate:

``` r

retry_plan <- retry_project_run(
  scfg, run_id, include_blocked = TRUE, dry_run = TRUE
)
```

## Cancel work that is still active

Cancellation applies only to tracked jobs that are queued or running. It
does not delete data, logs, or completed outputs. Preview the scheduler
commands before making a separate explicit cancellation request:

``` r

cancel_project_run(scfg, run_id, dry_run = TRUE)
cancel_project_run(scfg, run_id, dry_run = FALSE)
```

## Command-line inspection and recovery

The command line exposes the same inspection resolutions:

``` bash
# Replace this value with the run ID reported by BrainGnomes status.
run_id="REPLACE_WITH_RUN_ID"
BrainGnomes status /project/my_study
BrainGnomes status /project/my_study --view=stages
BrainGnomes status /project/my_study --view=subjects
BrainGnomes status /project/my_study --sub-id=540294
BrainGnomes status /project/my_study --view=active --refresh
BrainGnomes status /project/my_study --view=reconciliation
BrainGnomes status /project/my_study --run=latest --watch
BrainGnomes status /project/my_study --run="$run_id" --view=jobs --format=csv

BrainGnomes diagnose /project/my_study
BrainGnomes diagnose /project/my_study --run="$run_id" --interactive
BrainGnomes diagnose /project/my_study --subject-id=540294 --interactive
BrainGnomes diagnose /project/my_study --job-id=66273010 --interactive
BrainGnomes logs /project/my_study --run="$run_id" --failed-only --tail=50
BrainGnomes provenance /project/my_study --run="$run_id" --format=json

BrainGnomes retry /project/my_study --run="$run_id" --dry-run
BrainGnomes retry /project/my_study --run="$run_id" --yes
BrainGnomes cancel /project/my_study --run="$run_id" --dry-run
BrainGnomes cancel /project/my_study --run="$run_id" --yes
```

Retry and cancellation require either a dry-run preview or explicit
confirmation. Neither routine inspection nor diagnosis changes scheduler
state.
