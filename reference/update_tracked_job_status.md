# Update Job Status in Tracking SQLite Database

Updates the status of a specific job in a tracking database, optionally
cascading failure status to downstream jobs.

## Usage

``` r
update_tracked_job_status(
  sqlite_db = NULL,
  job_id = NULL,
  status,
  cascade = FALSE,
  exclude = NULL
)
```

## Arguments

- sqlite_db:

  Character string. Path to the SQLite database file used for job
  tracking.

- job_id:

  Character string or numeric. ID of the job to update. If numeric, it
  will be coerced to a string.

- status:

  Character string. The job status to set. Must be one of: `"QUEUED"`,
  `"STARTED"`, `"FAILED"`, `"COMPLETED"`, `"FAILED_BY_EXT"`.

- cascade:

  Logical. If `TRUE`, and the `status` is a failure type (`"FAILED"` or
  `"FAILED_BY_EXT"`), the failure is recursively propagated to child
  jobs not listed in `exclude`.

- exclude:

  Character or numeric vector. One or more job IDs to exclude from
  cascading failure updates.

## Value

Invisibly returns `NULL`. Side effect is a modification to the SQLite
job tracking table.

## Details

The function updates both the job `status` and a timestamp corresponding
to the status type:

- `"QUEUED"` -\> updates `time_submitted`

- `"STARTED"` -\> updates `time_started`

- `"FAILED"`, `"COMPLETED"`, or `"FAILED_BY_EXT"` -\> updates
  `time_ended`

If `cascade = TRUE`, and the status is `"FAILED"` or `"FAILED_BY_EXT"`,
any dependent jobs (as determined via
[`get_tracked_job_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_tracked_job_status.md))
will be recursively marked as `"FAILED_BY_EXT"`, unless their status is
already `"FAILED"` or they are listed in `exclude`.

If `sqlite_db` or `job_id` is invalid or missing, the function fails
silently and returns `NULL`.
