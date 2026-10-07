# Update Job Status in Tracking SQLite Database

Updates the status of a specific job in a tracking database, optionally
cascading failure status to downstream jobs.

## Usage

``` r
update_tracked_job_status(
  sqlite_db = NULL,
  job_id = NULL,
  status,
  output_manifest = NULL,
  cascade = FALSE,
  exclude = NULL,
  strict = FALSE
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

- output_manifest:

  Character string. Optional JSON manifest of output files to store when
  status is `"COMPLETED"`. See
  [`capture_output_manifest`](https://hallquistlab.github.io/BrainGnomes/reference/capture_output_manifest.md).

- cascade:

  Logical. If `TRUE`, and the `status` is a failure type (`"FAILED"` or
  `"FAILED_BY_EXT"`), the failure is recursively propagated to child
  jobs not listed in `exclude`.

- exclude:

  Character or numeric vector. One or more job IDs to exclude from
  cascading failure updates.

- strict:

  Logical. If `TRUE`, invalid tracking targets, failed SQLite writes,
  and unmatched job identifiers raise errors instead of warning or
  silently returning. Worker commands use this to require persisted
  updates.

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

A `"STARTED"` update also writes a one-time runtime receipt when the
tracking row has an associated job manifest. The receipt records the
compute host and verifies that the sealed manifest and execution-driving
files still match their submission-time checksums. A manifest configured
with the default `"fail"` drift policy stops execution when verification
fails.

When `status` is `"COMPLETED"` and `output_manifest` is provided, the
manifest is stored in the `output_manifest` column for later
verification. Completion status, its timestamp, and the manifest are
written atomically in one SQL update.

If `cascade = TRUE`, and the status is `"FAILED"` or `"FAILED_BY_EXT"`,
any dependent jobs (as determined via
[`get_tracked_job_status()`](https://hallquistlab.github.io/BrainGnomes/reference/get_tracked_job_status.md))
will be recursively marked as `"FAILED_BY_EXT"`, unless their status is
already `"FAILED"` or they are listed in `exclude`.

If no tracking row matches `job_id`, a warning is emitted and no
manifest/cascade updates are attempted, preventing silent status-update
failures.

If `sqlite_db` or `job_id` is invalid or missing, the function fails
silently and returns `NULL`, unless `strict = TRUE`.
