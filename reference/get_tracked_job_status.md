# Query job status in tracking SQLite database

Query job status in tracking SQLite database

## Usage

``` r
get_tracked_job_status(
  job_id = NULL,
  return_children = FALSE,
  return_parent = FALSE,
  sqlite_db
)
```

## Arguments

- job_id:

  The job id for which to retreive the status

- return_children:

  Return child jobs of this job

- return_parent:

  Return parent jobs of this job

- sqlite_db:

  Character string of sqlite database

## Value

An R data.frame version of the tracking database
