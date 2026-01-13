# Internal helper funciton to insert a job into the tracking SQLite database

Internal helper funciton to insert a job into the tracking SQLite
database

## Usage

``` r
insert_tracked_job(sqlite_db, job_id, tracking_args = list())
```

## Arguments

- sqlite_db:

  Path to SQLite database used for tracking

- job_id:

  Character string job ID

- tracking_args:

  List of tracking arguments for SQLite database
