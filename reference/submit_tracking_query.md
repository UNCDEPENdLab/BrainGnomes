# Internal helper function to submit a query to the tracking SQLite database

Internal helper function to submit a query to the tracking SQLite
database

## Usage

``` r
submit_tracking_query(str, sqlite_db, params = NULL)
```

## Arguments

- str:

  Character string SQLite query

- sqlite_db:

  Path to SQLite database used for tracking

- params:

  List of parameters/arguments to be used in query
