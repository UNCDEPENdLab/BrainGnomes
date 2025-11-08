# helper function to establish sqlite connection and submit query

helper function to establish sqlite connection and submit query

## Usage

``` r
submit_sqlite_query(
  str = NULL,
  sqlite_db = NULL,
  params = NULL,
  busy_timeout = 10
)
```

## Arguments

- str:

  Character query statement to execute

- sqlite_db:

  Character path to SQLite database

- params:

  Optional list of parameters to pass to statement

- busy_timeout:

  Time (in s) after which to retry write operations; default is 10 s

## Value

description
