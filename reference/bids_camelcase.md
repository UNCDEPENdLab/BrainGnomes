# Convert a string to BIDS-compatible camelCase

Removes hyphens/underscores and capitalizes the letter following them.
E.g., "task-ridl_name" -\> "taskRidlName".

## Usage

``` r
bids_camelcase(x)
```

## Arguments

- x:

  A character string.

## Value

A character string in camelCase form.

## Examples

``` r
if (FALSE) { # \dontrun{
  bids_camelcase("task-ridl_name")
  bids_camelcase("echo_time-series")
  bids_camelcase("space-mni152nlin2009casym")
} # }
```
