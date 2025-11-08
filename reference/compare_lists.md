# Recursively Compare Two List Objects

Compares two list objects and prints a summary of any differences in
structure or values.

## Usage

``` r
compare_lists(old, new, path = "", max_diffs = 100)
```

## Arguments

- old:

  First list object.

- new:

  Second list object.

- path:

  Internal parameter to track the location within the nested structure
  (used recursively).

- max_diffs:

  Maximum number of differences to report (default: 20).

## Value

Invisibly returns `TRUE` if no differences are found; otherwise `FALSE`.
