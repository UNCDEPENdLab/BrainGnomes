# Parse CLI-style arguments into a nested list using args_to_df()

This function tokenizes command-line arguments using
[`args_to_df()`](https://uncdependlab.github.io/BrainGnomes/reference/args_to_df.md)
and builds a nested list by interpreting forward slashes in keys (e.g.,
`--a/b=10 11`) as nested structure.

## Usage

``` r
parse_cli_args(args, sep = "/", type_values = TRUE)
```

## Arguments

- args:

  A character vector (e.g., from `commandArgs(trailingOnly = TRUE)`).

- sep:

  A character used to separate nested keys. Default is `"/"`.

- type_values:

  Logical; whether to attempt to conver right-hand side strings to
  relevant data types using `type.convert`.

## Value

A nested list of parsed CLI arguments.
