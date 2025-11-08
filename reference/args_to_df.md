# Parse command-line arguments into a structured data frame

Converts a character vector of CLI-style arguments into a data frame
with fields for position, argument name, value, number of hyphens, and
whether the argument used an equals sign.

## Usage

``` r
args_to_df(arg_vec = NULL)
```

## Arguments

- arg_vec:

  A character vector of shell-style argument strings (e.g.,
  `"--arg=value"` or `"--arg value"`).

## Value

A data frame with one row per parsed argument and the following columns:

- argpos:

  The index of the original string in the input vector.

- lhs:

  The left-hand side of the argument (name).

- rhs:

  The right-hand side of the argument (value), or `NA` if none found.

- has_equals:

  Logical; `TRUE` if the argument used `=`, otherwise `FALSE`.

- nhyphens:

  The number of hyphens used in the argument prefix (1 or 2).

## Details

Supports both `--arg=value` and `--arg value` formats. Multi-token
values following a key are collapsed into a single space-separated
string.
