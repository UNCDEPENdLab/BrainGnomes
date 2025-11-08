# helper function that takes a character vector of CLI arguments and replaces matching old values with intended new values

helper function that takes a character vector of CLI arguments and
replaces matching old values with intended new values

## Usage

``` r
set_cli_options(args = NULL, new_values = NULL, collapse = FALSE)
```

## Arguments

- args:

  a character vector of existing CLI arguments

- new_values:

  a character vector of new CLI arguments to be substituted into `args`

- collapse:

  a flag indicating whether to collapse the return argument into a
  single string

## Value

a modified character vector of CLI arguments
