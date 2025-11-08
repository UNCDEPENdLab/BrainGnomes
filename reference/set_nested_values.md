# Assign values to a nested list using key-value strings or named list

Parses assignments like `"a/b/c=10"` or named lists like
`list("a/b/c" = 10)` and returns `list(a = list(b = list(c = 10)))`.

## Usage

``` r
set_nested_values(assignments, sep = "/", lst = NULL, type_values = TRUE)
```

## Arguments

- assignments:

  Either a character vector of assignment strings (e.g., `"a/b=1"`) or a
  named list where names encode nested keys (e.g., `list("a/b" = 1)`).

- sep:

  A character used to separate keys. Default is `"/"`.

- lst:

  Optional list to update. If `NULL`, a new list is created.

- type_values:

  Logical; whether to convert character values to appropriate types.

  Supports values containing `=` characters; only the first `=` is
  treated as the separator between key and value. Subsequent `=`
  characters are preserved in the value.

## Value

A nested list with the specified keys and values.
