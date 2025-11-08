# Convert a nested list into CLI-style arguments using slash-separated keys

Recursively traverses a nested list and returns a character vector or
string of arguments like `--a/b=10 --c=25`.

## Usage

``` r
nested_list_to_args(lst, sep = "/", collapse = FALSE)
```

## Arguments

- lst:

  A named (possibly nested) list.

- sep:

  Separator used for nested keys (default is "/").

- collapse:

  Logical; if TRUE, returns a single space-separated string.

## Value

A character vector (or single string if `collapse = TRUE`) of CLI-style
arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
   nested_list_to_args(list(a = list(b = c(10, 11, 12)), c = 25))
} # }
```
