# A Safe Version of `select.list()` for Interactive and TTY Use

Provides a cross-platform wrapper for R's
[`select.list()`](https://rdrr.io/r/utils/select.list.html) that works
both in interactive R sessions and in non-interactive `Rscript` sessions
at a terminal (TTY). If running interactively, it calls
[`utils::select.list()`](https://rdrr.io/r/utils/select.list.html). If
running in a non-interactive terminal, it displays a textual menu and
captures numeric input from the user.

## Arguments

- choices:

  A character vector of choices to present.

- title:

  Optional title string to display above the menu (like in
  [`select.list()`](https://rdrr.io/r/utils/select.list.html)).

- multiple:

  Logical; if `TRUE`, the user may select multiple items by entering
  space-separated numbers.

## Value

A character string or character vector of selections. Returns an empty
string (`""`) if the user cancels by entering `0`. If `multiple = TRUE`,
a character vector of selected items is returned.

## Details

This function is especially useful in command-line scripts that need
user input in a controlled and fallback-compatible way.

## See also

[`utils::select.list()`](https://rdrr.io/r/utils/select.list.html)

## Examples

``` r
if (FALSE) { # \dontrun{
  select_list_safe(c("Apples", "Oranges", "Bananas"), title = "Select fruit:")
} # }
```
