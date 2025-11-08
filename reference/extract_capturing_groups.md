# helper function to extract capturing groups from a string

helper function to extract capturing groups from a string

## Usage

``` r
extract_capturing_groups(strings, pattern, groups = NULL, sep = "_", ...)
```

## Arguments

- strings:

  a character vector containing the strings to be processed

- pattern:

  a regex pattern to match the strings

- groups:

  a numeric vector specifying the indices of the capturing groups to be
  extracted. Default: `NULL`, which extracts all capturing groups.

- sep:

  a character string to separate the captured groups. Default: `"_"`.

- ...:

  additional arguments passed to `regexec` (e.g., `perl = TRUE`)

## Value

a character vector containing the captured groups

## Details

This function uses the `regexec` and `regmatches` functions to extract
the capturing groups from the strings. The function returns a character
vector containing the captured groups. If no matches are found, `NA` is
returned.
