# Helper function to check whether a given file or directory exists and, optionally, is readable

Helper function to check whether a given file or directory exists and,
optionally, is readable

## Usage

``` r
validate_exists(
  input,
  description = "",
  directory = FALSE,
  prompt_change = FALSE,
  check_readable = TRUE
)
```

## Arguments

- input:

  a file or directory to check for existence

- description:

  a character string describing what this file is if we are prompted to
  change it

- directory:

  if TRUE, check whether a directory exists. If FALSE (default), check
  that the file exists

- prompt_change:

  if TRUE, if the file/directory exists, ask the user if they wish to
  change the value. If so, return FALSE

- check_readable:

  if TRUE, validation fails (return `FALSE`) when the file/directory
  exists but is not readable

## Value

a boolean (`TRUE/FALSE`) indicating whether the file or directory exists
and is valid
