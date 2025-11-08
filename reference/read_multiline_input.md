# Read Multi-Line User Input from Console

Prompts the user for multi-line input from the console until a specified
number of consecutive blank lines are entered. Optionally collapses the
result into a single string.

## Usage

``` r
read_multiline_input(
  instruct = NULL,
  prompt = "> ",
  n_blank = 1,
  collapse = NULL
)
```

## Arguments

- instruct:

  Optional character string providing initial instructions to display
  before prompting.

- prompt:

  Character string used as the prompt symbol for each input line.
  Defaults to `"> "` .

- n_blank:

  Integer indicating how many consecutive blank lines should terminate
  input. Defaults to 1.

- collapse:

  If non-`NULL`, collapses the returned lines into a single string using
  the given delimiter.

## Value

A character vector of input lines, or a single collapsed string if
`collapse` is specified.

## Examples

``` r
if (FALSE) { # \dontrun{
read_multiline_input("Enter notes (press Enter twice to finish):")
} # }
```
