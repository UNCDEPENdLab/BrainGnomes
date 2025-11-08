# Interactive Command-Line Argument Builder

Provides an interactive interface for entering, reviewing, editing, and
deleting command-line arguments. Accepts an optional starting list of
arguments and returns a modified or new set.

## Usage

``` r
build_cli_args(
  args = NULL,
  prompt = "> ",
  instruct = "Enter arguments (press Enter to finish): ",
  collapse = NULL
)
```

## Arguments

- args:

  Optional character vector of existing arguments to edit. If `NULL`, a
  new set of arguments is entered.

- prompt:

  Prompt string shown for each new argument line. Defaults to `"> "` .

- instruct:

  Instructional message shown at the start of the session.

- collapse:

  If non-`NULL`, collapses the final argument list into a single string
  using the specified delimiter.

## Value

A character vector of CLI arguments, or a single collapsed string if
`collapse` is specified.

## Examples

``` r
if (FALSE) { # \dontrun{
args <- build_cli_args()
cat("Final arguments:\n", paste(args, collapse = " "), "\n")
} # }
```
