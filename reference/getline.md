# Read a Line of Input from the User in Both Interactive and Non-Interactive Sessions

Provides a safe and portable way to prompt the user for input from the
terminal, working seamlessly in both interactive R sessions and
non-interactive `Rscript` sessions (when connected to a TTY).

## Usage

``` r
getline(prompt)
```

## Arguments

- prompt:

  A character string to display as the input prompt.

## Value

A character string with the user's input, or `NULL` (`R_NilValue`) if
input is cancelled via Escape (`ESC`, ASCII 27) or EOF (`Ctrl+D` or
equivalent).

## Details

In an interactive session (e.g., RStudio or R console), this function
delegates to base R's
[`readline()`](https://rdrr.io/r/base/readline.html). In non-interactive
contexts where a TTY is available, it switches the terminal to
non-canonical mode and performs low-level character-by-character input
handling with support for echoing, backspace editing, and cancellation
via Escape or EOF. Ctrl+C will terminate the process as usual.

- Supports visible typing and live editing with backspace.

- Returns early with `NULL` if the user presses Escape or EOF.

- Uses raw terminal input in non-interactive mode; requires that stdin
  is a TTY.

- Does **not** intercept `Ctrl+C`; this will terminate the process as
  normal.

## Examples

``` r
if (FALSE) { # \dontrun{
  # In Rscript or R console
  input <- readline_safe("Enter your name: ")
  if (!is.null(input)) cat("Hello,", input, "!\n")
} # }
```
