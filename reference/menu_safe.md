# Portable Menu Prompt for Interactive or TTY Sessions

This function mimics the behavior of base R's
[`menu()`](https://rdrr.io/r/utils/menu.html) function. In interactive
sessions (e.g., R console or RStudio), it calls
[`utils::menu()`](https://rdrr.io/r/utils/menu.html) directly. In
non-interactive but TTY-capable sessions (e.g., an `Rscript` run in a
terminal), it displays a numbered list of choices and uses standard
input to read the user's selection.

## Usage

``` r
menu_safe(choices, title = NULL)
```

## Arguments

- choices:

  A character vector of menu options to present to the user.

- title:

  Optional character string to display as the menu title.

## Value

An integer corresponding to the selected menu item (1-based index), or 0
if cancelled.

## Details

The menu allows selection of an option by entering the corresponding
number, with `0` used to cancel the selection (consistent with
[`menu()`](https://rdrr.io/r/utils/menu.html) behavior).

- If `menu_safe()` is called in an interactive R session, it defers to
  [`utils::menu()`](https://rdrr.io/r/utils/menu.html).

- In a non-interactive terminal (TTY), it prints the options and reads
  user input via `std::getline()`.

- If standard input is not connected to a terminal (e.g., piped input),
  the function returns 0 and prints a warning.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive R session
choice <- menu_safe(c("Apple", "Banana", "Cherry"), "Choose a fruit:")
if (choice == 0) cat("You cancelled the selection.\n")
else cat("You selected:", choice, "\n")

# From a terminal via Rscript
# Rscript -e 'Rcpp::sourceCpp("menu_safe.cpp"); menu_safe(c("Yes", "No"))'
} # }
```
