# Obtain user input from the console

Obtain user input from the console

## Usage

``` r
prompt_input(
  prompt = "",
  prompt_eol = ">",
  instruct = NULL,
  type = "character",
  lower = -Inf,
  upper = Inf,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  split = NULL,
  among = NULL,
  required = TRUE,
  uniq = FALSE,
  default = NULL,
  empty_keyword = "NA"
)
```

## Arguments

- prompt:

  The character string to display before the user input prompt (e.g.,
  `"Enter location"`).

- prompt_eol:

  The character string to place at the end of the prompt (e.g., `">"`).

- instruct:

  Instructions to display above the prompt (character string, or `NULL`
  for none).

- type:

  The expected type of the input. Must be one of `"numeric"`,
  `"integer"`, `"character"`, `"file"`, or `"flag"`.

- lower:

  For numeric inputs, the lowest valid value.

- upper:

  For numeric inputs, the highest valid value.

- len:

  The number of expected values to return. If `NULL`, any number of
  values is allowed.

- min.len:

  The minimum number of values required (if applicable).

- max.len:

  The maximum number of values allowed (if applicable).

- split:

  Characters to split the input string into multiple values (used when
  multiple values are expected).

- among:

  A vector of valid values that the input must match (or `NULL` for no
  restriction).

- required:

  If `TRUE`, the user must provide a value; if `FALSE`, pressing Enter
  yields an empty/default value.

- uniq:

  If `TRUE`, all entries must be unique.

- default:

  A default value to use if the user presses Enter without typing
  anything. If non-`NULL`, the prompt will show this default choice.

- empty_keyword:

  Optional single token (default `"NA"`) that the user can enter
  (case-insensitive) to explicitly return a missing value when `default`
  is non-`NULL` and `required = FALSE`. Use `NULL` to disable this
  shortcut entirely.

## Value

The user input, converted to the appropriate type (`numeric`, `integer`,
`character`, etc.), or the `default` if provided and no input is given.

## Details

The function will keep prompting the user until a valid input is
provided. It displays instructions and enforces constraints (e.g., value
range, length, uniqueness). When `empty_keyword` is supplied and
`default` is non-`NULL` with `required = FALSE`, typing the token
(case-insensitive) returns a missing value for the given `type` without
accepting the default.

## Note

This function is intended for interactive use and may not work as
expected in non-interactive environments (e.g., batch scripts).
