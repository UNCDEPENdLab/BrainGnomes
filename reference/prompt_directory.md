# Prompt for a directory path and confirm behavior if it does not exist

Prompts the user for a directory path. If it exists, optionally checks:

- ownership (check_owner)

- readability (check_readable; can try make_readable if owned)

- writability (check_writable; can try make_writable if owned) Any
  remaining issues are combined into a single proceed prompt.

## Usage

``` r
prompt_directory(
  check_owner = FALSE,
  check_readable = FALSE,
  check_writable = FALSE,
  make_readable = FALSE,
  make_writable = FALSE,
  default = NULL,
  ...
)
```

## Arguments

- check_owner:

  logical; if TRUE, confirm the directory is owned by the current user
  (or prompt to proceed).

- check_readable:

  logical; require directory to be readable (or confirm proceed).

- check_writable:

  logical; require directory to be writable (or confirm proceed).

- make_readable:

  logical; if TRUE and owned, add user-read (+x for dirs).

- make_writable:

  logical; if TRUE and owned, add user-write.

- default:

  Default directory path to suggest.

- ...:

  Additional args forwarded to
  [`prompt_input()`](https://uncdependlab.github.io/BrainGnomes/reference/prompt_input.md)
  (e.g., `instruct`, `prompt`).

## Value

A character scalar path (may or may not exist).
