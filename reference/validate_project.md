# Validate the structure of a project configuration object

Validate the structure of a project configuration object

## Usage

``` r
validate_project(scfg = list(), quiet = FALSE, correct_problems = FALSE)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

- quiet:

  if TRUE, suppress messages about validation failures

- correct_problems:

  if TRUE, prompt user to correct validation failures. In this case, an
  amended scfg object will be returned. If FALSE, `validate_project`
  will simply return `TRUE/FALSE` to indicate whether the project is
  valid.

## Details

If `correct_problems = FALSE`, the return will be TRUE/FALSE to indicate
whether the project passed validation. If it did not, an attribute
called `'gaps'` will be added to the return containing the failed fields
in the nested list string syntax (e.g. `metadata/log_directory`).
