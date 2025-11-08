# Determine if a path lies outside the project directory

Determine if a path lies outside the project directory

## Usage

``` r
is_external_path(path, project_dir)
```

## Arguments

- path:

  The path to evaluate.

- project_dir:

  The root project directory.

## Value

`TRUE` if `path` is not within `project_dir`, otherwise `FALSE`.
