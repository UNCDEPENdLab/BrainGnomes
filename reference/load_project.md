# Load a project configuration from a file

Load a project configuration from a file

## Usage

``` r
load_project(input = NULL, validate = TRUE)
```

## Arguments

- input:

  A path to a YAML file, or a project directory containing
  `project_config.yaml`.

- validate:

  Logical indicating whether to validate the configuration after
  loading. Default: TRUE

## Value

A list representing the project configuration (class
`"bg_project_cfg"`). If `validate` is TRUE, the returned object is
validated (missing fields may be set to NULL and noted).
