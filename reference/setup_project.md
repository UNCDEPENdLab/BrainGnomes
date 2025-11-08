# Setup the processing pipeline for a new fMRI study

Setup the processing pipeline for a new fMRI study

## Usage

``` r
setup_project(input = NULL, fields = NULL)
```

## Arguments

- input:

  A `bg_project_cfg` object, a path to a YAML file, or a project
  directory containing `project_config.yaml`. If a directory is supplied
  but the file is missing, `setup_project` starts from an empty list
  with a warning. For `setup_project` only, this argument may also be
  `NULL` to create a new configuration from scratch.

- fields:

  A character vector of fields to be prompted for. If `NULL`, all fields
  will be prompted for.

## Value

A `bg_project_cfg` list containing the project configuration. New fields
are added based on user input, and missing entries are filled with
defaults. The configuration is written to `project_config.yaml` in the
project directory unless the user declines to overwrite an existing
file.
