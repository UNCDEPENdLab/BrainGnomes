# Save a project configuration to YAML

Writes the configuration to a file named `project_config.yaml` inside
the project's root directory. The function verifies that the output
directory exists, offering to create it or allowing the user to select
an alternate location. If a configuration file already exists, the user
is shown a summary of differences and asked whether to overwrite the
file.

## Usage

``` r
save_project_config(scfg, file = NULL)
```

## Arguments

- scfg:

  A `bg_project_cfg` object.

- file:

  Optional path for the YAML output. Defaults to
  `file.path(scfg$metadata$project_directory, "project_config.yaml")`.

## Value

Invisibly returns `scfg`.
