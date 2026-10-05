# Save a project configuration to YAML

Writes the configuration to a file named `project_config.yaml` inside
the project's root directory. The function verifies that the output
directory exists and offers to create it. If a configuration file
already exists, the user is shown a summary of differences and asked
whether to overwrite the file. After confirmation, the final save uses
[`write_project_config()`](https://hallquistlab.github.io/BrainGnomes/reference/write_project_config.md)
so the destination is replaced atomically.

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
  The destination must be colocated with the project root.

## Value

Invisibly returns `scfg`.
