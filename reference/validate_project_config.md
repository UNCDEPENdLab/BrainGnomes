# Validate a BrainGnomes project configuration without changing it

This optional inspection entry point is useful for scripts, continuous
integration, and configuration review. It is not required before
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md),
which automatically uses the same checks for selected work. Unlike the
historical repair path in
[`validate_project()`](https://hallquistlab.github.io/BrainGnomes/reference/validate_project.md),
this function never opens the setup wizard and never writes the
configuration.

## Usage

``` r
validate_project_config(
  input = getwd(),
  quiet = FALSE,
  steps = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL
)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.
  Defaults to the current working directory.

- quiet:

  Suppress the printed validation summary.

- steps:

  Optional stages (or `"all"`) to validate with the same checks as
  plans, dry runs, and submission. NULL inspects the whole
  configuration.

- postprocess_streams:

  Optional selected postprocessing streams; requires steps.

- extract_streams:

  Optional selected ROI-extraction streams; requires steps.

## Value

A `bg_project_validation` object containing `valid`, `issues`,
`messages`, and the parsed `config`.

## Details

Selected-work checks exclude unrelated stages and streams. Configured
future output directories may be absent; required inputs, resource
settings, containers, and licenses must be valid. No directories are
created.
