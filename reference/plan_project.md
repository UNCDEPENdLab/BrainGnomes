# Inspect or persist the resolved project execution model

`plan_project()` is optional inspection and automation tooling. It
records a request: the stages, streams, known subject/session scope,
resources, dependencies, and implicit setup work resolved at planning
time. It is not a pre-rendered scheduler job list. When Flywheel
synchronization can add data, the plan reports deferred scope and the
run records the realized subjects after synchronization. Each actual
scheduler submission is sealed separately in a job manifest.
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
resolves the same request model internally, so creating or submitting a
plan is not required for a direct run.

## Usage

``` r
plan_project(
  input = getwd(),
  steps = "all",
  subject_filter = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL,
  force = FALSE,
  allow_invalid = FALSE,
  quiet = FALSE
)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.
  Defaults to the current working directory.

- steps:

  Pipeline stages or `"all"`.

- subject_filter:

  Optional subject IDs or a data frame with `sub_id` and optionally
  `ses_id`.

- postprocess_streams:

  Optional postprocessing streams.

- extract_streams:

  Optional ROI-extraction streams.

- force:

  Include work whose completion markers would otherwise skip it.

- allow_invalid:

  Build the plan despite configuration validation errors.

- quiet:

  Suppress the printed plan.

## Value

A serializable `bg_project_plan` object. Its `preview$work` table lists
concrete subject/session/stage/stream units, input and output roots, log
locations, dependencies, resources, and current completion-marker
decisions. Console output is bounded; the table retains the full
selection. Deferred scope stays unknown until sync. Counts describe
requested work units, not runtime or cost estimates or an exact
scheduler job count.

## See also

[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
for the standard direct execution path.
