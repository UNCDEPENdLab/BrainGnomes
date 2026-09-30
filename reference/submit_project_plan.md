# Submit a saved or in-memory request plan

The saved configuration, requested stages and streams, and resolved
subject scope are reused. Deferred scope is discovered after Flywheel
synchronization. Retry plans also retain the exact work units and
source-run provenance. The plan itself is not the final scheduler
contract: BrainGnomes writes an immutable manifest immediately before
each job is submitted and a runtime receipt when that job starts.

## Usage

``` r
submit_project_plan(plan, debug = FALSE, log_level = "INFO")
```

## Arguments

- plan:

  A `bg_project_plan` object or YAML plan path.

- debug:

  Enable debug submission mode.

- log_level:

  Pipeline log threshold.

## Value

A `bg_project_run` object returned by
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md).
