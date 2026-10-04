# Diagnose failed project work

Reports unresolved failed, cancelled, and blocked work and locates
matching logs. In an interactive R session, the default opens the guided
dependency and log browser. In a non-interactive session, the default
returns a structured diagnosis of the current project state assembled by
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
across runs. Select a run for a historical post-mortem or set
`interactive` explicitly when behavior must not depend on the session.

## Usage

``` r
diagnose_project(
  input = getwd(),
  run_id = NULL,
  interactive = base::interactive(),
  subject_id = NULL,
  job_id = NULL
)
```

## Arguments

- input:

  A project configuration object, YAML file, project directory, or a
  `bg_project_inspection` object returned by
  [`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md).
  Defaults to the current working directory.

- run_id:

  Optional run ID. Use `NULL` for current project failures across runs,
  `"latest"` for the newest run, or an explicit historical run ID.

- interactive:

  If `TRUE`, open the guided interactive browser. If `FALSE`, return a
  structured diagnosis. The default is
  [`base::interactive()`](https://rdrr.io/r/base/interactive.html), so
  console users get the guided browser while scripts, tests, and reports
  get structured output. Both modes support subject, job, and historical
  run selection.

- subject_id:

  Optional subject identifier, with or without the `sub-` prefix.
  Restricts both structured and interactive diagnosis to that subject
  while preserving the selected run scope.

- job_id:

  Optional scheduler job identifier. Opens or returns that exact tracked
  job; this can select a historical job when `run_id` is `NULL`.

## Value

When `interactive = FALSE`, a `bg_project_diagnosis` object containing
the underlying `inspection`, its `jobs`, unresolved `failures`, and
matching `logs` for the current project state or selected run.
Interactive mode returns the selected result from the guided browser,
usually invisibly.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for routine progress monitoring and
[`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
to preview a new run after correcting a failure.

## Examples

``` r
if (FALSE) { # \dontrun{
diagnose_project(scfg) # guided browser in an interactive R session

diagnosis <- diagnose_project(scfg, subject_id = "014", interactive = FALSE)
diagnosis$failures
diagnosis$logs

diagnose_project(scfg, job_id = "66273010")

old_run <- diagnose_project(scfg, run$run_id, interactive = FALSE)
} # }
```
