# Run the project workflow

This remains the standard execution path after
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md).
It resolves the same stages, streams, subject/session scope, and force
setting exposed by
[`plan_project()`](https://hallquistlab.github.io/BrainGnomes/reference/plan_project.md)
before submission; calling
[`plan_project()`](https://hallquistlab.github.io/BrainGnomes/reference/plan_project.md)
first is optional. When stages are selected interactively, only omitted
arguments are prompted for. Explicit choices, including
`dry_run = TRUE`, are always retained.

## Usage

``` r
run_project(
  scfg = getwd(),
  steps = NULL,
  subject_filter = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL,
  debug = FALSE,
  force = FALSE,
  dry_run = FALSE,
  log_level = c("INFO", "DEBUG", "WARN", "ERROR", "TRACE", "FATAL")
)
```

## Arguments

- scfg:

  A `bg_project_cfg` object, YAML configuration file, or project
  directory containing `project_config.yaml`. Defaults to the current
  working directory.

- steps:

  Character vector of project stages to execute. Supported stages are
  `"flywheel_sync"`, `"bids_conversion"`, `"mriqc"`, `"fmriprep"`,
  `"aroma"`, `"postprocess"`, and `"extract_rois"`. Use `"all"` to run
  all enabled stages. If `NULL`, the user will be prompted for which
  stages to run. BIDS validation is configured with the project but
  submitted separately through
  [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md);
  it is not a `run_project()` stage.

- subject_filter:

  Optional character vector or data.frame specifying which subjects (and
  optionally sessions) to process. When omitted and run interactively,
  the user will be prompted to enter space-separated subject IDs (press
  ENTER to process all subjects). Explicit `NULL` selects all subjects
  without prompting. When a data.frame is provided, it must contain a
  `sub_id` column and may include a `ses_id` column to filter on
  specific subject/session combinations.

- postprocess_streams:

  Optional character vector specifying which postprocessing streams
  should run. When `"postprocess"` is included in `steps`, `NULL`
  selects every configured postprocessing stream.

- extract_streams:

  Optional character vector specifying which ROI extraction streams
  should run. When `"extract_rois"` is included in `steps`, `NULL`
  selects every configured extraction stream.

- debug:

  A logical value indicating whether to run in debug mode (verbose
  output for debugging, no true processing).

- force:

  A logical value indicating whether to force the execution of all
  steps, regardless of their current status.

- dry_run:

  A logical value indicating whether to perform a dry run. Dry runs
  validate settings and report subject/session scope plus resolved
  postprocessing and extraction stream settings without submitting any
  jobs.

- log_level:

  Character string controlling log verbosity. One of `TRACE`, `DEBUG`,
  `INFO`, `WARN`, `ERROR`, or `FATAL`.

## Value

For submitted work, an invisible `bg_project_run` object containing the
run UUID, scheduler job IDs known at submission time, and the path to
the complete run provenance record. Dry runs invisibly return `TRUE`
after printing the resolved plan.

## Details

Before submission, BrainGnomes reports when it is checking project
folders, finding matching subjects, and saving the run record. The first
use of a large container in a project may take longer because
BrainGnomes reads the complete file once to identify the exact copy
used. During large submissions, periodic messages report progress
through the subject list.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
to monitor current progress;
[`get_run_provenance()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_provenance.md)
to read the recorded configuration, execution context, and artifact
fingerprints;
[`plan_project()`](https://hallquistlab.github.io/BrainGnomes/reference/plan_project.md)
for optional inspection or persistence of the resolved request;
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
to submit the BIDS validation configured with the project;
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
and
[`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
for optional recovery after a submitted run fails.

## Examples

``` r
  if (FALSE) { # \dontrun{
    # Assuming you have a valid project configuration list named `study_config`
    run_project(study_config, steps = "fmriprep", force = FALSE)
  } # }
```
