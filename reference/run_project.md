# Run the processing pipeline

Run the processing pipeline

## Usage

``` r
run_project(
  scfg,
  steps = NULL,
  subject_filter = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL,
  debug = FALSE,
  force = FALSE
)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

- steps:

  Character vector of pipeline steps to execute (or `"all"` to run all
  steps). Options are c("flywheel_sync", "bids_conversion", "mriqc",
  "fmriprep", "aroma", "postprocess", "extract_rois"). If `NULL`, the
  user will be prompted for which steps to run.

- subject_filter:

  Optional character vector or data.frame specifying which subjects (and
  optionally sessions) to process. When `NULL` and run interactively,
  the user will be prompted to enter space-separated subject IDs (press
  ENTER to process all subjects). When a data.frame is provided, it must
  contain a `sub_id` column and may include a `ses_id` column to filter
  on specific subject/session combinations.

- postprocess_streams:

  Optional character vector specifying which postprocessing streams
  should be run. If ``` "postprocess"`` is included in  ```steps\`, then
  this setting lets the user choose streams. If NULL, all postprocess
  streams will be run.

- extract_streams:

  Optional character vector specifying which ROI extraction streams
  should be run. If ``` "extract_rois"`` is included in  ```steps\`,
  then this setting lets the user choose streams. If NULL, all
  extraction streams will be run.

- debug:

  A logical value indicating whether to run in debug mode (verbose
  output for debugging, no true processing).

- force:

  A logical value indicating whether to force the execution of all
  steps, regardless of their current status.

## Value

A logical value indicating whether the processing pipeline was
successfully run.

## Examples

``` r
  if (FALSE) { # \dontrun{
    # Assuming you have a valid project configuration list named `study_config`
    run_project(study_config, prompt = TRUE, force = FALSE)
  } # }
```
