# Schedule subject-level processing

Schedule subject-level processing

## Usage

``` r
submit_subjects(
  scfg,
  steps,
  subject_filter = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL,
  parent_ids = NULL
)
```

## Arguments

- scfg:

  A bg_project_cfg object

- steps:

  Named logical vector of steps

- subject_filter:

  Optional subject/session filter (character or data.frame)

- postprocess_streams:

  Optional character vector of postprocess streams

- extract_streams:

  Optional character vector of extraction streams

- parent_ids:

  Optional character vector of job IDs to depend on

## Details

This function is not meant to be called by users! Instead, it is called
internally after flywheel sync completes.
