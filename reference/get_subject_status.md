# Get processing status for a single subject

Get processing status for a single subject

## Usage

``` r
get_subject_status(scfg, sub_id, ses_id = NULL)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

- sub_id:

  Subject identifier.

- ses_id:

  Optional session identifier. When `NULL`, all sessions found in the
  subject's directory are returned.

## Value

A data.frame with columns indicating completion status and times for
each enabled step.
