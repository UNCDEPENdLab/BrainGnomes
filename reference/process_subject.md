# Preprocess a single subject

Preprocess a single subject

## Usage

``` r
process_subject(
  scfg,
  sub_cfg = NULL,
  steps = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL,
  parent_ids = NULL,
  sequence_id = NULL,
  permission_check_cache = NULL
)
```

## Arguments

- scfg:

  A list of configuration settings

- sub_cfg:

  A data.frame of subject configuration settings

- steps:

  A named logical vector indicating which steps to run

- postprocess_streams:

  Optional character vector of postprocess configuration names to run.
  If NULL, all available streams will be run.

- parent_ids:

  An optional character vector of HPC job ids that must complete before
  this subject is run.

- sequence_id:

  An identifying ID for a set of jobs in a sequence used for job
  tracking

- permission_check_cache:

  Optional environment used to memoize write-permission checks across
  repeated submissions.

## Value

A logical value indicating whether the preprocessing was successful

## Details

When postprocessing is requested without running `fmriprep`, the
function verifies that the expected fMRIPrep outputs exist. If the
configured fMRIPrep directory lies outside the project directory, only
the existence of the subject's directory is required. For fMRIPrep
directories inside the project directory, a `.complete` file in the
project's log directory is still necessary.
