# Postprocess confounds to match fMRI operations

Handles optional spike regression file creation, filtering of confound
time series and writing of postprocessed confounds/regressor files. This
is used internally by
[`postprocess_subject()`](https://uncdependlab.github.io/BrainGnomes/reference/postprocess_subject.md).

## Usage

``` r
postprocess_confounds(
  proc_files,
  cfg,
  processing_sequence,
  output_bids_info,
  fsl_img = NULL,
  lg = NULL
)
```

## Arguments

- proc_files:

  List of files returned by
  [`get_fmriprep_outputs()`](https://uncdependlab.github.io/BrainGnomes/reference/get_fmriprep_outputs.md).

- cfg:

  Configuration list passed to `postprocess_subject`.

- processing_sequence:

  Character vector of enabled processing steps.

- output_bids_info:

  Named list of BIDS entities for the postprocessed output file

- fsl_img:

  Optional path to a Singularity image with FSL installed.

- lg:

  Logger object for messages.

## Value

Path to the nuisance regressor file or `NULL` if not created.
