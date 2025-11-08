# Remove Censored Volumes and Update Confounds

Removes timepoints flagged in a censor file from a 4D NIfTI image. When
`confound_files` are provided, the corresponding rows in those files are
removed so that the time series remain aligned.

## Usage

``` r
scrub_timepoints(
  in_file,
  censor_file = NULL,
  out_file,
  confound_files = NULL,
  overwrite = FALSE,
  lg = NULL
)
```

## Arguments

- in_file:

  Path to the input 4D NIfTI file.

- censor_file:

  Path to the 1D censor vector used to identify volumes to remove.

- out_file:

  The full path for the file output by this step

- confound_files:

  Optional character vector of confound or regressor files to update
  alongside the fMRI data.

- overwrite:

  Logical; overwrite the output NIfTI if it exists.

- lg:

  Optional `Logger` object for message output.

## Value

The path to the scrubbed NIfTI image.
