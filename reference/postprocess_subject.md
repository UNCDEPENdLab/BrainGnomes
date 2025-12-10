# Postprocess a single fMRI BOLD image using a configured pipeline

Applies a sequence of postprocessing operations to a single
subject-level BOLD NIfTI file, as specified by the user-defined
configuration object. Operations may include brain masking, spatial
smoothing, ICA-AROMA denoising, temporal filtering, confound regression,
and intensity normalization. The function also optionally computes and
saves a filtered confounds file for downstream analyses.

## Usage

``` r
postprocess_subject(in_file, cfg = NULL)
```

## Arguments

- in_file:

  Path to a subject-level BOLD NIfTI file output by fMRIPrep.

- cfg:

  A list containing configuration options, including TR (`cfg$tr`),
  enabled processing steps (`cfg$<step>$enable`), logging
  (`cfg$log_file`), and paths to resources such as singularity images
  (`cfg$fsl_img`). A whole-brain mask is automatically generated using
  [`automask()`](https://uncdependlab.github.io/BrainGnomes/reference/automask.md)
  and used for relevant processing steps.

## Value

The path to the final postprocessed BOLD NIfTI file. Side effects
include writing a confounds TSV file (if enabled), and logging to a
subject-level log file.

## Details

The processing sequence can be enforced by the user
(`force_processing_order = TRUE`) or determined dynamically based on the
`enable` flags in the configuration. Intermediate NIfTI and confound
files are staged in a scratch workspace (under `cfg$scratch_directory`)
and final outputs are written or moved to the postprocessing output
directory. Logging is handled via the `lgr` package and is directed to
subject-specific log files inferred from BIDS metadata.

Required `cfg` entries:

- `tr`: Repetition time in seconds.

- `bids_desc`: A BIDS-compliant `desc` label for the output filename.

- `processing_steps`: Optional character vector specifying processing
  order (if `force_processing_order = TRUE`).

- `scratch_directory`: Optional directory used for staging intermediate
  files (defaults to [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  if unset).

- `project_name`: Optional project label used to organize scratch
  workspaces.

Optional steps controlled by `cfg$<step>$enable`:

- `apply_mask`

- `spatial_smooth`

- `apply_aroma`

- `temporal_filter`

- `confound_regression`

- `intensity_normalize`
