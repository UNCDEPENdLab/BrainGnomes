# Select reference voxels and calculate the run intensity multiplier

Selects a fixed region of stable, positive-signal functional voxels from
the input BOLD image. It then measures the reference intensity after
enabled masking and smoothing: a 10% trimmed temporal mean is calculated
for each reference voxel, followed by the spatial median across voxels.
The function returns `target / reference_location`, which is applied to
the complete run before temporal denoising.

## Usage

``` r
prepare_intensity_reference(
  in_file,
  target = 10000,
  calibration_file = in_file,
  calibration_steps = character(),
  confounds_file = NULL,
  censor_file = NULL,
  automask_file = NULL,
  core_file = "",
  sidecar_file = NULL,
  lg = NULL
)
```

## Arguments

- in_file:

  Path to the input 4D BOLD NIfTI image used to choose the reference
  voxels and baseline-estimation volumes.

- target:

  Desired run reference intensity after scaling. Specifically, this is
  the desired spatial median across reference voxels of their
  10%-trimmed temporal means.

- calibration_file:

  Path to the 4D BOLD image after enabled masking and smoothing but
  before temporal denoising. The run reference intensity is measured on
  this image and the multiplier is applied to it. Defaults to `in_file`
  for direct use and backward compatibility.

- calibration_steps:

  Character vector naming processing steps completed before
  `calibration_file`.

- confounds_file:

  Optional path to an fMRIPrep confounds TSV file used to identify
  non-steady-state volumes.

- censor_file:

  Optional path to the pipeline censor vector used to omit censored
  volumes from reference estimation.

- automask_file:

  Optional path for the temporary conservative automask. If `NULL`, a
  temporary file is created and removed automatically.

- core_file:

  Output path for the fixed reference-region mask, named the
  reference-core mask in saved files and metadata.

- sidecar_file:

  Optional JSON provenance sidecar path.

- lg:

  Optional logger.

## Value

List containing paths to the reference-region outputs, the run reference
intensity (`reference_location`), requested `target`, calculated
`scale_factor`, logical baseline-estimation volume vector
(`include_frames`), and QA summaries.

## Details

This is the single internal reference-selection policy used by the
postprocessing pipeline. Its conservative quality thresholds are fixed
to support consistent scaling across projects rather than exposed as
tuning options. The input image must retain a finite, positive temporal
baseline; an image that has already been demeaned or residualized is
unsuitable.
