# Apply temporal filtering to a 4D NIfTI image

Apply high-pass and/or low-pass temporal filtering to an fMRI time
series. By default this calls FSL's `fslmaths -bptf` but a Butterworth
filter implemented in `butterworth_filter_4d` can also be used. Filter
cutoffs are specified in Hz; for the FSL implementation they are
internally converted to sigma values in volumes using a standard
FWHM-to-sigma transformation.

## Usage

``` r
temporal_filter(
  in_file,
  out_file,
  low_pass_hz = NULL,
  high_pass_hz = NULL,
  tr = NULL,
  overwrite = FALSE,
  lg = NULL,
  fsl_img = NULL,
  method = c("fslmaths", "butterworth")
)
```

## Arguments

- in_file:

  Path to the input 4D NIfTI file.

- out_file:

  The full path for the file output by this step

- low_pass_hz:

  Upper frequency cutoff in Hz. Frequencies above this are removed
  (low-pass). Use `NULL` to omit the low-pass component (internally
  treated as `Inf`).

- high_pass_hz:

  Lower frequency cutoff in Hz. Frequencies below this are removed
  (high-pass). Use `NULL` or a non-positive value to omit the high-pass
  component (internally treated as `-Inf`).

- tr:

  Repetition time (TR) in seconds. Required to convert Hz to volumes.

- overwrite:

  Logical; whether to overwrite the output file if it exists.

- lg:

  Optional lgr object used for logging messages

- fsl_img:

  Optional Singularity image to execute FSL commands in a containerized
  environment.

- method:

  Character. "fslmaths" to use FSL's -bptf or "butterworth" for a
  Butterworth filter.

## Value

The path to the temporally filtered output NIfTI file.

## Details

The mean image is added back after filtering to preserve signal
intensity. Filtering is skipped if the output file already exists and
`overwrite = FALSE`.
