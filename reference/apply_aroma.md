# Apply AROMA-based denoising to an fMRI image

Performs ICA-AROMA denoising by regressing out identified noise
components from an fMRI time series using the internal
[`lmfit_residuals_4d()`](https://uncdependlab.github.io/BrainGnomes/reference/lmfit_residuals_4d.md)
helper. When `nonaggressive = TRUE` (the default) only the unique
variance attributable to the specified components is removed, matching
FSL's non-aggressive `fsl_regfilt` behavior. Set `nonaggressive = FALSE`
for aggressive regression that fully removes the listed components.

## Usage

``` r
apply_aroma(
  in_file,
  out_file,
  mixing_file,
  noise_ics,
  overwrite = FALSE,
  lg = NULL,
  nonaggressive = TRUE
)
```

## Arguments

- in_file:

  Path to the input 4D NIfTI file.

- out_file:

  The full path for the file output by this step

- mixing_file:

  Path to the MELODIC mixing matrix (e.g., `*_desc-MELODIC_mixing.tsv`).

- noise_ics:

  Vector of ICA components to regress out (usually pulled from relevant
  aroma_timeseries.tsv file).

- overwrite:

  Logical; whether to overwrite the output file if it exists.

- lg:

  Optional lgr object used for logging messages

- nonaggressive:

  Logical; `TRUE` (default) performs partial regression to emulate
  non-aggressive AROMA. Set to `FALSE` for aggressive regression.

## Value

Path to the denoised output NIfTI file. If required files are missing,
returns `in_file` unmodified.
