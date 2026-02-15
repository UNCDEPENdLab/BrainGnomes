# Optionally add framewise displacement to confound_calculate columns

If confound calculation is enabled and framewise displacement was not
explicitly requested, prompt the user to add it. Users can choose
whether to use FD recomputed after motion filtering (when enabled) and
whether FD should be processed with the same filtering/denoising steps
as the BOLD data (`columns`) or retained as an unprocessed QC covariate
(`noproc_columns`).

## Usage

``` r
maybe_add_framewise_displacement(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  Optional vector of fields being edited.

## Value

Modified `ppcfg`.
