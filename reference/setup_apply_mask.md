# Configure brain masking for postprocessing

This function configures the optional step of applying a brain mask to
the functional MRI data in postprocessing. This step removes signal
outside the brain (e.g., in air or non-brain tissue) by zeroing out
voxels outside the specified mask. Users can define a custom mask file
or rely on a default mask derived from the preprocessing pipeline (e.g.,
fMRIPrep outputs).

## Usage

``` r
setup_apply_mask(ppcfg = list(), fields = NULL)
```

## Arguments

- ppcfg:

  a postprocessing configuration list (nested within scfg\$postprocess)

- fields:

  A character vector of fields to be prompted for. If `NULL`, all fields
  related to brain masking will be prompted.

## Value

A modified version of `ppcfg` with the `$apply_mask` entry populated.

## Details

This step is especially useful when preparing data for statistical
modeling, as it constrains the analysis to in-brain voxels and reduces
computational burden.
