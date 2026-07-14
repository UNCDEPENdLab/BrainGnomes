# Identify volumes used to estimate the run reference intensity

All volumes are included when no matching metadata are available. When
an fMRIPrep confounds table is present, volumes marked as
non-steady-state are excluded. A matching pipeline censor vector is also
honored. These exclusions affect estimation of the voxelwise temporal
baselines and run reference intensity; the eventual multiplier is still
applied to every volume in the run.

## Usage

``` r
intensity_reference_frames(
  in_file,
  confounds_file = NULL,
  censor_file = NULL,
  lg = NULL
)
```

## Arguments

- in_file:

  Path to the input 4D BOLD NIfTI image.

- confounds_file:

  Optional path to an fMRIPrep confounds TSV file.

- censor_file:

  Optional path to a censor file containing one value per volume, where
  a positive value means that the volume is included.

- lg:

  Optional logger for warnings about incompatible metadata.

## Value

Logical vector with one value per BOLD volume. `TRUE` means that the
volume is used to estimate the intensity reference.
