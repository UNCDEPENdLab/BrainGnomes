# Compute framewise displacement from motion parameters

Calculates Power-style framewise displacement (FD) from a matrix or data
frame containing the six rigid-body motion parameters. Rotations are
converted to displacements on the surface of a sphere with radius
`head_radius` (in millimetres) before summation. Optional arguments
mirror typical preprocessing steps (dropping initial dummy scans,
truncating to a final volume, or handling rotations recorded in
degrees).

## Usage

``` r
framewise_displacement(
  motion,
  head_radius = 50,
  columns = c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z"),
  rot_units = c("rad", "deg"),
  drop_volumes = 0L,
  last_volume = NULL,
  na_action = 0
)
```

## Arguments

- motion:

  Numeric matrix or data frame with columns for `rot_x`, `rot_y`,
  `rot_z`, `trans_x`, `trans_y`, and `trans_z`.

- head_radius:

  Radius of the head (in millimetres) used to convert rotational
  parameters to linear displacements. Defaults to 50 mm.

- columns:

  Character vector giving the column order expected in `motion`.
  Defaults to the six canonical fMRIPrep motion columns.

- rot_units:

  Unit of the rotation parameters (`"rad"` or `"deg"`). Values in
  degrees are internally converted to radians prior to differencing.

- drop_volumes:

  Number of leading timepoints to discard (e.g., if dummy scans were
  dropped from the fMRI data). Defaults to 0.

- last_volume:

  Optional index of the final volume to retain. If supplied, frames
  beyond `last_volume` are excluded before FD is computed.

- na_action:

  Replacement applied to any remaining `NA` values after subsetting.
  Defaults to `0`, matching the behavior used in
  [`notch_filter()`](https://uncdependlab.github.io/BrainGnomes/reference/notch_filter.md).

## Value

Numeric vector of framewise displacement values (same length as the
number of rows in `motion`). The first entry is zero by definition.
