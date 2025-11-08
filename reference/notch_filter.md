# Notch filter regressors (esp. motion parameters) from the confounds file

Uses `iirnotch_r()` to design a single-frequency notch filter that
suppresses respiratory-related oscillations in the six rigid-body motion
parameters. The function filters the requested columns with
[`filtfilt_cpp()`](https://uncdependlab.github.io/BrainGnomes/reference/filtfilt_cpp.md)
(zero-phase), and optionally writes the updated confounds table to disk.

## Usage

``` r
notch_filter(
  confounds_dt = NULL,
  tr = NULL,
  band_stop_min = NULL,
  band_stop_max = NULL,
  columns = c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z"),
  add_poly = TRUE,
  out_file = NULL,
  padtype = "constant",
  padlen = NULL,
  use_zi = TRUE,
  lg = NULL
)
```

## Arguments

- confounds_dt:

  A data.table containing confound regressors to filter

- tr:

  The repetition time of the scan sequence in seconds. Used to check
  that the stop band falls within the

- band_stop_min:

  Lower bound of the notch stop-band, in breaths per minute. This will
  be converted to Hz internally.

- band_stop_max:

  Upper bound of the notch stop-band, in breaths per minute. This will
  be converted to Hz internally.

- columns:

  Columns in `confounds_dt` to filter. Defaults to the standard six
  rigid-body parameters: `rot_x`, `rot_y`, `rot_z`, `trans_x`,
  `trans_y`, `trans_z`.

- out_file:

  Optional output path. When provided, the filtered confounds table is
  written here (tab-delimited). If `NULL`, the modified table is
  returned invisibly.

- padtype:

  Passed to
  [`filtfilt_cpp()`](https://uncdependlab.github.io/BrainGnomes/reference/filtfilt_cpp.md);
  governs how the edges are padded before filtering. One of
  `"constant"`, `"odd"`, `"even"`, or `"zero"`. Defaults to `"constant"`
  to match SciPy.

- padlen:

  Optional integer pad length forwarded to
  [`filtfilt_cpp()`](https://uncdependlab.github.io/BrainGnomes/reference/filtfilt_cpp.md).
  If `NULL`, the default inside
  [`filtfilt_cpp()`](https://uncdependlab.github.io/BrainGnomes/reference/filtfilt_cpp.md)
  (`-1L`) is used.

- use_zi:

  Logical; whether to initialise the filter state using steady-state
  conditions (the default in
  [`filtfilt_cpp()`](https://uncdependlab.github.io/BrainGnomes/reference/filtfilt_cpp.md)).

- lg:

  Optional `Logger` (from `lgr`) used for status messages.

## Value

If `out_file` is `NULL`, the filtered confounds are returned as a
`data.table`. Otherwise the path to `out_file` is returned invisibly.
