# Interactive menu for managing postprocessing streams

Presents options to add, edit, delete, show, or finish editing
postprocessing streams. Used by both
[`setup_postprocess_streams()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_postprocess_streams.md)
and
[`edit_project()`](https://uncdependlab.github.io/BrainGnomes/reference/edit_project.md).

## Usage

``` r
manage_postprocess_streams(scfg, allow_empty = FALSE)
```

## Arguments

- scfg:

  A project configuration object, as produced by
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md).

- allow_empty:

  Logical indicating whether finishing with zero streams is permitted
  without confirmation.

## Value

Modified `scfg` with updated postprocessing streams
