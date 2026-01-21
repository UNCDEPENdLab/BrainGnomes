# Check whether a pipeline step is complete

Determines if the expected output directory and `.complete` marker are
present for a given subject/session and processing step.

## Usage

``` r
is_step_complete(
  scfg,
  sub_id,
  ses_id = NULL,
  step_name,
  pp_stream = NULL,
  verify_manifest = TRUE
)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

- sub_id:

  Subject identifier

- ses_id:

  Optional session identifier

- step_name:

  Name of the processing step

- pp_stream:

  Name of the postprocessing stream when `step_name` is "postprocess"

- verify_manifest:

  Logical. If TRUE and a manifest exists in the database, verify that
  output files still exist and match (default TRUE).

## Value

List containing `complete` (logical), `dir`, `complete_file`,
`db_status` (character or NA), `manifest_verified` (logical or NA), and
`verification_source` (character indicating how completion was
determined)
