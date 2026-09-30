# Set up a BrainGnomes project workflow for a new fMRI study

By default, this function opens the guided project-configuration
workflow. Set `interactive = FALSE` to create a portable project from
deterministic defaults without prompting. Non-interactive setup creates
the project and standard data directories and writes
`project_config.yaml`. Processing stages are disabled unless inherited
from a template. New guided configurations start by asking what data you
already have. Steps completed outside BrainGnomes are skipped, and
downstream choices remain explicit. Existing configured projects retain
their stage selections.

## Usage

``` r
setup_project(
  input = NULL,
  fields = NULL,
  project_name = NULL,
  project_directory = NULL,
  template = NULL,
  interactive = TRUE,
  overwrite = FALSE,
  reuse_template_paths = FALSE,
  starting_point = NULL
)
```

## Arguments

- input:

  A `bg_project_cfg` object, a path to a YAML file, or a project
  directory containing `project_config.yaml`. If a directory is supplied
  but the file is missing, `setup_project` starts from an empty list
  with a warning. This argument may also be `NULL` to create a new
  configuration from scratch. In non-interactive mode, `input` can be
  used as the starting template.

- fields:

  A character vector of fields to be prompted for. If `NULL`, all fields
  will be prompted for. Only available in interactive mode.

- project_name:

  Project label. Required in non-interactive mode. When supplied in
  interactive mode, it is used as the initial project name.

- project_directory:

  Project root directory. Required in non-interactive mode. When
  supplied in interactive mode, it is used as the initial project
  directory.

- template:

  Optional configuration object, YAML file, or project directory to use
  as a base. Supply either `input` or `template`, not both.

- interactive:

  Whether to use the guided configuration workflow. Defaults to `TRUE`
  to preserve the standard interactive R workflow.

- overwrite:

  Replace an existing `project_config.yaml` in non-interactive mode.

- reuse_template_paths:

  In non-interactive mode, retain the template's original paths,
  including outputs and its tracking database. Defaults to `FALSE`:
  project-owned directories are rebased into the new root, mutable
  external destinations are replaced, and a new database path is used.
  External input directories, shared TemplateFlow caches, and resource
  files such as containers, atlases, and licenses are retained. Enabled
  stages and processing settings are inherited. Set `TRUE` only to
  intentionally share the original project state.

- starting_point:

  Optional starting point for guided setup: `"flywheel"` (download
  DICOMs first), `"dicom"` (local DICOMs), `"bids"` (existing BIDS),
  `"fmriprep"` (existing derivatives plus the corresponding BIDS
  dataset), `"existing"` (inspect a project without changing it), or
  `"custom"` (choose stages individually). When omitted, new or
  headless-only configurations offer a menu; configured projects keep
  their settings. Only available with `interactive = TRUE` and
  `fields = NULL`.

## Value

A `bg_project_cfg` list containing the project configuration. New fields
are added based on user input or portable defaults. The configuration is
written to `project_config.yaml` in the project directory. Interactive
setup asks before replacing a changed file; non-interactive setup
requires `overwrite = TRUE`. Selecting `"existing"` instead returns the
loaded configuration invisibly after showing project status, without
saving it.
