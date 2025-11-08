# Configure ICA-AROMA denoising

This function configures the ICA-AROMA (Independent Component
Analysis-based Automatic Removal Of Motion Artifacts) step for
post-fMRIPrep processing.

## Usage

``` r
setup_aroma(scfg, fields = NULL)
```

## Arguments

- scfg:

  A project configuration object, as produced by
  [`load_project()`](https://uncdependlab.github.io/BrainGnomes/reference/load_project.md)
  or
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md).

- fields:

  A character vector of field names to prompt for. If `NULL`, all fields
  related to AROMA will be prompted.

## Value

A modified version of the `scfg` list with the `$aroma` entry added or
updated.

## Details

ICA-AROMA is a data-driven denoising method that identifies
motion-related independent components and removes them from BOLD time
series using non-aggressive regression. This step should be run **after
fMRIPrep** has completed. The settings configured here specify compute
resource usage (e.g., memory, cores, time), command-line options, and
scheduler-specific arguments for running AROMA on each subject/session.

By default, this function sets:

- `memgb`: 32 (memory in GB)

- `nhours`: 36 (max runtime in hours)

- `ncores`: 1 (number of CPU cores)

- `cli_options`: "" (any extra command-line flags for the wrapper)

- `sched_args`: "" (additional job scheduler directives)

Users may also opt to remove large intermediate AROMA outputs after
completion via the `cleanup` flag. These NIfTI/JSON files are not
required for applying AROMA during postprocessing and can be deleted to
save disk space. Cleanup is only available when fMRIPrep output spaces
do not include `MNI152NLin6Asym:res-2`; if that space is later added,
cleanup will be skipped.
