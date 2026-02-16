# Changelog

## BrainGnomes 0.8

Released 2026-02-14

- Add optional low-pass filtering of motion parameters before FD
  recomputation; rename notch config fields to
  `bandstop_min_bpm`/`bandstop_max_bpm` (deprecated:
  `band_stop_min`/`band_stop_max`).
- All HPC jobs are now tracked in detail by an SQLite database
- Job failures and other errors can now be investigated using
  `diagnose_pipeline`
- Added a new vignette, “Diagnosing Pipeline Runs”, that walks through
  [`get_project_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_project_status.md),
  [`get_subject_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_subject_status.md),
  and interactive use of
  [`diagnose_pipeline()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_pipeline.md)
- Improved error logging in HPC scripts so that success and failure are
  indicated more clearly
- Stale .fail files are removed when a newer .complete file exists,
  clarifying status of processing steps
- Jobs now write a manifest of files and times to the job tracking
  database for more thorough completeness tests
- Added optional low-pass filtering of motion parameters, matching
  Gratton
- Gracefully adjust motion filtering parameters if they fall above
  Nyquist at this TR
- Modify extract ROIs config to avoid input_regex and always generate it
  internally from postproc stream
- Add optional header row for postprocessed confounds TSVs, configurable
  via postprocess YAML and validated during setup
- Added extensive checks on write/permission issues with directories and
  files
- bugfix: Get CSF probseg image for MRIQC during prefetch
- [`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
  now skips TemplateFlow prefetch only when a prior successful prefetch
  covers requested spaces and the TemplateFlow manifest in job tracking
  still verifies; missing/deleted template files trigger re-prefetch.
- bugfix: preserve user-specified `metadata/log_directory` (including
  external paths) instead of always resetting to
  `<project_directory>/logs`.
- During postprocess setup, `confound_calculate` now offers guided
  prompts to add `framewise_displacement` when omitted, including
  whether to use motion-filtered FD and whether FD should be processed
  vs kept as `noproc` for QC/exclusion workflows.
- Increase consistency of instructions and formatting in
  [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md)
- bugfix: avoid spurious “Already disconnected” warnings on exit from
  [`diagnose_pipeline()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_pipeline.md)
- bugfix:
  [`diagnose_pipeline()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_pipeline.md)
  now respects configured `metadata/log_directory` instead of assuming
  `<project_directory>/logs`
- bugfix:
  [`diagnose_pipeline()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_pipeline.md)
  now matches subjects by exact `sub-<id>` tokens to avoid accidental
  partial matches
- bugfix: `run_bg_and_wait()` now suppresses and restores `ERR` trap
  handling around `wait`, so non-zero container exits can be reconciled
  against success tokens before jobs are marked failed.
- bugfix: shell trap handlers now attempt a best-effort SQLite status
  update to `FAILED` before exit, reducing `_fail`/DB mismatch after
  abrupt failures.
- bugfix:
  [`update_tracked_job_status()`](https://uncdependlab.github.io/BrainGnomes/reference/update_tracked_job_status.md)
  now warns when no tracking rows are updated for a job_id (instead of
  failing silently).
- bugfix:
  [`check_status_reconciliation()`](https://uncdependlab.github.io/BrainGnomes/reference/check_status_reconciliation.md)
  now checks `.fail` markers against DB status and reports mismatch
  details.

## BrainGnomes 0.7-5

Released 2026-01-13

- Add `calculate_motion_outliers` function to calculate motion outliers
  in a BIDS dataset
  - Returns mean FD alongside max FD
  - Includes task and run columns from BIDS info
  - Supports optional `output_file` argument to write results (CSV or
    TSV, with gzip support)
  - Defaults for notch filter: `bandstop_min_bpm = 12`,
    `bandstop_max_bpm = 18` BPM
- Use the `scratch_directory` for postprocessing images to avoid
  collisions and ensure that intermediates do not clog the output folder
- Check that python packages directory is writable prior to attempting
  to resample a stereotaxic template to an image; fall back to a managed
  `reticulate` environment if not.
- More robust postprocess logging (fallback log directory if requested
  location is unavailable) and clearer reporting of retained volumes
  during confound regression.
- bugfix: 0 values for temporal filter cutoffs now disable the
  corresponding low/high-pass filter components.
- bugfix: more complete handling of cases where confound
  calculate/regress is enabled, but no columns are specified.
- bugfix: ROI extraction now writes empty connectivity outputs (with
  warnings) when all ROIs are dropped after filtering.
- bugfix: add T2w to template pre-fetch so that fmriprep does not try to
  obtain this when users have T2w images
- bugfix: correct regex in postprocessing step substitution when using
  user-specified order
- bugfix: prevent spurious failure files for fMRIPrep/AROMA jobs that
  return non-zero exit codes despite successful completion

## BrainGnomes 0.7-4

Released 2025-11-23

- Add log messages for key R calls in pipeline, such as
  lmfit_residuals_4d
- Implement log levels to allow user to control log detail when calling
  run_project
- bug fixes for cases where confound calculate or confound regression
  are enabled, but no columns are specified
- Add file lock mechanism to avoid race condition on reticulate setup in
  resample_template_to_img
- bug fix for logger glue in run_fsl_command

## BrainGnomes 0.7-3

Released 2025-11-11

- bugfix: correctly handle unsigned integer data types in NIfTIs
- lmfit_residuals_4d now handles partial (ala fsl_regfilt) and full
  regression and is used for applying AROMA
- Nonaggressive and aggressive AROMA now supported
- fsl_regfilt.R wrapper script removed from pipeline – all regression
  now happens with lmfit_residuals_4d
- bugfix: args_to_df now tolerate multiple arguments after a hyphen
- Added pre-fetch step for TemplateFlow files so that network access can
  be turned off inside singularity containers, avoiding socket errors
  that crop up within python’s multiprocessing module.
- Amended PBS scripts to match current pipeline
- TemplateFlow prefetch now runs via dedicated Slurm/PBS scripts that
  mirror other steps, including trapping and logging
- bugfix: confound regression does not crash when scrubbing is disabled
- bugfix: incorporate additional BIDS entities into location of
  confounds file

## BrainGnomes 0.7-2

Released 2025-10-09

- Allow `min_vox_per_roi` to be specified as a percentage or proportion
  of atlas voxels during ROI extraction
- ROI extraction allows for an optional brain mask that is applied to
  the atlas and time series data
- Add support for an explicit empty response (returns `NA`) in
  `prompt_input` when a default is provided
- Support notch filtering of motion parameters prior to calculation of
  framewise_displacement
- Support different head sizes for calculation of framewise displacement
- `run_project` with `force=TRUE` enables `overwrite` for downstream
  operations, ensuring that steps are re-run
- Persist location of non-standard YAML file locations when loading from
  file.
- Pass through user-specified CLI to heudiconv, support overwrite and
  clearing the cache
- bugfix: look for MNI152NLin6Asym_res-2 recursively when verifying
  readiness for AROMA

## BrainGnomes 0.7-1

Released 2025-09-24

- UI/UX improvements to ask query enable/disable for postprocessing and
  ROI extraction during edit_project
- Do not ask about ROI extraction details if no postprocessing streams
  are defined
- Corrected validation of band-pass cutoffs and clarified temporal
  filtering prompts/documentation
- Improved log messages related to temporal filtering

## BrainGnomes 0.7

Released 2025-09-16

- Tested and vetted flywheel sync
- Defer subject processing loop when flywheel sync is the first step
- validate_project adds argument correct_problems to prompt user for
  corrections if requested
- run_project does not continue if no steps are requested
- check that the scratch_directory is writable when the project is
  loaded and prompt for a new directory if not
- bugfix: validate_project does not return a top-level ‘gap’ for
  postprocess when config is valid
- bugfix: edit_project allows enable/disable modifications
- bugfix: edit_project looks for missing config settings when enabling a
  previously disabled step
- bugfix: do not display postproc or extract menus when filling in
  configuration gaps

## BrainGnomes 0.6-1

Released 2025-09-04

- Added Rcpp automask function, mimicking AFNI 3dAutomask.
- Use automask to get approximate whole-brain mask for image quantiles
  in postprocessing (spatial_smooth, intensity_normalize)
- Use the user-specific mask file in apply_mask, if relevant
- Added prompt_directory, which asks for confirmation when user
  specifies a non-existent directory
- Support use of AROMA in postproc for outputs generated by fmriprep 23
  and before

## BrainGnomes 0.6

Released 2025-08-30

- Added ROI extraction workflow (`extract_rois`) with correlation
  options and a dedicated vignette.
- Introduced a Flywheel synchronization step for retrieving data.
- Added support for external BIDS and fMRIPrep directories, including an
  `is_external_path` helper, path normalization, and configurable
  `postproc_directory`.
- Enhanced postprocessing: new `output_dir` argument, direct output
  movement without symlinks, robust file handling, and optional AROMA
  cleanup with safety checks for MNI res-2 outputs.
- Refined project setup and validation by verifying directories before
  saving, prompting for required containers, and allowing projects to
  run without a postprocessing directory.
- Documentation and test improvements, including an expanded quickstart
  guide and instructions for building the FSL container.
