# BrainGnomes (development version)

* Reject image objects, spatial arrays, and binary payloads before writing
  provenance JSON, configuration snapshots, validation records, or QC inventories.
  Keep image files out of provenance companion directories.

* Keep intensity-reference NIfTI voxel arrays out of derivative JSON; record
  published companion paths and compact header geometry instead. Validate
  intensity normalization across all volumes in bounded chunks to avoid
  memory spikes on long runs. Recognize the externally validated template-mask
  SUSAN case of a 5 mm kernel on isotropic 2 mm images, while keeping other
  uncalibrated resolution/kernel combinations outside support.

* Match QC validation audits to missing outputs through filesystem aliases,
  retaining their failed status instead of incorrectly marking them invalid.
  Canonicalize missing-configuration diagnostics consistently across platforms.
  Keep setup menu text portable and bound headless CLI tests so inherited
  Windows consoles cannot stall the test suite.
* Resolve relative configuration paths from a colocated project YAML: the YAML
  establishes the project root, all runtime and scheduler paths become
  absolute, and project-contained paths are saved portably relative to that
  root. Reject detached or mismatched project configurations before execution,
  and remove unsafe `project_directory` changes from the ordinary field editor.
* Ask about motion filtering after optionally adding framewise displacement
  during postprocessing setup, ensuring the saved stream records an explicit
  motion-filter decision and can be passed directly to `run_project()`.
* Present temporal-filtering and intensity-normalization methods as descriptive
  numbered menus during guided setup instead of requiring users to type internal
  configuration values. Interactive prompts no longer insert an extra blank line
  between their final instruction and the input marker.
* Allow `BrainGnomes setup_project` (or `init`) with no project name to open the
  guided setup and prompt for the project name, project directory, and workflow
  settings. Supplying a project name retains prompt-free creation and defaults
  its project directory to the current working directory. Explicit project
  directories, configuration YAMLs, and saved run plans remain supported;
  overwriting a prompt-free configuration and retrying/cancelling work still
  require their existing explicit flags.
* Organize the website reference by task: setup, execution, monitoring and
  recovery, QC, provenance, and advanced tools.
* Remove `diagnose_pipeline()` and its export, without a compatibility wrapper.
  Use `diagnose_project()`; pass `interactive = TRUE` for the guided browser
  or `interactive = FALSE` for structured results. The CLI remains `diagnose`.

* Use the same selected-stage and stream configuration checks for plans, dry
  runs, and submission. Invalid licenses/resources now fail before project
  writes or scheduling, while unrelated stages/streams do not block selected
  work. Saved plans are revalidated at submission. Use `steps` (and optional
  stream selections) with `validate_project_config()` or `config validate` to
  inspect the same checks; exploratory invalid plans are explicitly labeled.

* Make CLI JSON output a single document on stdout, with progress, warnings,
  and log tails on stderr. Validate formats before acting, reject machine-format
  watch/interactive combinations, and require explicit stages for JSON runs.
* Make plans and dry runs concrete: show subject/session/stage/stream work,
  input/output and log locations, dependencies, per-job resources, and
  completion-marker skip decisions. Flywheel scope remains explicitly deferred.
  `run_project(dry_run = TRUE)` now returns a `bg_project_plan` instead of `TRUE`
  and no longer creates directories or repairs permissions. Full preview rows
  remain available in `plan$preview$work` and CLI JSON beyond the console limit.

* Resolve existing parent-directory aliases before rebasing missing template
  paths. Cloned projects now keep future Flywheel DICOM inputs and nested
  outputs under the new project on macOS, Windows, and symlinked filesystems,
  without creating those directories in the source project or shared inputs.

* Begin new guided setup with the user's starting point: DICOMs on Flywheel,
  local DICOMs, existing BIDS, or existing fMRIPrep derivatives. Preview the
  route, skip completed upstream stages, distinguish existing inputs from
  future download/output destinations, and retain explicit downstream choices.
  The menu also offers read-only project inspection and custom setup. Existing
  configured projects and targeted edits retain their selections; headless
  creation remains prompt-free. Use `starting_point` to preselect a guided route.

* Keep explicit `run_project()` arguments when choosing stages interactively,
  including `dry_run = TRUE`. The CLI preserves the same argument precedence.
* Retry exact failed subject/session/stage/stream combinations instead of
  recombining independent selections. Saved retry plans retain that scope and
  their source-run provenance; missing or ambiguous work fails safely.
* Isolate new projects created from templates: rebase project directories and
  give outputs, logs, scratch space, and tracking a fresh destination. Preserve
  external inputs and resource files, and inherit stage settings. Intentional
  sharing requires `reuse_template_paths = TRUE` (CLI: `--reuse-template-paths`).

* Restrict scheduler-contract checksums to recognized immutable execution
  inputs and explicitly selected environment artifacts. Mutable operational
  files, including SQLite tracking databases, logs, status markers, state
  files, receipts, and output manifests, remain recorded as environment
  metadata but no longer trigger false contract-drift failures when jobs start.

* Native postprocessing and ROI outputs now carry versioned JSON provenance,
  draft methods text, BibTeX, and available upstream citations and reports.
  Records distinguish executed, skipped, failed, and reused operations, retain
  actual confound columns and censor indices, and identify source files,
  atlas geometry, software, and applicable run/job records. Use
  `read_derivative_provenance()` to inspect a derivative without SQLite and
  `export_derivative_provenance()` to copy it with verified companions into a
  portable bundle. Generated methods always require human review. Existing
  derivatives without producer records are explicitly marked unknown.

* Add `collect_qc_inventory()`, `write_qc_inventory()`, and
  `render_qc_dashboard()` for study QC snapshots. R objects and TSV tables join
  configured workflow expectations, job history, derivative availability,
  postprocessing audits, original fMRIPrep motion summaries, MRIQC IQMs, censor
  masks, and regional voxel retention. The optional Quarto dashboard includes
  searchable React tables, interactive plots, expandable evidence, and snapshot
  downloads. Collection is read-only; no human review or inclusion decisions
  are implemented.
* Treat platform-equivalent filesystem spellings as one QC path identity,
  preventing duplicate derivative rows from Windows short/long path aliases.

* Consolidate project creation in `setup_project()`. It now supports portable,
  prompt-free creation with `interactive = FALSE`, including templates and
  overwrite control. The headless `BrainGnomes init` command uses this pathway,
  and the redundant `initialize_project()` entry point has been removed.
* Remove the redundant `doctor()` R alias before 1.0. Use
  `doctor_project()` for R preflight checks; the `BrainGnomes doctor` CLI
  command remains available.
* Harmonize project inputs across the R lifecycle API. `load_project()`,
  `edit_project()`, `validate_project_config()`, `doctor_project()`,
  `plan_project()`, `run_project()`, `run_bids_validation()`, status and log
  helpers, provenance inspection, retry, and cancellation now default to the
  current working directory and accept project configuration objects, YAML
  files, or project directories where applicable.
* Distinguish optional request plans from final scheduler contracts. Plans now
  report whether their subject scope is resolved or deferred. Flywheel runs
  record the realized post-sync scope before downstream submission; every
  tracked scheduler job receives an immutable pre-submission manifest and a
  compute-host runtime receipt that verifies execution-driving files. Structured
  work-unit and contract fields are stored in the tracking database with an
  automatic migration for existing projects.

# BrainGnomes 0.9-2

Released 2026-09-02

* Add opt-in nearest-neighbour atlas resampling to ROI extraction. Setting
  `allow_atlas_resampling: true` lets an atlas already in the BOLD coordinate
  space be placed onto a differing BOLD voxel grid. The atlas space is inferred
  from a formal BIDS `space-<label>` filename entity, with `atlas_space` as a
  fallback for legacy names. Exact grid matches remain untouched; conflicting
  or differing coordinate-space labels are rejected; resampled atlases must
  preserve every positive label and are cached by atlas content and target
  geometry for reuse across jobs.

* Extend `inspect_project()` with subject-wide focus and an active-job view.
  `subject_id` now restricts every structured resolution, while `$active`
  reports queue/runtime age, requested wall time, and noteworthy health states.
  Optional `refresh = TRUE` performs a read-only scheduler comparison and
  records differences in `$reconciliation`. Scheduler refresh and
  `wait_for_job()` share one internal adapter over the existing Slurm, TORQUE,
  and local-process query functions rather than maintaining parallel query
  implementations. Matching `status --sub-id`, `--view=active`,
  `--view=reconciliation`, and `--refresh` options are available in the CLI.

* Make `diagnose_project()` open its guided dependency and log browser by
  default in interactive R sessions, while scripts, tests, reports, and the
  non-interactive CLI continue to receive structured diagnosis data. Callers
  can still select either behavior explicitly with `interactive = TRUE/FALSE`.
  `inspect_project()`, `diagnose_project()`, and the deprecated
  `diagnose_pipeline()` now accept an explicit project directory or YAML path
  and default to the project in the current working directory when input is
  omitted. Guided diagnosis now starts with current unresolved problems,
  groups repeated stage/stream failures, and preserves every subject, run, and
  job selection while drilling down. Exact `subject_id` and `job_id` shortcuts
  are available in R and the CLI, and full run-wide job lists appear only when
  explicitly requested.

* Add `inspect_project()` as the single front door for database-backed project
  and run monitoring. Its compact dashboard integrates the newest attempt for
  each work unit across runs, while ordinary data frames expose project, run,
  stage, subject, subject-stage, attempt, and raw-job resolutions. Consolidate
  post-mortem investigation in `diagnose_project()`, add matching CLI status
  views, and deprecate the overlapping `get_project_runs()`, `get_run_jobs()`,
  and `diagnose_pipeline()` entry points.

* Make `run_project()` explain its pre-submission work in plain language,
  including subject discovery, run-record creation, and the first full read of
  a large container file. Large cohort submissions now report bounded progress
  through the subject list, and explicit subject filters are applied before
  session directories are inspected so small requested subsets do not scan the
  full project tree.
* Extend the established `setup_project()` -> `run_project()` workflow across the
  complete R and command-line lifecycle without adding required setup steps.
  Optional inspection tooling now provides non-mutating configuration validation,
  `doctor_project()` preflight checks, and serializable execution plans; direct runs
  resolve the same execution model exposed by plans. Run handles, tracked-run
  views, log discovery, non-interactive diagnosis, failed-job retry planning, and
  guarded scheduler cancellation support observation and recovery. Flywheel
  controller snapshots are now run-specific so concurrent submissions cannot
  overwrite one another.
* Record complete provenance before each run begins submission. Every run now
  retains its resolved request and subject scope, exact configuration, planned
  resources and dependencies, software/R/host/scheduler identity, and content
  fingerprints for selected containers and other execution-driving files.
  `get_run_provenance()` and the `BrainGnomes provenance` command expose the
  record together with current job-tracking rows; plan and retry origins are
  retained explicitly.
* Expand applied-user recovery documentation around the established workflow.
  The Quickstart and diagnosis guide now show how to select one run, inspect its
  failed jobs, logs, and provenance, preview a retry, submit it as a separate
  new run, decide whether to include downstream blocked work, and safely preview
  cancellation. Function and CLI help use the same plain-language behavior and
  safety guidance.
* Make the R recovery APIs preview-first as well: `retry_project_run()` and
  `cancel_project_run()` now default to `dry_run = TRUE`, and require an
  explicit `dry_run = FALSE` to submit or cancel work. Retry planning preserves
  postprocessing and extraction stream names containing underscores.
* Add an executable motion-QC vignette for `calculate_motion_outliers()`. It
  demonstrates raw and filtered FD summaries, strict threshold interpretation,
  unavailable-filter handling, run-level QC/exclusion exports, and the boundary
  between interactive QC summaries and postprocessing scrubbing.
* Make guided `setup_project()` and `edit_project()` saves use the same atomic
  configuration writer as non-interactive lifecycle tooling after the user
  confirms replacement of an existing file.
* Make guided project setup easier to scan with terminal-width section rules,
  wrapped instruction paragraphs, consistently indented lists, and prompts on
  their own lines. The shared prompt renderer retains the TTY-safe input path
  used by `Rscript`. Headless setup through `BrainGnomes setup_project` remains
  fully argument-driven.
* Recalibrate masked-SUSAN validation on real fMRIPrep BOLD data for the
  distinct no-input-mask, fMRIPrep-mask, and TemplateFlow-mask conditions.
  Validation now enforces the selected detrending-plus-MAD estimator, uses up
  to 96 timepoints deterministically distributed over the complete run (or all
  timepoints in shorter runs), reads only those volumes with RNifti, and cannot
  pass by extrapolating across input-mask, kernel-size, voxel-size, or sampling
  support. Calibration retains the full-run SUSAN threshold, temporal mean, and
  extents while estimating smoothness from the selected timepoints.
* Validate the promoted 3--8 mm masked-SUSAN models on independent fMRIPrep
  25.2.5 derivatives and a 96-volume postprocessing E2E fixture drawn across
  the complete run.
  The 10 mm stress kernel remains outside the supported calibration range.
* Allow Slurm and PBS fsaverage setup to copy with GNU `cp` when newer fMRIPrep
  containers do not provide `rsync`, while retaining the existing `rsync` path
  when present.
* Strengthen postprocessing validation so masking is replayed exactly on 32
  deterministic volumes distributed across the complete run (or every volume
  in shorter runs), interpolation preserves retained volumes and matches
  sampled natural-spline values, removed volumes match the censor vector in
  order, and AROMA/confound regression samples are deterministic, pre-selected,
  and spatially balanced across image resolutions.
* Allow up to `1e-4` absolute float32 conversion error when replaying values
  inside an applied mask, while retaining relative-error checks and requiring
  values outside the mask to remain finite and exactly zero.
* Make temporal-filter validation deterministic and pre-selected, require
  finite per-voxel stopband and passband evidence, verify that no-noise-IC
  AROMA output is actually unchanged, reject wholly invalid AROMA component
  requests, and fail every image validator when spatial NIfTI grid metadata
  change unexpectedly.
* Install `multitaper` as a required runtime dependency so strict
  temporal-filter validation is available in ordinary package installations.
  Preserve target qform/sform matrices and coordinate-system codes when
  resampling TemplateFlow masks, and automatically rebuild incompatible cached
  masks rather than failing later validation.
* Reduce peak memory use in `image_quantile()` by avoiding a second full image
  copy and whole-image inclusion bitmap; masked quantile values and numerical
  behavior are unchanged.
* Route postprocessing checks through a common validation runner: validator
  errors now obey the configured continue/stop policy, reused intermediates are
  checked, structured results are retained in a JSON audit beside the subject
  log, and final images remain staged until last-step validation completes.

# BrainGnomes 0.9-1

Released 2026-08-27

* Preserve temporal means during partial/non-aggressive AROMA regression even
  when retained mixing columns are not centered, and make AROMA validation
  replay the production intercept and mean-preservation settings.
* Retarget raw `desc-preproc` input regexes to each postprocessing stream's
  output description during ROI discovery, validate censor vectors, and avoid
  applying an original-length censor vector twice after physical scrubbing.
* Make Savitzky-Golay smoothing in temporal-filter validation safe for short
  spectra.

# BrainGnomes 0.9

Released 2026-08-25

* Document stage-specific runtime requirements in package metadata, the README, and Quickstart. Add an installed miniature project configuration and an executable local-onboarding vignette that clearly separates no-cluster helpers, submission-free dry runs, and scheduler/container-dependent project execution.
* Correct ROI-connectivity provenance and execution: estimator-specific filenames are now unique, scheduled extraction honors nested correlation settings, `cor.shrink` has a stable BIDS entity, and time-series-only extraction supports `cor_method = "none"`.
* Preserve the one-to-one association between multiple postprocessing input streams and their BIDS descriptions during ROI extraction. Ambiguous vector lengths now fail explicitly instead of selecting or combining unintended inputs.
* Apply interactively configured ROI masks, preserve every atlas label when an ROI is fully masked, and return schema- and dimension-stable time-series, connectivity, and diagnostic outputs for empty ROIs and entirely masked atlases.
* Report per-stream ROI-extraction state in project and subject status. Scheduled extraction now writes an explicit manifest of its actual time-series, connectivity, and diagnostic outputs so completion checks do not depend on a directory-wide snapshot.
* Honor `save_ts = FALSE` in scheduled extraction and reject contradictory extraction configurations before output is created.
* Align the CLI, `run_project()` help, examples, and vignettes with the seven supported submitted stages. BIDS validation remains project-configured but is submitted separately with `run_bids_validation()`; stream selection and dry-run output now expose resolved settings.
* Harden public and native interfaces with complete help signatures, working examples, stable empty data-frame schemas, finite image-quantile validation, zero-length and dimension checks, and clearer argument errors.
* Keep development-only calibration resources, local Codex files, audit reports, prior build products, and Python bytecode caches out of source packages and installed-package tests. Remove the obsolete `ROI_TempCorr.R` entry point after migrating its useful voxel-retention diagnostics into supported ROI extraction.
* Stabilize empty-result contracts: `extract_bids_info(character())` now returns its complete typed BIDS schema, and `get_project_status()` returns configured status columns even before any subject jobs exist. `image_quantile()` now rejects empty, missing, NaN, and infinite probability vectors before reading image data.
* Improve onboarding and release hygiene: generate Quickstart CLI help from the installed command, document the standalone BIDS-validation boundary consistently, show resolved postprocessing and extraction stream settings during dry runs, and remove the obsolete `ROI_TempCorr.R` installed entry point after migrating its useful diagnostics into `extract_rois()`.
* Add optional per-ROI voxel-retention diagnostics to ROI extraction. Reports
  separately track atlas size, optional-mask survival, BOLD-valid voxels,
  minimum-voxel requirements, retention status, and exclusion reasons; the
  scheduled extraction workflow includes requested diagnostics in its explicit
  output manifest.
* Preserve source FD in notch-filtered calculated confounds and write the
  recomputed series immediately beside it as `framewise_displacement_filtered`.
  The output respects the configured header setting and logs column order when
  headers are disabled; filtering-based scrubbing and confound regression use
  the filtered FD. Clearly reversed notch bounds are repaired automatically;
  skipped filters are logged and never produce a misleading filtered-FD label.
  When FD is selected as a processed confound, both the source-derived and
  notch-derived FD columns retain all configured BOLD-matched confound
  processing.
* Add a `voxel_psc` intensity-normalization mode that uses the existing robust reference-core and eligible-frame policy and applies denominator-guarded baseline-to-100 scaling after spatial processing. Reliable local baselines use ordinary PSC scaling, very low positive baselines use a lower denominator bound, and invalid baselines or those with too few eligible frames use a conservative run-level fallback. The guards do not clip observations or mask voxels; the user's `apply_mask` decision is preserved, and the multiplier map and guard counts are saved for provenance. Guard counts and percentages within the conservative automask are logged at info level, with complete-grid counts at debug level.
* Add a user-oriented intensity-normalization vignette documenting the `target` convention, robust reference-core policy, provenance outputs, QA, and limitations.
* Replace `automask()`'s background-sensitive positive-voxel quantile interpolation with an iterative AFNI-style clip estimator and a smoothly varying local threshold field.
* Match AFNI's `automask()` peeling more closely with a 17-of-18 NN2 survival rule, layer-aware restoration, and post-peel face-connected reclustering.
* Replace postprocessing's late 4D-median intensity estimate with an automask-based robust reference core selected from the original positive-scale BOLD image; measure and apply the run factor after masking/smoothing but before AROMA, temporal filtering, confound regression, or timepoint removal, and save the core mask and JSON provenance.
* Accept `postprocess/intensity_normalize/target` as the simplified normalization setting while retaining `global_median` as a backward-compatible alias.
* Preserve each voxel's pre-AROMA temporal mean during both aggressive and non-aggressive AROMA denoising, retaining the positive baseline intensity used for cross-run scaling.
* Refactor postprocessing to use job arrays and sentinels for cleanup
* Add additional templates to prefetch needed by MRIQC
* Preserve user-specified `metadata/sqlite_db` values and expose `sqlite_db` in `edit_project()`.
* Clean postprocessing scratch workspaces and temporary automask files on errors as well as successful exits.
* Use exit-time cleanup for temporary FSL postprocessing files generated during temporal filtering, smoothing, confound regression, and brain-mask computation.
* Add regression tests for editable SQLite database configuration and postprocessing temp-file cleanup after failures.
* Prefetch resolution-1 T1w and brain-mask assets used by fMRIPrep anatomical reports, including when an output space explicitly requests another resolution.
* Recalibrate spatial-smoothing validation on held-out BOLD runs from three datasets and resolutions; expected post-smoothing FWHM now conditions on baseline smoothness, program/mask mode, and the voxel-to-kernel ratio.

# BrainGnomes 0.8-1

Released 2026-03-10

* Improve CLI interface to support --help or BrainGnomes <command> help
* Add CLI status command to get project status from command line
* Add dry_run option to run_project to see what would be run without executing it
* Refactor prefetch to accept cohort specifications and extend them to T2w fetch.
* Refactor prefetch to fall back to no desc field if desc:brain fails
* Expand fMRIPrep TemplateFlow defaults to include MNI152NLin2009cAsym boldref, res-2 brain mask, brain probseg, and carpet dseg assets observed during workflow construction
* Add conditional CIFTI TemplateFlow defaults so prefetch only stages MNI152NLin6Asym and fsLR sphere assets when fMRIPrep CLI options request `--cifti-output`
* Harden prefetch caching and validation checks so that later failures invalidate skip logic
* Make prefetch state query-specific so that an exact snapshot of templateflow files is retained
* Move prefetch state files out of `templateflow_home` and into hashed project log paths; legacy state files in `templateflow_home` are now migrated and removed to avoid poisoning TemplateFlow standard-space discovery.
* Expand TemplateFlow default to desc=None for T1w to mirror some versions of fmriprep.
* Harden check on flywheel location to accommodate missing fw command.
* Update RSQLite connections to default to `synchronous=NULL` to prevent spurious warnings
* Included OASIS30 as a default template space for prefetch because it is used by fmriprep
* bugfix: preserve `cohort-<n>` in BIDS parsing/reconstruction so postprocessing can resolve cohort-qualified fMRIPrep outputs such as `space-MNIPediatricAsym_cohort-2`

# BrainGnomes 0.8

Released 2026-02-17

* Add optional low-pass filtering of motion parameters before FD recomputation; rename notch config fields to
  `bandstop_min_bpm`/`bandstop_max_bpm` (deprecated: `band_stop_min`/`band_stop_max`).
* All HPC jobs are now tracked in detail by an SQLite database
* Job failures and other errors can now be investigated using `diagnose_pipeline`
* Added a new vignette, "Diagnosing Pipeline Runs", that walks through `get_project_status()`, `get_subject_status()`,
  and interactive use of `diagnose_pipeline()`
* Improved error logging in HPC scripts so that success and failure are indicated more clearly
* Stale .fail files are removed when a newer .complete file exists, clarifying status of processing steps
* Jobs now write a manifest of files and times to the job tracking database for more thorough completeness tests
* Added optional low-pass filtering of motion parameters, matching Gratton
* Gracefully adjust motion filtering parameters if they fall above Nyquist at this TR
* Modify extract ROIs config to avoid input_regex and always generate it internally from postproc stream
* Add optional header row for postprocessed confounds TSVs, configurable via postprocess YAML and validated during setup
* Added extensive checks on write/permission issues with directories and files
* bugfix: Get CSF probseg image for MRIQC during prefetch
* `run_project()` now skips TemplateFlow prefetch only when a prior successful prefetch covers requested spaces and the TemplateFlow manifest in job tracking still verifies; missing/deleted template files trigger re-prefetch.
* bugfix: preserve user-specified `metadata/log_directory` (including external paths) instead of always resetting to `<project_directory>/logs`.
* During postprocess setup, `confound_calculate` now offers guided prompts to add `framewise_displacement` when omitted and to choose BOLD-matched processing vs `noproc` output for QC/exclusion workflows. When motion filtering is enabled, FD is recomputed from the filtered motion automatically.
* Increase consistency of instructions and formatting in `setup_project()`
* bugfix: avoid spurious "Already disconnected" warnings on exit from `diagnose_pipeline()`
* bugfix: `diagnose_pipeline()` now respects configured `metadata/log_directory` instead of assuming `<project_directory>/logs`
* bugfix: `diagnose_pipeline()` now matches subjects by exact `sub-<id>` tokens to avoid accidental partial matches
* bugfix: `run_bg_and_wait()` now suppresses and restores `ERR` trap handling around `wait`, so non-zero container exits can be reconciled against success tokens before jobs are marked failed.
* bugfix: shell trap handlers now attempt a best-effort SQLite status update to `FAILED` before exit, reducing `_fail`/DB mismatch after abrupt failures.
* bugfix: `update_tracked_job_status()` now warns when no tracking rows are updated for a job_id (instead of failing silently).
* bugfix: `check_status_reconciliation()` now checks `.fail` markers against DB status and reports mismatch details.

# BrainGnomes 0.7-5

Released 2026-01-13

* Add `calculate_motion_outliers` function to calculate motion outliers in a BIDS dataset
  - Returns mean FD alongside max FD
  - Includes task and run columns from BIDS info
  - Supports optional `output_file` argument to write results (CSV or TSV, with gzip support)
  - Defaults for notch filter: `bandstop_min_bpm = 12`, `bandstop_max_bpm = 18` BPM
* Use the `scratch_directory` for postprocessing images to avoid collisions and ensure that intermediates do not clog the output folder
* Check that python packages directory is writable prior to attempting to resample a stereotaxic template to an image; fall back to
    a managed `reticulate` environment if not.
* More robust postprocess logging (fallback log directory if requested location is unavailable) and clearer reporting of retained
    volumes during confound regression.
* bugfix: 0 values for temporal filter cutoffs now disable the corresponding low/high-pass filter components.
* bugfix: more complete handling of cases where confound calculate/regress is enabled, but no columns are specified.
* bugfix: ROI extraction now writes empty connectivity outputs (with warnings) when all ROIs are dropped after filtering.
* bugfix: add T2w to template pre-fetch so that fmriprep does not try to obtain this when users have T2w images
* bugfix: correct regex in postprocessing step substitution when using user-specified order
* bugfix: prevent spurious failure files for fMRIPrep/AROMA jobs that return non-zero exit codes despite successful completion

# BrainGnomes 0.7-4

Released 2025-11-23

* Add log messages for key R calls in pipeline, such as lmfit_residuals_4d
* Implement log levels to allow user to control log detail when calling run_project
* bug fixes for cases where confound calculate or confound regression are enabled, but no columns are specified
* Add file lock mechanism to avoid race condition on reticulate setup in resample_template_to_img
* bug fix for logger glue in run_fsl_command

# BrainGnomes 0.7-3

Released 2025-11-11

* bugfix: correctly handle unsigned integer data types in NIfTIs
* lmfit_residuals_4d now handles partial (ala fsl_regfilt) and full regression and is used for applying AROMA
* Nonaggressive and aggressive AROMA now supported
* fsl_regfilt.R wrapper script removed from pipeline -- all regression now happens with lmfit_residuals_4d
* bugfix: args_to_df now tolerate multiple arguments after a hyphen
* Added pre-fetch step for TemplateFlow files so that network access can be turned off inside singularity containers,
    avoiding socket errors that crop up within python's multiprocessing module.
* Amended PBS scripts to match current pipeline
* TemplateFlow prefetch now runs via dedicated Slurm/PBS scripts that mirror other steps, including trapping and logging
* bugfix: confound regression does not crash when scrubbing is disabled
* bugfix: incorporate additional BIDS entities into location of confounds file

# BrainGnomes 0.7-2

Released 2025-10-09

* Allow `min_vox_per_roi` to be specified as a percentage or proportion of atlas voxels during ROI extraction
* ROI extraction allows for an optional brain mask that is applied to the atlas and time series data
* Add support for an explicit empty response (returns `NA`) in `prompt_input` when a default is provided
* Support notch filtering of motion parameters prior to calculation of framewise_displacement
* Support different head sizes for calculation of framewise displacement
* `run_project` with `force=TRUE` enables `overwrite` for downstream operations, ensuring that steps are re-run
* Persist location of non-standard YAML file locations when loading from file.
* Pass through user-specified CLI to heudiconv, support overwrite and clearing the cache
* bugfix: look for MNI152NLin6Asym_res-2 recursively when verifying readiness for AROMA

# BrainGnomes 0.7-1

Released 2025-09-24

* UI/UX improvements to ask query enable/disable for postprocessing and ROI extraction during edit_project
* Do not ask about ROI extraction details if no postprocessing streams are defined
* Corrected validation of band-pass cutoffs and clarified temporal filtering prompts/documentation
* Improved log messages related to temporal filtering

# BrainGnomes 0.7

Released 2025-09-16

* Tested and vetted flywheel sync
* Defer subject processing loop when flywheel sync is the first step
* validate_project adds argument correct_problems to prompt user for corrections if requested
* run_project does not continue if no steps are requested
* check that the scratch_directory is writable when the project is loaded and prompt for a new directory if not
* bugfix: validate_project does not return a top-level 'gap' for postprocess when config is valid
* bugfix: edit_project allows enable/disable modifications
* bugfix: edit_project looks for missing config settings when enabling a previously disabled step
* bugfix: do not display postproc or extract menus when filling in configuration gaps

# BrainGnomes 0.6-1

Released 2025-09-04

* Added Rcpp automask function, mimicking AFNI 3dAutomask.
* Use automask to get approximate whole-brain mask for image quantiles in postprocessing (spatial_smooth, intensity_normalize)
* Use the user-specific mask file in apply_mask, if relevant
* Added prompt_directory, which asks for confirmation when user specifies a non-existent directory
* Support use of AROMA in postproc for outputs generated by fmriprep 23 and before

# BrainGnomes 0.6

Released 2025-08-30

* Added ROI extraction workflow (`extract_rois`) with correlation options and a dedicated vignette.
* Introduced a Flywheel synchronization step for retrieving data.
* Added support for external BIDS and fMRIPrep directories,
  including an `is_external_path` helper, path normalization, and configurable `postproc_directory`.
* Enhanced postprocessing: new `output_dir` argument, direct output movement without symlinks,
  robust file handling, and optional AROMA cleanup with safety checks for MNI res-2 outputs.
* Refined project setup and validation by verifying directories before saving,
  prompting for required containers, and allowing projects to run without a postprocessing directory.
* Documentation and test improvements, including an expanded quickstart guide and instructions for building the FSL container.
