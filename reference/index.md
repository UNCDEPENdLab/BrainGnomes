# Package index

## All functions

- [`add_tracked_job_parent()`](https://uncdependlab.github.io/BrainGnomes/reference/add_tracked_job_parent.md)
  : Add parent/child id relationship to tracking database
- [`automask`](https://uncdependlab.github.io/BrainGnomes/reference/automask.md)
  : Create an automatic brain mask from a NIfTI image (Rcpp
  implementation)
- [`butterworth_filter_4d()`](https://uncdependlab.github.io/BrainGnomes/reference/butterworth_filter_4d.md)
  : Apply a Butterworth Filter to a 4D NIfTI Image
- [`calculate_motion_outliers()`](https://uncdependlab.github.io/BrainGnomes/reference/calculate_motion_outliers.md)
  : Summarize framewise displacement outliers across runs
- [`cluster_job_submit()`](https://uncdependlab.github.io/BrainGnomes/reference/cluster_job_submit.md)
  : This function submits a single script to a high-performance cluster
  using a scheduler (Slurm or TORQUE). It accepts a vector of arguments
  to be passed to the scheduler and a vector of environment variables
  that should be passed to the compute node at job execution.
- [`construct_bids_filename()`](https://uncdependlab.github.io/BrainGnomes/reference/construct_bids_filename.md)
  : Construct BIDS-Compatible Filenames from Extracted Entity Data
- [`construct_bids_regex()`](https://uncdependlab.github.io/BrainGnomes/reference/construct_bids_regex.md)
  : Construct a Regular Expression for Matching BIDS Filenames
- [`diagnose_pipeline()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_pipeline.md)
  : Function for diagnosing errors in a run of the pipeline
- [`edit_project()`](https://uncdependlab.github.io/BrainGnomes/reference/edit_project.md)
  : Interactively edit a project configuration by field (field-guided)
- [`extract_bids_info()`](https://uncdependlab.github.io/BrainGnomes/reference/extract_bids_info.md)
  : Extract fields from BIDS filenames
- [`extract_rois()`](https://uncdependlab.github.io/BrainGnomes/reference/extract_rois.md)
  : Extract ROI timeseries and connectivity matrices
- [`filtfilt_cpp`](https://uncdependlab.github.io/BrainGnomes/reference/filtfilt_cpp.md)
  : Zero-Phase IIR Filtering via Forward and Reverse Filtering
- [`get_fmriprep_outputs()`](https://uncdependlab.github.io/BrainGnomes/reference/get_fmriprep_outputs.md)
  : Identify fMRIPrep-Derived Outputs for a NIfTI File
- [`get_postproc_output_files()`](https://uncdependlab.github.io/BrainGnomes/reference/get_postproc_output_files.md)
  : List postprocessed output files for a stream based on its input spec
- [`get_project_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_project_status.md)
  : Get processing status for all subjects
- [`get_step_title()`](https://uncdependlab.github.io/BrainGnomes/reference/get_step_title.md)
  : helper for retrieving a human-readable title for a pipeline step
- [`get_subject_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_subject_status.md)
  : Get processing status for a single subject
- [`get_tracked_job_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_tracked_job_status.md)
  : Query job status in tracking SQLite database
- [`getline()`](https://uncdependlab.github.io/BrainGnomes/reference/getline.md)
  : Read a Line of Input from the User in Both Interactive and
  Non-Interactive Sessions
- [`image_quantile`](https://uncdependlab.github.io/BrainGnomes/reference/image_quantile.md)
  : Compute Quantiles from a 3D or 4D NIfTI Image
- [`insert_df_sqlite()`](https://uncdependlab.github.io/BrainGnomes/reference/insert_df_sqlite.md)
  : helper function to insert a keyed data.frame into the sqlite storage
  database
- [`insert_tracked_job()`](https://uncdependlab.github.io/BrainGnomes/reference/insert_tracked_job.md)
  : Internal helper funciton to insert a job into the tracking SQLite
  database
- [`lmfit_residuals_4d`](https://uncdependlab.github.io/BrainGnomes/reference/lmfit_residuals_4d.md)
  : Apply Confound Regression to 4D fMRI Data Using Voxelwise Linear
  Models
- [`load_project()`](https://uncdependlab.github.io/BrainGnomes/reference/load_project.md)
  : Load a project configuration from a file
- [`natural_spline_4d`](https://uncdependlab.github.io/BrainGnomes/reference/natural_spline_4d.md)
  : Interpolate fMRI Time Series with Cubic Splines in a NIfTI File
- [`natural_spline_interp`](https://uncdependlab.github.io/BrainGnomes/reference/natural_spline_interp.md)
  : Cubic spline interpolation with natural spline and linear
  extrapolation
- [`parse_cli_args()`](https://uncdependlab.github.io/BrainGnomes/reference/parse_cli_args.md)
  : Parse CLI-style arguments into a nested list using args_to_df()
- [`postprocess_subject()`](https://uncdependlab.github.io/BrainGnomes/reference/postprocess_subject.md)
  : Postprocess a single fMRI BOLD image using a configured pipeline
- [`remove_nifti_volumes`](https://uncdependlab.github.io/BrainGnomes/reference/remove_nifti_volumes.md)
  : Remove Specified Timepoints from a 4D NIfTI Image
- [`resample_template_to_img()`](https://uncdependlab.github.io/BrainGnomes/reference/resample_template_to_img.md)
  : Resample TemplateFlow Mask to fMRIPrep Image Using Python
- [`run_bids_validation()`](https://uncdependlab.github.io/BrainGnomes/reference/run_bids_validation.md)
  : Run BIDS validation on the project BIDS directory
- [`run_fsl_command()`](https://uncdependlab.github.io/BrainGnomes/reference/run_fsl_command.md)
  : Run an FSL command with optional Singularity container support and
  structured logging
- [`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
  : Run the processing pipeline
- [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md)
  : Setup the processing pipeline for a new fMRI study
- [`summary(`*`<bg_project_cfg>`*`)`](https://uncdependlab.github.io/BrainGnomes/reference/summary.bg_project_cfg.md)
  : summary method for project configuration object
- [`summary(`*`<bg_status_df>`*`)`](https://uncdependlab.github.io/BrainGnomes/reference/summary.bg_status_df.md)
  : Summarize project status
- [`tracking_df_to_tree()`](https://uncdependlab.github.io/BrainGnomes/reference/tracking_df_to_tree.md)
  : helper for converting tracking data.frame into a multi-level
  data.tree hierarchy
- [`update_tracked_job_status()`](https://uncdependlab.github.io/BrainGnomes/reference/update_tracked_job_status.md)
  : Update Job Status in Tracking SQLite Database
- [`wait_for_job()`](https://uncdependlab.github.io/BrainGnomes/reference/wait_for_job.md)
  : This function pauses execution of an R script while a scheduled qsub
  job is not yet complete.
