# BrainGnomes Quickstart

## Introduction

BrainGnomes is an R package that streamlines the preprocessing and
analysis of fMRI data on high-performance computing (HPC) clusters. It
serves as a wrapper around common fMRI processing tools such as
HeuDiConv (for DICOM-to-BIDS conversion), BIDS Validator, MRIQC (quality
control), fMRIPrep (preprocessing pipeline), and ICA-AROMA (automatic
removal of motion artifacts), orchestrating their execution in a
coherent pipeline. BIDS validation is configured with the project but
submitted separately through
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md).
The package also provides additional processing steps for preparing fMRI
data for analysis, such as spatial smoothing, temporal filtering, and
confound regression. BrainGnomes uses containerized software
(Singularity images) to ensure reproducible environments for imaging
tools and it manages job submission to HPC schedulers (SLURM or TORQUE).
In this tutorial, we will walk through the full workflow of setting up
and running an fMRI preprocessing study using BrainGnomes. We will cover
the three key functions:

- setup_project(): Initialize a new study configuration (either from
  scratch or from an existing YAML configuration file)
- edit_project(): Interactively review and modify the study
  configuration by sections
- run_project(): Execute the preprocessing pipeline steps on your
  dataset, utilizing the HPC environment

Along the way, we assume you have the required Singularity container
images for each tool and a working HPC environment. Additional
information about setting up the compute environment is provided in
[BrainGnomes Singularity container
setup](https://hallquistlab.github.io/BrainGnomes/articles/building_containers.md).
This vignette provides example R code snippets, explains expected inputs
and outputs, and offers troubleshooting tips for common issues. By the
end of the guide, a new user should understand how to configure a study
and launch the BrainGnomes pipeline from start to finish.

Full pipeline submission is designed for HPC clusters using SLURM or
TORQUE/PBS. The package itself can be installed and loaded on a standard
computer, where configuration inspection, BIDS helpers, status-table
handling, and native imaging helpers remain available. See [Local
onboarding and
prerequisites](https://hallquistlab.github.io/BrainGnomes/articles/local_onboarding.md)
for an executable no-cluster introduction.

### Basic process flow of BrainGnomes

BrainGnomes identifies data for all subjects (and sessions) in your
DICOM and BIDS folders, then determines which subjects need to be
processed. It processes data by looping over subjects, submitting jobs
for each subject and processing step to the HPC scheduler. This ensures
that each subject is processed fully (unless a crash occurs).

![BrainGnomes pipeline
flow](../../../_temp/Library/BrainGnomes/extdata/braingnomes_flow.png)

### Running a subset of subject or a subset of processing steps

As detailed below, the
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
function submits fMRI processing jobs to the scheduler. Although
BrainGnomes is broadly intended to run all processing steps and all
subjects, you can also run a subset of subjects. In the interactive mode
of
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md),
you can enter subject IDs when you do not want to run all of them.

As detailed below, a BrainGnomes project can be configured to run only
some steps (for example, fMRIPrep and postprocessing). In this case, you
will be asked only about these steps in
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md).
Even if you have enabled a step, however, you can choose not to run it
in
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md).

For example, if you only wish to run postprocessing stream “postproc1”
on subject 540311, you might respond to the prompts like this:

    Enter subject IDs to process, separated by spaces. Press enter to process all subjects. 

     (Press enter to skip) > 540311

    Please select which steps to run:
    Run MRIQC? 
     (yes/no) > n
    Run fmriprep? 
     (yes/no) > n
    Run ICA-AROMA? 
     (yes/no) > n
    Run postprocessing? 
     (yes/no) > y
    Which postprocessing streams should be run? Press ENTER to select all.

    1:   postproc1
    2:   rest

    Enter one or more numbers separated by spaces and then ENTER, or 0 to cancel
    1: 1

You can achieve the same thing by passing in arguments for `steps`,
`postprocess_streams`, and `subject_filter`, which will schedule the
jobs without prompting you.

    run_project(scfg, steps="postprocess", postprocess_streams="postproc1", subject_filter="540311")

## Prerequisites and Setup

BrainGnomes is designed to support end-to-end processing of fMRI data,
from original DICOM files to postprocessed data ready for analysis. That
said, you are not required to set up or run every step. For example, if
your data have already been processed through fMRIPrep, you can use
BrainGnomes only for postprocessing. During setup, container questions
for steps you do not intend to run can be skipped.

Before using `BrainGnomes`, identify the requirements for the stages you
plan to enable:

| Capability or stage | Requirements beyond the installed R package |
|----|----|
| Local configuration/BIDS/image helpers | None; no scheduler or container is needed |
| Any scheduled stage | SLURM or TORQUE/PBS, Bash, and accessible project, scratch, and log storage |
| Flywheel synchronization | Flywheel `fw` CLI and account access |
| DICOM-to-BIDS conversion | HeuDiConv container, DICOM inputs, and a study heuristic |
| BIDS validation | BIDS validator; submitted separately through [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md) |
| MRIQC | MRIQC container |
| fMRIPrep | fMRIPrep container, BIDS inputs, TemplateFlow cache, and FreeSurfer license |
| ICA-AROMA | fMRIPost-AROMA container |
| Postprocessing | FSL container; Python with `nibabel`, `nilearn`, and `templateflow` for template-mask resampling |
| ROI extraction | Postprocessed BOLD and compatible atlas/mask images; project-managed runs use the scheduler |

BrainGnomes batch scripts call `singularity`; Apptainer can be used when
it provides the compatible `singularity` command.

The corresponding setup checklist is:

1.  Installation: Install the BrainGnomes R package. The package is not
    on CRAN (yet), so for now, you must install from GitHub. Then load
    it in your R session:

``` r

# Install (if needed) and load BrainGnomes
# devtools::install_github("HallquistLab/BrainGnomes")
library(BrainGnomes)
```

2.  Working Python installation: During postprocessing, the pipeline
    supports masking the data by a brain mask that matches the
    stereotaxic space. For example, if your fmriprep data are in
    MNI152NLin2009cAsym with a resolution of 2mm, a custom brain mask
    released with the template can be applied to the data to remove
    non-brain voxels. This relies on TemplateFlow
    (<http://templateflow.org/>), a Python library that provides a
    number of templates. For this step to work, you must have a working
    Python installation.
3.  HPC Access: Access to an HPC cluster with a job scheduler supported
    by BrainGnomes (currently SLURM or TORQUE). You should know which
    scheduler your cluster uses.
4.  Singularity: Singularity must be available on the HPC system
5.  You must have downloaded or built the required container image files
    for:

- fMRIPrep (e.g., a .sif image of fMRIPrep)
- HeuDiConv (container for DICOM to BIDS conversion)
- MRIQC (container for MRI quality metrics)
- ICA-AROMA (if you plan to use ICA-AROMA for denoising)
- BIDS Validator (either a container or an installed binary for the BIDS
  validation tool) Make note of the filesystem paths to each of these
  container files or executables, as the configuration will require
  them.

6.  Data and Files:

- DICOM files for your study, organized in subject (and optionally
  session) folders.
- A heuristic file for HeuDiConv (a Python script defining how to
  translate DICOM filenames into BIDS format). You or your lab should
  have this .py file prepared for your study’s naming conventions.
- A FreeSurfer license file (e.g., license.txt or
  FreeSurferLicense.txt). fMRIPrep requires a valid FreeSurfer license
  to run. You can obtain one for free from the FreeSurfer website. Save
  the license file path for the configuration.
- Optionally, a TemplateFlow directory if you want to use a non-default
  location for TemplateFlow data (standard brain templates). If unsure,
  you can specify an empty or new directory and fMRIPrep will manage
  downloading templates there.

With these in place, we can proceed to create a project configuration.

## Creating a Project Configuration with `setup_project()`

The first step in using `BrainGnomes` is to create a project
configuration, which is a structured list containing the settings and
paths needed for your pipeline. This configuration can be created
interactively using
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md).
In typical use, call
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
with no arguments to start a new configuration from scratch. It prompts
for the required information, returns an object with your settings, and
saves a YAML file named `project_config.yaml` in the project directory.
In later sessions,
[`load_project()`](https://hallquistlab.github.io/BrainGnomes/reference/load_project.md)
reloads this file.

Let’s run
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
to initialize a new project config:

``` r

# Start an interactive setup for a new fMRI project
scfg <- setup_project()
```

### Choose your starting point

The first question is what data you already have, before any storage,
container, or scheduler questions. Choose the earliest step this project
still needs:

| Starting point | What setup does next |
|----|----|
| DICOMs are on Flywheel | Enable sync and DICOM-to-BIDS conversion. Ask where downloads will land and which directory will contain subject folders after sync. |
| DICOMs are already local | Skip sync; configure DICOM-to-BIDS conversion from the existing subject root. |
| A BIDS dataset already exists | Skip sync and conversion; ask about fMRIPrep, quality checks, and downstream analysis. |
| fMRIPrep outputs already exist | Skip sync, conversion, and fMRIPrep. Ask for the derivative root and corresponding BIDS dataset, then offer downstream steps. For new configurations, raw-data QC is left off. |

Setup previews the route and asks you to confirm it; declining returns
to the menu. Enter `0` at the menu to cancel before any configuration is
created or saved. Starting points do not select a denoising recipe:
ICA-AROMA, postprocessing, and ROI extraction remain explicit decisions.
Only enabled stages ask for their containers and job resources. MRIQC or
BIDS validation can be enabled later through
[`edit_project()`](https://hallquistlab.github.io/BrainGnomes/reference/edit_project.md)
when starting from existing derivatives.

Existing inputs must already be readable directories. A Flywheel
download destination or BIDS conversion output directory can be empty or
not exist yet. If Flywheel creates enclosing project folders, use the
expected subject root inside the download destination for DICOM
discovery. The Flywheel job runs before discovery of downstream
subjects; setup itself never downloads data or submits jobs.

The menu also offers **Inspect an existing BrainGnomes project**, which
opens status without changing its configuration, and **Custom setup**,
which lets you choose stages individually. For a known entry point:

``` r

scfg <- setup_project(starting_point = "flywheel")
# Other routes: "dicom", "bids", "fmriprep", "existing", "custom"
```

Already configured projects retain their choices when passed to
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md).
An untouched headless configuration receives the menu when opened in
guided setup. To deliberately change an existing project’s entry point,
supply `starting_point`; setup asks for confirmation before changing
upstream selections and retains other settings. `starting_point` is for
full interactive setup, not `interactive = FALSE` or targeted `fields`
edits.

### Project locations and stage settings

After the starting-point choice, setup asks relevant questions in the R
console:

- Project Name – A short name for your project (used for labeling and
  logging). Example: “MyStudy2025”.
- Project Directory – The root directory where all project outputs will
  be stored. You can provide a path (absolute or relative). If the
  directory does not exist, BrainGnomes will offer to create it for you.
  For example: “/proj/Longleaf/MyStudy2025” (this will contain
  subfolders for BIDS data, fMRIPrep outputs, logs, etc.).
- DICOM Directory – For local DICOMs, choose the existing folder
  containing subject (and optional session) subfolders. When starting
  from Flywheel, choose the expected subject root after sync; it need
  not exist yet. BIDS and fMRIPrep starting points skip this question.
- TemplateFlow Directory – Path to your TemplateFlow data (standard
  templates for fMRIPrep). If you have a central TemplateFlow directory
  (e.g., ~/templateflow or a shared path), provide it.
- Scratch Directory – A path for temporary scratch space. This is often
  on a fast storage (like \$TMPDIR or a scratch disk) that is not
  intended for long-term storage. If you’re not sure, you can use a
  sub-directory in your project directory (e.g.,
  “/proj/Longleaf/MyStudy2025/scratch”). Just make sure to keep an eye
  on the size of this folder and clean it up periodically if this isn’t
  done automatically by your HPC system.

Steps implied by the starting point are already selected, so their
enable questions are not repeated. You then choose optional downstream
work. Before saving, setup summarizes enabled stages and explains how to
preview without submitting jobs. For each enabled stage, you will be
asked to specify these HPC scheduler settings:

- The amount of memory needed for the job in GB.
- The number of hours needed to complete the job. (Always start a bit
  higher than you think you need to avoid killed jobs.)
- How many CPU cores should be requested for this job.
- Any other command line arguments that will be passed to the command
  (e.g., fmriprep)
- Any arguments that should be passed to the HPC scheduler (e.g., SLURM)
  beyond what the pipeline already handles itself.

### Flywheel sync setup

If your DICOM data live in a Flywheel project, BrainGnomes can
optionally run a Flywheel-to-filesystem synchronization step before any
downstream processing. This uses the Flywheel CLI (`fw sync`) to
download DICOMs to your project so that BIDS conversion (and later
steps) operate on a complete, current dataset.

- Enable in setup: Choose the Flywheel starting point (or answer “Run
  Flywheel sync?” with yes in custom setup; existing projects can use
  [`edit_project()`](https://hallquistlab.github.io/BrainGnomes/reference/edit_project.md)
  → “Flywheel Sync”). You will be prompted for:
  - Flywheel project URL (`source_url`), for example:
    `fw://hallquistm/momentum/`. Watch out for the trailing slash!
    Flywheel sync operates a lot like rsync, so having the trailing
    slash here means, “synchronize files within momentum.”
  - Drop-off directory for downloaded DICOMs
    (`metadata/flywheel_sync_directory`). This is typically your project
    DICOM folder.
  - Temporary directory for transfers
    (`metadata/flywheel_temp_directory`). Defaults under your scratch
    directory.
  - Whether to save audit logs (`flywheel_sync/save_audit_logs`). When
    enabled, a CSV is written under your project `logs/`.
  - Location of the Flywheel CLI binary
    (`compute_environment/flywheel`), i.e., the path to the `fw`
    executable.
- Get an API token: The Flywheel CLI requires an API key associated with
  your user. Generate one in the Flywheel web UI:
  - Navigate to your Flywheel site (for example,
    `https://flywheel.example.edu`).
  - Open your user profile → API Keys and create a key. A direct route
    in many deployments is `https://<your-flywheel-host>/#/profile` then
    the API Keys tab.
  - Copy the generated token and keep it secure.
- Log in with the Flywheel CLI before syncing: BrainGnomes does not
  perform `fw login` for you. You must complete CLI login once on the
  machine/account that will run the jobs, e.g.:

&nbsp;

    fw login <YOUR_API_TOKEN>

After successful login, the CLI stores credentials locally so subsequent
runs can use `fw sync` non-interactively. If `fw login` is not complete,
the Flywheel sync step will fail.

What BrainGnomes runs: The sync job uses `fw sync` with sensible
defaults (includes DICOMs, non-interactive `-y`, your specified temp
path, and optional audit log). Data are written to your
`flywheel_sync_directory`. Typically the call will look something like
this:

    /path/to/fw sync --include dicom -y --tmp_path <scfg$metadata$flywheel_temp_directory> \
      --save-audit-logs <logs/flywheel_sync_audit.csv> \
      fw://hallquistm/momentum/ \
      /proj/mnhallqlab/studies/momentum/data/fMRI_MRI/raw_images/unc_flywheel_sync

#### A note about trailing slashes in the fw:// string

Flywheel sync operates a lot like rsync, so having the trailing slash in
the `fw://` project string like `fw://hallquistm/momentum/` indicates
that folders within that project are synchronized to the top level of
the flywheel_sync_directory (in the example,
`/proj/mnhallqlab/studies/momentum/data/fMRI_MRI/raw_images/unc_flywheel_sync`).
Thus, you typically want a trailing slash after your project name in the
`fw://` string so that the top-level folder itself isn’t copied to the
sync folder.

#### Deferred queueing of subject processing when flywheel comes first

Usage tip: You can include Flywheel sync as part of a larger unattended
run, for example:

``` r

run_project(scfg, steps = c("flywheel_sync", "bids_conversion", "fmriprep"))
```

When `flywheel_sync` is in the requested steps, BrainGnomes defers
subject-level job scheduling until after the sync job completes. This
ensures downstream steps (BIDS conversion, fMRIPrep, etc.) see all
current data from Flywheel.

### BIDS conversion

You will be prompted for whether you want to include BIDS conversion:
`Run BIDS conversion? (yes/no; Press enter to accept default: yes)`. If
you say yes, you will be asked for the location of a heudiconv
container, followed by questions about memory, compute hours, CPU cores,
command line arguments, and scheduler arguments.

`BrainGnomes` uses HeuDiConv (<https://github.com/nipy/heudiconv>) to
convert DICOM images into BIDS-compatible files and folders. HeuDiConv
requires information on how to identify subjects/sessions in your DICOM
folder and how to convert them. As part of the BIDS conversion setup,
you will be asked to provide:

1.  Subject Regex: A regular expression used to match all subject
    directories in the root of your DICOM directory. Consider, for
    example, a DICOM directory that includes the following folders:
    `logs`, `5300`, and `5420`, where the numbered directories are the
    subject IDs. In this case, a good regular expression for identifying
    subjects would be, `[0-9]+` – matching folders containing numbers.
    If you want *any* folder in the root of your DICOM directory to be
    considered a subject folder, use `.*`.

2.  (Optional) Session Regex: if you have multi-session data and these
    are stored in your DICOM directory as subfolders (i.e., something
    like sub-100/ses-1, sub-100/ses-2) you may specify a regular
    expression to match session folders inside each subject’s folder.
    For example, a session regex of `ses-[0-9]+` would match any
    subfolder starting with `ses-` followed by one or more numbers. If
    you do not have multisession data in this form, leave this blank.

3.  Subject ID Match Regex: Whereas the Subject Regex specifies what
    folders in the root of your DICOM directory should be considered,
    the Subject ID Match regular expression specifies how to *extract*
    the ID from the folder name. Use parentheses to denote which part of
    the folder name should be preserved as part of the ID. For example,
    if folders are named something like `sub-<numbers>` and you want to
    keep the numeric part as the ID, the Subject ID Match Regex would
    be, `sub-([0-9]+)`. If the ID is stored in different parts of the
    folder name, you can use more than one set of parentheses, in which
    case these will be extracted and pasted together with an underscore
    separating each part. For example, consider a folder
    `subject-16-visit-3`. You could extract a subject ID of `16_3` using
    the Regex: `subject-([0-9]+)-visit-([0-9]+)`.

4.  Session Match Regex: This follows the same logic as the Subject ID
    Match Regex, but applies to session folders nested within subject
    folders. For example, if you have a folder like: `subject-12/ses-1`,
    you would specify `subject-([0-9]+)` as your subject ID match regex
    and `ses-([0-9]+)` as your session match regex.

5.  Heuristic File: Path to the heudiconv heuristic Python script for
    your project. For example:
    `"/proj/Longleaf/MyStudy2025/heuristic.py"`. This script defines the
    naming scheme for converting DICOMs to BIDS format. Details for
    preparing such files can be found here:
    <https://heudiconv.readthedocs.io/en/latest/heuristics.html>.

6.  Overwrite and Clear Cache: Boolean flags (overwrite and clear_cache)
    that determine if heudiconv should overwrite existing converted data
    and clear any caching between runs. Usually, you can accept defaults
    (FALSE) unless you are re-running conversion and want to start
    fresh.

### fmriprep setup

Next, you will be asked to decide whether to include fmriprep in the
pipeline:
`Do you want to include fMRIPrep as part of your preprocessing pipeline? (yes/no; Press enter to accept default: yes)`.
If you say yes, you will be asked for the following:

1.  Location of the fmriprep container
2.  Resource requirements: Defaults for BrainGnomes fmriprep are 48 GB
    RAM, 24 hours, and 12 cores
3.  Additional command line arguments to fmriprep. See
    <https://fmriprep.org/en/stable/usage.html> for details.
4.  Additional arguments to be passed to the HPC scheduler
5.  Output Spaces: You will choose the standard spaces that fMRIPrep
    should output the preprocessed data in. Common choices include MNI
    anatomical spaces (e.g., MNI152NLin6Asym), the subject’s T1w native
    space, fsaverage (for surface data), etc. The setup_project()
    function will present a menu or prompt for selecting these. You can
    pick multiple spaces. For example, a typical selection might be
    MNI152NLin6Asym and T1w (and if analyzing surface data, possibly
    fsaverage).

- You will also be asked to provide a ‘resolution index’ for each
  template, which governs the spatial resolution of the output image. If
  this is omitted, the resolution of the output file will match the
  voxel size of the input file. Note that the index is not synonymous
  with the voxel size (e.g. ‘2’ may not be 2mm). See
  <https://fmriprep.org/en/stable/spaces.html#standard-spaces> for
  details.

6.  FreeSurfer License File: You must provide the path to your
    FreeSurfer license file so fMRIPrep can run FreeSurfer during
    preprocessing.

### MRIQC setup

Next, you will decide whether to include MRIQC in the pipeline:
`Run MRIQC? (yes/no; Press enter to accept default: yes)`. If you say
yes, you will be asked for:

1.  Location of the mriqc container
2.  Resource requirements: Defaults for BrainGnomes mriqc are 32 GB RAM,
    12 hours, and 1 core
3.  Additional command line arguments to mriqc. See
    <https://mriqc.readthedocs.io/en/latest/usage.html#command-line-interface>
    for details.
4.  Additional arguments to be passed to the HPC scheduler

### ICA-AROMA setup

Next, you will decide whether to include ICA-AROMA in the pipeline:
`Run ICA-AROMA? (yes/no; Press enter to accept default: yes)`. If you
say yes, you will be asked for:

1.  Location of the ICA-AROMA container
2.  Resource requirements: Defaults for BrainGnomes mriqc are 32 GB RAM,
    36 hours, and 1 core
3.  Additional command line arguments to fmripost_aroma. See
    <https://fmripost-aroma.readthedocs.io/latest/usage.html#command-line-arguments>
    for details.
4.  Additional arguments to be passed to the HPC scheduler

(Note: As of fMRIPrep v20.2+, ICA-AROMA is no longer integrated in
fMRIPrep and must be run separately as a BIDS-App called
fmripost-aroma.)

### Postprocessing setup

Next, you will decide whether to include postprocessing in the pipeline:
`Do you want to enable postprocessing of the BOLD data?`. Postprocessing
includes several optional steps, including

- Applying a brain mask
- Spatial smoothing
- Denoising using ICA-AROMA
- ‘Scrubbing’ of high-motion/high-artifact timepoints
- Temporal filtering (e.g., high-pass filtering)
- Intensity normalization
- Confound calculation and regression

If you say ‘yes’ to postprocessing, you will be asked to set up one or
more postprocessing streams for your data. BrainGnomes supports multiple
postprocessing streams, such that the same data can be postprocessed in
multiple ways. For example, you might wish to compare results with
different levels of smoothing or with different temporal filtering
settings.

To specify streams, you will be dropped into a menu system that looks
like this:

    Postprocessing supports multiple streams, allowing you to postprocess data in multiple ways.
    Each stream also asks about which files should be postprocessed using the stream. For example,
    files with 'rest' in their name could be postprocessed in one way and files with 'nback' could
    be processed a different way.

    Current postprocessing streams:
      (none defined yet)
    Modify postprocessing streams: 

    1: Add a stream
    2: Edit a stream
    3: Delete a stream
    4: Show stream settings
    5: Finish

For additional details about postprocessing, see the [Postprocessing
vignette](https://hallquistlab.github.io/BrainGnomes/articles/postprocessing.md).

### ROI time series extraction and functional connectivity calculation

Finally, you will be asked whether to include ROI extraction and
functional connectivity calculation in the pipeline. This step allows
you to extract average time series from regions of interest (ROIs) in
one or more atlases/ROI masks. You can also ask BrainGnomes to compute
correlations among the ROI timeseries for functional connectivity
analyses.

The
[`extract_rois()`](https://hallquistlab.github.io/BrainGnomes/reference/extract_rois.md)
function – called using
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
– loops over configured atlases and writes out ROI time series and
correlation matrices to the project’s `data_rois` directory using
BIDS-style filenames. For more details about ROI reduction choices and
correlation options, see the [Extracting ROIs
vignette](https://hallquistlab.github.io/BrainGnomes/articles/extract_rois.md).

### BIDS validation setup

BIDS validation is configured with the project, but it is not a
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
stage. Submit it separately through
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
whenever validation of the BIDS directory is desired, usually after BIDS
conversion. Saying yes to the prompt
`Enable BIDS validation? (yes/no; Press enter to accept default: yes)`
records the validator settings for that later submission. You will be
asked for:

1.  Location of the bids-validator program
2.  Resource requirements: Defaults for BrainGnomes bids-validator are
    32 GB RAM, 2 hours, and 1 core.
3.  Additional command lines arguments to bids-validator.
    <https://bids-validator.readthedocs.io/en/stable/user_guide/command-line.html>
    for details.
4.  Additional arguments to be passed to the HPC scheduler

### Conclusion

After
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
completes, it writes `project_config.yaml` into your project directory
(if one exists, you will be prompted whether to overwrite it). This file
contains your settings and can be used later to load the project into R.

## BrainGnomes workflow in a nutshell

### Step 1: Set up the project

Use
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
to interactively complete or correct configuration entries:

``` r

scfg <- setup_project()
```

This saves `project_config.yaml` in the project directory. In later R
sessions, you may pass the saved configuration object, its YAML file, or
the project directory to every project lifecycle helper. When the
project root is the current working directory, omit the input entirely.

### Step 2: Run the project

Pass the configuration directly to
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md).
With no `steps` argument, the familiar prompts select subjects, stages,
streams, debug/force behavior, and log level.

``` r

run <- run_project(scfg)

# Equivalent forms in a later session:
run <- run_project("/path/to/my/project")
setwd("/path/to/my/project")
run <- run_project()
```

For unattended use, pass explicit selections. A direct run resolves its
stages, streams, subject/session scope, and force setting internally; no
separate inspection step is required.

``` r

run <- run_project(
  scfg,
  steps = c("postprocess", "extract_rois"),
  subject_filter = c("242", "510")
)
```

The available submitted stages are `"flywheel_sync"`,
`"bids_conversion"`, `"mriqc"`, `"fmriprep"`, `"aroma"`,
`"postprocess"`, and `"extract_rois"`. BIDS validation is configured
with the project but scheduled independently through
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md).

Before submitting, preview the selected work without creating
directories, changing permissions, or contacting the scheduler:

``` r

preview <- run_project(scfg, steps = "all", dry_run = TRUE)
preview$preview$work
```

Dry runs return a `bg_project_plan` rather than `TRUE`. The preview
lists subjects/sessions, stages and streams, input/output and log
locations, requested resources, and dependencies. Completion markers
distinguish `would_skip` from `would_submit`; `force = TRUE` includes
already-completed work. Project setup checks and deferred Flywheel scope
are labeled separately. These counts are work units, not exact scheduler
job counts: postprocessing controllers can expand into image arrays and
sentinel jobs, and runtime preflight still applies. Console output shows
at most 20 detailed rows; the returned table retains all rows. Optional
saved plans and retry previews expose the same information.

For scripts, keep JSON stdout separate from diagnostic stderr:

``` r

# Shell equivalent:
# BrainGnomes run_project /project/my_study --steps=all --dry-run --format=json > preview.json
```

Each `--format=json` invocation emits one complete result document.
Progress, warnings, errors, and requested log tails go to stderr. Do not
combine stdout and stderr before parsing. JSON runs require explicit
`--steps` (or a saved plan); `status --watch` and
`diagnose --interactive` require table output. Poll
`status --format=json` for individual snapshots. Invalid formats fail
before work starts with exit status 2. Execution errors use status 1 and
empty stdout; validation and doctor failures instead retain their
structured report and use status 1.

The returned run handle has a stable run ID connecting tracking rows,
logs, inspection, diagnosis, provenance, retry, and cancellation. Before
the first scheduler submission, BrainGnomes writes a run-specific
provenance bundle beneath `<log_directory>/runs/<run_id>/`. It captures
the resolved request and subjects, exact configuration, modeled
resources and dependencies, software and host details, scheduler
identity, and content fingerprints for containers, heuristics, licenses,
atlases, masks, and other selected execution files.

``` r

status <- inspect_project(scfg)
status$stages
status$subjects
status$active

# Focus every view and count on one subject.
subject_status <- inspect_project(scfg, subject_id = "540294")

# Optionally compare active database records with the scheduler (read-only).
refreshed <- inspect_project(scfg, refresh = TRUE)
refreshed$reconciliation

# Restrict the same views to this submission.
run_status <- inspect_project(scfg, run_id = run$run_id)
provenance <- get_run_provenance(scfg, run$run_id)
find_run_logs(scfg, run$run_id, failed_only = TRUE)
```

### Step 3: Diagnose when needed

Diagnosis is not a routine prerequisite. In an interactive R session,
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
opens the guided dependency and log browser for current failed, blocked,
or cancelled work:

``` r

diagnose_project(scfg)
```

The guided browser starts with unresolved problems and retains each
narrowing choice. Provide `subject_id` or `job_id` to open a smaller
scope immediately:

``` r

diagnose_project(scfg, subject_id = "540294")
diagnose_project(scfg, job_id = "66273010")
```

Use `interactive = FALSE` for a structured result that can be queried in
R. This is also the default in scripts, tests, and reports:

``` r

diagnosis <- diagnose_project(scfg, interactive = FALSE)
diagnosis$failures
diagnosis$logs
```

The configuration object is optional when the project root is the
current working directory. In that case,
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
and
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
find `project_config.yaml` in
[`getwd()`](https://rdrr.io/r/base/getwd.html); they do not search
parent directories.

Supply a run ID for a historical post-mortem. Set the mode explicitly
when the code must behave the same way in interactive and
non-interactive sessions:

``` r

diagnosis <- diagnose_project(
  scfg, run_id = run$run_id, interactive = FALSE
)
diagnose_project(scfg, run_id = run$run_id, interactive = TRUE)
```

## Optional inspection and automation tools

Config, doctor, and plan are not additional required workflow steps.
They support review, environment inspection, and automation around the
direct workflow.

### Config: inspect or validate YAML

[`validate_project_config()`](https://hallquistlab.github.io/BrainGnomes/reference/validate_project_config.md)
reports malformed or incomplete configuration without opening the setup
wizard or writing files. This is useful in scripts, continuous
integration, or configuration review.
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
can also create a portable disabled-stage starting configuration without
prompts.

``` r

scfg <- setup_project(
  project_name = "my_study",
  project_directory = "/project/my_study",
  interactive = FALSE
)
validation <- validate_project_config(scfg)
stopifnot(validation$valid)
```

Direct
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
calls retain their existing checks for the selected stages; separate
config validation is optional.

### Doctor: inspect the submission environment

[`doctor_project()`](https://hallquistlab.github.io/BrainGnomes/reference/doctor_project.md)
performs a broader, non-mutating preflight covering scheduler commands,
container compatibility, storage permissions, stage-specific files, and
the job-tracking database. It is valuable on a new cluster, after
modules, containers, or storage have changed, or before an expensive
submission.

``` r

preflight <- doctor_project(scfg)
stopifnot(preflight$ok)
```

Pass `deep = TRUE` to also initialize the active reticulate Python and
inspect optional modules used by template-dependent postprocessing.
Doctor never submits jobs or changes project files and is not required
before
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md).

### Plan: inspect or persist resolved work

Plans record requested stages, known subject/session scope, streams,
resources, dependencies, and implicit setup jobs before submission.
Saved plans include their configuration and known scope, making a
reviewed request reusable in an approval or automation workflow. A plan
is an optional view of the request model that
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
resolves internally; it is not an exact list of scheduler jobs. When
Flywheel synchronization may add data, the plan reports deferred scope.
BrainGnomes records the subjects and sessions found after
synchronization before it submits their downstream work.

``` r

plan <- plan_project(
  scfg,
  steps = c("postprocess", "extract_rois"),
  subject_filter = c("242", "510")
)
write_project_plan(plan, "/project/my_study/plans/analysis.yaml")
run <- submit_project_plan(plan)
```

The final contract is recorded one job at a time. Immediately before
each managed `sbatch` or `qsub` submission, BrainGnomes writes an
immutable job manifest beneath the run’s `jobs` directory. The manifest
records the exact command, resources, dependencies, relevant
environment, logs, and checksums of files that control the job. When the
scheduler starts that job, a runtime receipt records the compute host
and verifies that the manifest, configuration, scripts, and containers
have not changed. If the manifest or an execution-driving file changed
or disappeared, the job stops and its receipt explains the mismatch.

For a Flywheel run, `scope-realization.json` records the subjects and
sessions found after synchronization. The downstream manifests are
created from that realized scope. Editing the project’s live YAML after
the run starts does not change the run-specific configuration snapshot.

## Optional run operations

By default,
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
integrates the newest attempt for every tracked subject, stage, and
stream across runs. Use `subject_id` to make all returned counts and
tables subject-specific, or a run ID when you specifically want one
submission. Its `$active` table is database-backed; `refresh = TRUE`
adds a read-only comparison with the scheduler in `$active` and
`$reconciliation`. If work fails, diagnose the failure and correct its
cause before retrying it.

``` r

# Inspect current project progress, then diagnose unresolved failures.
status <- inspect_project(scfg)
diagnosis <- diagnose_project(status, interactive = FALSE)

# Select one run when preparing a run-specific retry.
run_status <- inspect_project(scfg, run_id = run$run_id)
diagnosis <- diagnose_project(
  scfg, run_id = run$run_id, interactive = FALSE
)
failed_logs <- find_run_logs(scfg, run$run_id, failed_only = TRUE)

# Review what would be retried. This does not submit anything.
retry_plan <- retry_project_run(scfg, run$run_id, dry_run = TRUE)

# After correcting the cause, submit the retry as a new run.
retry_run <- retry_project_run(scfg, run$run_id, dry_run = FALSE)
```

A retry does not resume or change the original run. It creates a new run
and reruns the failed or cancelled stages and subjects even if old
completion files would otherwise cause them to be skipped. The new run’s
provenance records the source run ID. Jobs marked `FAILED_BY_EXT` did
not run because an earlier job failed. They are not included by default;
set `include_blocked = TRUE` when the new run should include that
affected downstream work too.

Cancellation is separate from retry. Preview it first; the second call
below immediately asks the scheduler to cancel jobs that are still
queued or running. It does not delete project data or outputs.

``` r

cancel_project_run(scfg, run$run_id, dry_run = TRUE)
cancel_project_run(scfg, run$run_id, dry_run = FALSE)
```

## Running BrainGnomes on the command line

BrainGnomes preserves the same primary workflow without requiring an
interactive R session. The established command names are shown here;
`init` and `run` are available as shorter aliases.

``` bash
BrainGnomes setup_project my_study /project/my_study
BrainGnomes run_project /project/my_study

# Only when a run needs investigation:
BrainGnomes diagnose /project/my_study --interactive
```

The same optional inspection and automation tools are available at the
command line. These commands are examples, not prerequisites for
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md):

``` bash
BrainGnomes config validate /project/my_study
BrainGnomes doctor /project/my_study
BrainGnomes plan /project/my_study --steps=all --output=/project/my_study/run.yaml
BrainGnomes run /project/my_study/run.yaml
```

Run operations support observation and recovery:

``` bash
BrainGnomes status /project/my_study
BrainGnomes status /project/my_study --view=subjects
BrainGnomes status /project/my_study --sub-id=540294
BrainGnomes status /project/my_study --view=active --refresh
BrainGnomes status /project/my_study --run=latest --watch
BrainGnomes provenance /project/my_study --run=latest --format=json
BrainGnomes logs /project/my_study --run=latest --failed-only --tail=50
BrainGnomes retry /project/my_study --run=<run-id> --dry-run
BrainGnomes retry /project/my_study --run=<run-id> --yes
BrainGnomes cancel /project/my_study --run=<run-id> --dry-run
BrainGnomes cancel /project/my_study --run=<run-id> --yes
```

Use the run ID reported by `status --runs` when retrying so the source
is unambiguous. Retry and cancellation require `--dry-run` or the
explicit `--yes` confirmation flag. Use
`BrainGnomes validate-bids /project/my_study` to submit the separately
configured BIDS validation job.

To get BrainGnomes on the command line, you need to add the location of
the package to your Linux path. If you don’t know where it is installed,
run this in an R session.

``` r

find.package("BrainGnomes")
```

Add that package directory to your path when a terminal starts. If you
use bash, the following portable form can be added to `~/.bashrc`:

``` bash
export PATH="$(Rscript -e 'cat(find.package(\"BrainGnomes\"))'):$PATH"
```

This enables the `BrainGnomes` command. The block below is generated by
running the installed command itself when this vignette is built, so it
stays synchronized with `inst/BrainGnomes`:

    Usage: BrainGnomes <command> [options]

    Typical workflow:
      setup_project <project_name> <project_directory>
      run_project <project_directory|config.yaml> [run options]
      status <project_directory|config.yaml> [--run=<id|latest>] [--watch]
      diagnose <project_directory|config.yaml> [--interactive]

    Optional inspection and automation:
      config validate <project_directory|config.yaml> [--format=table|json]
      config edit <project_directory|config.yaml>
      config show <project_directory|config.yaml> [--format=table|json]
      doctor <project_directory|config.yaml> [--steps=<steps>] [--deep]
      plan <project_directory|config.yaml> [run options] [--output=<plan.yaml>]

    Run operations:
      provenance <project_directory|config.yaml> [--run=<id|latest>] [--format=table|json]
      logs <project_directory|config.yaml> [--run=<id|latest>] [--failed-only]
      retry <project_directory|config.yaml> [--run=<id|latest>] --dry-run|--yes
      cancel <project_directory|config.yaml> [--run=<id|latest>] --dry-run|--yes
      validate-bids <project_directory|config.yaml> [--outfile=<report.html>]
      help [command]

    Also accepted: init, edit_project, and run.
    Config, doctor, and plan are optional; run_project resolves and submits directly.
    Machine output: --format=json writes one JSON document to stdout; diagnostics go to stderr.
    Use 'BrainGnomes help <command>' or 'BrainGnomes <command> --help' for details.

Use `BrainGnomes <command> --help` for command-specific options and
examples. Table output is intended for people; commands that return
structured results also provide JSON, and tabular status/log commands
provide CSV. Compatibility aliases for `setup_project`, `edit_project`,
and `run_project` remain available for existing scripts.
