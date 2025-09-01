# BrainGnomes

BrainGnomes streamlines preprocessing and analysis of fMRI data on high performance computing (HPC) clusters. The package wraps common neuroimaging tools (HeuDiConv, MRIQC, fMRIPrep and others) and provides helper functions for managing configuration files, submitting jobs and running post‑processing steps. All heavy weight dependencies are executed through Singularity containers so that pipelines are reproducible across systems.

## Vignettes

Documentation is provided as several vignettes found in the `vignettes/` folder:

- **BrainGnomes Quickstart** – an end to end introduction to setting up and running a study [`vignettes/braingnomes_quickstart.html`](vignettes/braingnomes_quickstart.html).
- **Building Singularity containers for BrainGnomes** – instructions on creating the container images used by the pipeline [`vignettes/building_containers.html`](vignettes/building_containers.html).
- **BrainGnomes Postprocessing Walkthrough** – details the optional post‑processing utilities such as smoothing, scrubbing and denoising [`vignettes/postprocessing.html`](vignettes/postprocessing.html).
- **ROI extraction and connectivity calculation** – details the optional step of extracting ROIs from a mask/atlas and computing correlations among them [`vignettes/extract_rois.html`](vignettes/extract_rois.html).

## Key source files

Core functionality resides in a handful of scripts:

- `R/setup_project.R` – interactive functions for creating and validating project configuration files.
- `R/process_subject.R` – runs preprocessing for a single participant.
- `R/run_project.R` – orchestrates multi‑subject pipelines on the cluster.
- `R/pipeline_functions.R` – utilities for job scripts and pipeline bookkeeping.
- `src/` – C++ helpers for filtering, interpolation and other fast numerical routines.
- `inst/hpc_scripts/` – SLURM and TORQUE templates for submitting container jobs.

Together these components allow BrainGnomes to manage the full workflow from raw BIDS conversion through fMRIPrep and optional post‑processing steps.
