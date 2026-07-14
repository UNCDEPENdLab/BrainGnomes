# BrainGnomes

<!-- badges: start -->
[![R-CMD-check](https://github.com/UNCDEPENdLab/BrainGnomes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UNCDEPENdLab/BrainGnomes/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/UNCDEPENdLab/BrainGnomes/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/UNCDEPENdLab/BrainGnomes/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

BrainGnomes is an R package for configuring, submitting, and monitoring reproducible fMRI workflows on high-performance computing (HPC) systems. It coordinates containerized neuroimaging tools and scheduler jobs from one project configuration, while retaining logs and job-tracking information for each run.

The package supports the parts of a workflow that you need: optional Flywheel synchronization, DICOM-to-BIDS conversion with HeuDiConv, BIDS validation, MRIQC, fMRIPrep, ICA-AROMA, postprocessing, and ROI time-series/connectivity extraction. You can begin with raw DICOMs or use it only for later steps when BIDS or fMRIPrep outputs already exist.

## Is BrainGnomes a good fit?

BrainGnomes is designed for studies run on an HPC cluster with a SLURM or TORQUE scheduler and containerized imaging software. It is especially useful when a project needs repeatable per-subject processing, configured resource requests, dependency-aware job submission, and a clear record of what completed or failed.

Before starting, make sure you have:

- R (version 4.1 or later) on the system from which you will submit jobs;
- access to a SLURM or TORQUE cluster and appropriate filesystem locations for project data, scratch space, and logs;
- Singularity-compatible images and any required licenses for the steps you enable (for example, fMRIPrep and its FreeSurfer license); and
- source data, a HeuDiConv heuristic when converting DICOMs, and any site-specific scheduler requirements.

See the [Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html) for a fuller checklist and configuration guidance.

## Installation

BrainGnomes is installed from GitHub. In R:

```r
install.packages("remotes")  # once, if needed
remotes::install_github("UNCDEPENdLab/BrainGnomes")

library(BrainGnomes)
```

### Install a specific release

To install a particular tagged release rather than the latest development
version, supply its tag with `ref`. For example:

```r
remotes::install_github("UNCDEPENdLab/BrainGnomes", ref = "0.8-1")
```

See the [available tags](https://github.com/UNCDEPENdLab/BrainGnomes/tags)
to choose an available tag.

## Typical workflow

1. Create an interactive project configuration. `setup_project()` records project paths, enabled pipeline stages, container locations, scheduler settings, and resource requests in `project_config.yaml`.
2. Review or update that configuration with `edit_project()`, or reload it later with `load_project()`.
3. Submit the enabled stages with `run_project()`. Jobs are submitted per subject/session with their dependencies tracked automatically.
4. Check progress with `get_project_status()` or `get_subject_status()`. Use `diagnose_pipeline()` to inspect the tracked job tree and logs when a run needs attention.

```r
library(BrainGnomes)

# Creates and saves project_config.yaml after guided setup.
scfg <- setup_project()

# Validate the planned work without submitting jobs.
run_project(scfg, steps = "all", dry_run = TRUE)

# When ready, submit enabled processing stages.
run_project(scfg, steps = "all")
```

`run_project()` can also target selected subjects, stages, postprocessing streams, or ROI-extraction streams. The [Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html) shows both interactive and scripted examples.

## Documentation

The [package website](https://uncdependlab.github.io/BrainGnomes/) includes function reference pages, release notes, and the following guides:

- [BrainGnomes Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html) — set up a project and run an end-to-end workflow.
- [Building Singularity containers for BrainGnomes](https://uncdependlab.github.io/BrainGnomes/articles/building_containers.html) — create the container images used by pipeline stages.
- [Postprocessing Walkthrough](https://uncdependlab.github.io/BrainGnomes/articles/postprocessing.html) — configure masking, smoothing, AROMA, filtering, scrubbing, intensity normalization, and confound regression.
- [Extracting ROI Timeseries and Connectivity](https://uncdependlab.github.io/BrainGnomes/articles/extract_rois.html) — configure atlas/mask ROI extraction and connectivity outputs.
- [Diagnosing Pipeline Runs](https://uncdependlab.github.io/BrainGnomes/articles/diagnosing_pipeline.html) — triage project or subject status and investigate failures from job-tracking records and logs.
- [Run-wise Intensity Normalization](https://uncdependlab.github.io/BrainGnomes/articles/intensity_normalization.html) — understand the robust reference-core approach, targets, provenance, quality checks, and troubleshooting.

## Getting help and contributing

Please [open an issue](https://github.com/UNCDEPENdLab/BrainGnomes/issues) for bugs, questions, or feature requests. Contributions are welcome; see [CONTRIBUTING.md](CONTRIBUTING.md) for the development workflow.
