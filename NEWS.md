# BrainGnomes 0.6

Released 2025-08-12

* Added ROI extraction workflow (`extract_rois`) with correlation options and a dedicated vignette.
* Introduced a Flywheel synchronization step for retrieving data.
* Added support for external BIDS and fMRIPrep directories,
  including an `is_external_path` helper, path normalization, and configurable `postproc_directory`.
* Enhanced postprocessing: new `output_dir` argument, direct output movement without symlinks,
  robust file handling, and optional AROMA cleanup with safety checks for MNI res-2 outputs.
* Refined project setup and validation by verifying directories before saving,
  prompting for required containers, and allowing projects to run without a postprocessing directory.
* Documentation and test improvements, including an expanded quickstart guide and instructions for building the FSL container.
