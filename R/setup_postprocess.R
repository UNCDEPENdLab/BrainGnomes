#' Configure postprocessing settings for a study
#'
#' This function enables and configures the postprocessing steps to be applied after fMRIPrep.
#' Postprocessing may include denoising, smoothing, filtering, intensity normalization,
#' and confound regression applied to preprocessed BOLD data.
#'
#' The function interactively prompts the user (or selectively prompts based on `fields`)
#' to specify whether postprocessing should be performed, and if so, how each step should be configured.
#'
#' @param scfg A study configuration object, as produced by `setup_project()`.
#' @param fields A character vector of field names to prompt for. If `NULL`, all postprocessing fields will be prompted.
#'
#' @return A modified version of `scfg` with the `$postprocess` field populated.
#'
#' @details
#' Postprocessing is applied to the outputs of fMRIPrep to prepare BOLD time series for statistical modeling.
#' This may include:
#' - Applying brain masks
#' - Spatial smoothing
#' - ICA-AROMA denoising
#' - Temporal filtering
#' - Intensity normalization
#' - Confound calculation and regression
#'
#' Each step is optional and configurable. This function sets default values for memory, runtime,
#' and cores, and invokes a series of sub-setup functions to collect postprocessing parameters.
#'
#' @importFrom checkmate test_class
#' @importFrom glue glue
#' @keywords internal
setup_postprocess <- function(scfg = list(), fields = NULL) {
  if (!checkmate::test_class(scfg, "bg_project_cfg")) {
    stop("scfg input must be a bg_project_cfg object produced by setup_project")
  }

  defaults <- list(
    memgb = 48,
    nhours = 8,
    ncores = 1L,
    cli_options = "",
    sched_args = ""
  )

  if (is.null(scfg$postprocess$enable) || (isFALSE(scfg$postprocess$enable) && any(grepl("postprocess/", fields)))) {
    scfg$postprocess$enable <- prompt_input(
      instruct = glue("\n\n
      Postprocessing refers to the set of steps applied after fMRIPrep has produced preprocessed BOLD data.
      These steps may include:
        - Applying a brain mask
        - Spatial smoothing
        - Denoising using ICA-AROMA
        - Temporal filtering (e.g., high-pass filtering)
        - Intensity normalization
        - Confound calculation and regression

      Postprocessing is highly configurable and optimal choices may depend on the intended downstream analyses.
      You will be prompted to configure each of these steps in detail.

      Do you want to enable postprocessing for your BOLD data?\n
      "),
      prompt = "Enable postprocessing?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(scfg$postprocess$enable)) return(scfg)

  scfg <- setup_job(scfg, "postprocess", defaults, fields)
  scfg <- setup_postprocess_globals(scfg, fields)
  scfg <- setup_apply_mask(scfg, fields)
  scfg <- setup_spatial_smooth(scfg, fields)
  scfg <- setup_apply_aroma(scfg, fields)
  scfg <- setup_temporal_filter(scfg, fields)
  scfg <- setup_intensity_normalization(scfg, fields)
  scfg <- setup_confound_calculate(scfg, fields)
  scfg <- setup_scrubbing(scfg, fields)
  scfg <- setup_confound_regression(scfg, fields)
  scfg <- setup_postproc_steps(scfg, fields)

}

setup_postprocess_globals <- function(scfg, fields = NULL) {

  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$input_regex)) fields <- c(fields, "postprocess/input_regex")
    if (is.null(scfg$postprocess$bids_desc)) fields <- c(fields, "postprocess/bids_desc")
    if (is.null(scfg$postprocess$keep_intermediates)) fields <- c(fields, "postprocess/keep_intermediates")
    if (is.null(scfg$postprocess$overwrite)) fields <- c(fields, "postprocess/overwrite")
    if (is.null(scfg$postprocess$tr)) fields <- c(fields, "postprocess/tr")
    if (is.null(scfg$postprocess$apply_mask)) fields <- c(fields, "postprocess/apply_mask")
    if (is.null(scfg$postprocess$brain_mask)) fields <- c(fields, "postprocess/brain_mask")
  }

  # global postprocessing settings
  if ("postprocess/input_regex" %in% fields) {
    scfg$postprocess$input_regex <- prompt_input(
      "What is the relevant file extension (or regular expression) for inputs?",
      type = "character", len = 1L, default = ".*_desc-preproc_bold.nii.gz$",
      instruct = glue("
      \nPostprocessing is typically only applied to BOLD data that have completed preprocessing in fmriprep.
      These files usually have a suffix like _desc-preproc_bold.nii.gz. However, you may have postprocessing settings
      that only apply to certain outputs, such as for a particular experimental task or for resting state.

      What is the file extension for functional data to be postprocessed? If, for example, you only want
      files for a task called 'ridl', use a regular expression like, '.*_task-ridl.*_desc-preproc_bold.nii.gz$'. Note
      that having the $ add the end of the regular expression ensures that the file ends with the specified suffix.\n
      ")
    )
  }

  if ("postprocess/bids_desc" %in% fields) {
    scfg$postprocess$bids_desc <- prompt_input(
      "Enter the BIDS description ('desc') for the fully postprocessed file: ",
      type = "character", len = 1L, default="postproc",
      instruct = glue("
      \nWhat should be the description field for the final postprocessed file?
      This will yield a name like sub-540294_task-ridl_run-01_space-MNI152NLin6Asym_desc-postproc_bold.nii.gz.\n
      ")
    )
  }
  
  if ("postprocess/keep_intermediates" %in% fields) {
    scfg$postprocess$keep_intermediates <- prompt_input("Do you want to keep postprocess intermediate files? This is typically only for debugging.", type = "flag", default = FALSE)
  }
  if ("postprocess/overwrite" %in% fields) {
    scfg$postprocess$overwrite <- prompt_input("Overwrite existing postprocess files?", type = "flag", default = FALSE)
  }
  if ("postprocess/tr" %in% fields) {
    scfg$postprocess$tr <- prompt_input("Repetition time (in seconds) of the scan sequence:", type = "numeric", lower = 0.01, upper = 100, len = 1)
  }

  if ("postprocess/brain_mask" %in% fields) {
    scfg$postprocess$brain_mask <- prompt_input("Brain mask to be used in postprocessing: ",
      instruct = glue("
      \nHere, you can specify a single file (e.g., the brain mask provided by MNI) that can be used
      across datasets. This is especially desirable if you will *apply* that mask to the data, an optional step, as
      having a common high-quality mask is important to ensure comparability across subjects.

      If you provide a mask here, please make sure that it matches the resolution and orientation of the data to which
      it will be applied, as the pipeline will not check this for you. If you have a mask that is correct in the same
      stereotaxic space as the data, but has a different resolution, I recommend using AFNI's 3dresample like so:
        3dresample -input <current_mask> -master <a_preproc_nifti_file_from_study> -prefix <resampled_mask> -rmode NN

      If you do not provide a brain mask, the pipeline will first try to obtain a mask in the template space of the image.
      For example, if the file has 'space-MNI152NLin2009cAsym' in its name, the pipeline will download the brain mask
      from TemplateFlow for this space and resample it to the the data.

      If this is not possible (e.g., if the data is in native space), the pipeline will look for the mask calculated by fmriprep ('_desc-brain_mask')
      and if this is not available, the pipeline will calculate a mask using FSL's 98/2 method used in its preprocessing stream.\n"),
      type = "file", len = 1L, required = FALSE
    )
  }

  return(scfg)

}

#' Specify the postprocessing steps for a study
#'
#' This function determines the sequence of postprocessing steps to be applied after fMRIPrep.
#' Steps are included based on whether the corresponding `$enable` field is `TRUE` in the study configuration.
#' If the user opts to override the default order, they may manually specify a custom sequence.
#'
#' @param scfg a study configuration object produced by `setup_project`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with the `$postprocess$processing_steps` field populated
#' @keywords internal
#' @details This function is used to set up the postprocessing steps for a study. It prompts the user for
#'   the order of the processing steps and whether to apply them. The order of the processing steps is important,
#'   particularly because if we filter certain frequencies from the fMRI data, we must filter any regressors that we
#'   later apply to the data -- that is, confounds and fMRI data must match in frequency content prior to regression.
#'   See Hallquist, Hwang, & Luna (2013) or Lindquist (2019) for details.
setup_postproc_steps <- function(scfg = list(), fields = NULL) {
  # if (is.null(scfg$postprocess$processing_steps)) {
  #   stop("missing processing_steps. Run out of order?")
  # }

  # typical (usually correct) order
  step_order <- c(
    "apply_mask",
    "spatial_smooth",
    "apply_aroma",
    "temporal_filter",
    "confound_regression",
    "intensity_normalize"
  )

  processing_sequence <- character(0)
  for (step in step_order) {
    enabled <- tryCatch(isTRUE(scfg$postprocess[[step]]$enable), error = function(e) FALSE)
    if (enabled) processing_sequence <- c(processing_sequence, step)
  }

  # Prompt to override the default order
  # if (is.null(scfg$postprocess$force_processing_order) || "postprocess/force_processing_order" %in% fields) {
  #   scfg$postprocess$force_processing_order <- prompt_input("Do you want to specify the postprocessing sequence?",
  #     instruct = glue("\n\n
  #       The order of postprocessing steps is important. For instance, confound regressors must be filtered to match
  #       filtered fMRI data before confound regression is applied to the data.

  #       Here, we have ordered the processing steps in what we believe is the best sequence for ensuring a sensible pipeline that
  #       avoids pitfalls, including the aforementioned matter of frequency alignment. Note that if temporal filtering is used,
  #       confound regressors are filtered to match. Likewise, if AROMA is used, confound regressors will first have AROMA components removed.

  #       See Hallquist et al. (2013) or Lindquist (2019) for more discussion.
  #       \nYou can override the default order, but we recommend caution when doing so.\n
  #     ", .trim = TRUE),
  #     type = "flag", required = TRUE
  #   )
  # }

  # Prompt to override the default order
  if (is.null(scfg$postprocess$force_processing_order) || "postprocess/force_processing_order" %in% fields) {
    scfg$postprocess$force_processing_order <- prompt_input("Do you want to specify the postprocessing sequence?",
      instruct = glue("\n
        The order of postprocessing steps is important. For instance, regressors must be filtered to match filtered fMRI data
        before regression can occur. Our default order is intended to maximize denoising and avoid problems with filter mismatches.
        See Hallquist et al. (2013) or Lindquist (2019) for more discussion.
        \nYou can override the default order, but we recommend caution when doing so.\n
      ", .trim = TRUE),
      type = "flag", required = TRUE
    )
  }

  # If user wants to override, let them reorder the steps
  if (isTRUE(scfg$postprocess$force_processing_order)) {
    proceed <- FALSE
    while (!proceed) {
      seq_glue <- glue("\nEnabled processing steps:\n\n{paste(seq_along(processing_sequence), processing_sequence, collapse = '\n', sep = '. ')}\n")
      ss <- prompt_input(
        "Choose the order (separated by spaces): ",
        instruct = seq_glue,
        type = "integer", lower = 1, upper = length(processing_sequence), len = length(processing_sequence),
        split = "\\s+", uniq = TRUE
      )
      proceed_glue <- glue("\nYou specified the following order:\n\n{paste(seq_along(ss), processing_sequence[ss], collapse = '\n', sep = '. ')}\n")
      proceed <- prompt_input("Is this correct?", instruct = proceed_glue, type = "flag")
    }
    scfg$postprocess$processing_steps <- processing_sequence[ss]
  } else {
    scfg$postprocess$processing_steps <- processing_sequence
  }
  
  # if (isTRUE(scfg$postprocess$force_processing_order)) {
  #   proceed <- FALSE
  #   while (!proceed) {
  #     seq_glue <- glue("\nProcessing steps:\n\n{paste(seq_along(processing_sequence), processing_sequence, collapse = '\n', sep = '. ')}\n", .trim = FALSE)
  #     ss <- prompt_input(
  #       "Choose the order (separated by spaces): ",
  #       instruct = seq_glue,
  #       type = "integer", lower = 1, upper = length(processing_sequence), len = length(processing_sequence), split = "\\s+", uniq = TRUE
  #     )

  #     proceed_glue <- glue("\nYou specified the following processing order:\n\n{paste(seq_along(ss), processing_sequence[ss], collapse = '\n', sep = '. ')}\n", .trim = FALSE)
  #     proceed <- prompt_input("Is this correct?", instruct = proceed_glue, type = "flag")
  #   }

  #   scfg$postprocess$processing_steps <- processing_sequence[ss]
  # } else {
  #   scfg$postprocess$processing_steps <- processing_sequence
  # }

  return(scfg)
}

#' Configure scrubbing of high-motion volumes
#'
#' Generates spike regressors based on expressions evaluated on the confounds
#' file (e.g., "fd > 0.9" or "dvars > 1.5; -1:1"). These regressors can later be
#' used to censor volumes during modeling.
#'
#' @param scfg A study configuration object.
#' @param fields Optional vector of fields to prompt for.
#' @return Modified `scfg` with `$postprocess$scrubbing` populated.
#' @keywords internal
setup_scrubbing <- function(scfg = list(), fields = NULL) {
  if (is.null(scfg$postprocess$scrubbing$enable) ||
      (isFALSE(scfg$postprocess$scrubbing$enable) && any(grepl("postprocess/scrubbing/", fields)))) {
    scfg$postprocess$scrubbing$enable <- prompt_input(
      instruct = glue("\n\nScrubbing identifies high-motion volumes and creates spike regressors.\n",
        "Provide expressions evaluated against the confounds file, such as 'fd > 0.9'",
        " or 'dvars > 1.5; -1:1'.\nDo you want to generate scrubbing regressors?\n"),
      prompt = "Enable scrubbing?",
      type = "flag",
      default = FALSE
    )
  }

  if (isFALSE(scfg$postprocess$scrubbing$enable)) return(scfg)

  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$scrubbing$expression)) fields <- c(fields, "postprocess/scrubbing/expression")
  }

  if ("postprocess/scrubbing/expression" %in% fields) {
    scfg$postprocess$scrubbing$expression <- prompt_input(
      "Spike expression(s): ", type = "character", split = "\\s+", required = TRUE
    )
  }

  return(scfg)
}

#' Configure brain masking for postprocessing
#'
#' This function configures the optional step of applying a brain mask to the functional MRI data
#' in postprocessing. This step removes signal outside the brain (e.g., in air or non-brain tissue)
#' by zeroing out voxels outside the specified mask. Users can define a custom mask file or rely on
#' a default mask derived from the preprocessing pipeline (e.g., fMRIPrep outputs).
#'
#' This step is especially useful when preparing data for statistical modeling, as it constrains the
#' analysis to in-brain voxels and reduces computational burden.
#'
#' @param scfg A study configuration object created by `setup_project()`.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all fields related to brain masking will be prompted.
#'
#' @return A modified version of `scfg` with the `$postprocess$apply_mask` entry populated.
#' @keywords internal
setup_apply_mask <- function(scfg = list(), fields = NULL) {
  if (is.null(scfg$postprocess$apply_mask$enable) ||
      (isFALSE(scfg$postprocess$apply_mask$enable) && any(grepl("postprocess/apply_mask/", fields)))) {

    scfg$postprocess$apply_mask$enable <- prompt_input(
      instruct = glue("\n\n
      Applying a brain mask to your fMRI data ensures that only in-brain voxels are retained during analysis.
      This step is optional but often recommended for improving efficiency and accuracy in subsequent processing.
      
      The mask will be applied as a binary filter to the 4D functional data, zeroing out signal outside the brain.
      You can specify a custom mask file (in the same space and resolution as your fMRI data), or use the default
      mask produced by your preprocessing pipeline.

      Do you want to apply a brain mask to your fMRI data?\n
      "),
      prompt = "Apply brain mask?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(scfg$postprocess$apply_mask$enable)) return(scfg)

  # Determine which fields to prompt for
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$apply_mask$mask_file)) fields <- c(fields, "postprocess/apply_mask/mask_file")
    if (is.null(scfg$postprocess$apply_mask$prefix))    fields <- c(fields, "postprocess/apply_mask/prefix")
  }

  if ("postprocess/apply_mask/mask_file" %in% fields) {
    scfg$postprocess$apply_mask$mask_file <- prompt_input(
      prompt = "Path to binary brain mask file (NIfTI):",
      type = "file",
      required = FALSE
    )
  }

  if ("postprocess/apply_mask/prefix" %in% fields) {
    scfg$postprocess$apply_mask$prefix <- prompt_input(
      prompt = "File prefix for masked output:",
      type = "character",
      default = "m"
    )
  }

  return(scfg)
}


  # if ("postprocess/apply_mask" %in% fields) {
  #   scfg$postprocess$apply_mask <- prompt_input(
  #     "Apply brain mask to postprocessed data?",
  #     type = "flag",
  #     instruct = glue("
  #     \nA brain mask is used in postprocessing to calculate quantiles (e.g., median) of the image to be used in smoothing and
  #     intensity normalization. Optionally, you can also apply a mask to the data as part of postprocessing.
  #     Many people analyze their data without a mask, applying one later (e.g., when reviewing group maps).

  #     Do you want to apply a brain mask to the postprocessed data? This will mask out non-brain voxels, which can
  #     make it faster for analysis (since empty voxels are omitted) and easier for familywise error correction
  #     (since you don't want to correct over non-brain voxels). If you say yes, I would strongly recommend specifying
  #     a single brain mask to be used for all subjects in the pipeline to avoid heterogeneity in masks from data-driven
  #     brain-extraction algorithms, which can lead to inconsistencies in the group (intersection) mask. If you do not
  #     apply the mask to the postprocessed data, I wouldn't worry too much about providing one here, though if you have
  #     a good one handy, it's a good idea.

  #     You'll be asked about providing a custom mask next.\n")
  #   )
  # }


#' Configure confound regression for postprocessing
#'
#' This function configures voxelwise regression of nuisance confounds from fMRI data. Confounds are typically drawn
#' from the fMRIPrep confounds file. Users can select confounds to be temporally filtered to match the BOLD data
#' (e.g., continuous-valued regressors) and those that should not be filtered (e.g., binary spike regressors).
#'
#' Regression is applied on a voxelwise basis. Filtered regressors typically include motion parameters, CompCor components,
#' DVARS, or global signal. Unfiltered regressors usually include 0/1 indicators of outlier volumes.
#'
#' @param scfg A study configuration object created by `setup_project()`.
#' @param fields A character vector of fields to be prompted for. If `NULL`, all fields will be prompted.
#'
#' @return A modified version of `scfg` with the `$postprocess$confound_regression` entry populated.
#' @keywords internal
setup_confound_regression <- function(scfg = list(), fields = NULL) {
    if (is.null(scfg$postprocess$confound_regression$enable) ||
      (isFALSE(scfg$postprocess$confound_regression$enable) && any(grepl("postprocess/confound_regression/", fields)))) {
    
    scfg$postprocess$confound_regression$enable <- prompt_input(
      instruct = glue("\n\n
      Confound regression applies voxelwise multiple regression to remove nuisance signals from the fMRI data.
      These regressors are typically selected from the confounds file produced by fMRIPrep.

      You can specify two types of confound regressors:
        - Filtered regressors: continuous-valued (e.g., a_comp_cor_*, DVARS, global signal)
        - Unfiltered regressors: discrete-valued (e.g., motion_outlier*) that should not be filtered

      Use wildcards (*) or ranges (e.g., a_comp_cor_[1-6]) to specify multiple components.
      The regressed data will be written to new files prefixed by your chosen output prefix.

      Do you want to apply confound regression to the fMRI data during postprocessing?\n
      "),
      prompt = "Apply confound regression?",
      type = "flag"
    )
  }

  if (isFALSE(scfg$postprocess$confound_regression$enable)) return(scfg)

  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$confound_regression$columns)) fields <- c(fields, "postprocess/confound_regression/columns")
    if (is.null(scfg$postprocess$confound_regression$noproc_columns)) fields <- c(fields, "postprocess/confound_regression/noproc_columns")
    if (is.null(scfg$postprocess$confound_regression$prefix)) fields <- c(fields, "postprocess/confound_regression/prefix")
  } 
    
  if ("postprocess/confound_regression/columns" %in% fields) {
    scfg$postprocess$confound_regression$columns <- prompt_input(
      prompt = "Confounds that will be filtered:",
      type = "character", split = "\\s+", required = FALSE
    )
  }

  if ("postprocess/confound_regression/noproc_columns" %in% fields) {
    scfg$postprocess$confound_regression$noproc_columns <- prompt_input(
      prompt = "Confounds that will not be filtered:",
      type = "character", split = "\\s+", required = FALSE
    )
  }

  if ("postprocess/confound_regression/prefix" %in% fields) {
    scfg$postprocess$confound_regression$prefix <- prompt_input(
      prompt = "File prefix:",
      type = "character", default = "r"
    )
  }

  return(scfg)
}

#' Configure confound calculation for postprocessing
#'
#' This function configures the generation of a confound file during postprocessing. The resulting file includes
#' nuisance regressors (e.g., motion parameters, CompCor components, DVARS, global signal) that may be used during
#' task-based modeling to account for noise without directly altering the fMRI data.
#'
#' Confounds can be filtered (e.g., with the same temporal filter as applied to fMRI data) or left unfiltered.
#' Filtered regressors should typically include continuous-valued signals (e.g., a_comp_cor_*, global signal), while
#' spike regressors or discrete values (e.g., motion_outlier*) should not be filtered.
#'
#' This function only generates the confound regressors file. Actual regression is handled separately.
#'
#' @param scfg A study configuration object created by `setup_project()`.
#' @param fields A character vector of fields to prompt for. If `NULL`, all relevant fields will be prompted.
#'
#' @return A modified version of `scfg` with the `$postprocess$confound_calculate` entry updated.
#' @keywords internal
setup_confound_calculate <- function(scfg = list(), fields = NULL) {
  if (is.null(scfg$postprocess$confound_calculate$enable) ||
      (isFALSE(scfg$postprocess$confound_calculate$enable) && any(grepl("postprocess/confound_calculate/", fields)))) {
    
    scfg$postprocess$confound_calculate$enable <- prompt_input(
      instruct = glue("\n\n
      Confound calculation creates a nuisance regressor file that includes relevant noise signals (e.g., motion, 
      CompCor, global signal). This step does not apply denoising but prepares a file that can be used in later 
      statistical analyses (e.g., voxelwise GLMs).

      You can specify two types of confound regressors:
        - Filtered regressors: typically continuous-valued signals derived from fMRI (e.g., DVARS, a_comp_cor_*, global signal)
        - Unfiltered regressors: typically discrete-valued indicators (e.g., motion_outlier*) that should not be filtered

      Wildcards (*) and ranges ([1-6]) can be used to match multiple column names.

      Do you want to create a confound file in postprocessing?\n
      "),
      prompt = "Generate confound file?",
      type = "flag"
    )
  }

  if (isFALSE(scfg$postprocess$confound_calculate$enable)) return(scfg)

  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$confound_calculate$columns)) fields <- c(fields, "postprocess/confound_calculate/columns")
    if (is.null(scfg$postprocess$confound_calculate$noproc_columns)) fields <- c(fields, "postprocess/confound_calculate/noproc_columns")
    if (is.null(scfg$postprocess$confound_calculate$demean)) fields <- c(fields, "postprocess/confound_calculate/demean")
  }

  if ("postprocess/confound_calculate/columns" %in% fields) {
    scfg$postprocess$confound_calculate$columns <- prompt_input("Confounds that will be filtered: ", type = "character", split = "\\s+", required = FALSE)
  }

  if ("postprocess/confound_calculate/noproc_columns" %in% fields) {
    scfg$postprocess$confound_calculate$noproc_columns <- prompt_input("Confounds that will not be filtered: ", type = "character", split = "\\s+", required = FALSE)
  }
  
  if ("postprocess/confound_calculate/demean" %in% fields) {
    scfg$postprocess$confound_calculate$demean <- prompt_input("Demean (filtered) regressors?", type = "flag", default = TRUE)
  }

  ## Deprecated. We now always name the file to align with the corresponding postprocessed NIfTI
  # if ("postprocess/confound_calculate/output_file" %in% fields) {
  #   scfg$postprocess$confound_calculate$output_file <- prompt_input(
  #     instruct = "The confound file will be placed in the same directory as the fMRI data.",
  #     prompt = "Confound file name: ", type = "character"
  #   )
  #
  #   # ignore any path included in the output_file
  #   scfg$postprocess$confound_calculate$output_file <- basename(scfg$postprocess$confound_calculate$output_file)
  # }

  return(scfg)
}

#' Configure intensity normalization settings for postprocessing
#'
#' This function configures the intensity normalization step in the postprocessing pipeline.
#' Intensity normalization rescales the fMRI time series so that the median signal across the entire 4D image
#' reaches a specified global value (e.g., 10,000). This step can help ensure comparability across runs and subjects.
#'
#' @param scfg A study configuration object created by `setup_project()`.
#' @param fields A character vector of field names to prompt for. If `NULL`, all intensity normalization fields will be prompted.
#'
#' @return A modified version of `scfg` with the `$postprocess$intensity_normalize` entry updated.
#' @keywords internal
setup_intensity_normalization <- function(scfg = list(), fields = NULL) {
  if (is.null(scfg$postprocess$intensity_normalize$enable) ||
    (isFALSE(scfg$postprocess$intensity_normalize$enable) && any(grepl("postprocess/intensity_normalize/", fields)))) {
    scfg$postprocess$intensity_normalize$enable <- prompt_input(
      instruct = glue("\n\n
      Intensity normalization rescales the BOLD signal so that the global median intensity of the 4D image
      is equal across subjects and runs. This step can reduce variance due to scanner-related intensity differences
      and can help ensure consistent scaling of BOLD signal before statistical modeling.

      Do you want to apply intensity normalization to each fMRI run?\n
      "),
      prompt = "Apply intensity normalization?",
      type = "flag",
      default = TRUE
    )
  }
  
  # Exit early if user disabled the step
  if (isFALSE(scfg$postprocess$intensity_normalize$enable)) return(scfg)


  # if fields passed in, only bother use about the requested fields
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$intensity_normalize$global_median)) fields <- c(fields, "postprocess/intensity_normalize/global_median")
    if (is.null(scfg$postprocess$intensity_normalize$prefix)) fields <- c(fields, "postprocess/intensity_normalize/prefix")
  }

  if ("postprocess/intensity_normalize/global_median" %in% fields) {
    scfg$postprocess$intensity_normalize$global_median <- prompt_input("Global (4D) median intensity: ", type="numeric", lower=-1e8, upper=1e8, default=10000)
  }

  if ("postprocess/intensity_normalize/prefix" %in% fields) {
    scfg$postprocess$intensity_normalize$prefix <- prompt_input("File prefix: ", type = "character", default = "n")
  }

  return(scfg)
}

#' Configure spatial smoothing settings for fMRI postprocessing
#'
#' This function configures the spatial smoothing step for postprocessing of BOLD fMRI data.
#' Spatial smoothing increases signal-to-noise ratio by averaging nearby voxels and can improve
#' the statistical properties of the data, especially for group-level analyses.
#'
#' The user is asked whether they want to apply smoothing, and if so, to specify the full width at half maximum (FWHM)
#' of the Gaussian smoothing kernel and a filename prefix.
#'
#' @param scfg A study configuration object created by `setup_project()`.
#' @param fields A character vector of field names to prompt for. If `NULL`, all spatial smoothing fields will be prompted.
#'
#' @return A modified version of `scfg` with the `$postprocess$spatial_smooth` field populated.
#'
#' @details
#' If enabled, spatial smoothing is applied to the preprocessed BOLD data using a Gaussian kernel
#' with the user-specified FWHM in millimeters. This can help improve sensitivity and inter-subject alignment,
#' especially in standard space. This is accomplished using FSL's contrast-sensitive susan smoothing command.
#'
#' @keywords internal
setup_spatial_smooth <- function(scfg = list(), fields = NULL) {
  if (is.null(scfg$postprocess$spatial_smooth$enable) || (isFALSE(scfg$postprocess$spatial_smooth$enable) && any(grepl("postprocess/spatial_smooth/", fields)))) {
    scfg$postprocess$spatial_smooth$enable <- prompt_input(
      instruct = glue("\n\n
      Spatial smoothing involves applying a Gaussian kernel to the BOLD fMRI data,
      which increases the signal-to-noise ratio and improves overlap across subjects
      by reducing high-frequency spatial noise.

      You will be asked to specify the size of the smoothing kernel in millimeters
      (full width at half maximum, or FWHM). Common choices range from 4mm to 8mm.

      Do you want to apply spatial smoothing to the BOLD data as part of postprocessing?\n
      "),
      prompt = "Apply spatial smoothing?",
      type = "flag"
    )
  }
  
  # skip out if spatial smoothing is not requested
  if (isFALSE(scfg$postprocess$spatial_smooth$enable)) return(scfg)
  
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$spatial_smooth$fwhm_mm)) fields <- c(fields, "postprocess/spatial_smooth/fwhm_mm")
    if (is.null(scfg$postprocess$spatial_smooth$prefix)) fields <- c(fields, "postprocess/spatial_smooth/prefix")
  }

  if ("postprocess/spatial_smooth/fwhm_mm" %in% fields) {
    scfg$postprocess$spatial_smooth$fwhm_mm <- prompt_input("Spatial smoothing FWHM (mm): ", type = "numeric", lower = 0.1, upper = 100)
  }

  if ("postprocess/spatial_smooth/prefix" %in% fields) {
    scfg$postprocess$spatial_smooth$prefix <- prompt_input("File prefix: ", type = "character", default = "s")
  }
  
  return(scfg)
}

#' Configure temporal filtering settings for postprocessing
#'
#' This function configures the temporal filtering step in the postprocessing pipeline.
#' Temporal filtering removes unwanted frequency components from the BOLD signal, such as
#' slow drifts (via high-pass filtering) or physiological noise (via low-pass filtering).
#' This step is often used to improve signal quality for subsequent statistical analysis.
#'
#' @param scfg A study configuration object created by `setup_project()`.
#' @param fields A character vector of field names to prompt for. If `NULL`, all temporal filtering fields will be prompted.
#'
#' @return A modified version of `scfg` with the `$postprocess$temporal_filter` entry updated.
#' @keywords internal
setup_temporal_filter <- function(scfg = list(), fields = NULL) {
  if (is.null(scfg$postprocess$temporal_filter$enable) ||
      (isFALSE(scfg$postprocess$temporal_filter$enable) && any(grepl("postprocess/temporal_filter/", fields)))) {
    
    scfg$postprocess$temporal_filter$enable <- prompt_input(
      instruct = glue("\n\n
      Temporal filtering removes low- and/or high-frequency components from the fMRI time series.
      A high-pass filter (e.g., 0.008 Hz) is commonly used to remove slow scanner drift, while a low-pass
      filter can remove physiological noise such as respiratory or cardiac fluctuations.

      Do you want to apply temporal filtering to each fMRI run?\n
      "),
      prompt = "Apply temporal filtering?",
      type = "flag",
      default = TRUE
    )
  }

  # Exit early if user disabled the step
  if (isFALSE(scfg$postprocess$temporal_filter$enable)) return(scfg)

  # Determine which fields to prompt for
  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$temporal_filter$low_pass_hz)) fields <- c(fields, "postprocess/temporal_filter/low_pass_hz")
    if (is.null(scfg$postprocess$temporal_filter$high_pass_hz)) fields <- c(fields, "postprocess/temporal_filter/high_pass_hz")
    if (is.null(scfg$postprocess$temporal_filter$prefix)) fields <- c(fields, "postprocess/temporal_filter/prefix")
  }

  if ("postprocess/temporal_filter/low_pass_hz" %in% fields) {
    scfg$postprocess$temporal_filter$low_pass_hz <- prompt_input("Low-pass cutoff (Hz): ", type = "numeric", lower = 0)
  }
  
  if ("postprocess/temporal_filter/high_pass_hz" %in% fields) {
    scfg$postprocess$temporal_filter$high_pass_hz <- prompt_input("High-pass cutoff (Hz): ", type = "numeric", lower = 0)
  }

  if (!is.null(scfg$postprocess$temporal_filter$low_pass_hz) && !is.null(scfg$postprocess$temporal_filter$high_pass_hz) &&
    scfg$postprocess$temporal_filter$low_pass_hz > scfg$postprocess$temporal_filter$high_pass_hz) stop("Low-pass cutoff cannot be larger than high-pass cutoff")

  if ("postprocess/temporal_filter/prefix" %in% fields) {
    scfg$postprocess$temporal_filter$prefix <- prompt_input("File prefix: ", type = "character", default = "f")
  }
  
  return(scfg)
}

#' Configure ICA-AROMA denoising application in postprocessing
#'
#' This function configures the application of ICA-AROMA denoising to fMRI data as part of postprocessing.
#' ICA-AROMA (Pruim et al., 2015) identifies and labels motion-related independent components (ICs) using
#' spatiotemporal features, and outputs regressors that can be used to remove these components.
#'
#' If enabled, this step applies the AROMA regressors to remove noise components from the BOLD time series
#' using either 'aggressive' or 'nonaggressive' regression. Nonaggressive denoising is recommended as it
#' preserves shared variance with signal components.
#'
#' This step assumes that ICA-AROMA has already been run using a tool like `fmripost-aroma`.
#'
#' @param scfg A study configuration object created by `setup_project()`.
#' @param fields A character vector of fields to prompt for. If `NULL`, all fields will be prompted.
#'
#' @return A modified version of `scfg` with the `$postprocess$apply_aroma` entry updated.
#' @keywords internal
setup_apply_aroma <- function(scfg = list(), fields = NULL) {
  if (is.null(scfg$postprocess$apply_aroma$enable) ||
      (isFALSE(scfg$postprocess$apply_aroma$enable) && any(grepl("postprocess/apply_aroma/", fields)))) {
    
    scfg$postprocess$apply_aroma$enable <- prompt_input(
      instruct = glue("\n\n
      ICA-AROMA identifies motion-related independent components from the fMRI data and outputs
      noise regressors (e.g., *_desc-aroma_timeseries.tsv) that can be used to denoise the BOLD signal.

      This step applies those regressors to the data using voxelwise regression, either:
        - nonaggressively (recommended), which removes unique variance from noise components only, or
        - aggressively, which removes all variance associated with the noise components.

      Do you want to apply ICA-AROMA denoising during postprocessing?\n
      "),
      prompt = "Apply AROMA denoising?",
      type = "flag"
    )
  }

  if (isFALSE(scfg$postprocess$apply_aroma$enable)) return(scfg)

  if (is.null(fields)) {
    fields <- c()
    if (is.null(scfg$postprocess$apply_aroma$nonaggressive)) fields <- c(fields, "postprocess/apply_aroma/nonaggressive")
    if (is.null(scfg$postprocess$apply_aroma$prefix)) fields <- c(fields, "postprocess/apply_aroma/prefix")
  }

  if ("postprocess/apply_aroma/nonaggressive" %in% fields) {
    scfg$postprocess$apply_aroma$nonaggressive <- prompt_input("Use nonaggressive denoising?", type = "flag", default = TRUE)
  }
  
  if ("postprocess/apply_aroma/prefix" %in% fields) {
    scfg$postprocess$apply_aroma$prefix <- prompt_input("File prefix: ", type = "character", default = "a")
  }
  
  return(scfg)
}

