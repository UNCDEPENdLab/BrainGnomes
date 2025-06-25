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
  scfg <- setup_spatial_smooth(scfg, fields)
  scfg <- setup_apply_aroma(scfg, fields)
  scfg <- setup_temporal_filter(scfg, fields)
  scfg <- setup_intensity_normalization(scfg, fields)
  scfg <- setup_confound_calculate(scfg, fields)
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

  if ("postprocess/apply_mask" %in% fields) {
    scfg$postprocess$apply_mask <- prompt_input(
      "Apply brain mask to postprocessed data?",
      type = "flag",
      instruct = glue("
      \nA brain mask is used in postprocessing to calculate quantiles (e.g., median) of the image to be used in smoothing and
      intensity normalization. Optionally, you can also apply a mask to the data as part of postprocessing.
      Many people analyze their data without a mask, applying one later (e.g., when reviewing group maps).

      Do you want to apply a brain mask to the postprocessed data? This will mask out non-brain voxels, which can
      make it faster for analysis (since empty voxels are omitted) and easier for familywise error correction
      (since you don't want to correct over non-brain voxels). If you say yes, I would strongly recommend specifying
      a single brain mask to be used for all subjects in the pipeline to avoid heterogeneity in masks from data-driven
      brain-extraction algorithms, which can lead to inconsistencies in the group (intersection) mask. If you do not
      apply the mask to the postprocessed data, I wouldn't worry too much about providing one here, though if you have
      a good one handy, it's a good idea.

      You'll be asked about providing a custom mask next.\n")
    )
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

  # arrange them in a desirable order
  processing_sequence <- c()
  if ("apply_mask" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "apply_mask")
  if ("spatial_smooth" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "spatial_smooth")
  if ("apply_aroma" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "apply_aroma")
  if ("temporal_filter" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "temporal_filter")
  if ("confound_regression" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "confound_regression")
  if ("intensity_normalize" %in% scfg$postprocess$processing_steps) processing_sequence <- c(processing_sequence, "intensity_normalize")

  if (is.null(scfg$postprocess$force_processing_order) || "postprocess/force_processing_order" %in% fields) {
    scfg$postprocess$force_processing_order <- prompt_input("Do you want to specify the postprocessing sequence?",
      instruct = glue("
      \nThe order of postprocessing steps is important, particularly because if we filter certain frequencies from the fMRI data, we must filter
      any regressors that we later apply to the data -- that is, confounds and fMRI data must match in frequency content prior to regression.
      See Hallquist, Hwang, & Luna (2013) or Lindquist (2019) for details.

      Here, we have ordered the processing steps in what we believe is the best sequence for ensuring a sensible pipeline that
      avoids pitfalls, including the aforementioned matter of frequency alignment. Note that if temporal filtering is used,
      confound regressors are filtered to match. And likewise, if AROMA is used, confound regressors will also have AROMA components removed.

      You can specify a different postprocessing order than what is recommended, but with the cautionary note that we have not
      tested all possible sequences and unfortunate sequences could occur.\n
      ", .trim = TRUE),
      type = "flag", required = TRUE
    )
  }
  
  if (isTRUE(scfg$postprocess$force_processing_order)) {
    proceed <- FALSE
    while (!proceed) {
      seq_glue <- glue("\nProcessing steps:\n\n{paste(seq_along(processing_sequence), processing_sequence, collapse = '\n', sep = '. ')}\n", .trim = FALSE)
      ss <- prompt_input(
        "Choose the order (separated by spaces): ",
        instruct = seq_glue,
        type = "integer", lower = 1, upper = length(processing_sequence), len = length(processing_sequence), split = "\\s+", uniq = TRUE
      )

      proceed_glue <- glue("\nYou specified the following processing order:\n\n{paste(seq_along(ss), processing_sequence[ss], collapse = '\n', sep = '. ')}\n", .trim = FALSE)
      proceed <- prompt_input("Is this correct?", instruct = proceed_glue, type = "flag")
    }

    scfg$postprocess$processing_steps <- processing_sequence[ss]
  } else {
    scfg$postprocess$processing_steps <- processing_sequence
  }
  
  if ("confound_calculate" %in% scfg$postprocess$processing_steps) {
    cat(glue("
      Confound calculation is also included as a part of post-processing.
      Whatever (relevant) processing steps are applied to the fMRI data will also be applied
      to the calculated confounds file. If you look at the config file, you'll also see 
      'confound_calculate' as a step in 'processing_steps'.
    "), .trim=FALSE)
    scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "confound_calculate")
  }

  return(scfg)
}

#' Specify the confound regression settings for postprocessing
#' @param scfg a study configuration object created by `setup_project`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with the `$postprocess$confound_regression` field populated
#' @keywords internal
setup_confound_regression <- function(scfg = list(), fields = NULL) {

  if (is.null(fields)) {
    cur_val <- "confound_regression" %in% scfg$postprocess$processing_steps
    if ("confound_regression" %in% names(scfg$postprocess)) {
      cat(strwrap(glue("
        Current confound regression settings:
          Apply confound regression: {cur_val}
          Columns that will be filtered to match fMRI data: {paste(scfg$postprocess$confound_regression$columns)}
          Columns that will not be filtered: {paste(scfg$postprocess$confound_regression$noproc_columns)}
          File prefix: {scfg$postprocess$confound_regression$prefix}
      "), width = 80, exdent = 4), sep = "\n")

      change <- prompt_input("Change settings?", type = "flag")
      if (!change) return(scfg) # skip out
    }

    cat(glue("
      If applied, confound regression removes one or more confounds from the fMRI data using voxelwise regession.
      Two kinds of confounds can be removed: those that are first filtered to match the fMRI data and those that
      are not filtered. Any continuous-valued regressor that was generated from the fMRI timeseries data or head motion
      parameters -- for example, DVARS, components from CompCor, the global signal, or cerebrospinal fluid -- should
      be filtered. Regressors that are discrete-valued -- usually 0/1 spike regressors -- should not be filtered
      (e.g., motion_outlier* regressors in the confounds.tsv file produced by fmriprep).

      Here, you can specify '*' to include all components matching that wildcard, such as 'a_comp_cor_*'. You can also
      specify a range of values using the syntax '[low-high]', such as 'a_comp_cor_[1-10]', which would include the
      first 10 of these regressors as confounds.
    ", .trim = FALSE))

    apply_step <- prompt_input("Apply confound regression?", type = "flag")
    if (apply_step && !cur_val) {
      scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "confound_regression")
    } else if (!apply_step && cur_val) {
      scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "confound_regression"]
    }

    fields <- c()
    if (is.null(scfg$postprocess$confound_regression$columns)) fields <- c(fields, "postprocess/confound_regression/columns")
    if (is.null(scfg$postprocess$confound_regression$noproc_columns)) fields <- c(fields, "postprocess/confound_regression/noproc_columns")
    if (is.null(scfg$postprocess$confound_regression$prefix)) fields <- c(fields, "postprocess/confound_regression/output_file")

  } else {
    apply_step <- "confound_regression" %in% scfg$postprocess$processing_steps
  }
  
  # only ask for details if they want the step
  if (apply_step) {
    scfg$postprocess$confound_regression$columns <- prompt_input("Confounds that will be filtered: ", type = "character", split = "\\s+")
    scfg$postprocess$confound_regression$noproc_columns <- prompt_input("Confounds that will not be filtered: ", type = "character", split = "\\s+", required=FALSE)
    scfg$postprocess$confound_regression$prefix <- prompt_input("File prefix: ", type = "character")
  }
  return(scfg)
}

#' Specify the confound calculation settings for postprocessing
#' @param scfg a study configuration object created by `setup_project`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with the `$postprocess$confound_calculate` field populated
#' @keywords internal
setup_confound_calculate <- function(scfg = list(), fields = NULL) {
  if (is.null(fields)) {
    cur_val <- "confound_calculate" %in% scfg$postprocess$processing_steps
    if ("confound_calculate" %in% names(scfg$postprocess)) {
      cat(strwrap(glue("
        Current confound calculation settings:
          Calculate confounds file: {cur_val}
          Columns that will be filtered to match fMRI data: {paste(scfg$postprocess$confound_calculate$columns)}
          Columns that will not be filtered: {paste(scfg$postprocess$confound_calculate$noproc_columns)}
          Demean confounds: {paste(scfg$postprocess$confound_calculate$demean)}
          Confound file name: {scfg$postprocess$confound_calculate$output_file}
      "), width = 80, exdent = 4), sep = "\n")

      change <- prompt_input("Change settings?", type = "flag")
      if (!change) return(scfg) # skip out
    }
    
    cat(glue("
      Confound calculation creates a file containing a set of confound regressors, but it does *not* apply these
      to the fMRI data in any way. This file could be used subsequently -- often in the context of a task-based fMRI
      GLM analysis -- to remove nuisance regressors while also computing the effects for regressors of interest
      (usually, task-related modulation).

      Two kinds of confounds can be added to the confounds file: those that are first filtered to match the fMRI data 
      and those that are not filtered. Any continuous-valued regressor that was generated from the fMRI timeseries data
      or head motion parameters -- for example, DVARS, components from CompCor, the global signal, or cerebrospinal 
      fluid -- should be filtered. Regressors that are discrete-valued -- usually 0/1 spike regressors -- should not 
      be filtered (e.g., motion_outlier* regressors in the confounds.tsv file produced by fmriprep).

      Here, you can specify '*' to include all components matching that wildcard, such as 'a_comp_cor_*'. You can also
      specify a range of values using the syntax '[low-high]', such as 'a_comp_cor_[1-10]', which would include the
      first 10 of these regressors as confounds.
    ", .trim=FALSE))

    apply_step <- prompt_input("Calculate confounds?", type="flag")
    if (apply_step && !cur_val) {
      scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "confound_calculate")
    } else if (!apply_step && cur_val) {
      scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "confound_calculate"]
    }
    
    fields <- c()
    if (is.null(scfg$postprocess$confound_calculate$columns)) fields <- c(fields, "postprocess/confound_calculate/columns")
    if (is.null(scfg$postprocess$confound_calculate$noproc_columns)) fields <- c(fields, "postprocess/confound_calculate/noproc_columns")
    if (is.null(scfg$postprocess$confound_calculate$demean)) fields <- c(fields, "postprocess/confound_calculate/demean")
    if (is.null(scfg$postprocess$confound_calculate$output_file)) fields <- c(fields, "postprocess/confound_calculate/output_file")
  }

  if ("postprocess/confound_calculate/columns" %in% fields) {
    scfg$postprocess$confound_calculate$columns <- prompt_input("Confounds that will be filtered: ", type = "character", split = "\\s+")
  }

  if ("postprocess/confound_calculate/noproc_columns" %in% fields) {
    scfg$postprocess$confound_calculate$noproc_columns <- prompt_input("Confounds that will not be filtered: ", type = "character", split = "\\s+", required = FALSE)
  }
  
  if ("postprocess/confound_calculate/demean" %in% fields) {
    scfg$postprocess$confound_calculate$demean <- prompt_input("Demean (filtered) regressors?", type = "flag")
  }

  if ("postprocess/confound_calculate/output_file" %in% fields) {
    scfg$postprocess$confound_calculate$output_file <- prompt_input(
      instruct = "Confound files will always be placed in the same directory as fMRI data.",
      prompt = "Confound file name: ", type = "character"
    )

    # ignore any path included in the output_file
    scfg$postprocess$confound_calculate$output_file <- basename(scfg$postprocess$confound_calculate$output_file)
  }

  return(scfg)
}

#' Specify the intensity normalization settings for postprocessing
#' @param scfg a study configuration object created by `setup_project`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with the `$postprocess$intensity_normalize` field populated
#' @keywords internal
setup_intensity_normalization <- function(scfg = list(), fields = NULL) {

  # if fields passed in, only bother use about the requested fields
  if (is.null(fields)) {
    cur_val <- "intensity_normalize" %in% scfg$postprocess$processing_steps
    if ("intensity_normalize" %in% names(scfg$postprocess)) {
      cat(glue("
        Current intensity normalization settings:
          Apply intensity normalization: {cur_val}
          Global (4D) median intensity: {scfg$postprocess$intensity_normalize$global_median}
          File prefix: {scfg$postprocess$intensity_normalize$prefix}
      "))

      change <- prompt_input("Change settings?", type="flag")
      if (!change) return(scfg) # skip out
    }

    apply_step <- prompt_input("Apply intensity normalization?", type="flag")
    if (apply_step && !cur_val) {
      scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "intensity_normalize")
    } else if (!apply_step && cur_val) {
      scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "intensity_normalize"]
    }

    fields <- c()
    if (is.null(scfg$postprocess$intensity_normalize$global_median)) fields <- c(fields, "postprocess/intensity_normalize/global_median")
    if (is.null(scfg$postprocess$intensity_normalize$prefix)) fields <- c(fields, "postprocess/intensity_normalize/prefix")
  }

  if ("postprocess/intensity_normalize/global_median" %in% fields) {
    scfg$postprocess$intensity_normalize$global_median <- prompt_input("Global (4D) median intensity: ", type="numeric", lower=-1e8, upper=1e8)
  }

  if ("postprocess/intensity_normalize/prefix" %in% fields) {
    scfg$postprocess$intensity_normalize$prefix <- prompt_input("File prefix: ", type = "character", default = "n")
  }

  return(scfg)
}

#' Specify the spatial smoothing settings for postprocessing
#' @param scfg a study configuration object created by `setup_project`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with the `$postprocess$spatial_smooth` field populated
#' @keywords internal
setup_spatial_smooth <- function(scfg = list(), fields = NULL) {

  if (is.null(fields)) {
    # only prompt a change when certain fields aren't already requested
    cur_val <- "spatial_smooth" %in% scfg$postprocess$processing_steps
    if ("spatial_smooth" %in% names(scfg$postprocess)) {
      cat(glue("
        Current spatial smoothing settings:
          Apply spatial smoothing: {cur_val}
          Smoothing FWHM (mm): {scfg$postprocess$spatial_smooth$fwhm_mm}
          File prefix: {scfg$postprocess$spatial_smooth$prefix}
      "))

      change <- prompt_input("Change settings?", type = "flag")
      if (!change) return(scfg) # skip out
    }

    apply_step <- prompt_input("Apply spatial smoothing?", type="flag")
    if (apply_step && !cur_val) {
      scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "spatial_smooth")
    } else if (!apply_step && cur_val) {
      scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "spatial_smooth"]
    }

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

#' Specify the temporal filtering settings for postprocessing
#' @param scfg a study configuration object created by `setup_project`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with the `$postprocess$temporal_filter` field populated
#' @keywords internal
setup_temporal_filter <- function(scfg = list(), fields = NULL) {
  if (is.null(fields)) {
    cur_val <- "temporal_filter" %in% scfg$postprocess$processing_steps
    if ("temporal_filter" %in% names(scfg$postprocess)) {
      cat(glue("
        Current temporal filtering settings:
          Apply temporal filter: pretty_arg({cur_val})
          Low-pass cutoff (Hz): {pretty_arg(scfg$postprocess$temporal_filter$low_pass_hz)}
          High-pass cutoff (Hz): {pretty_arg(scfg$postprocess$temporal_filter$high_pass_hz)}
          File prefix: {pretty_arg(scfg$postprocess$temporal_filter$prefix)}
      "))

      change <- prompt_input("Change settings?", type="flag")
      if (!change) return(scfg) # skip out
    }

    apply_step <- prompt_input("Apply temporal filter?", type="flag")
    if (apply_step && !cur_val) {
      scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "temporal_filter")
    } else if (!apply_step && cur_val) {
      scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "temporal_filter"]
    }

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

#' Specify the intensity normalization settings for postprocessing
#' @param scfg a study configuration object created by `setup_project`
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `scfg` with the `$postprocess$apply_aroma` field populated
#' @keywords internal
setup_apply_aroma <- function(scfg = list(), fields = NULL) {
  if (is.null(fields)) {
    cur_val <- "apply_aroma" %in% scfg$postprocess$processing_steps
    if ("apply_aroma" %in% names(scfg$postprocess)) {
      cat(glue("
        Current AROMA removal settings:
          Apply AROMA denoising: {cur_val}
          Use nonaggressive denoising: {scfg$postprocess$apply_aroma$nonaggressive}
          File prefix: {scfg$postprocess$apply_aroma$prefix}
      "))

      change <- prompt_input("Change settings?", type="flag")
      if (!change) return(scfg) # skip out
    }

    cat(glue("
      As part of the fMRI processing pipeline, ICA-AROMA can be run for your data. This will compute a spatiotemporal
      decomposition of the data for each run, then figure out which spatiotemporal components likely correspond to
      motion-related artifacts. This is accomplished by the fmripost-aroma workflow (https://github.com/nipreps/fmripost-aroma).
      The result is that you will have a derivatives containing the ICA components x time matrix and a tsv file of
      potential regressors that can be used in denoising. This may look something like:

      sub-<label>/
        func/
          sub-<label>_space-MNI152NLin6Asym_res-2_desc-melodic_mixing.tsv
          sub-<label>_[specifiers]_desc-aroma_timeseries.tsv

      Here, in postprocessing, you can choose to now *apply* these noise regressors to the fMRI data in order to remove
      these sources of noise from the BOLD data. If you choose to apply AROMA denoising, the motion-related components
      will be removed using 'aggressive' or 'nonaggressive' regression. Pruim et al. 2015 recommend 'nonaggressive', which
      in simple terms only removes the unique variance in (bad) noise components not shared with (good) signal components.
      'Aggressive' denoising, on the other hand, simply uses voxelwise multiple regression with all noise components
      as regressors, such that any variance in noise components is removed from the data. We agree that 'nonaggressive' is
      the best default for this procedure.
    ", .trim=FALSE))

    apply_step <- prompt_input("Apply AROMA denoising?", type="flag")
    if (apply_step && !cur_val) {
      scfg$postprocess$processing_steps <- c(scfg$postprocess$processing_steps, "apply_aroma")
    } else if (!apply_step && cur_val) {
      scfg$postprocess$processing_steps <- scfg$postprocess$processing_steps[scfg$postprocess$processing_steps != "apply_aroma"]
    }
    
    fields <- c()
    if (is.null(scfg$postprocess$apply_aroma$nonaggressive)) fields <- c(fields, "postprocess/apply_aroma/nonaggressive")
    if (is.null(scfg$postprocess$apply_aroma$prefix)) fields <- c(fields, "postprocess/apply_aroma/prefix")
  }

  if ("postprocess/apply_aroma/nonaggressive" %in% fields) {
    scfg$postprocess$apply_aroma$nonaggressive <- prompt_input("Use nonaggressive denoising? (Recommended: yes) ", type = "flag")
  }
  
  if ("postprocess/apply_aroma/prefix" %in% fields) {
    scfg$postprocess$apply_aroma$prefix <- prompt_input("File prefix: ", type = "character", default = "a")
  }
  
  return(scfg)
}

