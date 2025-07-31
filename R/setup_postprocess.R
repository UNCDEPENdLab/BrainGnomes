
#' Interactive menu for managing postprocessing streams
#'
#' Presents options to add, edit, delete, show, or finish editing postprocessing
#' streams. Used by both `setup_postprocess_streams()` and `edit_project()`.
#'
#' @param scfg A study configuration object, as produced by `setup_project()`.
#' @param allow_empty Logical indicating whether finishing with zero streams is
#'   permitted without confirmation.
#' @return Modified `scfg` with updated postprocessing streams
#' @keywords internal
manage_postprocess_streams <- function(scfg, allow_empty = FALSE) {
  postprocess_field_list <- function() {
    c(
      "input_regex", "bids_desc", "keep_intermediates", "overwrite",
      "tr", "brain_mask",
      "apply_mask/mask_file", "apply_mask/prefix",
      "spatial_smooth/fwhm_mm", "spatial_smooth/prefix",
      "apply_aroma/nonaggressive", "apply_aroma/prefix",
      "temporal_filter/low_pass_hz", "temporal_filter/high_pass_hz",
      "temporal_filter/method", "temporal_filter/prefix",
      "intensity_normalize/global_median", "intensity_normalize/prefix",
      "confound_calculate/columns", "confound_calculate/noproc_columns",
      "confound_calculate/demean",
      "scrubbing/expression", "scrubbing/add_to_confounds",
      "scrubbing/interpolate", "scrubbing/interpolate_prefix",
      "scrubbing/apply", "scrubbing/prefix",
      "confound_regression/columns", "confound_regression/noproc_columns",
      "confound_regression/prefix",
      "force_processing_order", "processing_steps"
    )
  }

  show_val <- function(val) {
    if (is.null(val)) "[NULL]"
    else if (is.logical(val)) toupper(as.character(val))
    else if (is.character(val) && length(val) > 1) paste(val, collapse = ", ")
    else as.character(val)
  }

  repeat {
    streams <- get_postprocess_stream_names(scfg)
    cat("\nCurrent postprocessing streams:\n")
    if (length(streams) == 0) {
      cat("  (none defined yet)\n")
    } else {
      cat("\n")
      for (i in seq_along(streams)) cat(sprintf("  [%d] %s\n", i, streams[i]))
      cat("\n")
    }

    choice <- menu(c("Add a stream", "Edit a stream", "Delete a stream",
                     "Show stream settings", "Finish"),
                   title = "Modify postprocessing streams:")

    if (choice == 1) {
      scfg <- setup_postprocess_stream(scfg) # add new stream
    } else if (choice == 2) {
      if (length(streams) == 0) {
        cat("No streams to edit.\n\n")
        next
      }
      # if we only have one stream, then default to editing it.
      sel <- if (length(streams) == 1L) streams else utils::select.list(streams, multiple = FALSE, title = "Select stream to edit")
      if (sel == "") next
      rel_fields <- postprocess_field_list()
      field_display <- sapply(rel_fields, function(fld) {
        val <- get_nested_values(scfg, paste0("postprocess/", sel, "/", fld))
        sprintf("%s [ %s ]", fld, show_val(val))
      })
      selected <- utils::select.list(field_display,
        multiple = TRUE,
        title = sprintf("Select fields to edit in %s:", sel)
      )

      if (length(selected) == 0) next
      selected_fields <- names(field_display)[field_display %in% selected]
      scfg <- setup_postprocess_stream(
        scfg,
        fields = paste0("postprocess/", sel, "/", selected_fields),
        stream_name = sel
      )
    } else if (choice == 3) {
      if (length(streams) == 0) {
        cat("No streams to delete.\n")
        next
      }
      sel <- utils::select.list(streams, multiple = TRUE, title = "Select stream(s) to delete")
      if (length(sel) == 0) next
      scfg$postprocess[sel] <- NULL
    } else if (choice == 4) {
      if (length(streams) == 0) {
        cat("No streams defined.\n")
        next
      }
      for (nm in streams) {
        cat(sprintf("\nStream: %s\n", nm))
        cat(yaml::as.yaml(scfg$postprocess[[nm]]))
      }
    } else if (choice == 5) {
      if (!allow_empty && length(streams) == 0L) {
        proceed <- prompt_input("No postprocessing streams were setup. Are you sure you want to finish? This will disable postprocessing entirely.", type = "flag", default = FALSE)
        if (!proceed) next
      }
      break
    }
  }

  return(scfg)
}

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
#' Interactively manage multiple postprocessing configurations. Users can add,
#' edit, or delete postprocessing streams. This wrapper is called by
#' `setup_project()` and invokes `setup_postprocess_stream()` for each stream.
#'
#' @return Modified `scfg` with one or more postprocessing streams
#' @keywords internal
setup_postprocess_streams <- function(scfg = list(), fields = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")

  if (is.null(scfg$postprocess$enable) || (isFALSE(scfg$postprocess$enable) && any(grepl("postprocess/", fields)))) {
    scfg$postprocess$enable <- prompt_input(
      instruct = glue("\n\n
        Postprocessing refers to the set of steps applied after fMRIPrep has produced preprocessed BOLD data.
                These steps may include:
                  - Applying a brain mask
                  - Spatial smoothing
                  - Denoising using ICA-AROMA
                  - 'Scrubbing' of high-motion/high-artifact timepoints
                  - Temporal filtering (e.g., high-pass filtering)
                  - Intensity normalization
                  - Confound calculation and regression

        Do you want to enable postprocessing of the BOLD data?\n"
      ),
      prompt = "Enable postprocessing?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(scfg$postprocess$enable)) return(scfg)

  # if fields are present, prompt only for those that are present
  if (!is.null(fields) && any(grepl("^postprocess/", fields))) {
    postprocess_fields <- grep("^postprocess/", fields, value = TRUE)

    # Extract stream and setting using sub()
    # stream_setting <- sub("^postprocess/", "", postprocess_fields)
    stream_split <- strsplit(postprocess_fields, "/", fixed = TRUE)

    # Build a named list of settings by stream
    stream_list <- split(
      postprocess_fields,
      vapply(stream_split, function(parts) parts[[2]], character(1))
    )

    for (ss in seq_along(stream_list)) {
      scfg <- setup_postprocess_stream(scfg, fields = stream_list[[ss]], stream_name = names(stream_list)[ss])
    }

    return(scfg) # skip out before menu system when fields are passed
  }

  cat(glue("\n
      Postprocessing supports multiple streams, allowing you to postprocess data in multiple ways.
      Each stream also asks about which files should be postprocessed using the stream. For example,
      files with 'rest' in their name could be postprocessed in one way and files with 'nback' could
      be processed a different way.\n
      "))

  scfg <- manage_postprocess_streams(scfg, allow_empty = TRUE)

  return(scfg)
}


setup_postprocess_stream <- function(scfg = list(), fields = NULL, stream_name = NULL) {
  checkmate::assert_string(stream_name, null.ok = TRUE)

  if (!checkmate::test_class(scfg, "bg_project_cfg")) {
    stop("scfg input must be a bg_project_cfg object produced by setup_project")
  }

  defaults <- list(
    memgb = 48L,
    nhours = 8L,
    ncores = 1L,
    cli_options = "",
    sched_args = ""
  )

  # enable should be set by setup_postprocess_streams -- if it's FALSE, don't even think about specific streams
  if (isFALSE(scfg$postprocess$enable)) return(scfg)

  # convert fields from postprocess/<stream_name>/field to postprocess/field for simplicity in subordinate setup functions
  if (!is.null(fields)) fields <- sub(paste0("^postprocess/", stream_name, "/"), "postprocess/", fields)

  existing_cfg <- TRUE
  if (is.null(stream_name) || !stream_name %in% names(scfg$postprocess)) {
    ppcfg <- list()
    existing_cfg <- FALSE
  } else {
    ppcfg <- scfg$postprocess[[stream_name]]
  }
  
  prompt_name <- !existing_cfg
  if (existing_cfg && "postprocess/name" %in% fields) {
    prompt_name <- prompt_input(
      instruct=glue("This configuration is called {stream_name}."),
      prompt="Change name?", type = "flag")
  }
  
  stream_names <- get_postprocess_stream_names(scfg)
  if (prompt_name) {
    name_valid <- FALSE
    while (!name_valid) {
      stream_name <- prompt_input(prompt = "Name for this postprocess configuration", type = "character")
      if (stream_name %in% stream_names) {
        message("Configuration name must be unique. Existing names are: ", paste(stream_names, collapse = ", "))
      } else if (stream_name == "enable") {
        message("Stream name cannot be 'enable'.")
      } else {
        name_valid <- TRUE
      }
    }
  }
  
  # validate unique bids_desc
  all_bids_desc <- unlist(lapply(stream_names[stream_names != stream_name], function(nm) scfg$postprocess[[nm]]$bids_desc))
  ppcfg <- setup_postprocess_globals(ppcfg, fields, all_bids_desc)
  # setup_job requires the top-level list for postprocess -- spoof this for handling nested field names
  spoof <- list(postprocess = ppcfg)
  spoof <- setup_job(spoof, "postprocess", defaults, fields)
  ppcfg <- spoof$postprocess
  ppcfg <- setup_apply_mask(ppcfg, fields)
  ppcfg <- setup_spatial_smooth(ppcfg, fields)
  ppcfg <- setup_apply_aroma(ppcfg, fields)
  ppcfg <- setup_temporal_filter(ppcfg, fields)
  ppcfg <- setup_intensity_normalization(ppcfg, fields)
  ppcfg <- setup_confound_calculate(ppcfg, fields)
  ppcfg <- setup_scrubbing(ppcfg, fields)
  ppcfg <- setup_confound_regression(ppcfg, fields)
  ppcfg <- setup_postprocess_steps(ppcfg, fields)

  # repopulate the relevant part of scfg
  scfg$postprocess[[stream_name]] <- ppcfg
  return(scfg)
}

setup_postprocess_globals <- function(ppcfg = list(), fields = NULL, all_bids_desc = NULL) {

  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$input_regex)) fields <- c(fields, "postprocess/input_regex")
    if (is.null(ppcfg$bids_desc)) fields <- c(fields, "postprocess/bids_desc")
    if (is.null(ppcfg$keep_intermediates)) fields <- c(fields, "postprocess/keep_intermediates")
    if (is.null(ppcfg$overwrite)) fields <- c(fields, "postprocess/overwrite")
    if (is.null(ppcfg$tr)) fields <- c(fields, "postprocess/tr")
    if (is.null(ppcfg$apply_mask)) fields <- c(fields, "postprocess/apply_mask")
    if (is.null(ppcfg$brain_mask)) fields <- c(fields, "postprocess/brain_mask")
  }

  # global postprocessing settings
  if ("postprocess/input_regex" %in% fields) {
    ppcfg$input_regex <- prompt_input(
      "What is the relevant file extension (or regular expression) for inputs?",
      type = "character", len = 1L, default = ".*_desc-preproc_bold.nii.gz$",
      instruct = glue("\n\n
      Postprocessing is typically only applied to BOLD data that have completed preprocessing in fmriprep.
      These files usually have a suffix like _desc-preproc_bold.nii.gz. However, you may have postprocessing settings
      that only apply to certain outputs, such as for a particular experimental task or for resting state.

      What is the file extension for functional data to be postprocessed? If, for example, you only want
      files for a task called 'ridl', use a regular expression like, '.*_task-ridl.*_desc-preproc_bold.nii.gz$'. Note
      that having the $ add the end of the regular expression ensures that the file ends with the specified suffix.\n
      ")
    )
  }

  if ("postprocess/bids_desc" %in% fields) {
    bids_desc_valid <- FALSE

    while (!bids_desc_valid) {
      ppcfg$bids_desc <- prompt_input(
        "Enter the BIDS description ('desc') for the fully postprocessed file",
        type = "character", len = 1L, default = "postproc",
        instruct = glue("\n
          What should be the description field for the final postprocessed file?
          This will yield a name like sub-540294_task-ridl_run-01_space-MNI152NLin6Asym_desc-postprocess_bold.nii.gz.\n
        ")
      )
      
      if (ppcfg$bids_desc %in% all_bids_desc) {
        message("bids_desc must be unique across postprocess configurations. Current values are: ", paste(all_bids_desc, collapse = ", "))
      } else {
        bids_desc_valid <- TRUE
      }
    }    
  }
  
  if ("postprocess/keep_intermediates" %in% fields) {
    ppcfg$keep_intermediates <- prompt_input("Do you want to keep postprocess intermediate files? This is typically only for debugging.", type = "flag", default = FALSE)
  }
  if ("postprocess/overwrite" %in% fields) {
    ppcfg$overwrite <- prompt_input("Overwrite existing postprocess files?", type = "flag", default = FALSE)
  }
  if ("postprocess/tr" %in% fields) {
    ppcfg$tr <- prompt_input("Repetition time (in seconds) of the scan sequence", type = "numeric", lower = 0.01, upper = 100, len = 1)
  }

  if ("postprocess/brain_mask" %in% fields) {
    ppcfg$brain_mask <- prompt_input("Brain mask to be used in postprocessing",
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

  return(ppcfg)

}

#' Specify the postprocessing steps for a study
#'
#' This function determines the sequence of postprocessing steps to be applied after fMRIPrep.
#' Steps are included based on whether the corresponding `$enable` field is `TRUE` in the study configuration.
#' If the user opts to override the default order, they may manually specify a custom sequence.
#'
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields a character vector of fields to be prompted for. If `NULL`, all fields will be prompted for.
#' @return a modified version of `ppcfg` with the `$postprocess$processing_steps` field populated
#' @keywords internal
#' @details This function is used to set up the postprocessing steps for a study. It prompts the user for
#'   the order of the processing steps and whether to apply them. The order of the processing steps is important,
#'   particularly because if we filter certain frequencies from the fMRI data, we must filter any regressors that we
#'   later apply to the data -- that is, confounds and fMRI data must match in frequency content prior to regression.
#'   See Hallquist, Hwang, & Luna (2013) or Lindquist (2019) for details.
setup_postprocess_steps <- function(ppcfg = list(), fields = NULL) {
  # if (is.null(scfg$postprocess$processing_steps)) {
  #   stop("missing processing_steps. Run out of order?")
  # }

  # typical (usually correct) order
  step_order <- c(
    "apply_mask",
    "spatial_smooth",
    "apply_aroma",
    "scrub_interpolate",
    "temporal_filter",
    "confound_regression",
    "scrub_timepoints",
    "intensity_normalize"
  )

  processing_sequence <- character(0)
  for (step in step_order) {
    enabled <- if (step == "scrub_timepoints") {
      tryCatch(isTRUE(ppcfg$scrubbing$enable) && isTRUE(ppcfg$scrubbing$apply), error = function(e) FALSE)
    } else if (step == "scrub_interpolate") {
      tryCatch(isTRUE(ppcfg$scrubbing$enable) && isTRUE(ppcfg$scrubbing$interpolate), error = function(e) FALSE)
    } else {
      tryCatch(isTRUE(ppcfg[[step]]$enable), error = function(e) FALSE)
    }
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
  if (is.null(ppcfg$force_processing_order) || "postprocess/force_processing_order" %in% fields) {
    ppcfg$force_processing_order <- prompt_input("Do you want to specify the postprocessing sequence?",
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
  if (isTRUE(ppcfg$force_processing_order)) {
    proceed <- FALSE
    while (!proceed) {
      seq_glue <- glue("\nEnabled processing steps:\n\n{paste(seq_along(processing_sequence), processing_sequence, collapse = '\n', sep = '. ')}\n")
      ss <- prompt_input(
        "Choose the order (separated by spaces)",
        instruct = seq_glue,
        type = "integer", lower = 1, upper = length(processing_sequence), len = length(processing_sequence),
        split = "\\s+", uniq = TRUE
      )
      proceed_glue <- glue("\nYou specified the following order:\n\n{paste(seq_along(ss), processing_sequence[ss], collapse = '\n', sep = '. ')}\n")
      proceed <- prompt_input("Is this correct?", instruct = proceed_glue, type = "flag")
    }
    ppcfg$processing_steps <- processing_sequence[ss]
  } else {
    ppcfg$processing_steps <- processing_sequence
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

  return(ppcfg)
}

#' Configure scrubbing of high-motion volumes
#'
#' Generates spike regressors based on expressions evaluated on the confounds
#' file (e.g., "framewise_displacement > 0.9" or "-1:1; dvars > 1.5"). These regressors can later be
#' used to censor volumes during modeling.
#'
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields Optional vector of fields to prompt for.
#' @return Modified `scfg` with `$postprocess$scrubbing` populated.
#' @keywords internal
setup_scrubbing <- function(ppcfg = list(), fields = NULL) {
  if (is.null(ppcfg$scrubbing$enable) ||
      (isFALSE(ppcfg$scrubbing$enable) && any(grepl("postprocess/scrubbing/", fields)))) {
    ppcfg$scrubbing$enable <- prompt_input(
      instruct = glue("\n\n
      -------------------------------------------------------------------------------------------------------------------------
      Scrubbing identifies timepoints (volumes) with excessive motion or artifacts based on a user-defined expression, such as:
      'framewise_displacement > 0.9'. If you choose 'yes,' you will be prompted to define what constitutes a bad timepoint.
      This will generate two output files:
        1. Spike Regressors File (*_scrub.tsv):
          A binary matrix with one column per bad timepoint. Each column contains a 1 at the scrubbed timepoint and 0 elsewhere.
	      2. AFNI-Compatible Censor File (*_censor.1D):
          A single-column file with 1s indicating good timepoints and 0s for bad timepoints.

      You will then be asked whether to append these spike regressors to the other confound variables identified during
      the confound calculation step. If you say yes, the final *_confounds.tsv file will include the spike regressors
      alongside the other selected confounds.

      Next, you will be asked whether to interpolate scrubbed timepoints before temporal filtering and confound regression:
	      - Interpolation before filtering helps reduce ringing artifacts caused by abrupt signal spikes (Carp, 2012, NeuroImage).
	      - Interpolation before regression ensures that model fits are influenced more by clean data than by high-motion spikes.
        
      Finally, you will be asked whether to remove bad timepoints from the final preprocessed NIfTI image. This is common in
      resting-state fMRI, where scrubbed data are used to compute functional connectivity. Selecting 'yes' will yield a NIfTI
      file with all bad timepoints excluded.

      Scrubbing expression FYI: You can specify a range of volumes to scrub using a format like: '-1:1; dvars > 1.5'.
	    The part before the semicolon (-1:1) defines a temporal window around the bad timepoint. For example, '-1:0'
      scrubs the bad timepoint and the volume before it. If omitted, only the bad timepoint (0) is scrubbed.
      The part after the semicolon ('dvars > 1.5') defines the thresholding condition.

      Do you want to generate scrubbing regressors?\n"),
      prompt = "Enable scrubbing?",
      type = "flag",
      default = FALSE
    )
  }

  if (isFALSE(ppcfg$scrubbing$enable)) return(ppcfg)

  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$scrubbing$expression)) fields <- c(fields, "postprocess/scrubbing/expression")
    if (is.null(ppcfg$scrubbing$add_to_confounds)) fields <- c(fields, "postprocess/scrubbing/add_to_confounds")
    if (is.null(ppcfg$scrubbing$interpolate)) fields <- c(fields, "postprocess/scrubbing/interpolate")
    if (is.null(ppcfg$scrubbing$interpolate_prefix)) ppcfg$scrubbing$interpolate_prefix <- "i"
    if (is.null(ppcfg$scrubbing$apply)) fields <- c(fields, "postprocess/scrubbing/apply")
    if (is.null(ppcfg$scrubbing$prefix)) ppcfg$scrubbing$prefix <- "x"
  }

  if ("postprocess/scrubbing/expression" %in% fields) {
    ppcfg$scrubbing$expression <- prompt_input(
      "Scrubbing expression(s)",
      type = "character", required = TRUE # expression is a single character string
    )
  }

  if ("postprocess/scrubbing/add_to_confounds" %in% fields) {
    ppcfg$scrubbing$add_to_confounds <- prompt_input(
      prompt="Add any spike regressors (bad volumes) to postprocessed confounds.tsv file?",
      type = "flag", required = TRUE, default = TRUE
    )
  }

  if ("postprocess/scrubbing/interpolate" %in% fields) {
    ppcfg$scrubbing$interpolate <- prompt_input(
      instruct = glue("\n\
      Do you want to interpolate over scrubbed timepoints before applying temporal filtering,
      confound regression, and/or intensity normalization? This is achieved using cubic natural spline interpolation.
      \n"),
      prompt = "Interpolate scrubbed values?",
      type = "flag",
      default = FALSE
    )
  }
  
  if ("postprocess/scrubbing/interpolate_prefix" %in% fields) {
    ppcfg$scrubbing$interpolate_prefix <- prompt_input(
      prompt = "File description prefix for interpolated output",
      type = "character", default = "i"
    )
  }

  if ("postprocess/scrubbing/apply" %in% fields) {
    ppcfg$scrubbing$apply <- prompt_input(
      instruct = glue("\n\
      Do you want to remove the scrubbed timepoints from the fMRI time series?
      \n"),
      prompt = "Remove scrubbed timepoints?",
      type = "flag",
      default = FALSE
    )
  }
  
  if ("postprocess/scrubbing/prefix" %in% fields) {
    ppcfg$scrubbing$prefix <- prompt_input(
      prompt = "File description prefix for scrubbed output",
      type = "character", default = "x"
    )
  }

  return(ppcfg)
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
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields A character vector of fields to be prompted for. If `NULL`, all fields related to brain masking will be prompted.
#'
#' @return A modified version of `ppcfg` with the `$apply_mask` entry populated.
#' @keywords internal
setup_apply_mask <- function(ppcfg = list(), fields = NULL) {
  if (is.null(ppcfg$apply_mask$enable) ||
      (isFALSE(ppcfg$apply_mask$enable) && any(grepl("postprocess/apply_mask/", fields)))) {

    ppcfg$apply_mask$enable <- prompt_input(
      instruct = glue("\n\n
      ------------------------------------------------------------------------------------------------------------------------
      Applying a brain mask to your fMRI data ensures that only in-brain voxels are retained during analysis.
      This step is optional but often recommended for improving efficiency and accuracy in subsequent processing.
      
      The mask will be applied as a binary filter to the 4D functional data, zeroing out signal outside the brain.
      You can specify a custom mask file (in the same space and resolution as your fMRI data), or use a mask
      that matches the template space of the preprocessed data (e.g., MNI152NLin2009cAsym).

      Do you want to apply a brain mask to your fMRI data?\n
      "),
      prompt = "Apply brain mask?",
      type = "flag",
      default = TRUE
    )
  }

  if (isFALSE(ppcfg$apply_mask$enable)) return(ppcfg)

  # Determine which fields to prompt for
  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$apply_mask$mask_file)) fields <- c(fields, "postprocess/apply_mask/mask_file")
    if (is.null(ppcfg$apply_mask$prefix)) ppcfg$apply_mask$prefix <- "m"
  }

  if ("postprocess/apply_mask/mask_file" %in% fields) {
    repeat {
      mask_file <- prompt_input(
        instruct = "Type 'template' to use the brain mask from TemplateFlow matching the stereotaxic space of the image ('space-<identifier>').",
        prompt = "Path to brain mask NIfTI file",
        type = "character",
        required = FALSE
      )
      # mask can be empty, template, or valid file
      if (checkmate::test_file_exists(mask_file) || is.na(mask_file[1L]) || mask_file == "template") {
        ppcfg$apply_mask$mask_file <- mask_file
        break
      } else {
        cat("Cannot find mask_file:", mask_file, "\n")
      }
    }
  }

  if ("postprocess/apply_mask/prefix" %in% fields) {
    ppcfg$apply_mask$prefix <- prompt_input(
      prompt = "File description prefix for masked output",
      type = "character", default = "m"
    )
  }

  return(ppcfg)
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
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields A character vector of fields to be prompted for. If `NULL`, all fields will be prompted.
#'
#' @return A modified version of `ppcfg` with the `$confound_regression` entry populated.
#' @keywords internal
setup_confound_regression <- function(ppcfg = list(), fields = NULL) {
    if (is.null(ppcfg$confound_regression$enable) ||
      (isFALSE(ppcfg$confound_regression$enable) && any(grepl("postprocess/confound_regression/", fields)))) {

    ppcfg$confound_regression$enable <- prompt_input(
      instruct = glue("\n\n
      ------------------------------------------------------------------------------------------------------------------------
      Confound regression applies voxelwise multiple regression to remove nuisance signals from the fMRI data.
      These regressors are typically selected from the confounds file produced by fMRIPrep.

      You can specify two types of confound regressors:
        - Filtered regressors: continuous-valued (e.g., a_comp_cor_.*, DVARS, global signal)
        - Unfiltered regressors: discrete-valued (e.g., motion_outlier.**) that should not be filtered

      The specification can be a regular expression (e.g., a_comp_cor_[0-9+]) or a numeric range using
      a syntax like a_comp_cor_<1-10> or a subset like a_comp_cor_<1,2,5>. These will then be expanded
      to encompass the matching nuisance regressors (e.g., the first 10 aCompCor components).

      If scrubbing is enabled, confound regression fits are based only on the good timepoints.

      Do you want to apply confound regression to the fMRI data during postprocessing?\n
      "),
      prompt = "Apply confound regression?",
      type = "flag"
    )
  }

  if (isFALSE(ppcfg$confound_regression$enable)) return(ppcfg)

  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$confound_regression$columns)) fields <- c(fields, "postprocess/confound_regression/columns")
    if (is.null(ppcfg$confound_regression$noproc_columns)) fields <- c(fields, "postprocess/confound_regression/noproc_columns")
    if (is.null(ppcfg$confound_regression$prefix)) ppcfg$confound_regression$prefix <- "r"
  } 
    
  if ("postprocess/confound_regression/columns" %in% fields) {
    ppcfg$confound_regression$columns <- prompt_input(
      prompt = "Confounds that will be filtered",
      type = "character", split = "\\s+", required = FALSE
    )
  }

  if ("postprocess/confound_regression/noproc_columns" %in% fields) {
    ppcfg$confound_regression$noproc_columns <- prompt_input(
      prompt = "Confounds that will not be filtered",
      type = "character", split = "\\s+", required = FALSE
    )
  }

  if ("postprocess/confound_regression/prefix" %in% fields) {
    ppcfg$confound_regression$prefix <- prompt_input(
      prompt = "File description prefix for regression output",
      type = "character", default = "r"
    )
  }

  return(ppcfg)
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
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields A character vector of fields to prompt for. If `NULL`, all relevant fields will be prompted.
#'
#' @return A modified version of `ppcfg` with the `$confound_calculate` entry updated.
#' @keywords internal
setup_confound_calculate <- function(ppcfg = list(), fields = NULL) {
  if (is.null(ppcfg$confound_calculate$enable) ||
      (isFALSE(ppcfg$confound_calculate$enable) && any(grepl("postprocess/confound_calculate/", fields)))) {

    ppcfg$confound_calculate$enable <- prompt_input(
      instruct = glue("\n\n
      ------------------------------------------------------------------------------------------------------------------------
      Confound calculation creates a nuisance regressor file that includes relevant noise signals (e.g., motion, 
      CompCor, global signal). This step does not apply denoising but prepares a file that can be used in later 
      statistical analyses (e.g., voxelwise GLMs).

      You can specify two types of confound regressors:
        - Filtered regressors: typically continuous-valued signals derived from fMRI (e.g., DVARS, a_comp_cor_*, global signal)
        - Unfiltered regressors: typically discrete-valued indicators (e.g., motion_outlier*) that should not be filtered

      The specification can be a regular expression (e.g., a_comp_cor_[0-9+]) or a numeric range using
      a syntax like a_comp_cor_<1-10> or a subset like a_comp_cor_<1,2,5>. These will then be expanded
      to encompass the matching confound regressors (e.g., the first 10 aCompCor components).

      Do you want to create a confound file in postprocessing?\n
      "),
      prompt = "Generate confound file?",
      type = "flag"
    )
  }

  if (isFALSE(ppcfg$confound_calculate$enable)) return(ppcfg)

  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$confound_calculate$columns)) fields <- c(fields, "postprocess/confound_calculate/columns")
    if (is.null(ppcfg$confound_calculate$noproc_columns)) fields <- c(fields, "postprocess/confound_calculate/noproc_columns")
    if (is.null(ppcfg$confound_calculate$demean)) fields <- c(fields, "postprocess/confound_calculate/demean")
  }

  if ("postprocess/confound_calculate/columns" %in% fields) {
    ppcfg$confound_calculate$columns <- prompt_input(
      prompt="Confounds that will be filtered",
      type = "character", split = "\\s+", required = FALSE
    )
  }

  if ("postprocess/confound_calculate/noproc_columns" %in% fields) {
    ppcfg$confound_calculate$noproc_columns <- prompt_input(
      prompt="Confounds that will not be filtered",
      type = "character", split = "\\s+", required = FALSE
    )
  }
  
  if ("postprocess/confound_calculate/demean" %in% fields) {
    ppcfg$confound_calculate$demean <- prompt_input("Demean (filtered) regressors?", type = "flag", default = TRUE)
  }

  return(ppcfg)
}

#' Configure intensity normalization settings for postprocessing
#'
#' This function configures the intensity normalization step in the postprocessing pipeline.
#' Intensity normalization rescales the fMRI time series so that the median signal across the entire 4D image
#' reaches a specified global value (e.g., 10,000). This step can help ensure comparability across runs and subjects.
#'
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields A character vector of field names to prompt for. If `NULL`, all intensity normalization fields will be prompted.
#'
#' @return A modified version of `ppcfg` with the `$intensity_normalize` entry updated.
#' @keywords internal
setup_intensity_normalization <- function(ppcfg = list(), fields = NULL) {
  if (is.null(ppcfg$intensity_normalize$enable) ||
    (isFALSE(ppcfg$intensity_normalize$enable) && any(grepl("postprocess/intensity_normalize/", fields)))) {
    ppcfg$intensity_normalize$enable <- prompt_input(
      instruct = glue("\n\n
      ------------------------------------------------------------------------------------------------------------------------
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
  if (isFALSE(ppcfg$intensity_normalize$enable)) return(ppcfg)


  # if fields passed in, only bother use about the requested fields
  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$intensity_normalize$global_median)) fields <- c(fields, "postprocess/intensity_normalize/global_median")
    if (is.null(ppcfg$intensity_normalize$prefix)) ppcfg$intensity_normalize$prefix <- "n"
  }

  if ("postprocess/intensity_normalize/global_median" %in% fields) {
    ppcfg$intensity_normalize$global_median <- prompt_input(
      prompt="Global (4D) median intensity",
      type = "numeric", lower = -1e8, upper = 1e8, default = 10000
    )
  }

  if ("postprocess/intensity_normalize/prefix" %in% fields) {
    ppcfg$intensity_normalize$prefix <- prompt_input(
      prompt="File description prefix for normalized output",
      type = "character", default = "n"
    )
  }

  return(ppcfg)
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
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields A character vector of field names to prompt for. If `NULL`, all spatial smoothing fields will be prompted.
#'
#' @return A modified version of `ppcfg` with the `$spatial_smooth` field populated.
#'
#' @details
#' If enabled, spatial smoothing is applied to the preprocessed BOLD data using a Gaussian kernel
#' with the user-specified FWHM in millimeters. This can help improve sensitivity and inter-subject alignment,
#' especially in standard space. This is accomplished using FSL's contrast-sensitive susan smoothing command.
#'
#' @keywords internal
setup_spatial_smooth <- function(ppcfg = list(), fields = NULL) {
  if (is.null(ppcfg$spatial_smooth$enable) || (isFALSE(ppcfg$spatial_smooth$enable) && any(grepl("postprocess/spatial_smooth/", fields)))) {
    ppcfg$spatial_smooth$enable <- prompt_input(
      instruct = glue("\n\n
      ------------------------------------------------------------------------------------------------------------------------
      Spatial smoothing applies a 3D Gaussian kernel to the BOLD fMRI data,
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
  if (isFALSE(ppcfg$spatial_smooth$enable)) return(ppcfg)
  
  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$spatial_smooth$fwhm_mm)) fields <- c(fields, "postprocess/spatial_smooth/fwhm_mm")
    if (is.null(ppcfg$spatial_smooth$prefix)) ppcfg$spatial_smooth$prefix <- "s"
  }

  if ("postprocess/spatial_smooth/fwhm_mm" %in% fields) {
    ppcfg$spatial_smooth$fwhm_mm <- prompt_input(
      prompt="Spatial smoothing FWHM (mm)",
      type = "numeric", lower = 0.1, upper = 100
    )
  }

  if ("postprocess/spatial_smooth/prefix" %in% fields) {
    ppcfg$spatial_smooth$prefix <- prompt_input(
      prompt = "File description prefix for smoothed output",
      type = "character", default = "s"
    )
  }
  
  return(ppcfg)
}

#' Configure temporal filtering settings for postprocessing
#'
#' This function configures the temporal filtering step in the postprocessing pipeline.
#' Temporal filtering removes unwanted frequency components from the BOLD signal, such as
#' slow drifts (via high-pass filtering) or physiological noise (via low-pass filtering).
#' This step is often used to improve signal quality for subsequent statistical analysis.
#'
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields A character vector of field names to prompt for. If `NULL`, all temporal filtering fields will be prompted.
#'
#' @return A modified version of `ppcfg` with the `$temporal_filter` entry updated.
#' @keywords internal
setup_temporal_filter <- function(ppcfg = list(), fields = NULL) {
  if (is.null(ppcfg$temporal_filter$enable) ||
      (isFALSE(ppcfg$temporal_filter$enable) && any(grepl("postprocess/temporal_filter/", fields)))) {

    ppcfg$temporal_filter$enable <- prompt_input(
      instruct = glue("\n\n
      ------------------------------------------------------------------------------------------------------------------------
      Temporal filtering removes low- and/or high-frequency components from the fMRI time series.
      A high-pass filter (e.g., 0.008 Hz) is commonly used to remove slow scanner drift, while a low-pass
      filter can remove physiological noise such as respiratory or cardiac fluctuations.

      We support both fslmaths -bptf and an Butterworth temporal filter. The former is more common for task-based fMRI and
      the latter with resting-state fMRI. If filtering is enabled, you will be asked to choose between these filtering methods.
      
      Per fsl's documentation, -bptf uses a local fit of a straight line (Gaussian-weighted within the line to
      give a smooth response) to remove low frequency artifacts. This is preferable to sharp rolloff FIR-based filtering
      as it does not introduce autocorrelations into the data. Lowpass temporal filtering reduces high frequency noise by
      Gaussian smoothing, but also reduces the strength of the signal of interest, particularly for single-event experiments.

      For the Butterworth filter, you will be asked to specify the filter order. An order of 2 is common and should be considered
      the default because it has a smooth roll-off, reducing ringing artifacts.

      Do you want to apply temporal filtering to each fMRI run?\n
      "),
      prompt = "Apply temporal filtering?",
      type = "flag",
      default = TRUE
    )
  }

  # Exit early if user disabled the step
  if (isFALSE(ppcfg$temporal_filter$enable)) return(ppcfg)

  # Determine which fields to prompt for
  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$temporal_filter$low_pass_hz)) fields <- c(fields, "postprocess/temporal_filter/low_pass_hz")
    if (is.null(ppcfg$temporal_filter$high_pass_hz)) fields <- c(fields, "postprocess/temporal_filter/high_pass_hz")
    if (is.null(ppcfg$temporal_filter$prefix)) ppcfg$temporal_filter$prefix <- "f"
    if (is.null(ppcfg$temporal_filter$method)) fields <- c(fields, "postprocess/temporal_filter/method")
  }

  if ("postprocess/temporal_filter/low_pass_hz" %in% fields) {
    ppcfg$temporal_filter$low_pass_hz <- prompt_input("Low-pass cutoff (Hz)", type = "numeric", lower = 0, required = FALSE)
    if (is.na(ppcfg$temporal_filter$low_pass_hz)) {
      cat("Omitting low-pass filtering\n")
      ppcfg$temporal_filter$low_pass_hz <- -Inf # indicates no low-pass cutoff
    }
  }
  
  if ("postprocess/temporal_filter/high_pass_hz" %in% fields) {
    ppcfg$temporal_filter$high_pass_hz <- prompt_input("High-pass cutoff (Hz) ", type = "numeric", lower = 0, required = FALSE)
    if (is.na(ppcfg$temporal_filter$high_pass_hz)) {
      cat("Omitting high-pass filtering\n")
      ppcfg$temporal_filter$high_pass_hz <- Inf # indicates no high-pass cutoff
    }
  }

  if (!is.null(ppcfg$temporal_filter$low_pass_hz) && !is.null(ppcfg$temporal_filter$high_pass_hz) &&
    ppcfg$temporal_filter$low_pass_hz > ppcfg$temporal_filter$high_pass_hz) stop("Low-pass cutoff cannot be larger than high-pass cutoff")


  if ("postprocess/temporal_filter/method" %in% fields) {
    ppcfg$temporal_filter$method <- prompt_input(
      prompt = "Filtering method (fslmaths/butterworth)",
      type = "character", among = c("fslmaths", "butterworth"), default = "fslmaths",
      instruct = glue("\n
        Choose the implementation for temporal filtering:
          - 'fslmaths' uses FSL's fslmaths -bptf command.
          - 'butterworth' uses an R-based Butterworth filter.\n")
    )
  }
  
  if ("postprocess/temporal_filter/prefix" %in% fields) {
    ppcfg$temporal_filter$prefix <- prompt_input(
      prompt="File description prefix for filtered output",
      type = "character", default = "f"
    )
  }
  
  return(ppcfg)
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
#' @param ppcfg a postprocessing configuration list (nested within scfg$postprocess)
#' @param fields A character vector of fields to prompt for. If `NULL`, all fields will be prompted.
#'
#' @return A modified version of `ppcfg` with the `$apply_aroma` entry updated.
#' @keywords internal
setup_apply_aroma <- function(ppcfg = list(), fields = NULL) {
  if (is.null(ppcfg$apply_aroma$enable) ||
      (isFALSE(ppcfg$apply_aroma$enable) && any(grepl("postprocess/apply_aroma/", fields)))) {

    ppcfg$apply_aroma$enable <- prompt_input(
      instruct = glue("\n\n
      ------------------------------------------------------------------------------------------------------------------------
      ICA-AROMA identifies motion-related independent components from the fMRI data and outputs
      noise regressors (e.g., *_desc-aroma_timeseries.tsv) that can be used to denoise the BOLD signal.
      The pipeline asks you decide about whether to run the data through ICA-AROMA, which generates various
      files, but does not make any changes to the fMRI data.

      Here, you can denoise the data using ICA-AROMA, which step applies the noise regressors 
      to the data using voxelwise regression, either:
        - nonaggressively (recommended), which removes *unique* variance from noise components only, or
        - aggressively, which removes all variance associated with the noise components.

      Do you want to apply ICA-AROMA denoising during postprocessing?\n
      "),
      prompt = "Apply AROMA denoising?",
      type = "flag"
    )
  }

  if (isFALSE(ppcfg$apply_aroma$enable)) return(ppcfg)

  if (is.null(fields)) {
    fields <- c()
    if (is.null(ppcfg$apply_aroma$nonaggressive)) fields <- c(fields, "postprocess/apply_aroma/nonaggressive")
    if (is.null(ppcfg$apply_aroma$prefix)) ppcfg$apply_aroma$prefix <- "a"
  }

  if ("postprocess/apply_aroma/nonaggressive" %in% fields) {
    ppcfg$apply_aroma$nonaggressive <- prompt_input("Use nonaggressive denoising?", type = "flag", default = TRUE)
  }
  
  if ("postprocess/apply_aroma/prefix" %in% fields) {
    ppcfg$apply_aroma$prefix <- prompt_input(
      prompt="File description prefix for AROMA output",
      type = "character", default = "a"
    )
  }
  
  return(ppcfg)
}

