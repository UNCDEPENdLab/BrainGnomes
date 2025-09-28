#' @keywords internal
manage_extract_streams <- function(scfg, allow_empty = FALSE) {
  extract_field_list <- function() {
    c("input_streams", "atlases", "mask_file", "roi_reduce", "correlation/method", "rtoz")
  }

  show_val <- function(val) {
    if (is.null(val)) "[NULL]"
    else if (is.logical(val)) toupper(as.character(val))
    else if (is.character(val) && length(val) > 1) paste(val, collapse = ", ")
    else as.character(val)
  }

  repeat {
    streams <- get_extract_stream_names(scfg)
    cat("\nCurrent extraction streams:\n")
    if (length(streams) == 0) {
      cat("  (none defined yet)\n")
    } else {
      cat("\n")
      for (i in seq_along(streams)) cat(sprintf("  [%d] %s\n", i, streams[i]))
      cat("\n")
    }

    choice <- menu_safe(c("Add a stream", "Edit a stream", "Delete a stream",
                         "Show stream settings", "Finish"),
                       title = "Modify extraction streams:")

    if (choice == 1) {
      all_streams <- get_postprocess_stream_names(scfg)
      if (length(all_streams) == 0) {
        cat("You must define at least one postprocess stream before defining an extraction stream.\n\n")
        next
      }
      scfg <- setup_extract_stream(scfg) # add new stream
    } else if (choice == 2) {
      if (length(streams) == 0) {
        cat("No streams to edit.\n\n")
        next
      }
      sel <- if (length(streams) == 1L) streams else select_list_safe(streams, multiple = FALSE, title = "Select stream to edit")
      if (sel == "") next
      rel_fields <- extract_field_list()
      field_display <- sapply(rel_fields, function(fld) {
        val <- get_nested_values(scfg, paste0("extract_rois/", sel, "/", fld))
        sprintf("%s [ %s ]", fld, show_val(val))
      })
      selected <- select_list_safe(field_display,
        multiple = TRUE,
        title = sprintf("Select fields to edit in %s:", sel)
      )
      if (length(selected) == 0) next
      selected_fields <- names(field_display)[field_display %in% selected]
      scfg <- setup_extract_stream(
        scfg,
        fields = paste0("extract_rois/", sel, "/", selected_fields),
        stream_name = sel
      )
    } else if (choice == 3) {
      if (length(streams) == 0) {
        cat("No streams to delete.\n")
        next
      }
      sel <- select_list_safe(streams, multiple = TRUE, title = "Select stream(s) to delete")
      if (length(sel) == 0) next
      scfg$extract_rois[sel] <- NULL
    } else if (choice == 4) {
      if (length(streams) == 0) {
        cat("No streams defined.\n")
        next
      }
      for (nm in streams) {
        cat(sprintf("\nStream: %s\n", nm))
        cat(yaml::as.yaml(scfg$extract_rois[[nm]]))
      }
    } else if (choice == 5) {
      if (!allow_empty && length(streams) == 0L) {
        proceed <- prompt_input("No extraction streams were setup. Are you sure you want to finish?", type = "flag", default = FALSE)
        if (!proceed) next
      }
      break
    }
    else if (choice == 0) {
      break
    }
  }

  return(scfg)
}

#' @keywords internal
setup_extract_streams <- function(scfg = list(), fields = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")

  if (is.null(scfg$extract_rois$enable) || (isFALSE(scfg$extract_rois$enable) && any(grepl("extract_rois/", fields))) || ("extract_rois/enable" %in% fields)) {
    scfg$extract_rois$enable <- prompt_input("Perform ROI extraction?", type = "flag", default = if (is.null(scfg$extract_rois$enable)) FALSE else isTRUE(scfg$extract_rois$enable))
  }

  if (!isTRUE(scfg$extract_rois$enable)) return(scfg)

  if (is.null(scfg$metadata$rois_directory)) {
    scfg <- setup_project_metadata(scfg, fields = "metadata/rois_directory")
  }

  # if fields are present, prompt only for those that are present
  if (!is.null(fields)) {
    if (!any(grepl("^extract_rois/", fields))) return(scfg) # if fields are passed, but none relate to extract rois, skip out

    extract_fields <- grep("^extract_rois/", fields, value = TRUE)

    # extract_rois stream and setting using sub()
    stream_split <- strsplit(extract_fields, "/", fixed = TRUE)

    # Build a named list of settings by stream
    stream_list <- split(
      extract_fields,
      vapply(stream_split, function(parts) parts[[2]], character(1))
    )

    for (ss in seq_along(stream_list)) {
      scfg <- setup_extract_stream(scfg, fields = stream_list[[ss]], stream_name = names(stream_list)[ss])
    }

    return(scfg) # skip out before menu system when fields are passed
  }

  scfg <- manage_extract_streams(scfg, allow_empty = TRUE)
  return(scfg)
}

get_extract_stream_names <- function(scfg) {
  if (is.null(scfg$extract_rois)) return(character())
  setdiff(names(scfg$extract_rois), "enable")
}

setup_extract_stream <- function(scfg, fields = NULL, stream_name = NULL) {
  checkmate::assert_string(stream_name, null.ok = TRUE)

  if (!checkmate::test_class(scfg, "bg_project_cfg")) {
    stop("scfg input must be a bg_project_cfg object produced by setup_project")
  }

  defaults <- list(
    memgb = 32L,
    nhours = 2L,
    ncores = 1L,
    cli_options = "",
    sched_args = ""
  )
  
  # enable should be set by setup_postprocess_streams -- if it's FALSE, don't even think about specific streams
  if (isFALSE(scfg$extract_rois$enable)) return(scfg)

  # convert fields from extract_rois/<stream_name>/field to extract_rois/field for simplicity in subordinate setup functions
  if (!is.null(fields)) fields <- sub(paste0("^extract_rois/", stream_name, "/"), "extract_rois/", fields)

  existing_cfg <- TRUE
  if (is.null(stream_name) || !stream_name %in% names(scfg$extract_rois)) {
    excfg <- list()
    existing_cfg <- FALSE
  } else {
    excfg <- scfg$extract_rois[[stream_name]]
  }

  prompt_name <- !existing_cfg
  if (existing_cfg && "extract_rois/name" %in% fields) {
    prompt_name <- prompt_input(
      instruct=glue("This configuration is called {stream_name}."),
      prompt="Change name?", type = "flag")
  }

  stream_names <- get_extract_stream_names(scfg)
  if (prompt_name) {
    name_valid <- FALSE
    while (!name_valid) {
      stream_name <- prompt_input(prompt = "Name for this ROI extraction configuration", type = "character")
      if (stream_name %in% stream_names) {
        message("Configuration name must be unique. Existing names are: ", paste(stream_names, collapse = ", "))
      } else if (stream_name == "enable") {
        message("Stream name cannot be 'enable'.")
      } else {
        name_valid <- TRUE
      }
    }
  }
  
  # setup_job requires the top-level list for extract_rois -- spoof this for handling nested field names
  spoof <- list(extract_rois = excfg)
  spoof <- setup_job(spoof, "extract_rois", defaults, fields)
  excfg <- spoof$extract_rois

  if (is.null(fields)) {
    fields <- c(
      "extract_rois/input_streams", "extract_rois/atlases", "extract_rois/mask_file", "extract_rois/roi_reduce", "extract_rois/save_ts",
      "extract_rois/correlation/method", "extract_rois/rtoz", "extract_rois/min_vox_per_roi"
    )
  }

  if ("extract_rois/input_streams" %in% fields) {
    all_streams <- get_postprocess_stream_names(scfg)
    if (length(all_streams) == 0) {
      stop("No postprocess streams available to attach to extraction stream")
    }
    sel <- select_list_safe(all_streams, multiple = TRUE, title = "Select postprocess stream(s) to use")
    excfg$input_streams <- sel
  }

  if ("extract_rois/atlases" %in% fields) {
    atlas <- prompt_input("Atlas NIfTI file(s) (separate using commas for multiple):", type = "character", split="\\s*,\\s*")
    excfg$atlases <- atlas
  }

  if ("extract_rois/mask_file" %in% fields) {
    mask_file <- prompt_input(
      instruct = glue("\n
        ROI extraction always removes constant and zero voxels from its calculations.
        In addition, you may provide a mask file that only retains voxels in the mask
        for ROI extraction and connectivity calculation. This mask is applied in addition
        to masking done in postprocessing, if requested, as well as the automatic removal
        of constant/zero voxels.
      "),
      prompt = "Mask file for ROI extraction",
      type = "character", required = FALSE,
      default = excfg$mask_file
    )
  }

  if ("extract_rois/roi_reduce" %in% fields) {
    reduce <- select_list_safe(c("mean", "median", "pca", "huber"),
                               multiple = FALSE, title = "ROI reduction method")
    excfg$roi_reduce <- reduce
  }

  if ("extract_rois/correlation/method" %in% fields) {
    cat(glue("
      BrainGnomes supports several methods for computing correlations among ROI time series.
      You may choose one or more of these, which will produce files ending _cor-<method>_connectivity.tsv.
      If you choose 'none', then no such files will be produced, but the ROI time series will
      be output to files ending _timeseries.tsv.
    "))
    repeat {
      method <- select_list_safe(c("pearson", "spearman", "kendall", "cor.shrink", "none"),
        multiple = TRUE, title = "Correlation method(s)"
      )
      if (any(method == "none")) {
        if (length(method) > 1L) {
          cat("You may not choose correlation methods if you choose 'none.'\n")
        } else {
          excfg$save_ts <- TRUE # if they say none, then they must intend to extract the timeseries (otherwise, nothing would happen!)
          fields <- fields[fields != "extract_rois/save_ts"]
          break
        }
      } else {
        break
      }
    }
    excfg$correlation$method <- method
  }

  if ("extract_rois/save_ts" %in% fields) {
    excfg$save_ts <- prompt_input(
      instruct = glue("
        BrainGnomes can export the time series from each ROI (aggregating voxels in the region)
        to a .tsv file that is volumes x rois in size. This can be helpful if you want to run
        external analyses on ROI time series."),
      prompt = "Output ROI time series?", type = "flag"
    )
  }

  if ("extract_rois/rtoz" %in% fields) {
    excfg$rtoz <- prompt_input("Perform (Fisher) r-to-z transformation on correlations?", type = "flag")
  }

  if ("extract_rois/min_vox_per_roi" %in% fields) {
    default_value <- if (is.null(excfg$min_vox_per_roi)) "5" else as.character(excfg$min_vox_per_roi)
    repeat {
      response <- prompt_input(
        instruct = glue("
          Sometimes smaller ROIs, especially near the edge of the brain, may not have enough voxels to yield
          a reasonable average time series. In this case, it's often best to exclude these ROIs, rather than
          include a noisy estimate. ROIs failing this threshold will be set to NA in outputs, preserving labeling.
          You can enter an absolute voxel count (e.g., 5) or a percentage/proportion (e.g., '80%' or 0.8) of atlas voxels."),
        prompt = "Enter the minimum voxel requirement for valid ROIs:", type = "character", len = 1L,
        default = default_value
      )

      response <- trimws(response)
      parsed <- tryCatch(parse_min_vox_per_roi(response), error = function(e) {
        cat(conditionMessage(e), "\n")
        NULL
      })

      if (!is.null(parsed)) {
        excfg$min_vox_per_roi <- canonicalize_min_vox_per_roi(parsed)
        break
      }

      default_value <- response
    }
  }

  scfg$extract_rois[[stream_name]] <- excfg
  return(scfg)
}


#' Convert a parsed specification to a canonical configuration value
#' @keywords internal
#' @noRd
canonicalize_min_vox_per_roi <- function(spec, digits = 1) {
  checkmate::assert_list(spec, any.missing = FALSE)

  if (spec$type == "count") return(spec$value)

  pct <- round(spec$value * 100, digits)
  fmt <- if (abs(pct - round(pct)) < .Machine$double.eps^0.5) {
    as.integer(round(pct))
  } else {
    pct
  }

  return(paste0(fmt, "%"))
}
