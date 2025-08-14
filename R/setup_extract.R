#' @keywords internal
manage_extract_streams <- function(scfg, allow_empty = FALSE) {
  extract_field_list <- function() {
    c(
      "postprocess_streams", "atlases", "roi_reduce", "correlation/method"
    )
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
        val <- get_nested_values(scfg, paste0("extract/", sel, "/", fld))
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
        fields = paste0("extract/", sel, "/", selected_fields),
        stream_name = sel
      )
    } else if (choice == 3) {
      if (length(streams) == 0) {
        cat("No streams to delete.\n")
        next
      }
      sel <- select_list_safe(streams, multiple = TRUE, title = "Select stream(s) to delete")
      if (length(sel) == 0) next
      scfg$extract[sel] <- NULL
    } else if (choice == 4) {
      if (length(streams) == 0) {
        cat("No streams defined.\n")
        next
      }
      for (nm in streams) {
        cat(sprintf("\nStream: %s\n", nm))
        cat(yaml::as.yaml(scfg$extract[[nm]]))
      }
    } else if (choice == 5) {
      if (!allow_empty && length(streams) == 0L) {
        proceed <- prompt_input("No extraction streams were setup. Are you sure you want to finish?", type = "flag", default = FALSE)
        if (!proceed) next
      }
      break
    }
  }

  return(scfg)
}

#' @keywords internal
setup_extract_streams <- function(scfg = list(), fields = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")

  if (is.null(scfg$extract$enable) || (isFALSE(scfg$extract$enable) && any(grepl("extract/", fields)))) {
    scfg$extract$enable <- prompt_input("Perform ROI extraction?", type = "flag", default = FALSE)
  }

  if (!isTRUE(scfg$extract$enable)) {
    return(scfg)
  }

  scfg <- manage_extract_streams(scfg, allow_empty = TRUE)
  return(scfg)
}

get_extract_stream_names <- function(scfg) {
  if (is.null(scfg$extract)) {
    return(character())
  }
  setdiff(names(scfg$extract), "enable")
}

setup_extract_stream <- function(scfg, fields = NULL, stream_name = NULL) {
  if (is.null(scfg$extract)) scfg$extract <- list(enable = TRUE)
  if (is.null(stream_name)) {
    stream_name <- prompt_input("Name of extraction stream:", type = "character")
  }

  if (is.null(fields)) fields <- c("extract/postprocess_streams", "extract/atlases",
                                   "extract/roi_reduce", "extract/correlation/method")

  if ("extract/postprocess_streams" %in% fields) {
    all_streams <- get_postprocess_stream_names(scfg)
    if (length(all_streams) == 0) {
      stop("No postprocess streams available to attach to extraction stream")
    }
    sel <- select_list_safe(all_streams, multiple = TRUE, title = "Select postprocess stream(s) to use")
    scfg$extract[[stream_name]]$postprocess_streams <- sel
  }

  if ("extract/atlases" %in% fields) {
    atlas <- prompt_input("Atlas NIfTI file(s) (comma separated):", type = "character")
    atlas <- trimws(strsplit(atlas, ",")[[1]])
    scfg$extract[[stream_name]]$atlases <- atlas
  }

  if ("extract/roi_reduce" %in% fields) {
    reduce <- select_list_safe(c("mean", "median", "pca", "huber"),
                               multiple = FALSE, title = "ROI reduction method")
    scfg$extract[[stream_name]]$roi_reduce <- reduce
  }

  if ("extract/correlation/method" %in% fields) {
    method <- select_list_safe(c("pearson", "spearman", "kendall", "cor.shrink", "none"),
                               multiple = TRUE, title = "Correlation method(s)")
    scfg$extract[[stream_name]]$correlation$method <- method
  }

  return(scfg)
}
