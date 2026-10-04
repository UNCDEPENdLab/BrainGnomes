#' Check for discrepancies between DB status, manifest verification, and .complete files
#'
#' Examines the job tracking database and compares against actual file system state
#' to identify subjects where completion status is inconsistent across different
#' verification methods.
#'
#' @param scfg A project configuration object as produced by `load_project` or `setup_project`
#' @param sub_ids Character vector of subject IDs to check. If NULL, checks all subjects.
#' @param steps Character vector of step names to check. Default is all main steps.
#' @param verbose Logical. If TRUE, print detailed reconciliation information.
#'
#' @return A data.frame with columns: sub_id, ses_id, step_name, db_status, 
#'   manifest_verified, complete_file_exists, fail_file_exists, discrepancy (logical), and details.
#'
#' @keywords internal
#' @importFrom cli cli_alert_warning cli_alert_info cli_alert_success
check_status_reconciliation <- function(scfg, sub_ids = NULL, 
                                        steps = c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess"),
                                        verbose = TRUE) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_character(sub_ids, null.ok = TRUE)
  checkmate::assert_character(steps)
  checkmate::assert_flag(verbose)
  
  # Get all subjects if not specified
  if (is.null(sub_ids)) {
    log_dir <- scfg$metadata$log_directory
    sub_dirs <- list.dirs(log_dir, recursive = FALSE, full.names = FALSE)
    sub_ids <- sub("^sub-", "", sub_dirs[grepl("^sub-", sub_dirs)])
  }
  
  if (length(sub_ids) == 0) {
    if (verbose) cli::cli_alert_info("No subjects found to check.")
    return(data.frame())
  }
  
  # Get postprocess streams if needed
  pp_streams <- if ("postprocess" %in% steps && isTRUE(scfg$postprocess$enable)) {
    get_postprocess_stream_names(scfg)
  } else {
    character(0)
  }
  
  results <- list()
  discrepancies_found <- 0
  
  for (sub_id in sub_ids) {
    # Determine sessions for this subject
    bids_sub_dir <- file.path(scfg$metadata$bids_directory, paste0("sub-", sub_id))
    ses_dirs <- if (dir.exists(bids_sub_dir)) {
      list.dirs(bids_sub_dir, recursive = FALSE, full.names = FALSE)
    } else {
      character(0)
    }
    ses_ids <- sub("^ses-", "", ses_dirs[grepl("^ses-", ses_dirs)])
    if (length(ses_ids) == 0) ses_ids <- NA_character_
    
    for (ses_id in ses_ids) {
      for (step in steps) {
        if (step == "postprocess") {
          # Check each postprocess stream
          for (stream in pp_streams) {
            chk <- is_step_complete(scfg, sub_id, 
                                    ses_id = if (!is.na(ses_id)) ses_id else NULL,
                                    step_name = "postprocess", 
                                    pp_stream = stream,
                                    verify_manifest = TRUE)
            
            # Check .complete file independently
            complete_file_exists <- checkmate::test_file_exists(chk$complete_file)
            fail_file_exists <- checkmate::test_file_exists(sub("_complete$", "_fail", chk$complete_file))
            
            # Identify discrepancies
            discrepancy <- FALSE
            details <- character(0)
            
            if (!is.na(chk$db_status) && chk$db_status == "COMPLETED") {
              if (!complete_file_exists) {
                discrepancy <- TRUE
                details <- c(details, "DB shows COMPLETED but .complete file missing")
              }
              if (isFALSE(chk$manifest_verified)) {
                discrepancy <- TRUE
                details <- c(details, "DB shows COMPLETED but manifest verification failed")
              }
            } else if (complete_file_exists && (is.na(chk$db_status) || chk$db_status != "COMPLETED")) {
              discrepancy <- TRUE
              details <- c(details, paste0(".complete file exists but DB status is: ", 
                                           if (is.na(chk$db_status)) "not found" else chk$db_status))
            }
            if (fail_file_exists && (is.na(chk$db_status) || !(chk$db_status %in% c("FAILED", "FAILED_BY_EXT")))) {
              discrepancy <- TRUE
              details <- c(details, paste0(".fail file exists but DB status is: ",
                                           if (is.na(chk$db_status)) "not found" else chk$db_status))
            } else if (!fail_file_exists && !is.na(chk$db_status) && chk$db_status %in% c("FAILED", "FAILED_BY_EXT")) {
              discrepancy <- TRUE
              details <- c(details, paste0("DB status is ", chk$db_status, " but .fail file is missing"))
            }
            
            if (discrepancy) discrepancies_found <- discrepancies_found + 1
            
            results[[length(results) + 1]] <- data.frame(
              sub_id = sub_id,
              ses_id = ses_id,
              step_name = paste0("postprocess_", stream),
              db_status = if (is.na(chk$db_status)) NA_character_ else chk$db_status,
              manifest_verified = chk$manifest_verified,
              complete_file_exists = complete_file_exists,
              fail_file_exists = fail_file_exists,
              verification_source = chk$verification_source,
              discrepancy = discrepancy,
              details = paste(details, collapse = "; "),
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Non-postprocess steps
          session_level <- step %in% c("bids_conversion")
          chk <- is_step_complete(scfg, sub_id, 
                                  ses_id = if (session_level && !is.na(ses_id)) ses_id else NULL,
                                  step_name = step,
                                  verify_manifest = TRUE)
          
          complete_file_exists <- checkmate::test_file_exists(chk$complete_file)
          fail_file_exists <- checkmate::test_file_exists(sub("_complete$", "_fail", chk$complete_file))
          
          discrepancy <- FALSE
          details <- character(0)
          
          if (!is.na(chk$db_status) && chk$db_status == "COMPLETED") {
            if (!complete_file_exists) {
              discrepancy <- TRUE
              details <- c(details, "DB shows COMPLETED but .complete file missing")
            }
            if (isFALSE(chk$manifest_verified)) {
              discrepancy <- TRUE
              details <- c(details, "DB shows COMPLETED but manifest verification failed")
            }
          } else if (complete_file_exists && (is.na(chk$db_status) || chk$db_status != "COMPLETED")) {
            discrepancy <- TRUE
            details <- c(details, paste0(".complete file exists but DB status is: ", 
                                         if (is.na(chk$db_status)) "not found" else chk$db_status))
          }
          if (fail_file_exists && (is.na(chk$db_status) || !(chk$db_status %in% c("FAILED", "FAILED_BY_EXT")))) {
            discrepancy <- TRUE
            details <- c(details, paste0(".fail file exists but DB status is: ",
                                         if (is.na(chk$db_status)) "not found" else chk$db_status))
          } else if (!fail_file_exists && !is.na(chk$db_status) && chk$db_status %in% c("FAILED", "FAILED_BY_EXT")) {
            discrepancy <- TRUE
            details <- c(details, paste0("DB status is ", chk$db_status, " but .fail file is missing"))
          }
          
          if (discrepancy) discrepancies_found <- discrepancies_found + 1
          
          results[[length(results) + 1]] <- data.frame(
            sub_id = sub_id,
            ses_id = ses_id,
            step_name = step,
            db_status = if (is.na(chk$db_status)) NA_character_ else chk$db_status,
            manifest_verified = chk$manifest_verified,
            complete_file_exists = complete_file_exists,
            fail_file_exists = fail_file_exists,
            verification_source = chk$verification_source,
            discrepancy = discrepancy,
            details = paste(details, collapse = "; "),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  df <- do.call(rbind, results)
  
  if (verbose) {
    if (discrepancies_found == 0) {
      cli::cli_alert_success("No discrepancies found across {nrow(df)} status checks.")
    } else {
      cli::cli_alert_warning("Found {discrepancies_found} discrepanc{?y/ies} across {nrow(df)} status checks.")
      discrepant <- df[df$discrepancy, ]
      for (i in seq_len(nrow(discrepant))) {
        row <- discrepant[i, ]
        ses_str <- if (!is.na(row$ses_id)) paste0("/ses-", row$ses_id) else ""
        cli::cli_alert_warning(
          "sub-{row$sub_id}{ses_str} [{row$step_name}]: {row$details}"
        )
      }
    }
  }
  
  invisible(df)
}
