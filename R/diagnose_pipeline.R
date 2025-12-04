#' Function for diagnosing errors in a run of the pipeline
#'
#' @param input A character path to an scfg object or an scfg object itself.
#'
#' @importFrom dplyr bind_rows
#' @importFrom cli cli_abort cli_warn cli_inform cli_alert no qty
#' @export
#'
#' @author Zach Vig & Dan Shallal

diagnose_pipeline <- function(input) {
  
  if (options()$warn < 1) {
    on.exit(options(warn = options()$warn))
    options(warn = 1)
  }

  cli::cli_inform(
    c("Running pipeline diagnosis...")
  )

  if (checkmate::test_class(input, "character")) {
    if (isFALSE(checkmate::test_directory_exists(input))) {
      cli::cli_abort(
        "Provided directory does not exist: {.path {input}}"
      )
    }
    proj_dir <- input
    proj_files <- list.files(proj_dir, include.dirs = TRUE)
    sqlite_db <- file.path(input, grep(".sqlite", proj_files, value = TRUE))
  }
  else if (checkmate::test_class(input, "bg_project_cfg")) {
    proj_dir <- input$metadata$project_directory
    proj_files <- list.files(proj_dir, include.dirs = TRUE)
    sqlite_db <- file.path(
      proj_dir,
      grep(".sqlite", proj_files, value = TRUE)
    )
  } else {
    cli::cli_abort(
      "Input must be either a path to a project directory or an scfg object."
    )
  }

  if (checkmate::test_file_exists(sqlite_db)) {
    if (isFALSE(sqlite_table_exists(sqlite_db, "job_tracking"))) {
      cli::cli_abort(
        c("Job tracking table does not exist in SQLite database.",
        "i" = "Maybe the analysis hasn't been run yet? Running the analysis will generate a job tracking table within an SQLite database."
        )
      )
    }
  } else {
    cli::cli_abort(
      "SQLite database not found: {.path {sqlite_db}}"
    )
  }

  cli::cli_inform(
    c("Found SQLite database at {.path {sqlite_db}}. Fetching sequence IDs...")
  )

  sequence_ids <- get_sequence_ids(sqlite_db)
  n_sequences <- length(sequence_ids)
  if (n_sequences == 0) {
    cli::cli_abort(
      c("No sequence IDs found in job tracking table.",
      "i" = "Maybe the analysis hasn't been run yet? Running the analysis will populate the job tracking table within the SQLite database."
      )
    )
  }
  else {
    cli::cli_inform(
      c("Found {n_sequences} pipeline run(s) that can be analyzed.")
    )
  }

  cli::cli_inform(
    c("Would you like to view a summary by subject or examine a specific sequence ID?")
  )
  
  view_options <- c(
    "subject" = cli::col_cyan("View summary by subject (across all runs)"),
    "sequence" = cli::col_cyan("Examine a specific sequence ID")
  )
  
  cli::cli_ol(view_options)
  
  view_choice <- prompt_input(
    prompt = "Enter 1 for subject summary or 2 for sequence ID",
    type = "integer",
    lower = 1,
    upper = 2,
    required = TRUE,
    len = 1
  )
  
  selected_view <- names(view_options)[view_choice]
  
  if (selected_view == "subject") {
    con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
    on.exit(DBI::dbDisconnect(con))
    all_tracking_df <- DBI::dbGetQuery(con, "SELECT * FROM job_tracking")
    if (nrow(all_tracking_df) > 0L && "job_obj" %in% names(all_tracking_df)) {
      all_tracking_df$job_obj <- lapply(all_tracking_df$job_obj, function(x) {
        if (!is.null(x)) unserialize(x) else NULL
      })
    }
    on.exit(NULL)
    
    all_job_names <- unique(all_tracking_df$job_name)
    subjects <- unique(regmatches(all_job_names, regexpr("sub-[^_]+", all_job_names)))
    subjects <- subjects[!is.na(subjects) & subjects != ""]
    subjects <- sort(subjects)
    
    if (length(subjects) == 0) {
      cli::cli_abort("No subjects found in job tracking table.")
    }
    
    cli::cli_inform(
      c("Found {length(subjects)} subject(s) across all runs.")
    )
    
    cli::cli_ol(paste0(cli::col_green(subjects)))
    
    sub_choice <- prompt_input(
      prompt = "Enter the number corresponding to the subject you want to view",
      type = "integer",
      lower = 1,
      upper = length(subjects),
      required = TRUE,
      len = 1
    )
    
    selected_subject <- subjects[sub_choice]
    
    subject_jobs_df <- all_tracking_df[grepl(selected_subject, all_tracking_df$job_name), ]
    
    if (nrow(subject_jobs_df) == 0) {
      cli::cli_abort("No jobs found for subject {.val {selected_subject}}")
    }
    
    cli::cli_h3(cli::style_bold(cli::col_cyan("Subject Summary: {selected_subject}")))
    print_subject_summary_tree(subject_jobs_df, selected_subject)
    
    cli::cli_inform(
      c("Would you like to examine any of these jobs more closely?",
        "i" = "Type 'yes' to continue, or 'no' to exit now.")
    )
    continue <- prompt_input(
      type = "flag",
      required = TRUE
    )
    
    if (isFALSE(continue)) {
      return(invisible(NULL))
    }
    
    subject_sequences <- unique(subject_jobs_df$sequence_id)
    subject_sequences <- subject_sequences[!is.na(subject_sequences)]
    
    if (length(subject_sequences) == 0) {
      cli::cli_abort("No sequence IDs found for subject {.val {selected_subject}}")
    }
    
    cli::cli_inform(
      c("Which sequence ID would you like to examine for {.val {selected_subject}}?",
        "i" = "Found {length(subject_sequences)} sequence ID(s) containing this subject.")
    )
    
    cli::cli_ol(paste0("Sequence ", cli::col_cyan(subject_sequences)))
    
    seq_choice <- prompt_input(
      prompt = "Enter the number corresponding to the sequence ID",
      type = "integer",
      lower = 1,
      upper = length(subject_sequences),
      required = TRUE,
      len = 1
    )
    
    this_sequence_id <- subject_sequences[seq_choice]
    tracking_df <- get_tracked_job_status(sequence_id = this_sequence_id, sqlite_db = sqlite_db)
    
  } else {
    ss <- prompt_input(
      prompt = "Enter which pipeline run to diagnose. The default is the most recent.",
      default = n_sequences,
      type = "integer",
      lower = min(n_sequences, 1),
      upper = n_sequences,
      len = 1
    )
    
    this_sequence_id <- sequence_ids[ss]
    tracking_df <- get_tracked_job_status(sequence_id = this_sequence_id, sqlite_db = sqlite_db)
  }
  
  retrieval_time <- Sys.time()
  
  if (isTRUE(nrow(tracking_df) == 0)) {
    cli::cli_abort(
      "Error retrieving job tracking information for sequence ID: {.val {this_sequence_id}}"
    )
  }
  
  this_sequence_tree <- tracking_df_to_tree(tracking_df)[[this_sequence_id]]
  n_chains <- this_sequence_tree$count
  
  cli::cli_inform(
    "The run you selected had {n_chains} top-level job{?s}:"
  )
  
  cli::cli_h3(cli::style_bold(cli::col_cyan("Sequence {this_sequence_id}")))
  print_step_tree_by_type(this_sequence_tree)
  cli::cli_end(id = "sequence_summary")
  
  # Ask if user wants to examine more closely
  cli::cli_inform(
    c("Would you like to examine any of these jobs more closely?",
      "i" = "Type 'yes' to continue, or 'no' to return full job tree and exit now.")
  )
  continue <- prompt_input(
    type = "flag",
    required = TRUE
  )
  
  if (isFALSE(continue)) {
    cli::cli_inform(
      c("Exiting diagnosis and returning full tracking tree...",
        "i" = "Make sure you load the {.pkg data.tree} package")
    )
    return(this_sequence_tree)
  }

  job_list <- list()
  counter <- 1
  
  number_jobs <- function(node, level = 0) {
    if (grepl("^Sequence_", node$name) && node$level == 1) {
      if (node$count > 0) {
        for (child in node$children) {
          number_jobs(child, level = 0)
        }
      }
      return()
    }
    
    job_list[[counter]] <<- list(node = node, level = level, number = counter)
    counter <<- counter + 1
    
    if (node$count > 0) {
      for (child in node$children) {
        number_jobs(child, level = level + 1)
      }
    }
  }
  
  number_jobs(this_sequence_tree)
  n_jobs <- length(job_list)
  
  cli::cli_inform(
    c("Which job would you like to examine?",
      "i" = "There are {n_jobs} jobs total (including nested jobs)")
  )
  
  job_choice <- prompt_input(
    type = "integer",
    required = TRUE,
    lower = 1,
    upper = n_jobs,
    prompt = "Enter the job number from the list above"
  )
  
  this_job <- job_list[[job_choice]]$node
  
  this_job_name <- this_job$name
  this_job_title <- get_step_title(this_job)
  this_job_status <- this_job$status
  
  if (this_job_status == "COMPLETED") {
    status_list <- c(
      "...was submitted at {this_job$time_submitted}",
      "...was started at {this_job$time_started}",
      "...{.emph successfully completed} at {this_job$time_ended}"
    )
  } else if (this_job_status == "QUEUED") {
    status_list <- c(
      "...was submitted at {this_job$time_submitted}",
      "...has {.emph not started} as of {retrieval_time}"
    )
  } else if (this_job_status == "STARTED") {
    status_list <- c(
      "...was submitted at {this_job$time_submitted}",
      "...was started at {this_job$time_started}",
      "...has {.emph not finished} as of {retrieval_time}"
    )
  } else {
    if (this_job_status == "FAILED_BY_EXT") {
      upstream_node <- data.tree::FindNode(this_job$root, filterFun = function(n) {
        !is.null(n$id) && n$id == this_job$parent_id
      })
      
      if (!is.null(upstream_node)) {
        extra <- "because upstream job {.code {upstream_node$name}} failed"
      } else {
        extra <- ""
      }
    } else {
      extra <- ""
    }
    
    was_queued <- !is.null(this_job$time_submitted) && !is.na(this_job$time_submitted)
    was_started <- !is.null(this_job$time_started) && !is.na(this_job$time_started)
    
    if (isTRUE(was_queued)) {
      if (isTRUE(was_started)) {
        status_list <- c(
          "...was submitted at {this_job$time_submitted}",
          "...and was started at {this_job$time_started}",
          paste("...{.emph failed} before finishing", extra)
        )
      } else {
        status_list <- c(
          "...was submitted at {this_job$time_submitted}",
          paste("...{.emph failed} before starting", extra)
        )
      }
    } else {
      status_list <- c(
        paste("...{.emph failed} before being submitted", extra)
      )
    }
  }
  
  all_nodes <- this_job$root$Get("parent_id", filterFun = data.tree::isNotRoot)
  downstream_nodes <- names(all_nodes[!is.na(all_nodes) & all_nodes == this_job$id])
  n_downstream <- length(downstream_nodes)
  
  downstream_message <- c(
    "...had {cli::no(n_downstream)} downstream (dependent) job{?s}"
  )
  
  children_message <- c(
    "...had {cli::no(this_job$count)} child job{?s}"
  )
  
  cli::cli_h3("Job {.code {this_job_name}}...")
  cli::cli_ul(id = "job_summary")
  cli::cli_li(status_list)
  cli::cli_li(downstream_message)
  if (n_downstream > 0) {
    ds <- cli::cli_ul()
    for (ds_name in downstream_nodes) {
      cli::cli_li("{.code {ds_name}}")
    }
    cli::cli_end(ds)
  }
  cli::cli_li(children_message)
    cli::cli_end("job_summary")
  
  if (this_job$count > 0) {
    children_status <- sapply(this_job$children, function(x) x$status)
    tab <- table(children_status)
    
    cli::cli_h3("Child job details:")
    for (status_name in names(tab)) {
      count <- unname(tab[status_name])
      jobs_with_status <- names(children_status[children_status == status_name])
      
      cli::cli_text("{count} of {this_job$count} [{get_status_symbol(status_name)}] {status_name}:")
      cli::cli_ul(id = "children_list")
      for (job_name in jobs_with_status) {
        cli::cli_li("{.code {job_name}}")
      }
      cli::cli_end(id = "children_list")
    }
  }

  subject_folder <- sub(".*(sub-\\d+).*", "\\1", this_job_name)
  logs_dir <- file.path(proj_dir, "logs", subject_folder)

  if (!dir.exists(logs_dir)) {
    cli::cli_warn("Logs directory not found for subject {.val {subject_folder}} at {.path {logs_dir}}")
    logs_dir <- NULL
  } else {
    job_id <- this_job$job_id
    
    out_candidates <- list.files(
      logs_dir,
      pattern = paste0(job_id, ".*\\.out$"),
      full.names = TRUE
    )
    err_candidates <- list.files(
      logs_dir,
      pattern = paste0(job_id, ".*\\.err$"),
      full.names = TRUE
    )
    
    if (length(out_candidates) == 0 && length(err_candidates) == 0) {
      cli::cli_warn("No log files found containing job ID {.val {job_id}} in {.path {logs_dir}}")
    }
    
    out_file <- if (length(out_candidates) > 0) out_candidates[[1]] else NULL
    err_file <- if (length(err_candidates) > 0) err_candidates[[1]] else NULL
  }

  repeat {
    actions <- c(
      "view_output" = cli::col_cyan("View the output file in console"),
      "return_output" = cli::col_cyan("Return output file as character object"),
      "view_error" = cli::col_cyan("View the error file in the console"),
      "return_error" = cli::col_cyan("Return error file as character object"),
      "exit" = cli::col_cyan("Exit")
    )
    
    cli::cli_inform("")

    cli::cli_inform("Further diagnosis...")
    cli::cli_ol(actions)
    
    action_choice <- prompt_input(
      prompt = "Enter an integer corresponding to your choice",
      type = "integer",
      lower = 1,
      upper = length(actions),
      required = TRUE
    )
    
    selected_action <- names(actions)[action_choice]
    
      if (selected_action == "view_output") {
    if (!is.null(out_file)) {
      n_lines <- prompt_input(
        prompt = "How many lines from the bottom of the output file to view?",
        type = "integer",
        default = 20,
        lower = 1
      )
      file_lines <- readLines(out_file)
      cli::cli_inform(paste(tail(file_lines, n_lines), collapse = "\n"))
    } else {
      cli::cli_warn("No output file to display for this job.")
    }
  } else if (selected_action == "return_output") {
    if (!is.null(out_file)) return(readLines(out_file))
    else cli::cli_warn("No output file to return for this job.")
  } else if (selected_action == "view_error") {
    if (!is.null(err_file)) {
      n_lines <- prompt_input(
        prompt = "How many lines from the bottom of the error file to view?",
        type = "integer",
        default = 20,
        lower = 1
      )
      file_lines <- readLines(err_file)
      cli::cli_inform(paste(tail(file_lines, n_lines), collapse = "\n"))
    } else {
      cli::cli_warn("No error file to display for this job.")
    }
  } else if (selected_action == "return_error") {
    if (!is.null(err_file)) return(readLines(err_file))
    else cli::cli_warn("No error file to return for this job.")
  } else if (selected_action == "exit") {
    cli::cli_inform(cli::col_cyan("Exiting without returning anything."))
    return(invisible(NULL))
  }

    
    continue_loop <- prompt_input(
      prompt = "Would you like to perform another action? (yes/no)",
      type = "flag",
      required = TRUE
    )
    if (!continue_loop) break
  }
  
  return(invisible(NULL))
}

#' helper for getting job type from job name
#'
#' @param job_name Character string job name
#' @return Character string job type
#'
#' @keywords internal
get_job_type <- function(job_name) {
  if (grepl("^fsaverage", job_name)) {
    return("Setup")
  } else if (grepl("^bids_conversion", job_name)) {
    return("BIDS Conversion")
  } else if (grepl("^bids_validation", job_name)) {
    return("BIDS Validation")
  } else if (grepl("^mriqc", job_name)) {
    return("MRIQC")
  } else if (grepl("^fmriprep", job_name)) {
    return("fMRIPrep")
  } else if (grepl("^aroma", job_name)) {
    return("ICA-AROMA")
  } else if (grepl("^extract_rois", job_name)) {
    return("ROI Extraction")
  } else if (grepl("^postprocess_", job_name)) {
    stream_match <- regmatches(job_name, regexpr("^postprocess_[^_]+", job_name))
    return(paste0("Postprocess ", sub("^postprocess_", "", stream_match)))
  } else {
    return("Other")
  }
}

#' helper for printing subject summary with tree structure showing best status across runs
#'
#' @param subject_jobs_df A data.frame of jobs for a specific subject across all sequence IDs
#' @param subject_id The subject ID being summarized
#'
#' @keywords internal
print_subject_summary_tree <- function(subject_jobs_df, subject_id) {
  if (nrow(subject_jobs_df) == 0) {
    cli::cli_inform("No jobs found for this subject.")
    return(invisible(NULL))
  }
  
  status_priority <- c("COMPLETED" = 5, "STARTED" = 4, "QUEUED" = 3, "FAILED_BY_EXT" = 2, "FAILED" = 1)
  
  job_summary <- list()
  
  for (i in seq_len(nrow(subject_jobs_df))) {
    job <- subject_jobs_df[i, ]
    job_name <- job$job_name
    
    if (grepl("^fsaverage", job_name)) {
      next
    }
    
    job_type <- get_job_type(job_name)
    
    if (grepl("^postprocess_", job_name)) {
      stream_match <- regmatches(job_name, regexpr("^postprocess_[^_]+", job_name))
      group_key <- paste0("Postprocess ", sub("^postprocess_", "", stream_match))
    } else {
      group_key <- job_type
    }
    
    if (is.null(job_summary[[group_key]])) {
      job_summary[[group_key]] <- list()
    }
    
    if (is.null(job_summary[[group_key]][[job_name]])) {
      job_summary[[group_key]][[job_name]] <- list(
        statuses = character(),
        job_ids = character(),
        sequence_ids = character()
      )
    }
    
    job_summary[[group_key]][[job_name]]$statuses <- c(
      job_summary[[group_key]][[job_name]]$statuses,
      as.character(job$status)
    )
    job_summary[[group_key]][[job_name]]$job_ids <- c(
      job_summary[[group_key]][[job_name]]$job_ids,
      as.character(job$job_id)
    )
    job_summary[[group_key]][[job_name]]$sequence_ids <- c(
      job_summary[[group_key]][[job_name]]$sequence_ids,
      as.character(job$sequence_id)
    )
  }
  
  step_order <- c(
    "BIDS Conversion", "BIDS Validation",
    "MRIQC", "fMRIPrep", "ICA-AROMA", "ROI Extraction"
  )
  
  ordered_types <- intersect(step_order, names(job_summary))
  remaining_types <- setdiff(names(job_summary), ordered_types)
  remaining_types <- setdiff(remaining_types, "Setup")
  remaining_types <- sort(remaining_types)
  all_types <- c(ordered_types, remaining_types)
  
  cli::cli_h2(cli::col_green("Subject: {subject_id}"))
  
  for (type in all_types) {
    if (is.null(job_summary[[type]])) next
    
    cli::cli_text("── {.strong {type}}")
    cli::cli_ul()
    
    for (job_name in names(job_summary[[type]])) {
      job_info <- job_summary[[type]][[job_name]]
      
      status_priorities <- status_priority[job_info$statuses]
      status_priorities <- status_priorities[!is.na(status_priorities)]
      
      if (length(status_priorities) == 0) {
        best_status <- "UNKNOWN"
        rep_job_id <- job_info$job_ids[1]
        rep_sequence_id <- job_info$sequence_ids[1]
      } else {
        best_priority <- max(status_priorities)
        best_status <- names(status_priority)[status_priority == best_priority][1]
        best_idx <- which(status_priorities == best_priority)[1]
        rep_job_id <- job_info$job_ids[best_idx]
        rep_sequence_id <- job_info$sequence_ids[best_idx]
      }
      
      sym <- get_status_symbol(best_status)
      status_colored <- get_status_color(best_status)
      cli::cli_li("{sym} {job_name} (job {cli::col_cyan(rep_job_id)}, sequence {cli::col_magenta(rep_sequence_id)}) [{status_colored}]")
    }
    
    cli::cli_end()
  }
}

#' helper for printing step summaries organized by subject then job type
#'
#' @param tree_root The root node of the sequence tree
#'
#' @keywords internal
print_step_tree_by_type <- function(tree_root) {
  if (tree_root$count == 0) {
    cli::cli_inform("No jobs found in this sequence.")
    return(invisible(NULL))
  }

  get_all_nodes <- function(node) {
    res <- list(node)
    if (node$count > 0) {
      for (child in node$children) {
        res <- c(res, get_all_nodes(child))
      }
    }
    return(res)
  }

  all_nodes <- unlist(lapply(tree_root$children, get_all_nodes), recursive = FALSE)

  fsaverage_jobs <- list()
  for (job in all_nodes) {
    job_name <- job$name
    if (grepl("^fsaverage", job_name)) {
      fsaverage_jobs[[length(fsaverage_jobs) + 1]] <- job
    }
  }

  subject_jobs <- list()
  for (job in all_nodes) {
    job_name <- job$name
    sub_match <- regmatches(job_name, regexpr("sub-[^_]+", job_name))
    if (length(sub_match) == 0) next
    sub_id <- sub_match[1]

    if (is.null(subject_jobs[[sub_id]])) subject_jobs[[sub_id]] <- list()

    type <- get_job_type(job_name)
    if (is.null(subject_jobs[[sub_id]][[type]])) subject_jobs[[sub_id]][[type]] <- list()
    subject_jobs[[sub_id]][[type]][[length(subject_jobs[[sub_id]][[type]]) + 1]] <- job
  }

  if (length(fsaverage_jobs) > 0) {
    for (sub_id in names(subject_jobs)) {
      if (is.null(subject_jobs[[sub_id]][["Setup"]])) {
        subject_jobs[[sub_id]][["Setup"]] <- list()
      }
      subject_jobs[[sub_id]][["Setup"]] <- c(fsaverage_jobs, subject_jobs[[sub_id]][["Setup"]])
    }
  }

  shown_as_children <- character()

  for (sub_id in sort(names(subject_jobs))) {
    cli::cli_h2(cli::col_green("Subject: {sub_id}"))
    job_types <- subject_jobs[[sub_id]]

    step_order <- c(
      "Setup", "BIDS Conversion", "BIDS Validation",
      "MRIQC", "fMRIPrep", "ICA-AROMA", "ROI Extraction"
    )
    ordered_types <- intersect(step_order, names(job_types))
    remaining_types <- setdiff(names(job_types), ordered_types)
    all_types <- c(ordered_types, remaining_types)

    for (type in all_types) {
      jobs_to_show <- list()
      for (job in job_types[[type]]) {
        if (job$name %in% shown_as_children) {
          next
        }

        if (grepl("^postprocess_", job$name) &&
            !is.null(job$parent) &&
            grepl("^postprocess_", job$parent$name)) {
          next
        }

        jobs_to_show[[length(jobs_to_show) + 1]] <- job
      }

      if (length(jobs_to_show) == 0) {
        next
      }

      cli::cli_text("── {.strong {type}}")
      cli::cli_ul()
      for (job in jobs_to_show) {
        sym <- get_status_symbol(job$status)
        status_colored <- get_status_color(job$status)
        cli::cli_li("{sym} {job$name} (job {job$job_id}) [{status_colored}]")

        print_children <- function(parent, indent = "  └─ ") {
          parent_parts <- strsplit(parent$name, "_")[[1]]
          if (parent_parts[1] == "postprocess") {
            parent_pref <- "postprocess"
          } else {
            parent_pref <- parent_parts[1]
          }
          
          for (child in parent$children) {
            if (startsWith(child$name, paste0(parent_pref, "_"))) {
              sym_c <- get_status_symbol(child$status)
              status_colored_c <- get_status_color(child$status)
              cli::cli_li(paste0(indent, sym_c, " ", child$name,
                                 " (job ", child$job_id, ") [", status_colored_c, "]"))
              shown_as_children <<- c(shown_as_children, child$name)
              if (child$count > 0)
                print_children(child, indent = paste0("  ", indent))
            }
          }
        }
        print_children(job)
      }
      cli::cli_end()
    }
  }
}

#' helper for printing symbols based on status
#'
#' @param status Character string job status
#' @importFrom cli col_green col_magenta col_yellow col_red
#'
#' @keywords internal
get_status_symbol <- Vectorize(
  function(status) {
    switch(status,
           "COMPLETED" = cli::col_green("\u2714"), # checkmark
           "QUEUED" = cli::col_magenta("\u2197"), # arrow
           "STARTED" = cli::col_yellow("\u22ef"), # ellipsis
           "FAILED" = cli::col_red("\u2717"), # X mark
           "FAILED_BY_EXT" = cli::col_red("\u2717")) # X mark
  },
  USE.NAMES = FALSE
  )

#' helper for coloring status text based on status
#'
#' @param status Character string job status
#' @importFrom cli col_green col_grey col_yellow col_red
#'
#' @keywords internal
get_status_color <- Vectorize(
  function(status) {
    switch(status,
           "COMPLETED" = cli::col_green(status),
           "QUEUED" = cli::col_grey(status),
           "STARTED" = cli::col_yellow(status),
           "FAILED" = cli::col_red(status),
           "FAILED_BY_EXT" = cli::col_red(status),
           status)
  },
  USE.NAMES = FALSE
  )