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
    # if input is path to project directory
    proj_dir <- input
    proj_files <- list.files(proj_dir, include.dirs = TRUE)
    sqlite_db <- file.path(input, grep(".sqlite", proj_files, value = TRUE))
  }
  else if (checkmate::test_class(input, "bg_project_cfg")) {
    # if input is scfg object
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
    # make sure job tracking table exists in database
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

  ss <- prompt_input(
    prompt = "Enter which pipeline run to diagnose. The default is the most recent.",
    default = n_sequences,
    type = "integer",
    lower = min(n_sequences, 1),
    upper = n_sequences,
    len = 1
  )
  
  this_sequence_id <- sequence_ids[ss]

  # find sequence in SQLite database & make sure job was tracked
  tracking_df <- get_tracked_job_status(sequence_id = this_sequence_id, sqlite_db = sqlite_db)
  retrieval_time <- Sys.time()
  
  if (isTRUE(nrow(tracking_df) == 0)) {
    cli::cli_abort(
      "Error retrieving job tracking information for sequence ID: {.val {this_sequence_id}}"
    )
  }
  
  # convert tracking data.frame to data.tree object
  this_sequence_tree <- tracking_df_to_tree(tracking_df)[[this_sequence_id]]
  n_chains <- this_sequence_tree$count
  
  cli::cli_inform(
    "The run you selected had {n_chains} top-level job{?s}:"
  )
  
  # Print tree structure
  cli::cli_h3("Sequence {this_sequence_id}")
  cli::cli_ol(id = "sequence_summary")
  print_step_tree(this_sequence_tree)
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

  # Build a flat list of all jobs with their display numbers
  job_list <- list()
  counter <- 1
  
  # Recursive function to number all jobs
  number_jobs <- function(node, level = 0) {
    if (grepl("^Sequence_", node$name) && node$level == 1) {
      # Skip root, recurse into children
      if (node$count > 0) {
        for (child in node$children) {
          number_jobs(child, level = 0)
        }
      }
      return()
    }
    
    # Add this job to the list
    job_list[[counter]] <<- list(node = node, level = level, number = counter)
    counter <<- counter + 1
    
    # Recurse into children
    if (node$count > 0) {
      for (child in node$children) {
        number_jobs(child, level = level + 1)
      }
    }
  }
  
  number_jobs(this_sequence_tree)
  # Filter job_list for level 0 (top-level) jobs only
  top_level_jobs <- Filter(function(x) x$level == 0, job_list)
  n_top_jobs <- length(top_level_jobs)

  cli::cli_inform(
    c("Which top-level job would you like to examine?",
      "i" = "Only level 0 jobs should be examined.")
  )

  job_choice <- prompt_input(
    type = "integer",
    required = TRUE,
    lower = 1,
    upper = n_top_jobs,
    prompt = "Enter the job number from the list above"
  )

  this_job <- top_level_jobs[[job_choice]]$node
  
  # Job info
  this_job_name <- this_job$name
  this_job_title <- get_step_title(this_job)
  this_job_status <- this_job$status
  
  # Build status message
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
  
  # Downstream summary (jobs that depend on this one)
  all_nodes <- this_job$root$Get("parent_id", filterFun = data.tree::isNotRoot)
  downstream_nodes <- names(all_nodes[!is.na(all_nodes) & all_nodes == this_job$id])
  n_downstream <- length(downstream_nodes)
  
  downstream_message <- c(
    "...had {cli::no(n_downstream)} downstream (dependent) job{?s}"
  )
  
  # Children summary
  children_message <- c(
    "...had {cli::no(this_job$count)} child job{?s}"
  )
  
  # Print detailed status
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
  
  # Show children details if they exist
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

  # Extract subject folder from job name

  subject_folder <- sub(".*_(sub-\\d+)$", "\\1", this_job_name)

  logs_dir <- file.path(proj_dir, "logs", subject_folder)
  if (!dir.exists(logs_dir)) {
    cli::cli_warn("Logs directory not found for subject {.val {subject_folder}} at {.path {logs_dir}}")
    logs_dir <- NULL
  }

  out_file <- err_file <- NULL

  if (!is.null(logs_dir)) {
  # Find .out file that contains job name and job id
  out_candidates <- list.files(
    logs_dir, 
    pattern = paste0(this_job_name, ".*", this_job$job_id, ".*\\.out$"), 
    full.names = TRUE
  )
  if (length(out_candidates) > 0) out_file <- out_candidates[1]

  # Find .err file that contains both job name and job id
  err_candidates <- list.files(
    logs_dir, 
    pattern = paste0(this_job_name, ".*", this_job$job_id, ".*\\.err$"), 
    full.names = TRUE
  )
  if (length(err_candidates) > 0) err_file <- err_candidates[1]

    if (is.null(out_file) && is.null(err_file)) {
      cli::cli_warn("No log files found containing {.val {this_job_name}} in {.path {logs_dir}}")
    } else {
      if (!is.null(out_file)) cli::cli_inform("Found output file: {.path {out_file}}")
      if (!is.null(err_file)) cli::cli_inform("Found error file: {.path {err_file}}")
    }
  }

  repeat {
    actions <- c(
      "view_output" = cli::col_cyan("View the output file in console"),
      "return_output" = cli::col_cyan("Return output file as character object"),
      "view_error" = cli::col_cyan("View the error file in the console"),
      "return_error" = cli::col_cyan("Return error file as character object"),
      "exit" = cli::col_cyan("Exit")
    )
    
    cli::cli_text("")  # spacing
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

#' helper for printing step summaries with proper nesting
#'
#' @param node current data.tree node/step
#' @param level current indentation level (for internal use)
#'
#' @keywords internal
print_step_tree <- function(node, level = 0) {
  # Skip the root sequence node itself - process its children
  if (grepl("^Sequence_", node$name) && node$level == 1) {
    # For root node, start numbered list and recurse into children
    if (node$count > 0) {
      for (child in node$children) {
        print_step_tree(child, level = 0)
      }
    }
    return(invisible(NULL))
  }
  
  # Step info - pass the node itself to get_step_title
  this_step_title <- get_step_title(node)
  this_step_status <- if (!is.null(node$status)) node$status else "UNKNOWN"
  this_step_symbol <- get_status_symbol(this_step_status)

  # Upstream/failure message
  if (this_step_status == "FAILED_BY_EXT" && !is.null(node$parent_id)) {
    upstream_node <- data.tree::FindNode(node$root, filterFun = function(n) {
      !is.null(n$id) && n$id == node$parent_id
    })
    
    if (!is.null(upstream_node)) {
      status_message <- sprintf("FAILED (because `%s` failed)", get_step_title(upstream_node))
    } else {
      status_message <- this_step_status
    }
  } else {
    status_message <- this_step_status
  }

  # For top-level items (level 0), use cli_li (numbered)
  # For nested items, prepend "--" prefix
  if (level == 0) {
    # Top level - numbered list item
    cli::cli_li("{this_step_title} [{this_step_symbol}] {status_message}")
    
    # If this has children, show nested list and then summary
    if (node$count > 0) {
      # Show individual children
      cli::cli_ul(id = paste0("children_", node$name))
      for (child in node$children) {
        print_step_tree(child, level = level + 1)
      }
      cli::cli_end(id = paste0("children_", node$name))
      
      # Get all descendant statuses (children + grandchildren + ...)
      all_descendants <- node$Get(
        "status", 
        filterFun = function(n) n$name != node$name
      )
      tab <- table(all_descendants)
      n_total <- length(all_descendants)
      
      # Format summary
      tab_name <- names(tab)
      tab_count <- unname(tab)
      n_row_tab <- length(tab)
      bullets <- c(rep("\u251c", n_row_tab - 1), "\u2514")
      summary_rows <- sprintf(
        "%s\u2500 %s of %s [%s] %s", 
        bullets, 
        tab_count, 
        n_total, 
        get_status_symbol(tab_name), 
        tab_name
      )
      names(summary_rows) <- rep(" ", n_row_tab)
      
      # Print summary
      cli::cli_bullets(
        c(" " = "Child Jobs:", summary_rows)
      )
    }
  } else {
    # Nested level - bullet list item with "--" prefix
    cli::cli_li("-- {this_step_title} [{this_step_symbol}] {status_message}")
    
    # If this nested item has children, create another nested bullet list
    if (node$count > 0) {
      cli::cli_ul(id = paste0("children_", node$name, "_", level))
      for (child in node$children) {
        print_step_tree(child, level = level + 1)
      }
      cli::cli_end(id = paste0("children_", node$name, "_", level))
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