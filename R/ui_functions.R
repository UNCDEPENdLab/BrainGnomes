
#' Read Multi-Line User Input from Console
#'
#' Prompts the user for multi-line input from the console until a specified number of consecutive blank lines are entered.
#' Optionally collapses the result into a single string.
#'
#' @param instruct Optional character string providing initial instructions to display before prompting.
#' @param prompt Character string used as the prompt symbol for each input line. Defaults to \code{"> "} .
#' @param n_blank Integer indicating how many consecutive blank lines should terminate input. Defaults to 1.
#' @param collapse If non-\code{NULL}, collapses the returned lines into a single string using the given delimiter.
#'
#' @return A character vector of input lines, or a single collapsed string if \code{collapse} is specified.
#'
#' @examples
#' \dontrun{
#' read_multiline_input("Enter notes (press Enter twice to finish):")
#' }
#' 
#' @keywords internal
read_multiline_input <- function(instruct=NULL, prompt="> ", n_blank=1, collapse=NULL) {
  if (!is.null(instruct)) cat(instruct, "\n")
  lines <- character()
  empty_count <- 0
  
  repeat {
    line <- readline(prompt)
    
    if (nchar(trimws(line)) == 0) {
      empty_count <- empty_count + 1
    } else {
      empty_count <- 0
    }
    
    if (empty_count >= n_blank) break
    
    lines <- c(lines, line)
  }
  
  # Collapse into one string or return vector of lines
  if (!is.null(collapse)) {
    return(paste(lines, collapse = collapse))
  } else {
    return(lines)
  }
}

#' Interactive Command-Line Argument Builder
#'
#' Provides an interactive interface for entering, reviewing, editing, and deleting command-line arguments.
#' Accepts an optional starting list of arguments and returns a modified or new set.
#'
#' @param args Optional character vector of existing arguments to edit. If \code{NULL}, a new set of arguments is entered.
#' @param prompt Prompt string shown for each new argument line. Defaults to \code{"> "} .
#' @param instruct Instructional message shown at the start of the session.
#' @param collapse If non-\code{NULL}, collapses the final argument list into a single string using the specified delimiter.
#'
#' @return A character vector of CLI arguments, or a single collapsed string if \code{collapse} is specified.
#'
#' @examples
#' \dontrun{
#' args <- build_cli_args()
#' cat("Final arguments:\n", paste(args, collapse = " "), "\n")
#' }
#'
#' @keywords internal
build_cli_args <- function(args=NULL, prompt="> ", instruct = "Enter arguments (press Enter to finish): ", collapse=NULL) {
  # Step 1: Multi-line input
  cat(instruct, "\n")
  lines <- character()

  # If existing args are passed in, prompt edits and confirmation of changes. If no args, just accept entry and return
  has_args <- !is.null(args)
  
  # Step 2: Interactive edit loop
  repeat {
    if (!is.null(args)) {
      cat("\nCurrent arguments:\n")
      if (length(args) == 0) {
        cat("  [None]\n")
      } else {
        for (i in seq_along(args)) {
          cat(sprintf("  [%d] %s\n", i, args[i]))
        }
      }

      cat("\nOptions:\n")
      cat("  1: Add argument\n")
      cat("  2: Edit argument\n")
      cat("  3: Delete argument\n")
      cat("  4: Done\n")

      choice <- readline("Enter choice [1-4]: ")
    } else {
      choice <- "1" # if no arguments, start by prompting
    }
    
    if (choice == "1") {
      new_arg <- read_multiline_input(prompt=prompt)
      args <- c(args, new_arg)
      if (!has_args) break # don't require confirmation on first entry of arguments
    } else if (choice == "2") {
      idx <- as.integer(readline("Enter argument number to edit: "))
      if (!is.na(idx) && idx >= 1 && idx <= length(args)) {
        current_val <- args[idx]
        new_val <- readline(sprintf("New value for [%s] (press Enter to keep): ", current_val))
        if (nzchar(new_val)) {
          args[idx] <- new_val
        } else {
          cat("Keeping existing value.\n")
        }
      } else {
        cat("Invalid index.\n")
      }
      
    } else if (choice == "3") {
      idx <- as.integer(readline("Enter argument number to delete: "))
      if (!is.na(idx) && idx >= 1 && idx <= length(args)) {
        args <- args[-idx]
      } else {
        cat("Invalid index.\n")
      }
      
    } else if (choice == "4") {
      break # Done action
    } else {
      cat("Invalid choice. Please enter 1, 2, 3, or 4.\n")
    }
  }
  
  # Collapse into one string or return vector of lines
  if (!is.null(collapse)) {
    return(paste(args, collapse = collapse))
  } else {
    return(args)
  }
}



#' Obtain user input from the console
#' @param prompt The character string to display before the user input prompt (e.g., `"Enter location"`).
#' @param prompt_eol The character string to place at the end of the prompt (e.g., `">"`).
#' @param instruct Instructions to display above the prompt (character string, or `NULL` for none).
#' @param type The expected type of the input. Must be one of `"numeric"`, `"integer"`, `"character"`, `"file"`, or `"flag"`.
#' @param lower For numeric inputs, the lowest valid value.
#' @param upper For numeric inputs, the highest valid value.
#' @param len The number of expected values to return. If `NULL`, any number of values is allowed.
#' @param min.len The minimum number of values required (if applicable).
#' @param max.len The maximum number of values allowed (if applicable).
#' @param split Characters to split the input string into multiple values (used when multiple values are expected).
#' @param among A vector of valid values that the input must match (or `NULL` for no restriction).
#' @param required If `TRUE`, the user must provide a value; if `FALSE`, pressing Enter yields an empty/default value.
#' @param uniq If `TRUE`, all entries must be unique.
#' @param default A default value to use if the user presses Enter without typing anything. If non-`NULL`, the prompt will show this default choice.
#' @return The user input, converted to the appropriate type (`numeric`, `integer`, `character`, etc.), or the `default` if provided and no input is given.
#' @details The function will keep prompting the user until a valid input is provided. 
#'   It displays instructions and enforces constraints (e.g., value range, length, uniqueness). 
#' 
#' @note This function is intended for interactive use and may not work as expected in non-interactive environments (e.g., batch scripts).
#' @importFrom glue glue
#' @importFrom checkmate test_string assert_string assert_subset assert_number
#' @keywords internal
prompt_input <- function(prompt = "", prompt_eol=">", instruct = NULL, type = "character", lower = -Inf, upper = Inf, 
  len = NULL, min.len=NULL, max.len=NULL, split = NULL, among = NULL, required = TRUE, uniq=FALSE, default = NULL) {

  if (!interactive()) stop("prompt_input() requires an interactive session.")

  if (is.null(prompt)) prompt <- ""
  if (is.null(prompt_eol)) prompt_eol <- ""

  checkmate::assert_string(prompt)
  checkmate::assert_string(prompt_eol)
  checkmate::assert_string(instruct, null.ok = TRUE)
  checkmate::assert_subset(type, c("numeric", "integer", "character", "file", "flag"))
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_number(len, lower = 1L, null.ok = TRUE)
  checkmate::assert_number(min.len, lower = 1L, null.ok = TRUE)
  checkmate::assert_number(max.len, lower = 1L, null.ok = TRUE)
  if (!is.null(min.len) && !is.null(max.len) && max.len < min.len) {
    stop("max.len must be greater than or equal to min.len")
  }

  if (type == "flag" && !is.null(len) && len > 1L) {
    warning("Ignoring len > 1 for type 'flag' -- only one return supported")
    len <- 1L
  }

  if (!is.null(len) && len > 1 && !checkmate::test_string(split)) {
    stop("When multiple return values are required, you must specify character(s) to split the input.")
  }

  # setup feedback about the number and type of inputs expected
  inp_expect <- ""
  plural <- "s"
  if (!is.null(len)) {
    n_expect <- ifelse(len == 1L, "a", glue("{len}"))
    if (len==1L) plural <- ""
  } else if ((is.null(min.len) || min.len == 1L) && (is.null(max.len) || is.infinite(max.len))) {
    n_expect <- ""
  } else if (is.null(min.len) || min.len == 1L) {
    # max only
    n_expect <- glue("no more than {max.len}")
  } else if (is.null(max.len) || is.infinite(max.len)) {
    # min only
    n_expect <- glue("at least {min.len}")
  } else {
    # min and max
    n_expect <- glue("{min.len}-{max.len}")
  }

  if (nchar(n_expect) > 0L) n_expect <- paste0(n_expect, " ") # add spacing for formatting

  if (type=="integer") {
    if (n_expect=="a ") n_expect <- "an "
    inp_expect <- glue("Input must be {n_expect}integer{plural} between {lower} and {upper}\n")
  } else if (type == "numeric") {
    inp_expect <- glue("Input must be {n_expect}number{plural} between {lower} and {upper}\n")
  } else if (type == "character") {
    inp_expect <- glue("Input must be {n_expect}string{plural} separated by '{split}'\n")
  }

  # Validate default value
  if (!is.null(default)) {
    valid_default <- switch(type,
      "integer" = checkmate::test_integerish(default, lower = lower, upper = upper, len = len, min.len = min.len, max.len = max.len),
      "numeric" = checkmate::test_numeric(default, lower = lower, upper = upper, len = len, min.len = min.len, max.len = max.len),
      "character" = checkmate::test_character(default, len = len, min.len = min.len, max.len = max.len),
      "flag" = is.logical(default) && length(default) == 1,
      "file" = all(sapply(default, checkmate::test_file_exists)),
      FALSE
    )
    if (!valid_default) {
      warning("Invalid default value. Ignoring default.")
      default <- NULL
    }
  }

  # add options for flag prompt
  if (type == "flag") {
    # always ask user for yes/no input
    prompt <- paste(prompt, ifelse(required, "(yes/no)", "(yes/no; press Enter to skip)"))
  } else if (!is.null(default)) {
    prompt <- glue::glue("{prompt} (Press enter to accept default: {default})") # let user know how to skip optional input
  } else if (!required) {
    prompt <- paste(prompt, "(Press enter to skip)") # let user know how to skip optional input
  }

  # always add trailing space to make prompt clear
  if (!grepl("\\s$", prompt)) prompt <- paste0(prompt, " ")
  if (!grepl("\\s$", prompt_eol)) prompt_eol <- paste0(prompt_eol, " ") # also ensure that prompt_eol has trailing space
  prompt <- paste0(prompt, prompt_eol)
  
  # print instructions
  if (checkmate::test_string(instruct)) cat(instruct, "\n")

  # obtain user input
  res <- ""
  while (is.na(res[1L]) || res[1L] == "") {
    r <- readline(prompt)
    if (!is.null(split)) r <- strsplit(r, split, perl = TRUE)[[1]]

    if (!is.null(default) && r[1L] == "") {
      return(default)
    } else if (isFALSE(required) && r[1L] == "") {
      empty <- switch(type,
        "integer" = NA_integer_,
        "numeric" = NA_real_,
        "character" = NA_character_,
        "flag" = NA,
        "file" = NA_character_
      )
      return(empty) # empty input and not required
    } else if (isTRUE(uniq) && length(unique(r)) != length(r)) {
      cat("All entries must be unique.\n")
    } else if (type == "flag") {
      r <- tolower(r)
      if (!r[1L] %in% c("yes", "no", "y", "n")) {
        cat("Please respond yes or no.\n")
      } else {
        res <- substr(r[1L], 1, 1) == "y" # TRUE if yes, FALSE if no
      }
    } else if (type == "integer") {
      r <- type.convert(r, as.is = TRUE) # convert to apparent atomic type for validation
      if (!checkmate::test_integerish(r, lower = lower, upper = upper, len = len, min.len = min.len, max.len = max.len)) {
        cat(inp_expect)
      } else {
        if (!is.null(among) && !all(r %in% among)) {
          cat(glue("Input must be integers in the set: {paste(among, collapse=', ')}\n"))
        } else {
          res <- r
        }
      }
    } else if (type == "numeric") {
      r <- type.convert(r, as.is = TRUE) # convert to apparent atomic type for validation
      if (!checkmate::test_numeric(r, lower = lower, upper = upper, len = len, min.len = min.len, max.len = max.len)) {
        cat(inp_expect)
      } else {
        res <- r
      }
    } else if (type == "character") {
      if (!checkmate::test_character(r, len = len, min.len = min.len, max.len = max.len)) {
        cat(inp_expect)
      } else {
        if (!is.null(among) && !all(r %in% among)) {
          cat(glue("Input must be {len} strings in the set: {paste(among, collapse=', ')}\n"))
        } else {
          res <- r
        }
      }
    } else if (type == "file") {
      # should probably think harder about unquoted filenames containing spaces throwing off len
      exist <- sapply(r, checkmate::test_file_exists)
      if (!all(exist)) {
        cat(glue("The following files could not be found: {paste(r[!exist], collapse=', ')}\n"))
      } else {
        res <- r
      }
    }
  }

  return(res)
}

# x <- prompt_input(prompt = "test me?", type = "character", split = " ")
# x <- prompt_input(prompt = "test me?", type = "character", split = " ", len = 3)
# x <- prompt_input(prompt = "test me?", type = "character", split = " ", min.len = 2)
# x <- prompt_input(prompt="test me?", type="character", split=" ", max.len=2)
# x <- prompt_input(prompt = "test me?", type = "character", split = " ", min.len = 2, max.len = 5)
# x <- prompt_input(prompt = "test me?", type = "numeric", split = " ", min.len = 2, max.len = 5)
# x <- prompt_input(prompt="test me?", type="integer", split=" ", min.len=2, max.len=5)


#' helper function to setup output spaces for fMRIPrep
#' @param output_spaces A string of existing output spaces to be modified.
#' @return A string of output spaces to be used in fMRIPrep.
#' @keywords internal
#' @importFrom glue glue
#' @importFrom utils menu select.list
#' @noRd
choose_fmriprep_spaces <- function(output_spaces = NULL) {
  cat(glue("\nfmriprep uses --output-spaces to control the stereotaxic space and resolution
           of preprocessed images. Its default space is MNI152NLin2009cAsym and the default
           spatial resolution matches the raw/native data. Here, you can specify the output
           spaces to be generated. This is not a comprehensive list of all templates available
           in TemplateFlow (https://github.com/templateflow/templateflow), but it encompasses
           the most useful ones. The default of MNI152NLin2009cAsym is pretty good, too!
           For more detail, see: https://fmriprep.org/en/stable/spaces.html.

           Of note, the 'res-' specifier controls the output resolution, but it is not the
           voxel size! Rather, it refers to a resolution index in the files uploaded to
           TemplateFlow. Usually, res-1 is 1mm and res-2 is 2mm, but you mileage may vary.

           If you are using AROMA as part of your pipeline, we will automatically add
           MNI152NLin6Asym:res-2 so that fmripost-aroma can run.\n\n
           "))

  templates_available <- c(
    "MNI152NLin2009cAsym", "MNI152NLin6Asym", "MNI152NLin6Sym",
    "MNI152NLin2006Asym", "MNIPediatricAsym", "MNIInfant",
    "MNIColin27", "MNI305", "OASIS30ANTs"
  )
  additional_spaces <- c("T1w", "T2w", "anat", "fsnative", "fsaverage", "fsaverage5", "fsaverage6")

  # Parse input string into initial set
  current_spaces <- character()
  if (!is.null(output_spaces)) {
    current_spaces <- unlist(strsplit(output_spaces, "\\s+"))
  }

  repeat {
    cat("\nCurrent --output-spaces:\n")
    if (length(current_spaces) == 0) {
      cat("  (none selected yet)\n")
    } else {
      for (i in seq_along(current_spaces)) {
        cat(sprintf("  [%d] %s\n", i, current_spaces[i]))
      }
    }

    cat("\nWhat would you like to do?\n")
    choice <- menu(c("Add a space", "Delete a space", "Finish and return"), title = "Modify output spaces:")

    if (choice == 1) {
      # Add a space
      type_choice <- menu(c("Template space", "Other space (e.g., T1w, fsaverage)"), title = "Add space type:")
      if (type_choice == 1) {
        # Select a template
        selected_template <- utils::select.list(templates_available, multiple = FALSE, title = "Choose a template")
        if (selected_template != "") {
          res_input <- readline(paste0("Enter resolution index for ", selected_template, " (or press ENTER to skip): "))
          space_string <- if (res_input == "") {
            selected_template
          } else {
            paste0(selected_template, ":res-", res_input)
          }
          current_spaces <- unique(c(current_spaces, space_string))
        }
      } else if (type_choice == 2) {
        selected_additional <- utils::select.list(additional_spaces, multiple = TRUE, title = "Choose additional space(s)")
        current_spaces <- unique(c(current_spaces, selected_additional))
      }
    } else if (choice == 2) {
      # Delete a space
      if (length(current_spaces) == 0) {
        cat("No spaces to delete.\n")
      } else {
        del_choice <- utils::select.list(current_spaces, multiple = TRUE, title = "Select space(s) to remove:")
        current_spaces <- setdiff(current_spaces, del_choice)
      }
    } else if (choice == 3) {
      break
    }
  }

  final_output_spaces <- paste(current_spaces, collapse = " ")
  cat("\nFinal --output-spaces argument:\n")
  cat("  ", final_output_spaces, "\n")
  return(final_output_spaces)
}