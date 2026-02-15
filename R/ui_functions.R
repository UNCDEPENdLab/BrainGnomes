
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
    line <- getline(prompt)
    
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
  has_args <- !is.null(args) && !all(is.na(args))
  args <- args[!is.na(args)] # NA is indicator of unset cli args
  
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

      choice <- getline("Enter choice [1-4]: ")
    } else {
      choice <- "1" # if no arguments, start by prompting
    }
    
    if (choice == "1") {
      new_arg <- read_multiline_input(prompt=prompt)
      args <- c(args, new_arg)
      if (!has_args) break # don't require confirmation on first entry of arguments
    } else if (choice == "2") {
      idx <- as.integer(getline("Enter argument number to edit: "))
      if (!is.na(idx) && idx >= 1 && idx <= length(args)) {
        current_val <- args[idx]
        new_val <- getline(sprintf("New value for [%s] (press Enter to keep): ", current_val))
        if (nzchar(new_val)) {
          args[idx] <- new_val
        } else {
          cat("Keeping existing value.\n")
        }
      } else {
        cat("Invalid index.\n")
      }
      
    } else if (choice == "3") {
      idx <- as.integer(getline("Enter argument number to delete: "))
      if (!is.na(idx) && idx >= 1 && idx <= length(args)) {
        args <- args[-idx]
      } else {
        cat("Invalid index.\n")
      }
      
    } else if (choice == "4") {
      if (length(args) == 0L) args <- NA_character_ # indicator of empty
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


# function that allows line input in interactive session or Rscript with tty
# this doesn't work as expected in the Rscript context -- shifted to Rcpp implementation
# getline <- function(prompt = "") {
#   if (interactive()) {
#     out <- readline(prompt)
#   } else if (base::isatty(stdin())) {
#     cat(prompt)
#     out <- tryCatch(
#       scan(file = "stdin", what = "", nmax = 1, quiet = TRUE, sep = "\n", blank.lines.skip = FALSE),
#       error = function(e) ""
#     )
#     if (length(out) == 0) return("")
#   } else {
#     stop("getline requires an interactive session or a tty")
#   }
#   
#   return(out)
# }

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
#' @param empty_keyword Optional single token (default `"NA"`) that the user can enter (case-insensitive) to explicitly return a missing value when `default` is non-`NULL` and `required = FALSE`. Use `NULL` to disable this shortcut entirely.
#' @return The user input, converted to the appropriate type (`numeric`, `integer`, `character`, etc.), or the `default` if provided and no input is given.
#' @details The function will keep prompting the user until a valid input is provided. 
#'   It displays instructions and enforces constraints (e.g., value range, length, uniqueness). 
#'   When `empty_keyword` is supplied and `default` is non-`NULL` with `required = FALSE`, typing the token (case-insensitive) returns a missing value for the given `type` without accepting the default.
#' 
#' @note This function is intended for interactive use and may not work as expected in non-interactive environments (e.g., batch scripts).
#' @importFrom glue glue
#' @importFrom checkmate test_string assert_string assert_choice assert_number
#' @importFrom utils type.convert
#' @keywords internal
prompt_input <- function(prompt = "", prompt_eol=">", instruct = NULL, type = "character", lower = -Inf, upper = Inf, 
  len = NULL, min.len=NULL, max.len=NULL, split = NULL, among = NULL, required = TRUE, uniq=FALSE, default = NULL,
  empty_keyword = "NA") {

  # In non-interactive sessions launched via Rscript, interactive() returns
  # FALSE even though reading from stdin is possible. Allow such cases when the
  # standard input is a TTY.
  if (!interactive() && !base::isatty(stdin())) {
    stop("prompt_input() requires an interactive session.")
  }
  
  if (is.null(prompt) || length(prompt) == 0L || is.na(prompt[1L])) prompt <- ""
  if (is.null(prompt_eol) || length(prompt_eol) == 0L || is.na(prompt_eol[1L])) prompt_eol <- ""

  checkmate::assert_string(prompt)
  checkmate::assert_string(prompt_eol)
  checkmate::assert_string(instruct, null.ok = TRUE)
  checkmate::assert_choice(type, c("numeric", "integer", "character", "file", "flag"))
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_number(len, lower = 1L, null.ok = TRUE)
  checkmate::assert_number(min.len, lower = 1L, null.ok = TRUE)
  checkmate::assert_number(max.len, lower = 1L, null.ok = TRUE)
  checkmate::assert_string(empty_keyword, null.ok = TRUE)
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

  empty_keyword_display <- NULL
  empty_keyword_lookup <- NULL
  if (!is.null(empty_keyword)) {
    empty_keyword <- trimws(empty_keyword) # always remove whitespace
    if (nzchar(empty_keyword)) {
      empty_keyword_lookup <- tolower(empty_keyword)
      empty_keyword_display <- glue::glue("'{empty_keyword}'")
    }
  }
  show_empty_hint <- !is.null(empty_keyword_display) && !is.null(default) && isFALSE(required)

  normalize_instruction_block <- function(x) {
    if (!checkmate::test_string(x)) return(NULL)

    x <- gsub("\r\n?", "\n", x)
    lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
    if (!length(lines)) return(NULL)

    while (length(lines) > 0L && !nzchar(trimws(lines[1L]))) lines <- lines[-1L]
    while (length(lines) > 0L && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
    if (!length(lines)) return(NULL)

    non_empty <- lines[nzchar(trimws(lines))]
    indent_counts <- nchar(sub("^([ \t]*).*", "\\1", non_empty))
    common_indent <- if (length(indent_counts)) min(indent_counts) else 0L

    if (common_indent > 0L) {
      lines <- substring(lines, common_indent + 1L)
    }

    paste(lines, collapse = "\n")
  }

  if (type == "flag") {
    # Handle yes/no prompts
    if (!is.null(default)) {
      # Add helpful hint that pressing enter accepts default
      prompt <- glue::glue("{prompt} (yes/no; Press enter to accept default: {ifelse(isTRUE(default), 'yes', 'no')}")
      if (show_empty_hint) prompt <- glue::glue("{prompt}; type {empty_keyword_display} to leave blank")
      prompt <- paste0(prompt, ")")
    } else if (required) {
      prompt <- glue::glue("{prompt} (yes/no)")
    } else {
      prompt <- glue::glue("{prompt} (yes/no; Press enter to skip)")
    }
  } else {
    # Non-flag input types
    if (!is.null(default)) {
      prompt <- glue::glue("{prompt} (Press enter to accept default: {default}")
      if (show_empty_hint) prompt <- glue::glue("{prompt}; type {empty_keyword_display} to leave blank")
      prompt <- paste0(prompt, ")")
    } else if (!required) {
      prompt <- glue::glue("{prompt} (Press enter to skip)")
    }
  }

  prompt <- trimws(prompt, which = "right")
  prompt_eol <- trimws(prompt_eol, which = "right")
  if (!nzchar(prompt_eol)) prompt_eol <- ">"
  if (nzchar(prompt)) {
    prompt <- paste0(prompt, "\n", prompt_eol, " ")
  } else {
    prompt <- paste0(prompt_eol, " ")
  }
  
  # print instructions
  instruct <- normalize_instruction_block(instruct)
  if (checkmate::test_string(instruct)) cat("\n", instruct, "\n\n", sep = "")

  # define typed empty return value
  empty <- switch(type,
        "integer" = NA_integer_,
        "numeric" = NA_real_,
        "character" = NA_character_,
        "flag" = NA,
        "file" = NA_character_
      )

  # obtain user input
  res <- ""
  while (is.na(res[1L]) || res[1L] == "") {
    #r <- readline(prompt)
    r <- getline(prompt)

    # handle skip-out on empty_keyword
    if (show_empty_hint && !is.null(empty_keyword_lookup)) {
      if (tolower(trimws(r)) == empty_keyword_lookup) return(empty)
    }

    if (!is.null(split) && nzchar(r)) r <- strsplit(r, split, perl = TRUE)[[1]]

    if (!is.null(default) && r[1L] == "") {
      return(default)
    } else if (isFALSE(required) && r[1L] == "") {
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

#' Prompt for a directory path and confirm behavior if it does not exist
#'
#' Prompts the user for a directory path. If it exists, optionally checks:
#' - ownership (check_owner)
#' - readability (check_readable; can try make_readable if owned)
#' - writability (check_writable; can try make_writable if owned)
#' Any remaining issues are combined into a single proceed prompt.
#'
#' @param check_owner logical; if TRUE, confirm the directory is owned by the current user (or prompt to proceed).
#' @param check_readable logical; require directory to be readable (or confirm proceed).
#' @param check_writable logical; require directory to be writable (or confirm proceed).
#' @param make_readable logical; if TRUE and owned, add user-read (+x for dirs).
#' @param make_writable logical; if TRUE and owned, add user-write.
#' @param default Default directory path to suggest.
#' @param ... Additional args forwarded to `prompt_input()` (e.g., `instruct`, `prompt`).
#' @return A character scalar path (may or may not exist).
#' @keywords internal
prompt_directory <- function(check_owner = FALSE,
                             check_readable = FALSE,
                             check_writable = FALSE,
                             make_readable = FALSE,
                             make_writable = FALSE,
                             default = NULL,
                             ...) {
  # ---- helpers -------------------------------------------------------------
  get_owner <- function(p) {
    p <- normalizePath(p, mustWork = TRUE)
    q <- shQuote(p)
    ow <- suppressWarnings(system2("stat", c("-c", "%U", q), stdout = TRUE, stderr = FALSE))
    if (length(ow) == 1L && !grepl("invalid option|usage:", ow, ignore.case = TRUE)) return(ow)
    ow <- suppressWarnings(system2("stat", c("-f", "%Su", q), stdout = TRUE, stderr = FALSE))
    if (length(ow) == 1L) return(ow)
    NA_character_
  }
  
  current_user <- function() {
    u <- suppressWarnings(system2("id", "-un", stdout = TRUE, stderr = FALSE))
    if (!length(u) || is.na(u) || !nzchar(u)) u <- Sys.info()[["user"]]
    if (!length(u) || is.na(u) || !nzchar(u)) u <- Sys.getenv("USER", unset = NA_character_)
    u
  }
  
  add_bits_if_owner <- function(path, oct_bits) {
    owner <- get_owner(path); who <- current_user()
    if (!length(owner) || is.na(owner) || !identical(owner, who)) return(FALSE)
    info <- file.info(path, extra_cols = TRUE); cur <- info$mode
    if (is.na(cur)) return(FALSE)
    new_mode <- bitwOr(cur, as.integer(strtoi(oct_bits, base = 8L)))
    ok <- isTRUE(Sys.chmod(path, mode = as.octmode(new_mode)))
    isTRUE(ok)
  }
  
  # u+r (and u+x for dirs)
  ensure_user_readable <- function(path) {
    if (isTRUE(file.access(path, 4L) == 0L)) return(TRUE)
    bits <- "0400"
    if (isTRUE(file.info(path, extra_cols = TRUE)$isdir)) {
      bits <- sprintf("%04o", strtoi("0400", 8L) + strtoi("0100", 8L)) # 0500
    }
    cat("Attempting to add read permission to", path, "\n")
    if (!add_bits_if_owner(path, bits)) return(FALSE)
    isTRUE(file.access(path, 4L) == 0L)
  }
  
  # u+w
  ensure_user_writable<- function(path) {
    if (isTRUE(file.access(path, 2L) == 0L)) return(TRUE)
    cat("Attempting to add write permission to", path, "\n")
    if (!add_bits_if_owner(path, "0200")) return(FALSE)
    isTRUE(file.access(path, 2L) == 0L)
  }
  
  # ---- main loop -----------------------------------------------------------
  repeat {
    dir_path <- prompt_input(type = "character", default = default, ...)
    
    if (checkmate::test_directory_exists(dir_path)) {
      dir_path <- normalizePath(dir_path, mustWork = TRUE)
      
      owner <- get_owner(dir_path)
      who   <- current_user()
      is_owner <- (length(owner) == 1L && !is.na(owner) && identical(owner, who))
      
      # Try to fix what we can first (only when owned)
      # READ
      if (isTRUE(check_readable) && !isTRUE(file.access(dir_path, 4L) == 0L) &&
          isTRUE(make_readable) && is_owner) {
        invisible(ensure_user_readable(dir_path))
      }
      # WRITE
      if (isTRUE(check_writable) && !isTRUE(file.access(dir_path, 2L) == 0L) &&
          isTRUE(make_writable) && is_owner) {
        invisible(ensure_user_writable(dir_path))
      }
      
      # Now collect any *remaining* issues into a single message
      issues <- character()
      
      if (isTRUE(check_owner) && !is_owner) {
        issues <- c(issues, glue::glue("it is owned by '{owner}', not the current user ('{who}')"))
      }
      if (isTRUE(check_readable) && !isTRUE(file.access(dir_path, 4L) == 0L)) {
        if (isTRUE(make_readable) && !is_owner) {
          issues <- c(issues, glue::glue("it is not readable and permissions cannot be changed because you are not the owner"))
        } else {
          issues <- c(issues, "it is not readable by the current user")
        }
      }
      if (isTRUE(check_writable) && !isTRUE(file.access(dir_path, 2L) == 0L)) {
        if (isTRUE(make_writable) && !is_owner) {
          issues <- c(issues, glue::glue("it is not writable and permissions cannot be changed because you are not the owner"))
        } else {
          issues <- c(issues, "it is not writable by the current user")
        }
      }
      
      # If there are any issues, prompt ONCE
      if (length(issues)) {
        instruct <- glue::glue("
          The selected directory '{dir_path}' has the following issue(s):
            - {paste(issues, collapse = '\\n  - ')}
          ")
        proceed <- prompt_input(
          instruct = instruct,
          prompt   = "Proceed with this directory anyway?",
          type     = "flag"
        )
        if (!isTRUE(proceed)) next
      }
      
      return(dir_path)
      
    } else {
      proceed <- prompt_input(
        instruct = glue::glue("
          The specified directory '{dir_path}' does not currently exist.
          BrainGnomes will attempt to create it later if needed, but if you expected it to exist, this may indicate a problem with your path.
        "),
        prompt = "Proceed with this directory?",
        type   = "flag"
      )
      if (isTRUE(proceed)) {
        return(normalizePath(dir_path, mustWork = FALSE))
      }
      # otherwise, re-prompt
    }
  }
}

#' helper function to setup output spaces for fMRIPrep
#' @param output_spaces A string of existing output spaces to be modified.
#' @return A string of output spaces to be used in fMRIPrep.
#' @keywords internal
#' @importFrom glue glue
#' @importFrom utils menu select.list
#' @noRd
choose_fmriprep_spaces <- function(output_spaces = NULL) {
  cat(glue("\n
      fmriprep uses --output-spaces to control the stereotaxic space and resolution
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
      MNI152NLin6Asym:res-2 so that fmripost-aroma can run.\n\n"))

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
    choice <- menu_safe(c("Add a space", "Delete a space", "Finish and return"), title = "Modify output spaces:")

    if (choice == 1) {
      # Add a space
      type_choice <- menu_safe(c("Template space", "Other space (e.g., T1w, fsaverage)"), title = "Add space type:")
      if (type_choice == 1) {
        # Select a template
        selected_template <- select_list_safe(templates_available, multiple = FALSE, title = "Choose a template")
        if (selected_template != "") {
          res_input <- getline(paste0("Enter resolution index for ", selected_template, " (or press ENTER to skip): "))
          space_string <- if (res_input == "") {
            selected_template
          } else {
            paste0(selected_template, ":res-", res_input)
          }
          current_spaces <- unique(c(current_spaces, space_string))
        }
      } else if (type_choice == 2) {
        selected_additional <- select_list_safe(additional_spaces, multiple = TRUE, title = "Choose additional space(s)")
        current_spaces <- unique(c(current_spaces, selected_additional))
      }
    } else if (choice == 2) {
      # Delete a space
      if (length(current_spaces) == 0) {
        cat("No spaces to delete.\n")
      } else {
        del_choice <- select_list_safe(current_spaces, multiple = TRUE, title = "Select space(s) to remove:")
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
