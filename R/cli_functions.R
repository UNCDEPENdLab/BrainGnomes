#' Helper function to allow a nested list to be traversed using a key string supporting nesting
#' @param lst a list to be traversed
#' @param key_strings a character vector of keys to traverse the list. Each key string should be
#'   a single string with the keys separated by a separator (default is "/"). For example, "parent/child/grandchild"
#'   would correspond to my_list$parent$child$grandchild
#' @param sep a character string to separate the keys in the key strings. Default is "/"
#' @param simplify Logical; whether to simplify the output to a vector (if possible). Default is `TRUE`.
#' @return a named list of values corresponding to the keys in the key strings
#' @keywords internal
get_nested_values <- function(lst, key_strings, sep = "/", simplify = TRUE) {
  split_keys_list <- strsplit(key_strings, sep, fixed = TRUE)
  
  ret <- lapply(split_keys_list, function(keys) {
    val <- lst
    for (j in seq_along(keys)) {
      key <- keys[[j]]
      if (is.null(val[[key]])) return(NULL)
      val <- val[[key]]
    }
    val
  })
  
  names(ret) <- key_strings
  
  if (simplify) {
    do_unlist <- FALSE
    # if more than one value is returned, see if they are atomic and of the same data type
    if (length(ret) > 1L) {
      atomic_lengths <- vapply(ret, function(x) is.atomic(x) && length(x) == 1, logical(1))
      atomic_types <- vapply(ret, typeof, character(1))
      # simplify when we have only 1 element or when all elements are atomic and the same type
      if (all(atomic_lengths) && length(unique(atomic_types)) == 1) do_unlist <- TRUE
    } else if (length(ret) == 1L) {
      if (is.atomic(ret[[1L]])) {
        do_unlist <- TRUE
      } else {
        ret <- ret[[1L]] # pull out first element (probably a list) but don't mess with its names
      }
    }
    
    if (do_unlist) ret <- unlist(ret, use.names = TRUE)
  }
  
  return(ret)
}

#' Assign values to a nested list using key-value strings or named list
#'
#' Parses assignments like \code{"a/b/c=10"} or named lists like \code{list("a/b/c" = 10)} and returns
#' \code{list(a = list(b = list(c = 10)))}.
#'
#' @param assignments Either a character vector of assignment strings (e.g., \code{"a/b=1"})
#'   or a named list where names encode nested keys (e.g., \code{list("a/b" = 1)}).
#' @param sep A character used to separate keys. Default is \code{"/"}.
#' @param lst Optional list to update. If \code{NULL}, a new list is created.
#' @param type_values Logical; whether to convert character values to appropriate types.
#'
#' Supports values containing `=` characters; only the first `=` is treated as the
#' separator between key and value. Subsequent `=` characters are preserved in
#' the value.
#'
#' @return A nested list with the specified keys and values.
#' @importFrom utils modifyList
#' @importFrom stats setNames
#' @importFrom checkmate assert_string assert_flag assert_list
#' @keywords internal
set_nested_values <- function(assignments, sep = "/", lst = NULL, type_values = TRUE) {
  checkmate::assert_string(sep)
  checkmate::assert_flag(type_values)
  checkmate::assert_list(lst, null.ok = TRUE)
  if (is.null(lst)) lst <- list()

  # handle character vector input
  if (is.character(assignments)) {
    for (a in assignments) {
      parts <- strsplit(a, "=", fixed = TRUE)[[1]]
      if (length(parts) < 2L) stop("Invalid assignment format: ", a)
      key_str <- parts[1]
      if (length(parts) > 2L) {
        # if additional equal signs are present, treat everything after the first as the value
        val_str <- paste(parts[-1], collapse = "=")
      } else {
        val_str <- parts[2]
      }

      keys <- strsplit(key_str, sep, fixed = TRUE)[[1]]

      value <- scan(text = val_str, what = character(), quote = "'\"", quiet = TRUE)
      if (type_values) value <- if (!is.na(value[1L]) && value[1L] == "NULL") NULL else type.convert(value, as.is = TRUE) # type.convert won't convert NULL

      nested <- value
      for (key in rev(keys)) {
        nested <- setNames(list(nested), key)
      }

      lst <- modifyList(lst, nested)
    }

  # handle named list input
  } else if (is.list(assignments) && !is.null(names(assignments))) {
    for (nm in names(assignments)) {
      if (!nzchar(nm)) stop("All elements of named list must have non-empty names.")
      keys <- strsplit(nm, sep, fixed = TRUE)[[1]]
      value <- assignments[[nm]]

      nested <- value
      for (key in rev(keys)) {
        nested <- setNames(list(nested), key)
      }

      lst <- modifyList(lst, nested)
    }

  } else {
    stop("assignments must be a character vector or a named list with non-empty names.")
  }

  return(lst)
}

#' Convert a nested list into CLI-style arguments using slash-separated keys
#'
#' Recursively traverses a nested list and returns a character vector or string
#' of arguments like \code{--a/b=10 --c=25}.
#'
#' @param lst A named (possibly nested) list.
#' @param sep Separator used for nested keys (default is "/").
#' @param collapse Logical; if TRUE, returns a single space-separated string.
#'
#' @return A character vector (or single string if \code{collapse = TRUE}) of CLI-style arguments.
#'
#' @examples
#' \dontrun{
#'    nested_list_to_args(list(a = list(b = c(10, 11, 12)), c = 25))
#' }
#'
#' @importFrom checkmate assert_list assert_string assert_flag
#' @keywords internal
nested_list_to_args <- function(lst, sep = "/", collapse = FALSE) {
  checkmate::assert_list(lst)
  checkmate::assert_string(sep)
  checkmate::assert_flag(collapse)
  
  flatten <- function(x, prefix = NULL) {
    args <- character()
    
    for (nm in names(x)) {
      key <- if (is.null(prefix)) nm else paste(prefix, nm, sep = sep)
      val <- x[[nm]]
      
      if (is.list(val) && !is.null(names(val))) {
        args <- c(args, flatten(val, key))
      } else {
        # quote argument consistently across platforms (especially Windows)
        # using single quotes via shQuote(..., type = "sh") to avoid wildcard
        # expansion and other oddities
        val_str <- paste(shQuote(as.character(val), type = "sh"), collapse = " ")
        args <- c(args, paste0("--", key, "=", val_str))
      }
    }
    
    return(args)
  }
  
  args <- flatten(lst)
  if (collapse) {
    return(paste(args, collapse = " "))
  } else {
    return(args)
  }
}


#' Parse CLI-style arguments into a nested list using args_to_df()
#'
#' This function tokenizes command-line arguments using \code{args_to_df()} and builds a nested list
#' by interpreting forward slashes in keys (e.g., \code{--a/b=10 11}) as nested structure.
#'
#' @param args A character vector (e.g., from \code{commandArgs(trailingOnly = TRUE)}).
#' @param sep A character used to separate nested keys. Default is \code{"/"}.
#' @param type_values Logical; whether to attempt to conver right-hand side strings to relevant data
#'   types using `type.convert`.
#'
#' @return A nested list of parsed CLI arguments.
#'
#' @importFrom checkmate assert_character
#' @export
parse_cli_args <- function(args, sep = "/", type_values = TRUE) {
  checkmate::assert_character(args)

  # always collapse into a single string to avoid continuation errors in args_to_df
  if (length(args) > 1L) args <- paste(args, collapse = " ")
  df <- args_to_df(args)
  assignments <- paste(df$lhs, df$rhs, sep="=")
  set_nested_values(assignments, sep = sep, type_values = type_values)
}


#' Parse command-line arguments into a structured data frame
#'
#' Converts a character vector of CLI-style arguments into a data frame with fields for position,
#' argument name, value, number of hyphens, and whether the argument used an equals sign.
#'
#' @param arg_vec A character vector of shell-style argument strings (e.g., \code{"--arg=value"} or \code{"--arg value"}).
#'
#' @return A data frame with one row per parsed argument and the following columns:
#' \describe{
#'   \item{argpos}{The index of the original string in the input vector.}
#'   \item{lhs}{The left-hand side of the argument (name).}
#'   \item{rhs}{The right-hand side of the argument (value), or \code{NA} if none found.}
#'   \item{has_equals}{Logical; \code{TRUE} if the argument used \code{=}, otherwise \code{FALSE}.}
#'   \item{nhyphens}{The number of hyphens used in the argument prefix (1 or 2).}
#' }
#'
#' @details Supports both \code{--arg=value} and \code{--arg value} formats. Multi-token values
#' following a key are collapsed into a single space-separated string.
#'
#' @keywords internal
#' @importFrom checkmate assert_character
args_to_df <- function(arg_vec = NULL) {
  checkmate::assert_character(arg_vec)
  results <- list()
  # require that each element of arg_vec begins with a hyphen (or two)
  hyp_start <- grepl("\\s*-{1,2}", arg_vec, perl=TRUE)
  if (!all(hyp_start)) stop("args_to_df requires that each element of the input vector start with one or two hyphens")
  
  # Split the string by whitespace to get individual arguments
  all_split <- strsplit(arg_vec, "\\s+")
  
  for (i in seq_along(arg_vec)) {
    tokens <- all_split[[i]]
    j <- 1
    while (j <= length(tokens)) {
      token <- tokens[j]
      nhyphens <- ifelse(grepl("^--", token), 2, ifelse(grepl("^-", token), 1, 0))
      if (nhyphens > 0L) {
        token_naked <- sub("^--?", "", token)
        
        if (grepl("=", token_naked)) {
          has_equals <- TRUE
          parts <- strsplit(token_naked, "=", fixed = TRUE)[[1]]
          lhs <- parts[1]
          rhs_vals <- if (length(parts) > 1) paste(parts[-1], collapse = "=") else ""
          #to_parse <- c(parts[2], tokens)
        } else {
          has_equals <- FALSE
          lhs <- token_naked
          rhs_vals <- character(0)
        }
      }
      
      # Gather all following tokens until next one starts with "-" or end of input
      while (j + 1 <= length(tokens) && !grepl("^-", tokens[j + 1])) {
        rhs_vals <- c(rhs_vals, tokens[j + 1])
        j <- j + 1
      }
      
      rhs <- if (length(rhs_vals) > 0) paste(rhs_vals, collapse = " ") else NA
      
      # add result to data.frame
      results[[length(results) + 1]] <- data.frame(
        argpos = i,
        lhs = lhs,
        rhs = rhs,
        has_equals = has_equals,
        nhyphens = nhyphens,
        stringsAsFactors = FALSE
      )
      
      j <- j + 1 # go to next hyphenated token
    }
  }
  
  do.call(rbind, results) # combine to data.frame
  
}


#' helper function that takes a character vector of CLI arguments and replaces matching old values with
#'   intended new values
#' @param args a character vector of existing CLI arguments
#' @param new_values a character vector of new CLI arguments to be substituted into `args`
#' @param collapse a flag indicating whether to collapse the return argument into a single string
#' @return a modified character vector of CLI arguments
#' @importFrom checkmate assert_character
#' @keywords internal
set_cli_options <- function(args = NULL, new_values = NULL, collapse=FALSE) {
  checkmate::assert_character(new_values)
  if (is.list(args) && length(args) ==  0L) args <- NULL # convert empty list to NULL
  if (!is.null(args) && length(args) == 1L && is.na(args[1L])) args <- NULL # convert NA input to NULL

  # helper to convert a parsed CLI data.frame back into a character string of CLI arguments
  df_to_args <- function(df) {
    split_df <- split(df, ~argpos)
    result <- character(length(split_df))

    for (i in seq_along(split_df)) {
      args <- split_df[[i]]
      tokens <- character(nrow(args))

      for (j in seq_len(nrow(args))) {
        prefix <- strrep("-", args$nhyphens[j])
        if (is.na(args$rhs[j])) {
          # No rhs
          tokens[j] <- paste0(prefix, args$lhs[j])
        } else {
          sep <- ifelse(args$has_equals[j], "=", " ")
          tokens[j] <- paste0(prefix, args$lhs[j], sep, args$rhs[j])
        }
      }

      # Concatenate the tokens with spaces
      result[i] <- paste(tokens, collapse = " ")
    }

    result
  }

  # helper to update rows in base_df, a parsed CLI data.frame with rows in updates_df, another parsed CLI data.frame
  update_cli_args <- function(base_df = NULL, updates_df) {
    for (i in seq_len(nrow(updates_df))) {
      upd <- updates_df[i, ]

      # Find matching lhs in base
      match_idx <- which(base_df$lhs == upd$lhs)

      if (length(match_idx) > 0) {
        # If found, update all matching rows
        base_df[match_idx, c("rhs", "has_equals", "nhyphens")] <- upd[, c("rhs", "has_equals", "nhyphens")]
      } else {
        # If not found, add the update to the first argpos (or choose strategy)
        upd$argpos <- ifelse(is.null(base_df), 1, min(base_df$argpos))
        base_df <- rbind(base_df, upd)
      }
    }

    # Re-sort by argpos if needed
    base_df <- base_df[order(base_df$argpos), ]
    rownames(base_df) <- NULL
    return(base_df)
  }

  if (is.null(args) || args[1L] == "") {
    args <- new_values
  } else {
    args_df <- args_to_df(args)
    new_values_df <- args_to_df(new_values)
    args <- df_to_args(update_cli_args(args_df, new_values_df))
  }

  # convert into a single cli string if requested
  if (isTRUE(collapse)) {
    args <- paste(args, collapse=" ")
  }

  return(args)
}

# Pretty print a list with indentation and line wrapping
pretty_print_list <- function(x, indent = 0, width = 80) {
  indent_str <- strrep("  ", indent)

  for (name in names(x)) {
    value <- x[[name]]

    if (is.list(value)) {
      cat(sprintf("%s%s:\n", indent_str, name))
      pretty_print_list(value, indent + 1, width)
    } else {
      # Format value as character string
      value_str <- paste(value, collapse = ", ")

      # Wrap long lines
      wrapped <- strwrap(value_str,
        width = width - nchar(indent_str) - nchar(name) - 2,
        exdent = 2, simplify = FALSE
      )[[1]]

      # Print wrapped lines
      cat(sprintf("%s%s: %s\n", indent_str, name, wrapped[1]))
      if (length(wrapped) > 1) {
        for (line in wrapped[-1]) {
          cat(sprintf("%s%s  %s\n", indent_str, strrep(" ", nchar(name)), line))
        }
      }
    }
  }
}
