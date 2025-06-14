% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cli_functions.R
\name{args_to_df}
\alias{args_to_df}
\title{Parse command-line arguments into a structured data frame}
\usage{
args_to_df(arg_vec = NULL)
}
\arguments{
\item{arg_vec}{A character vector of shell-style argument strings (e.g., \code{"--arg=value"} or \code{"--arg value"}).}
}
\value{
A data frame with one row per parsed argument and the following columns:
\describe{
\item{argpos}{The index of the original string in the input vector.}
\item{lhs}{The left-hand side of the argument (name).}
\item{rhs}{The right-hand side of the argument (value), or \code{NA} if none found.}
\item{has_equals}{Logical; \code{TRUE} if the argument used \code{=}, otherwise \code{FALSE}.}
\item{nhyphens}{The number of hyphens used in the argument prefix (1 or 2).}
}
}
\description{
Converts a character vector of CLI-style arguments into a data frame with fields for position,
argument name, value, number of hyphens, and whether the argument used an equals sign.
}
\details{
Supports both \code{--arg=value} and \code{--arg value} formats. Multi-token values
following a key are collapsed into a single space-separated string.
}
\keyword{internal}
