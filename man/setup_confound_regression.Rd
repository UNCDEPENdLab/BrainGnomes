% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_postprocess.R
\name{setup_confound_regression}
\alias{setup_confound_regression}
\title{Specify the confound regression settings for postprocessing}
\usage{
setup_confound_regression(scfg = list(), fields = NULL)
}
\arguments{
\item{scfg}{a study configuration object created by \code{setup_project}}

\item{fields}{a character vector of fields to be prompted for. If \code{NULL}, all fields will be prompted for.}
}
\value{
a modified version of \code{scfg} with the \verb{$postprocess$confound_regression} field populated
}
\description{
Specify the confound regression settings for postprocessing
}
\keyword{internal}
