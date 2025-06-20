% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_postprocess.R
\name{setup_temporal_filter}
\alias{setup_temporal_filter}
\title{Specify the temporal filtering settings for postprocessing}
\usage{
setup_temporal_filter(scfg = list(), fields = NULL)
}
\arguments{
\item{scfg}{a study configuration object created by \code{setup_project}}

\item{fields}{a character vector of fields to be prompted for. If \code{NULL}, all fields will be prompted for.}
}
\value{
a modified version of \code{scfg} with the \verb{$postprocess$temporal_filter} field populated
}
\description{
Specify the temporal filtering settings for postprocessing
}
\keyword{internal}
