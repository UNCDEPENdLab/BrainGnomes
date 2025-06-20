% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_postprocess.R
\name{setup_spatial_smooth}
\alias{setup_spatial_smooth}
\title{Specify the spatial smoothing settings for postprocessing}
\usage{
setup_spatial_smooth(scfg = list(), fields = NULL)
}
\arguments{
\item{scfg}{a study configuration object created by \code{setup_project}}

\item{fields}{a character vector of fields to be prompted for. If \code{NULL}, all fields will be prompted for.}
}
\value{
a modified version of \code{scfg} with the \verb{$postprocess$spatial_smooth} field populated
}
\description{
Specify the spatial smoothing settings for postprocessing
}
\keyword{internal}
