% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_postprocess.R
\name{setup_confound_regression}
\alias{setup_confound_regression}
\title{Configure confound regression for postprocessing}
\usage{
setup_confound_regression(ppcfg = list(), fields = NULL)
}
\arguments{
\item{ppcfg}{a postprocessing configuration list (nested within scfg$postprocess)}

\item{fields}{A character vector of fields to be prompted for. If \code{NULL}, all fields will be prompted.}
}
\value{
A modified version of \code{ppcfg} with the \verb{$confound_regression} entry populated.
}
\description{
This function configures voxelwise regression of nuisance confounds from fMRI data. Confounds are typically drawn
from the fMRIPrep confounds file. Users can select confounds to be temporally filtered to match the BOLD data
(e.g., continuous-valued regressors) and those that should not be filtered (e.g., binary spike regressors).
}
\details{
Regression is applied on a voxelwise basis. Filtered regressors typically include motion parameters, CompCor components,
DVARS, or global signal. Unfiltered regressors usually include 0/1 indicators of outlier volumes.
}
\keyword{internal}
