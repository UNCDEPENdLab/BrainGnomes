% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postprocess_functions.R
\name{confound_regression}
\alias{confound_regression}
\title{Regress confound time series from a 4D fMRI image}
\usage{
confound_regression(
  in_file,
  out_desc = NULL,
  to_regress = NULL,
  censor_file = NULL,
  overwrite = FALSE,
  lg = NULL,
  fsl_img = NULL
)
}
\arguments{
\item{in_file}{Path to the input 4D NIfTI file.}

\item{out_desc}{The BIDS description field for the file output by this step}

\item{to_regress}{Path to a text file containing nuisance regressors (one column per regressor).}

\item{censor_file}{An optional censor file (1s indicate volumes to keep) that is used to}

\item{overwrite}{Logical; whether to overwrite the output file if it already exists.}

\item{lg}{Optional lgr object used for logging messages}

\item{fsl_img}{Optional Singularity image to execute FSL commands in a containerized environment.}
}
\value{
Path to the residualized output NIfTI file.
}
\description{
Uses FSL's \code{fsl_glm} to remove nuisance regressors from a 4D NIfTI image. The residuals
from the regression are re-centered by adding back the temporal mean of the original image.
}
\details{
The regressors are converted to FSL's binary matrix format using \code{Text2Vest}.
The residuals are computed using \code{fsl_glm}, and the temporal mean of the original image is
added back to preserve baseline signal intensity.
}
\keyword{internal}
