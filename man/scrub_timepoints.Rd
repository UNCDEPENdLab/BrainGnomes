% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postprocess_functions.R
\name{scrub_timepoints}
\alias{scrub_timepoints}
\title{Remove Censored Volumes and Update Confounds}
\usage{
scrub_timepoints(
  in_file,
  censor_file = NULL,
  out_desc = NULL,
  confound_files = NULL,
  overwrite = FALSE,
  lg = NULL
)
}
\arguments{
\item{in_file}{Path to the input 4D NIfTI file.}

\item{censor_file}{Path to the 1D censor vector used to identify volumes to
remove.}

\item{out_desc}{The BIDS description field for the file output by this step}

\item{confound_files}{Optional character vector of confound or regressor
files to update alongside the fMRI data.}

\item{overwrite}{Logical; overwrite the output NIfTI if it exists.}

\item{lg}{Optional \code{Logger} object for message output.}
}
\value{
The path to the scrubbed NIfTI image.
}
\description{
Removes timepoints flagged in a censor file from a 4D NIfTI image. When
\code{confound_files} are provided, the corresponding rows in those files
are removed so that the time series remain aligned.
}
\keyword{internal}
