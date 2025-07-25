% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{butterworth_filter_cpp}
\alias{butterworth_filter_cpp}
\title{Apply Butterworth Filter to 4D NIfTI Image}
\usage{
butterworth_filter_cpp(
  infile,
  b,
  a,
  outfile = "",
  internal = FALSE,
  padtype = "even",
  padlen = -1L,
  use_zi = TRUE
)
}
\arguments{
\item{infile}{Character string. Path to the input 4D NIfTI file.}

\item{b}{Numeric vector. Numerator filter coefficients.}

\item{a}{Numeric vector. Denominator filter coefficients.}

\item{outfile}{Character string. Optional path to save the filtered image.}

\item{internal}{Logical. Whether to return an internal RNifti image object (default = false).}

\item{padtype}{String. Padding type: "even", "odd", "constant", or "zero" (default = "even").}

\item{use_zi}{Logical. Whether to use steady-state initial conditions (default = true).}
}
\value{
A 4D filtered NIfTI image as a niftiImage or internalImage object.
}
\description{
This function applies a temporal Butterworth filter to each voxel time series
in a 4D NIfTI image using forward-backward filtering.
}
\keyword{internal}
