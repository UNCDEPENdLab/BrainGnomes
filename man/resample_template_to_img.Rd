% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postprocess_functions.R
\name{resample_template_to_img}
\alias{resample_template_to_img}
\title{Resample TemplateFlow Mask to fMRIPrep Image Using Python}
\usage{
resample_template_to_img(
  in_file,
  output = NULL,
  template_resolution = 1,
  suffix = "mask",
  desc = "brain",
  extension = ".nii.gz",
  interpolation = "nearest",
  install_dependencies = TRUE,
  overwrite = FALSE
)
}
\arguments{
\item{in_file}{Path to the BIDS-compliant NIfTI file (e.g., an fMRIPrep preprocessed BOLD image).}

\item{output}{Optional path to write the resampled image. If NULL, a BIDS-style filename is constructed.}

\item{template_resolution}{Integer specifying the TemplateFlow resolution index (e.g., 1 = 1mm).}

\item{suffix}{TemplateFlow suffix (e.g., "mask", "T1w").}

\item{desc}{TemplateFlow descriptor (e.g., "brain").}

\item{extension}{File extension for the template image (default is ".nii.gz").}

\item{interpolation}{Interpolation method to use during resampling. Options are
"nearest", "linear", or "continuous".}

\item{install_dependencies}{Logical. If \code{TRUE} (default), attempts to automatically install
required Python packages (nibabel, nilearn, templateflow) if they are missing from the active environment.
If \code{FALSE}, the function will raise an error if dependencies are not found.}

\item{overwrite}{Logical. If \code{TRUE}, overwrite the existing output file (if present).}
}
\value{
Invisibly returns \code{TRUE} on success. A new NIfTI file is written to \code{output}.
}
\description{
This function uses Python (via \code{reticulate}) to identify and resample a TemplateFlow mask
to match the resolution and spatial dimensions of an fMRIPrep BOLD image.
}
\details{
The appropriate template is inferred from the \verb{space-} entity of the BIDS-formatted input filename.
For example, an input such as:
\code{sub-221256_task-trust_run-1_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz}
will lead to selection of the MNI152NLin2009cAsym template.

This function depends on a companion Python script (\code{fetch_matched_template_image.py})
that is bundled with the BrainGnomes package and sourced at runtime.
}
