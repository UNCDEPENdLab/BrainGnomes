\name{get_subject_status}
\alias{get_subject_status}
\title{Get processing status for a subject}
\usage{get_subject_status(scfg, sub_id, ses_id = NULL)}
\arguments{\item{scfg}{Study configuration list.}
\item{sub_id}{Subject identifier.}
\item{ses_id}{Optional session identifier.}}
\value{Data.frame summarizing completion status and times for the requested subject/session.}
\description{Checks the log directory for \code{.complete} files and returns completion information for each enabled processing step.}
\seealso{\code{\link{get_project_status}}}
