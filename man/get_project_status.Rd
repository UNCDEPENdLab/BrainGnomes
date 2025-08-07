\name{get_project_status}
\alias{get_project_status}
\title{Get processing status for all subjects}
\usage{get_project_status(scfg)}
\arguments{\item{scfg}{Study configuration list.}}
\value{Data.frame with one row per subject/session containing completion status columns.}
\description{Uses \code{is_step_complete()} to check whether pipeline steps have completed for each subject and session.}
\seealso{\code{\link{get_subject_status}}}
