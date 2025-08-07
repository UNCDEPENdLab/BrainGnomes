\name{summary.bg_status_df}
\alias{summary.bg_status_df}
\title{Summarize project status}
\usage{\method{summary}{bg_status_df}(object, ...)}
\arguments{\item{object}{A data.frame from \code{get_project_status}.}\item{...}{Additional arguments (unused).}}
\value{Data.frame with counts of subjects that have completed each processing step.}
\description{Provides a tabular summary of completion counts for each step in the pipeline.}
\seealso{\code{\link{get_project_status}}}
