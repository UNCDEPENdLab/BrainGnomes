% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edit_project.R
\name{edit_project}
\alias{edit_project}
\title{Interactively edit a study configuration by field (field-guided)}
\usage{
edit_project(input = NULL)
}
\arguments{
\item{input}{A \code{bg_project_cfg} object, a YAML file path, or a project
directory containing \code{project_config.yaml}. If a directory is provided
but the file is absent, \code{edit_project} will stop. This argument cannot
be \code{NULL}.}
}
\value{
An updated \code{bg_project_cfg} object. The updated configuration is
written to \code{project_config.yaml} in the project directory unless the user
chooses not to overwrite an existing file.
}
\description{
Allows the user to interactively browse and edit individual fields within the
configuration object, grouped by domain. Field paths are defined within the function
to avoid relying on a complete \code{scfg} structure.
}
