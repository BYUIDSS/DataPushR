% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dpr_create_package.R
\name{dpr_create_package}
\alias{dpr_create_package}
\title{Create an R package for data}
\usage{
dpr_create_package(
  package_name,
  export_folder = getwd(),
  git_remote,
  list_data = NULL
)
}
\arguments{
\item{package_name}{is the name of the created data R package.}

\item{export_folder}{is the base folder where the package folder will be created.}

\item{git_remote}{is the `HTTPS` url of the GitHub remote.}

\item{list_data}{is a list object of named ojbects that can be written to a csv. If NULL then no data writing actions happen.}
}
\description{
This function automates the process of building a Github R package with the desired data stored in the `raw-data` folder.
}
\examples{
dd <- read_csv(system.file("extdata", "Draft_vietnam.csv", package = "DataPushR"))
dpr_create_package(list_data = list(dat_draft = dd), package_name = "Test3", export_folder = getwd())
}
