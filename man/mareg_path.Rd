% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path.R
\name{mareg_path}
\alias{mareg_path}
\title{Retrieve the mareg data path}
\usage{
mareg_path(..., root = mareg_root(), create = FALSE)
}
\arguments{
\item{...}{char, path segments}

\item{root}{char, the root directory specification}

\item{create}{logical, if TRUE create the path if it doesn't exist}
}
\value{
path specification, possibly untested for existence
}
\description{
Retrieve the mareg data path
}
