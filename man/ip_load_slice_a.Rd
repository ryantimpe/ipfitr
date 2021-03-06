% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_slice_a.R
\name{ip_load_slice_a}
\alias{ip_load_slice_a}
\title{Convert a data frame of slice targets to individual data frames}
\usage{
ip_load_slice_a(slice, slice.value.name = "value",
  drop.names = c("Notes"), prefix = "frz")
}
\arguments{
\item{slice}{A data frame of assumptions for \code{freeze_slice} or \code{minmax_slice}.}

\item{slice.value.name}{The name of the series in \code{slice} with freeze (single) or min/max (array) values.}

\item{drop.names}{The name of the series in \code{slice} to remove from analysis.}
}
\value{
A list of data frames containing \code{freeze_slice} or \code{minmax_slice} individual data frames for use in ip_fit().
}
\description{
Convert a data frame of slice targets to individual data frames
Mostly for use in ip_fit() functions
}
