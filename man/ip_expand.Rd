% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expand.R
\name{ip_expand}
\alias{ip_expand}
\title{Wrapper function to create seed and perform iterative proportion fitting from N targets.}
\usage{
ip_expand(targets, target.value.names = "value",
  names.exclude = c("value"), value.set = 1, value.name = "value",
  override.warning = FALSE, max.error = 0.01, max.iterations = 25,
  freeze_cells = NULL, freeze_cells.value.name = "value",
  freeze_slice = NULL, freeze_slice.value.names = "value",
  minmax_cells = NULL, minmax_cells.value.names = c("value_min",
  "value_max"), minmax_slice = NULL,
  minmax_slice.value.names = c("value_min", "value_max"),
  minmax.smash.param = 1/3, save.tars = FALSE, show.messages = TRUE)
}
\arguments{
\item{targets}{A list of data frames containing subtotal targets. All totals in each target should be equal.
Series supplied in each target will shape the final data frame.}

\item{target.value.names}{The names of the series in \code{targets} containing subtotals to scale. Can be string or array of strings.}

\item{names.exclude}{Vector of names of series in targets to exclude from the data frame creation.}

\item{value.set}{Single number or vector of numbers to initialize the seed \code{value.name}.}

\item{value.name}{Name of series in the data frame containing \code{value.set}.}

\item{max.error}{The maximum total absolute difference allowed between final scaled values and targets.
Iterative scaling will complete once the error is below this threshold or \code{max.iterations} occur.}

\item{max.iterations}{The maximum number of iterations of scaling. Iterative scaling with end once this value is reached, even if the error is above \code{max.error}.}

\item{freeze_cells}{Optional data frame of values with same series columns as \code{datatable}, specifying exact values to hit in the scaling.
Any rows or values not listed, or \code{NA}s, will be scaled as normal.}

\item{freeze_cells.value.name}{The name of the series of iced values in \code{freeze_cells}.}

\item{freeze_slice}{Optional data frame containing subtotal targets for the \code{datatable}.
Unlike \code{targets}, these data frames can be subsets only containing subtotals for one or more rows.
Any rows or values not listed, or \code{NA}s, will be scaled as normal.
Using \code{freeze_slice} for partial targets will increase the number of required iterations for scaling. This may require the user to increase the value of \code{max.iterations}.
Separate elements of a dimension within a single cell using " + " to scale subtotals.}

\item{minmax_cells}{Optional data frame of values with same series columns as \code{datatable}, specifying bounded values to hit in the scaling.
Provide minimumn and maximum values for a cell to be scaled.
Any rows or values not listed, or \code{NA}s, will be scaled as normal.}

\item{minmax_cells.value.names}{An array of length 2 of the names of the minimum and maximum values in  \code{minmax_cells}.}

\item{minmax_slice}{Optional data frame containing subtotal targets for the \code{datatable}, specifying bounded values to hit in the scaling.
Provide minimumn and maximum values for a slice of the data frame to be scaled.
Unlike \code{targets}, these data frames can be subsets only containing subtotals for one or more rows.
Any rows or values not listed, or \code{NA}s, will be scaled as normal.
Using \code{minmax_slice} for partial targets will increase the number of required iterations for scaling. This may require the user to increase the value of \code{max.iterations}.}

\item{minmax.smash.param}{Numeric value of  0 < x < 1. Following an out-of-bounds occurence for \code{minmax_cells}, the \code{minmax.smash.param} is the additional value added to the scaled value to bring it back into bounds.
Values close to 0 bind the value to the violated bound, while close to 1 bind the value to the other bound.
Values closer to0 will require more iterations to complete.}

\item{freeze_slice.value.name}{The name or names of the series of iced values in \code{freeze_slice}.}

\item{minmax_slice.value.name}{The name or names of the series of the minimum and maximum values in \code{minmax_slice}.}
}
\value{
A dataframe with the same dimensionality as \code{datatable}, with all values scaled to the subtotals specified in each data frame in \code{targets}.
}
\description{
Wrapper function to create seed and perform iterative proportion fitting from N targets.
Combines the \code{ip_create_seed()} and \code{ip_fit()} functions into a single step. Creates a generic unary seed.
}
\examples{
tar1 <- data.frame(x = letters[1:2], value = c(50, 50))
tar2 <- data.frame(y = letters[3:5], value = c(20, 40, 40))
tar3 <- data.frame(z = letters[6:10], value = c(10, 20, 30, 40, 10))

tar.list <- list(tar1, tar2, tar3)
df <- ip_expand(tar.list)
}
