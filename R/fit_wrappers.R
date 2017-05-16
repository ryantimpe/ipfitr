#' Iterative proportional fitting to scale seed values to multiple target subtotals.
#'
#' Iterative proportional fitting to scale seed values to multiple target subtotals.
#'
#' @param datatable A data frame of values to be scaled to targets.
#' @param targets A list of data frames containing subtotal targets for the \code{datatable}. All totals in each target should be equal.
#' @param assumptions A single data frame containing data belonging to \code{freeze_cells}, \code{freeze_slice}, \code{minmax_cells}, and \code{minmax_slice}.
#' @param datatable.value.names The name of the series in \code{datatable} containing the values to be scaled.
#' @param target.value.names The names of the series in \code{targets} containing subtotals to scale. Can be string or array of strings.
#' @param assumption.value.names The names of the series in \code{assumptions} containing subtotals to scale. Three item array of the freeze value name, min value, max value.
#' @param assumption.drop.names The names of the series in \code{assumptions} to exclude from analysis. Usually columns containing notes.
#' @param max.error The maximum total absolute difference allowed between final scaled values and targets.
#'     Iterative scaling will complete once the error is below this threshold or \code{max.iterations} occur.
#' @param max.iterations The maximum number of iterations of scaling. Iterative scaling with end once this value is reached, even if the error is above \code{max.error}.
#' @param minmax.smash.param Numeric value of  0 < x < 1. Following an out-of-bounds occurence for \code{minmax_cells}, the \code{minmax.smash.param} is the additional value added to the scaled value to bring it back into bounds.
#' Values close to 0 bind the value to the violated bound, while close to 1 bind the value to the other bound.
#' Values closer to 0 will require more iterations to complete.
#' @return A dataframe with the same dimensionality as \code{datatable}, with all values scaled to the subtotals specified in each data frame in \code{targets} and meeting criteria supplied in \code{assumptions}.

#' @export
ip_fit_sl <- function(datatable, targets, assumptions,
                   datatable.value.name = "value", target.value.names = "value",
                   assumption.value.names = c("value", "value_min", "value_max"),
                   assumption.drop.names = c("Notes"),
                   max.error = 0.01, max.iterations = 25,
                   minmax.smash.param = 1/3,
                   save.tars = TRUE, show.messages = TRUE) {

  assumptions_pro <- ip_load_assumptions(assumptions,
                                         freeze.name = assumption.value.names[1], minmax.name = assumption.value.names[2:3],
                                         drop.names = assumption.drop.names)

  df <- datatable %>%
    ip_fit(targets,
           datatable.value.name = datatable.value.name, target.value.names = target.value.names,
           max.error = max.error, max.iterations = max.iterations,
           freeze_cells = assumptions_pro[["freeze_cells"]],
           freeze_slice = assumptions_pro[["freeze_slice"]],
           minmax_cells = assumptions_pro[["minmax_cells"]],
           minmax_slice = assumptions_pro[["minmax_slice"]],
           minmax.smash.param = minmax.smash.param,
           save.tars = save.tars, show.messages = show.messages)

  return(as.data.frame(df))

}
