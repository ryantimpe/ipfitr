
#' Compare data frame to target subtotals and calculate absolute error.
#'
#' @param datatable A data frame of values previously scaled to targets.
#' @param target A data frame of target subtotals over one or more dimensions.
#' @param series_start The name of the series in \code{datatable} to be compared to the \code{target}.
#' @param series_target The name of the \code{target} series.
#' @return A summarized data frame with the same dimensionality as \code{target}, with a measurement of error.
#' @examples
#' df <- data.frame(x = rep(letters[1:2], 2), y = c(rep("c", 2), rep("d", 2)), value = runif(4))
#' tar1 <- data.frame(x = letters[1:2], value = c(20, 30))
#' df %>% ip_scale(tar1) %>% ip_miss(tar1)

ip_miss <- function(datatable, target, series_start = "value", series_target = "value") {

  names(datatable)[names(datatable) == series_start] <- "value"
  names(target)[names(target) == series_target] <- "target_value"

  error <- datatable %>%
    left_join(target) %>%
    group_by_(.dots = as.list(c(names(target)))) %>%
    summarize(sum = sum(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(error = abs(sum - target_value))
}

#' Compare data frame to target subtotals and calculate absolute error.
#'
#' Compare data frame to target subtotals and calculate absolute error in a single data frame.
#' Analogous to \code{ip_miss}.
#' Primarily for use within other \code{ipfitr} functions to increase speed.
#'
#' @param datatable A data frame of values previously scaled to targets.
#' @param target_series Subset of \code{names(datatable)} containing series with target subtotals.
#' @param series_start The name of the series in \code{datatable} to be compared to the \code{target}.
#' @param series_target The name of the \code{target} series.
#' @return A summarized data frame with the same dimensionality as \code{target}, with a measurement of error.

ip_miss_a <- function(datatable, target_series, series_start = "value", series_target = "tar1") {

  names(datatable)[names(datatable) == series_target] <- "target"
  target_series[target_series == series_target] <- "target"

  error <- datatable %>%
    group_by_(.dots = as.list(unique(c(target_series, "target")))) %>%
    summarize(sum = sum(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(error = abs(sum - target))

  names(error)[names(error) == "target"] <- series_target

  return(error)
}
