
#' Single target scale function calling external dataframe.
#'
#' @param datatable A dataframe of values to be scaled to target.
#' @param target A dataframe of target subtotals over one or more dimensions.
#' @param series_start The name of the series in \code{datatable} to be scaled to the \code{target}.
#' @param series_target The name of the \code{target} series.
#' @return A dataframe with the same dimensionality as \code{datatable}, with \code{series_start} scaled to the subtotals specified by \code{series_target} .
#' @examples
#' df <- data.frame(x = rep(letters[1:2], 2), y = c(rep("c", 2), rep("d", 2)), value = runif(4))
#' tar1 <- data.frame(x = letters[1:2], value = c(20, 30))
#' ip_scale(df, tar1)
#'
#' df %>% ip_scale(tar1, series_start = "value", series_target = "value")

ip_scale <- function(datatable, target, series_start = "value", series_target = "value") {

  names(datatable)[names(datatable) == series_start] <- "value"
  names(target)[names(target) == series_target] <- "target_value"

  datatable <- datatable %>%
    left_join(target) %>%
    group_by_(.dots = as.list(names(target)[!(names(target) %in% c(series_target))])) %>%
    mutate(shr = value/sum(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(value = shr * target_value) %>%
    select(-shr, -target_value)

  return(datatable)
}

#' Single target scale function using single data frame.
#'
#' Single target scale function using single data frame.
#' Analogous to \code{ip_scale}.
#' Primarily for use within other \code{ipfitr} functions to increase speed.
#'
#' @param datatable A data frame containing starting values and the target values.
#' @param target_series Subset of \code{names(datatable)} containing series with target subtotals.
#' @param series_start The name of the series in \code{datatable} to be scaled to the \code{series_target}.
#' @param series_target The name of the series in \code{datatable} containing target subtotals.
#' @return A data frame with the same dimensionality as \code{datatable}, with \code{series_start} scaled to the subtotals specified by \code{series_target} .
#' @examples
#' ip_scale_a(df, c("series1", "series2"))
#'
#' df %>% ip_scale_a("series1", series_start = "count", series_target = "tar1")
#'

ip_scale_a <- function(datatable, target_series, series_start = "value", series_target = "tar1") {

  names(datatable)[names(datatable) == series_start] <- "value_temp"
  names(datatable)[names(datatable) == series_target] <- "tar_temp"
  target_series[target_series == series_target] <- "tar_temp"

  datatable <- datatable %>%
    group_by_(.dots = as.list(target_series[!(target_series %in% c(series_target))])) %>%
    mutate(shr = value_temp/sum(value_temp, na.rm=T)) %>%
    ungroup() %>%
    mutate(value_temp = shr * tar_temp) %>%
    select(-shr)

  names(datatable)[names(datatable) == "value_temp"] <- series_start
  names(datatable)[names(datatable) == "tar_temp"] <- series_target

  return(datatable)
}
