#' Scale data frame values to match growth targets over any series
#'
#' @param datatable A data frame of values.
#' @param target A data frame of target growth rates over one or more dimensions.
#' Should include an additional series to specify which series the growth are over.
#' @param series_start The name of the series in \code{datatable} to be compared to the \code{target}.
#' @param series_target The name of the \code{target} series containing growth rates.
#' @param series_growth_over The name of the series in \code{target} specifying the series that are covered by the growth rates.
#' @param sep Separator string to list multiple series in \code{series_growth_over}.
#' @param growth_distance Number of lags between observations when applying growth rates.
#' @return A data frame of target values for use in \code{freeze_slice} in \code{ip_fit}.
#' @export
ip_growth_transform <- function(datatable, target, series_start = "value", series_target = "growth",
                                series_growth_over = "growth__over", sep = " + ",
                                growth_distance = 1) {

  names(datatable)[names(datatable) == series_start] <- "value"
  names(target)[names(target) == series_target] <- "growth"
  names(target)[names(target) == series_growth_over] <- "growth__over"

  tars <- target

  # datatable <- datatable %>%
  #   mutate(value = ifelse(value == 0, NA, value))

  #Add column for grouping - unique sets of series included
  tars$type <- apply(tars, 1, function(x){
    series <- names(target)[!is.na(x)]
    series <- series[!(series  %in% c("growth", "growth__over"))]

    growth__over <- x[names(x) == "growth__over"]

    return(paste0(growth__over, " | ", paste(series, collapse = " ")))
  })

  tars.list <- lapply(unique(tars$type), function(x){
    df <- tars %>% filter(type == x) %>% select(-type)
    df <- df[, colSums(is.na(df)) == 0]
    return(df)
  })

  tars.values <- lapply(seq_along(tars.list), function(i){
    x <- tars.list[[i]] %>%
      select(-growth__over)

    growth__over <- strsplit(tars.list[[i]]$growth__over[1], split=sep)[[1]]

    dat <- datatable  %>%
      group_by_(.dots = as.list(names(x %>% select(-growth)))) %>%
      summarize(value_start = sum(value, na.rm=T)) %>%
      ungroup() %>%
      left_join(x, by = names(x %>% select(-growth))) %>%
      #Sort and group by the non-group__over series
      arrange_(.dots = names(.)[!names(.) %in% c("value_start", "growth", growth__over)]) %>%
      group_by_(.dots = names(.)[!names(.) %in% c("value_start", "growth", growth__over)]) %>%
      mutate(growth_start = value_start / lag(value_start, growth_distance) -1) %>%
      mutate(growth_new = ifelse(is.na(growth), growth_start, growth)) %>%
      mutate(value_base = value_start[min(which(!is.na(value_start)))]) %>%
      mutate(cum_new = ifelse(is.na(growth_new), 0, growth_new)) %>%
      mutate(cum_new = cumprod(cum_new+1)) %>%
      mutate(value_new = value_base * cum_new) %>%
      #Only need the targets where growth is specified & after(?)
      mutate(keep_growth = ifelse(is.na(growth), 0, growth)) %>%
      mutate(keep_growth = cumsum(keep_growth)) %>%
      ungroup() %>%
      filter(keep_growth > 0) %>%
      select(-value_start, -value_base, -growth, -growth_start, -growth_new, -cum_new, -keep_growth)

    names(dat)[names(dat) == "value_new"] <- paste0("tar__growth__", i)

    return(dat)

  })

  #tars.values <- bind_rows(tars.values)
  #Return the list of DFs

  return(tars.values)
}
