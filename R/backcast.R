#' Use one data value series to backcast other series
#'
#' @param datatable A dataframe with two value columns
#' @param series_target The name of the series in \code{datatable} to be backcast.
#' @param series_base The name of the data series in \code{datatable} to use as backcast for missing \code{series_target}
#' @param growth_over Array of series names to calculate growth over. E.g. Year for annual percent growth
#' @return A dataframe with the same dimensionality as \code{datatable}, with an additional data column of backcast data.
#' @export
ip_backcast <- function(datatable, series_target = "value", series_base = "value_base",
                        growth_over = c("Year")) {

  names(datatable)[names(datatable) == series_target] <- ".srs_targ"
  names(datatable)[names(datatable) == series_base]   <- ".srs_base"

  datatable <- datatable %>%
    dplyr::group_by(!!!rlang::syms(names(.)[!(names(.) %in% c(growth_over, ".srs_targ", ".srs_base"))])) %>%
    #We want an index to multiply by rather than a growth rate
    #If the value is NA, let's make that 1
    dplyr::mutate(.srs_base_index = .srs_base / dplyr::lag(.srs_base),
           .srs_base_index = ifelse(is.na(.srs_base_index), 1, .srs_base_index)) %>%
    #Then we want to get the cumulative or rolling product of this index
    #Remember to group by all series again. We keep this group for a few lines
    dplyr::mutate(.srs_base_index_cum = cumprod(.srs_base_index)) %>%
    #This next line looks ugly... we are taking every cum index and dividing it by the index where the first NEW source value is NOT NA
    dplyr::mutate(.srs_base_index_cum_rebase = .srs_base_index_cum /
             .srs_base_index_cum[min(which(!is.na(.srs_targ)))]) %>%
    #We also want this base value from the New source... using the logic above.
    dplyr::mutate(.srs_targ_base = .srs_targ[min(which(!is.na(.srs_targ)))]) %>%
    #Now we can backcast! Wherever NEW source is empty, using New_Base * Index_cum_rebase
    dplyr::mutate(.srs_backcast = ifelse(is.na(.srs_targ), .srs_targ_base * .srs_base_index_cum_rebase, .srs_targ)) %>%
    #Finally you can ungroup
    dplyr::ungroup()

  #Return names
  names(datatable)[names(datatable) == ".srs_targ"] <- series_target
  names(datatable)[names(datatable) == ".srs_base"]   <- series_base
  names(datatable)[names(datatable) == ".srs_backcast"]   <- paste0(series_target, "_bcst")

  return(datatable)
}

#' Use one data value series to backcast other series
#'
#' @param target Array of values to be backcast
#' @param backcast_source Array of value, whose growth rates will be applied to \code{target} as a backcast. \code{backcast_source[1]} aligns with \code{target[1]}. Must be same length as \code{target}
#' @param base_index Optional index to begin backcast in \code{target}
#' @return An array of same length as \code{target} with backcast values.
#' @examples
#' ia_backcast(c(rep(NA, 5), 10:20), c(1:10))
#'
#' ia_backcast(c(rep(NA, 5), 10:20), c(1:10), 10)
#' @export
#'
ia_backcast <- function(target, backcast_source, base_index = NULL){
  dat <- tibble::tibble(.srs_targ = target,
                .srs_base = backcast_source)%>%
    #We want an index to multiply by rather than a growth rate
    #If the value is NA, let's make that 1
    dplyr::mutate(.srs_base_index = .srs_base /  dplyr::lag(.srs_base),
           .srs_base_index = ifelse(is.na(.srs_base_index), 1, .srs_base_index)) %>%
    #Then we want to get the cumulative or rolling product of this index
    #Remember to group by all series again. We keep this group for a few lines
    dplyr::mutate(.srs_base_index_cum = cumprod(.srs_base_index))

  #If no base_index, use first non-NA
  if(!is.null(base_index) && !(is.numeric(base_index) && length(base_index) == 1)) {
    stop("base_index must be a single numeric value")
  }
  if(is.null(base_index) || !(is.numeric(base_index) && length(base_index) == 1)) {
    dat1 <- dat %>%
      #This next line looks ugly... we are taking every cum index and dividing it by the index where the first NEW source value is NOT NA
      dplyr::mutate(.srs_base_index_cum_rebase = .srs_base_index_cum /
               .srs_base_index_cum[min(which(!is.na(.srs_targ)))]) %>%
      #We also want this base value from the New source... using the logic above.
      dplyr::mutate(.srs_targ_base = .srs_targ[min(which(!is.na(.srs_targ)))]) %>%
      #Now we can backcast! Wherever NEW source is empty, using New_Base * Index_cum_rebase
      dplyr::mutate(.srs_backcast = ifelse(is.na(.srs_targ), .srs_targ_base * .srs_base_index_cum_rebase, .srs_targ))
  } else {
    dat1 <- dat %>%
      #This next line looks ugly... we are taking every cum index and dividing it by the index where the first NEW source value is NOT NA
      dplyr::mutate(.srs_base_index_cum_rebase = .srs_base_index_cum /
               .srs_base_index_cum[base_index]) %>%
      #We also want this base value from the New source... using the logic above.
      dplyr::mutate(.srs_targ_base = .srs_targ[base_index]) %>%
      #Now we can backcast! Wherever NEW source is empty, using New_Base * Index_cum_rebase
      dplyr::mutate(.srs_backcast = ifelse(is.na(.srs_targ), .srs_targ_base * .srs_base_index_cum_rebase, .srs_targ))
  }

  return(dat1$.srs_backcast)
}
