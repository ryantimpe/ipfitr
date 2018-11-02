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

  warning(
    "The function 'ip_backcast' is deprecated. Please use 'ia_cast()' inside a dplyr::mutate() statement."
  )

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

#' Use one vector to fore- or backcast another vector
#'
#' @param target Vector of values to be fore- or backcast.
#' @param cast_source Vector of values or growth rates to be applied to \code{target} as a cast. \code{cast_source[1]} aligns with \code{target[1]}. Must be same length as \code{target}
#' @param cast_source_metric 'level' to apply calculated from rates from \code{cast_source}. 'growth_rate' to use \code{cast_source[1]} as is.
#' @param direction Use 'forward' to forecast from \code{base_index} or 'backward' to backcast from  \code{base_index}, preserving original values in opposite direction. Use 'both' to cast in both directions from \code{base_index}.
#' @param base_index Optional index to begin cast in \code{target}. Otherwise first/last non-NA value will be used
#' @return An array of same length as \code{target} with backcast values.
#' @examples
#' ia_cast(c(10:20, rep(NA, 5)), c(1:16))
#'
#' ia_cast(c(rep(NA, 5), 10:20), c(1:16), direction = "backward", base_index = 10)
#' @export
#'
ia_cast <- function(target, cast_source, cast_source_metric = "level",
                    direction = "forward", base_index = NULL){

  #Check input sizes
  if(length(cast_source) < length(target)){
    warning("Length of `cast_source` less than `target`. Filling with NA.")
    cast_source <- c(cast_source, rep(NA, (length(target) - length(cast_source))))
  }
  if(length(cast_source) > length(target)){
    stop("Length of `cast_source` greater than `target`.")
  }

  #Check direction
  if(!(direction %in% c("forward", "backward", "both"))){
    stop("direction must either be 'forward', 'backward', or 'both'")
  }
  if(direction == "both" && is.null(base_index)){
    stop("if direction = 'both', a base index must be supplied.")
  }

  #Index of target to apply the cast
  if(!is.null(base_index)){
    bs_index <- as.numeric(base_index[1])
  } else if(direction == "forward"){
    bs_index <- max(which(!is.na(target)))
  } else if(direction == "backward"){
    bs_index <- min(which(!is.na(target)))
  }

  #cast_source_growth
  if(cast_source_metric == "level"){
    sr_metric <- cast_source / dplyr::lag(cast_source)
  } else if(cast_source_metric == "growth_rate") {
    sr_metric <- cast_source + 1
  } else {
    stop("cast_source_metric must be either 'level' or 'growth_rate'")
  }

  sr_metric <- ifelse(is.na(sr_metric), 1, sr_metric)
  sr_metric_cum <- cumprod(sr_metric)

  #normalize to bs_index
  sr_metric_norm <- sr_metric_cum / sr_metric_cum[bs_index]

  #New value
  bs_value <- target[bs_index]

  bs_cast <- bs_value * sr_metric_norm

  tar_out <- dplyr::case_when(
    direction == "forward"  ~  c(target[1:bs_index], bs_cast[(bs_index+1):length(bs_cast)]),
    direction == "backward" ~  c(bs_cast[1:(bs_index-1)], target[bs_index:length(target)]),
    direction == "both"     ~  bs_cast
  )

  return(tar_out)
}
