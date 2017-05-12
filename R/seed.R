
#' Create initial dataframe of all unique target series.
#'
#' Create initial dataframe of all unique target series.
#' Essentially \code{expand.grid()} for the tidyverse.
#'
#' @param tars A list of data frames containing series to expand.
#' @param names.exclude Vector of names of series to exclude from the data frame creation.
#' @param value.set Single number or vector of numbers to initialize the seed \code{value.name}.
#' @param value.name Name of series in the data frame containing \code{value.set}.
#' @return A data frame containing one row for each combination of the data frame series.
#'     The series in the first \code{tar} data frame vary fastest.
#'     The columns names match names supplied in each of the \code{tar} data frames.
#' @examples
#' tar1 <- data.frame(x = letters[1:2], value = runif(2))
#' tar2 <- data.frame(y = letters[3:5], value = runif(3))
#' ip_create_seed(list(tar1, tar2))

ip_create_seed <- function(tars, names.exclude = c("value"), value.set = 1, value.name = "value"){

  #Warnings
  if(is.null(tars) | !is.list(tars) | length(tars) == 1) {stop("Targets must be a list of at least two data frames")}

  #List of all unique values in each series
  series.list <- lapply(tars, function(x){
    lapply(x, unique)
  })
  series.list <- unlist(series.list, recursive = F)
  series.list <- series.list[!(names(series.list) %in% names.exclude)]
  series.list <- series.list[!(duplicated(series.list))]

  #Create DF, dropping the excluded series before creation
  df <- expand.grid(series.list)
  df <- df %>%
    mutate_if(is.factor, as.character)

  df[, value.name] <- value.set

  return(df)
}
