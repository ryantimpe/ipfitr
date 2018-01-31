
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
#' @export
ip_create_seed <- function(tars, names.exclude = c("value"), value.set = 1, value.name = "value",
                           override.warning = FALSE){

  #Warnings
  if(is.null(tars) | !is.list(tars) | length(tars) == 1) {stop("Targets must be a list of at least two data frames")}

  #List of all unique values in each series
  series.list <- lapply(tars, function(x){
    dat <- as.data.frame(x[, names(x)[!(names(x) %in% names.exclude)]],
                         stringsAsFactors = FALSE)
    names(dat) <- names(x)[!(names(x) %in% names.exclude)]
    lapply(dat, unique)
  })
  series.list <- unlist(series.list, recursive = F)

  series.list <- sapply(unique(names(series.list)), function(x){
    srs.lst <- series.list[which(names(series.list) == x)]

    #do I want them all???
    # unique(unlist(srs.lst))

    #Or the the common ones? I think common
    Reduce(intersect, srs.lst)

  }, USE.NAMES = TRUE)

  # This take a long time and creates an unnecessarily large data frame if there are many dims.
  # Check with user

  if(length(series.list) >= 10 & length(unlist(series.list)) > 100 & !override.warning){
    message("Seeds with many series or many elements in each series will take a long time to create.\nThe resulting data frame will be very large, likely containing many rows that will be 0 in the final IPF output.")
    message("It might be more efficient to supply a seed to the ip_fit() function.")
    continue_seed_creation <- readline("Are you sure you want to continue? ")
    continue_seed_creation <- tolower(substr(continue_seed_creation, 1, 1))
  } else {
    continue_seed_creation <- "y"
  }

  if(continue_seed_creation != "y") {
    stop("Stopped by user. Try supplying your own seed with existing data.")
  }

  #Create DF, dropping the excluded series before creation
  df <- expand.grid(series.list)
  df <- df %>%
    mutate_if(is.factor, as.character)

  df[, value.name] <- value.set

  return(df)
}
