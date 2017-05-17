#' Create all freeze and minmax assumption data frames from single data frame
#'
#' @param assumptions A data frame of assumptions that will be converted into \code{freeze_cells}, \code{freeze_slice},
#' \code{minmax_cells}, and \code{minmax_slice} for \code{ip_expand()} and \code{ip_fit()} functions.
#' @param freeze.name The name of the series in \code{assumptions} with freeze values.
#' @param minmax.name The names of the series in \code{assumptions} with min and max values.
#' @param drop.names The name of the series in \code{assumptions} to remove from analysis.
#' @return A list of data frames and list containing \code{freeze_cells}, \code{freeze_slice}, \code{minmax_cells}, and \code{minmax_slice}.
#' @export

ip_load_assumptions <- function(assumptions, freeze.name = "value", minmax.name = c("value_min", "value_max"),
                                drop.names = c("Notes")) {

  value.names <- c(freeze.name, minmax.name)

  as1 <- assumptions
  as1 <- as1[, names(as1)[!(names(as1) %in% drop.names)]]
  as1$tar__names <- apply(as1, 1, function(x){
    dat <- names(as1)[!is.na(x) & !(names(as1) %in% value.names)]
    return(paste(dat, collapse = " "))
  })

  as1$tar__type <- apply(as1, 1, function(x){
    dat <- length(names(as1)[!is.na(x) & !(names(as1) %in% value.names)])

    #All vars are accounted for and none of them are combos wtih +
    if(dat == length(names(as1)[!(names(as1) %in% value.names)]) & !any(grepl(" + ", x, fixed = T))) {
      if(!is.na(x[names(as1) == freeze.name])) {
        return("freeze_cells")
      } else{
        return("minmax_cells")
      }
    } else{
      if(!is.na(x[names(as1) == freeze.name])) {
          return("freeze_slice")
      } else{
          return("minmax_slice")
      }
    }
  })


  #Freeze Cells df
  freeze_cells <- as1 %>%
    filter(tar__type == "freeze_cells") %>%
    select(-dplyr::contains("__"))

  freeze_cells[, minmax.name] <- NULL
  freeze_cells <- as.data.frame(freeze_cells)
  if(nrow(freeze_cells) == 0){freeze_cells <- NULL}

  #Min/Max Cells df
  minmax_cells <- as1 %>%
    filter(tar__type == "minmax_cells") %>%
    select(-dplyr::contains("__"))

  minmax_cells[, freeze.name] <- NULL
  minmax_cells <- as.data.frame(minmax_cells)
  if(nrow(minmax_cells) == 0){minmax_cells <- NULL}

  #Freeze Slice df
  freeze_slice <- as1 %>%
    filter(tar__type == "freeze_slice") %>%
    select(-dplyr::contains("__"))

  freeze_slice[, minmax.name] <- NULL
  freeze_slice <- as.data.frame(freeze_slice)
  if(nrow(freeze_slice) == 0){freeze_slice <- NULL}

  #Min/Max Slice df
  minmax_slice <- as1 %>%
    filter(tar__type == "minmax_slice") %>%
    select(-dplyr::contains("__"))

  minmax_slice[, freeze.name] <- NULL
  minmax_slice <- as.data.frame(minmax_slice)
  if(nrow(minmax_slice) == 0){minmax_slice <- NULL}


  #Finalize

  organized.assumptions <- list()

  organized.assumptions[["freeze_cells"]] <- freeze_cells
  organized.assumptions[["freeze_slice"]] <- freeze_slice
  organized.assumptions[["minmax_cells"]] <- minmax_cells
  organized.assumptions[["minmax_slice"]] <- minmax_slice

  return(organized.assumptions)

}
