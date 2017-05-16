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
    if(dat == length(names(as1)[!(names(as1) %in% value.names)])) {
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

  #Freeze Slices list

  freeze_slice_all <- as1 %>%
    filter(tar__type == "freeze_slice")

  if(nrow(freeze_slice_all) == 0){
    freeze_slice <- NULL
  } else {
    freeze_slice <- lapply(unique(freeze_slice_all$tar__names), function(x){
      df <- freeze_slice_all %>%
        filter(tar__names == x) %>%
        select(-dplyr::contains("__"))

      df[, minmax.name] <- NULL
      df <- df[,(colSums(is.na(df))<nrow(df) | names(df) %in% value.names )]

      return(as.data.frame(df))
    })
  }

  #MinMax Slices list

  minmax_slice_all <- as1 %>%
    filter(tar__type == "minmax_slice")

  if(nrow(minmax_slice_all) == 0){
    minmax_slice <- NULL
  } else {
    minmax_slice <- lapply(unique(minmax_slice_all$tar__names), function(x){
      df <- minmax_slice_all %>%
        filter(tar__names == x) %>%
        select(-dplyr::contains("__"))

      df[, freeze.name] <- NULL
      df <- df[,(colSums(is.na(df))<nrow(df) | names(df) %in% value.names )]

      return(as.data.frame(df))
    })
  }

  organized.assumptions <- list()

  organized.assumptions[["freeze_cells"]] <- freeze_cells
  organized.assumptions[["freeze_slice"]] <- freeze_slice
  organized.assumptions[["minmax_cells"]] <- minmax_cells
  organized.assumptions[["minmax_slice"]] <- minmax_slice

  return(organized.assumptions)

}
