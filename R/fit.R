
#' Iterative proportional fitting to scale seed values to multiple target subtotals.
#'
#' Iterative proportional fitting to scale seed values to multiple target subtotals.
#'
#' @param datatable A data frame of values to be scaled to targets.
#' @param targets A list of data frames containing subtotal targets for the \code{datatable}. All totals in each target should be equal.
#' @param datatable.value.names The name of the series in \code{datatable} containing the values to be scaled.
#' @param target.value.names The names of the series in \code{targets} containing subtotals to scale. Can be string or array of strings.
#' @param max.error The maximum total absolute difference allowed between final scaled values and targets.
#'     Iterative scaling will complete once the error is below this threshold or \code{max.iterations} occur.
#' @param max.iterations The maximum number of iterations of scaling. Iterative scaling with end once this value is reached, even if the error is above \code{max.error}.
#' @param ice_cells Optional data frame of values with same series columns as \code{datatable}, specifying exact values to hit in the scaling.
#'    Any values not listed, or \code{NA}s, will be scaled as normal.
#' @param ice_cells.value.name The name of the series of iced values in \code{ice_cells}.
#' @return A dataframe with the same dimensionality as \code{datatable}, with all values scaled to the subtotals specified in each data frame in \code{targets}.
#' @examples
#' tar1 <- data.frame(x = letters[1:2], value = c(50, 50))
#' tar2 <- data.frame(y = letters[3:5], value = c(20, 40, 40))
#' tar3 <- data.frame(z = letters[6:10], value = c(10, 20, 30, 40, 10))
#'
#' tar.list <- list(tar1, tar2, tar3)
#' df <- ip_create_seed(tar.list) %>% ip_fit(tar.list)

ip_fit <- function(datatable, targets, datatable.value.name = "value", target.value.names = "value",
                   max.error = 0.01, max.iterations = 25,
                   ice_cells = NULL, ice_cells.value.name = "value") {

  #Warnings
  if(is.null(targets) | !is.list(targets) | length(targets) == 1) {stop("Targets must be a list of at least two data frames")}

  #Set initial conditions
  current.error     <- 10^9
  current.iteration <- 0

  tar.list <- targets
  df0 <- datatable
  names(df0)[names(df0) == datatable.value.name] <- "value"

  #Freeze 1 - Ice Cells
  if(!is.null(ice_cells)){
    print(names(ice_cells))
    names(ice_cells)[names(ice_cells) == ice_cells.value.name] <- "ice_c"

    df0 <- df0 %>%
      left_join(ice_cells) %>%
      #Iced cells are not unknown, so we don't need to include them in the IPF
      mutate(value = ifelse(is.na(ice_c), value, 0))

    #Since we 0'd out the seed, we should also remove all the iced values from the targets
    tar.list <- lapply(tar.list, function(x){
      names(x)[names(x) %in% target.value.names] <- "value"

      ice_target <- ice_cells %>%
        group_by_(.dots = as.list(names(x)[!(names(x) == "value")])) %>%
        summarize(iced = sum(ice_c, na.rm=T)) %>%
        ungroup()

      df <- x %>%
        left_join(ice_target) %>%
        mutate(iced = ifelse(is.na(iced), 0, iced)) %>%
        mutate(value = value - iced) %>%
        select(-iced)

      return(df)
    })
  } #End ice cells

  #Format targets - give each value unique names
  for( i in seq_along(tar.list)){
    x <- tar.list[[i]]

    names(x)[names(x) == "value"] <- paste0("tar", i)
    names(tar.list[[i]]) <- names(x)
  }

  #Format input seed
  for(x in tar.list){
    df0 <- df0 %>%
      left_join(x)
  }

  while(current.error > max.error & current.iteration < max.iterations ) {
    print(paste("Iteration", current.iteration))

    ##
    # Scaling
    ##

    df1 <- df0
    for(i in seq_along(tar.list)){
      x <- tar.list[[i]]
      df1 <- df1 %>%
        ip_scale_a(target_series = names(x)[!(names(x) %in% c("value"))], series_target = paste0("tar", i))
    }

    ###
    #Error
    ###

    #For each target (except final one), calculate the error over each element in the target,
    # Save results to a list of dfs
    err.list <- lapply(seq_along(head(tar.list, -1)), function(i){
      x <- tar.list[[i]]
      df1 %>%
        ip_miss_a(names(x)[!(names(x) %in% c("value"))], series_target = paste0("tar", i))
    })

    #For each error df, sum the error column, then sum those sums for total abs error
    current.error <- sum(sapply(err.list, function(x){
      return(sum(abs(x$error), na.rm=T))
    }), na.rm=T)

    current.iteration <- current.iteration + 1

    df0 <- df1

    print(paste("Error: ", round(current.error, 4)))
  }

  #Freeze 1 - Iced Cells
  #Add back Iced Cells
  if(!is.null(ice_cells)){
    df0 <- df0 %>%
      mutate(value = ifelse(is.na(ice_c), value, ice_c))
  }

  return(df0)

}
