
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
#'    Any rows or values not listed, or \code{NA}s, will be scaled as normal.
#' @param ice_cells.value.name The name of the series of iced values in \code{ice_cells}.
#' @param ice_slice Optional list of data frames containing subtotal targets for the \code{datatable}.
#'    Unlike \code{targets}, these data frames can be subsets only containing subtotals for one or more rows.
#'    Any rows or values not listed, or \code{NA}s, will be scaled as normal.
#'    Using \code{ice_slice} for partial targets will increase the number of required iterations for scaling. This may require the user to increase the value of \code{max.iterations}.
#' @param ice_slice.value.name The name or names of the series of iced values in \code{ice_slice}.
#' @param slush_cells Optional data frame of values with same series columns as \code{datatable}, specifying bounded values to hit in the scaling.
#'    Provide minimumn and maximum values for a cell to be scaled.
#'    Any rows or values not listed, or \code{NA}s, will be scaled as normal.
#' @param slush_cells.value.names An array of length 2 of the names of the minimum and maximum values in  \code{slush_cells}.
#' @param sluch.inbounds.param Numeric value of  0 < x < 1. Following an out-of-bounds occurence for \code{slush_cells}, the \code{slush.inbounds.parm} is the additional value added to the scaled value to bring it back into bounds.
#' Values close to 0 bind the value to the violated bound, while close to 1 bind the value to the other bound.
#' Values closer to0 will require more iterations to complete.
#' @return A dataframe with the same dimensionality as \code{datatable}, with all values scaled to the subtotals specified in each data frame in \code{targets}.
#' @examples
#' tar1 <- data.frame(x = letters[1:2], value = c(50, 50))
#' tar2 <- data.frame(y = letters[3:5], value = c(20, 40, 40))
#' tar3 <- data.frame(z = letters[6:10], value = c(10, 20, 30, 40, 10))
#'
#' tar.list <- list(tar1, tar2, tar3)
#' df <- ip_create_seed(tar.list) %>% ip_fit(tar.list)

ip_fit <- function(datatable, targets,
                   datatable.value.name = "value", target.value.names = "value",
                   max.error = 0.01, max.iterations = 25,
                   ice_cells = NULL, ice_cells.value.name = "value",
                   ice_slice = NULL, ice_slice.value.names = "value",
                   slush_cells = NULL, slush_cells.value.names = c("value_min", "value_max"),
                   slush.inbounds.parm = 1/3,
                   save.tars = TRUE) {

  #Warnings
  if(is.null(targets) | !is.list(targets) | length(targets) == 1) {stop("Targets must be a list of at least two data frames")}

  #Set initial conditions
  current.error     <- 10^9
  current.iteration <- 1
  slush_cells.oob <- TRUE

  #Freeze 2 - Ice Slices
  if(!is.null(ice_slice)){
    if(!is.list(ice_slice) | !is.data.frame(ice_slice[[1]])) {stop("Parameter ice_slice must be a list of data frames containing partial targets.")}
    tar.list <- c(targets, ice_slice)
  } else{
    tar.list <- targets
  }
  df0 <- datatable
  names(df0)[names(df0) == datatable.value.name] <- "value"

  #Freeze 1 - Ice Cells
  if(!is.null(ice_cells)){
    print(names(ice_cells))
    names(ice_cells)[names(ice_cells) == ice_cells.value.name] <- "ice__c"

    df0 <- df0 %>%
      left_join(ice_cells) %>%
      #Iced cells are not unknown, so we don't need to include them in the IPF
      mutate(value = ifelse(is.na(ice__c), value, 0))

    #Since we 0'd out the seed, we should also remove all the iced values from the targets
    tar.list <- lapply(tar.list, function(x){
      names(x)[names(x) %in% c(target.value.names, ice_slice.value.names)] <- "value"

      ice_target <- ice_cells %>%
        group_by_(.dots = as.list(names(x)[!(names(x) == "value")])) %>%
        summarize(iced = sum(ice__c, na.rm=T)) %>%
        ungroup()

      df <- x %>%
        left_join(ice_target) %>%
        mutate(iced = ifelse(is.na(iced), 0, iced)) %>%
        mutate(value = value - iced) %>%
        select(-iced)

      if(any(df$value < 0)){stop("Iced cell values exceed supplied targets. Unable to rationalize.")}

      return(df)
    })
  } #End ice cells

  #Freeze 3 - Slush Cells. Similar to Ice, but cells have a min/max bound, not specific value
  if(!is.null(slush_cells)) {
    names(slush_cells)[names(slush_cells) == slush_cells.value.names[1]] <- "slush__c_min"
    names(slush_cells)[names(slush_cells) == slush_cells.value.names[2]] <- "slush__c_max"

    df0 <- df0 %>%
      left_join(slush_cells)
  }

  #Format targets - give each value unique names
  for( i in seq_along(tar.list)){
    x <- tar.list[[i]]

    names(x)[names(x) == "value"] <- paste0("tar__", i)
    names(tar.list[[i]]) <- names(x)
  }

  #Format input seed
  for(x in tar.list){
    df0 <- df0 %>%
      left_join(x)
  }

  while((current.error > max.error | slush_cells.oob == TRUE) & current.iteration <= max.iterations ) {
    print(paste("Iteration", current.iteration))

    #Reset Freeze 3 - Slush Cells - to off
    slush_cells.oob <- FALSE

    ##
    # Scaling
    ##

    df1 <- df0
    for(i in seq_along(tar.list)){
      x <- tar.list[[i]]
      df1 <- df1 %>%
        ip_scale_a(target_series = names(x)[!(names(x) %in% c("value"))], series_target = paste0("tar__", i))
    }

    ###
    #Error
    ###

    #For each target (except final one), calculate the error over each element in the target,
    # Save results to a list of dfs
    err.list <- lapply(seq_along(head(tar.list, -1)), function(i){
      x <- tar.list[[i]]
      df1 %>%
        ip_miss_a(names(x)[!(names(x) %in% c("value"))], series_target = paste0("tar__", i))
    })

    #For each error df, sum the error column, then sum those sums for total abs error
    current.error <- sum(sapply(err.list, function(x){
      return(sum(abs(x$error), na.rm=T))
    }), na.rm=T)

    ###
    # Freeze 3 - Slush Cells
    ###
    #Similar to Ice, but cells have a min/max bound, not specific value
    #Calculate AFTER error, as this is its own deal breaker. If everything is in bounds, no problem
    if(!is.null(slush_cells)) {

      df1 <- df1 %>%
        mutate(check_slush_c = (value < slush__c_min) | (value > slush__c_max))  #Look for OOB

      #If any of the bounded cells are OOB, first update the trigger to True
      slush_cells.oob <- any(df1$check_slush_c, na.rm = T)
      df1$check_slush_c <- NULL

      #Then replace those values with 1/3 closer above/below that missed bound
      if(slush_cells.oob == TRUE) {
        print("Out of bounds conditions present for slush_cells.")
        df1 <- df1 %>%
          mutate(value = ifelse((value >= slush__c_min) | is.na(slush__c_min), value, slush__c_min + (slush.inbounds.parm)*(slush__c_max - slush__c_min)),
                 value = ifelse((value <= slush__c_max) | is.na(slush__c_max), value, slush__c_max - (slush.inbounds.parm)*(slush__c_max - slush__c_min))
          )
      }

    }

    ###
    # Prepare for next loop
    ###

    current.iteration <- current.iteration + 1

    df0 <- df1

    print(paste("Error: ", round(current.error, 4)))
  }

  #Freeze 1 - Iced Cells
  #Add back Iced Cells
  if(!is.null(ice_cells)){
    df0 <- df0 %>%
      mutate(value = ifelse(is.na(ice__c), value, ice__c))
  }

  #Save/print all the assumption columns?
  # Currently, tars are reduced by Freeze tars.
  # TODO: Add frozen values back into tars... or just replace tars. That's easier
  if(!save.tars){
    df0 <- df0 %>%
      select(-starts_with("tar__"), -starts_with("ice__"), -starts_with("slush__"))
  }

  return(df0)

}
