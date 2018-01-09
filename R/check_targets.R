#' Check the targets and seed for compatibility before running \code{ip_fit()}.
#'
#' In IPFs, common subtotals in each target must be equal, otherwise the IPF will not converge on a single result.
#' This function checks the common dimensions over each target combination for compatibility.
#' Further, this function checks the IPF seed, if provided, for compabitibilty with the targets.
#' If the seed, when summed over each dimension in a given target, contains any missing values or zero values, the target must also have a zero or missing value.
#'
#' @param targets A list of data frames containing subtotal targets. All totals in each target should be equal.
#' Series supplied in each target will shape the final data frame.
#' @param seed An optional data frame of seed values containing all final dimensions expected as output of \code{ip_fit()}.
#' Leave as \code{NULL} if the targets will be run using \code{ip_expand()} or will be used with a generic seed.
#' @param target.value.names The names of the series in \code{targets} containing subtotals to scale. Can be string or array of strings. Defaults to "value".
#' @param seed.value.name The name of the series in \code{seed} containing values. Defaults to "value".
#' @param max.error The maximum total absolute difference allowed between final scaled values and targets.
#' @return Messages in the console and an output data frame listing any incompitable line items in the seed and targets.
#' @examples
#' tar1 <- data.frame(x = letters[1:2], value = c(50, 50))
#' tar2 <- data.frame(y = letters[3:5], value = c(20, 40, 40))
#' tar3a <- data.frame(z = letters[6:10], value = c(15, 20, 25, 30, 10))
#'
#' tar3b <- data.frame(x = c(rep(letters[1], 5), rep(letters[2], 5)),
#'                    z = rep(letters[6:10], 2),
#'                    value = c(5, 10, 15, 20, 0, 10, 10, 10, 10, 10))
#'
#' tar.list.a <- list(tar1, tar2, tar3a)
#' tar.list.b <- list(tar1, tar2, tar3b)
#' check_targets(tar.list)
#' @export
check_targets <- function(targets, seed = NULL,
                          target.value.names = "value", seed.value.name = "value",
                          max.error = 0.01) {

  #Warnings
  if(is.null(targets) | !is.list(targets) | !is.data.frame(targets[[1]])) {
    stop("Targets must be a list of data frames.")
  }

  #Initialize
  if(length(targets) == 1 & is.null(seed)) {
    stop("Check_targets() looks for compatibility issues between targets and the seed. If only one target is supplied, you must also supply a seed.
         Otherwise, no need to check. :)")
  } else if(length(targets) == 1) {
    message("Beginning check... one target and seed provided")
  } else {
    message(paste0("Beginning check... ", length(targets), " targets provided",
                   if(is.null(seed)){"."} else {", as well as a seed."}))
  }

  ###
  # Check the targets against each other ----
  ###
  num.tars <- length(targets)
  tar.list <- targets

  names(tar.list) <- paste0("Tar", 1:num.tars)

  tar.combo <- data.frame(TarA = c(), TarB = c(), stringsAsFactors = FALSE)
  for(i in num.tars:2){
    for(j in (i-1):1){
      tar.combo <- tar.combo %>% bind_rows(
        data.frame(TarA = paste0("Tar", j), TarB = paste0("Tar", i), stringsAsFactors = FALSE)
      )
    }
  }
  tar.combo <- tar.combo %>%
    arrange(TarA, TarB)

  combine_tars_a <- function(tara, tarb, list_of_tars){
    TarA <- list_of_tars[[tara]]
    TarB <- list_of_tars[[tarb]]

    common.dims <- names(TarA)[names(TarA) %in% names(TarB)]
    common.dims <- common.dims[!(common.dims %in% target.value.names)]

    names(TarA)[names(TarA) %in% target.value.names] <- ".value"
    names(TarB)[names(TarB) %in% target.value.names] <- ".value"

    combo.tar <- TarA %>%
      mutate(.dftype = "Checker") %>% #This cleans up joins for targets with no common series
      group_by_(.dots = as.list(c(".dftype", common.dims))) %>%
      summarize(TarA = sum(.value, na.rm=TRUE)) %>%
      ungroup() %>%
      left_join(
        TarB %>%
          mutate(.dftype = "Checker")  %>%
          group_by_(.dots = as.list(c(".dftype", common.dims))) %>%
          summarize(TarB = sum(.value, na.rm=TRUE)) %>%
          ungroup() ,
        by = c(".dftype", common.dims)
      ) %>%
      mutate(Check_value = (TarA - TarB),
             Check_trigger = abs(Check_value) > max.error) %>%
      select(-.dftype)

    names(combo.tar)[names(combo.tar) == "TarA"] <- tara
    names(combo.tar)[names(combo.tar) == "TarB"] <- tarb

    return(combo.tar)
  }

  target.checks <- purrr::pmap(list(a = tar.combo$TarA, b = tar.combo$TarB),
                                  function(a, b){combine_tars_a(a, b, tar.list)})

  #Only keep dfs with violations
  target.checks.op <- target.checks[purrr::map_lgl(target.checks, function(x){any(x$Check_trigger)})]

  ####
  # Check the targets against the seed ----
  ####

  if(is.null(seed)){ seed.checks.op <- "No seed provided"} else {

    check_seed_a <- function(TarA, SeedA){
      dims.in.tar <- names(TarA)
      dims.in.tar <- dims.in.tar[!(dims.in.tar %in% c(target.value.names))]

      names(SeedA)[names(SeedA) == seed.value.name] <- ".value"

      seed.collapse <- SeedA %>%
        group_by_(.dots = as.list(dims.in.tar)) %>%
        summarize(.seed = sum(.value, na.rm = TRUE)) %>%
        ungroup()

    }

  } #End seed check

}
