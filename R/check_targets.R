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
#' @return Messages in the console and an output list of  data frames containing any incompitable line items in the seed and targets.
#' @examples
#' tar1 <- data.frame(x = letters[1:2], value = c(50, 50))
#' tar2 <- data.frame(y = letters[3:5], value = c(20, 40, 40))
#' tar3 <- data.frame(x = c(rep(letters[1], 5), rep(letters[2], 5)),
#'                    z = rep(letters[6:10], 2),
#'                    value = c(5, 10, 15, 20, 0, 10, 10, 10, 10, 10))
#'
#' tar.list <- list(tar1, tar2, tar3)
#' check_targets(tar.list)
#'
#' #This will find errors with the targets
#' tar3b <- data.frame(x = c(rep(letters[1], 5), rep(letters[2], 5)),
#'                    z = rep(letters[6:10], 2),
#'                    value = c(0, 10, 15, 20, 0, 10, 10, 10, 10, 10))
#'
#' tar.list <- list(tar1, tar2, tar3b)
#' seed <- ipfitr::ip_create_seed(tar.list)
#' check_targets(tar.list, seed)
#'
#' #This will find errors with the targets and the seed
#' seed2 <- seed %>% mutate(value = ifelse(x == "a" & z == "g", 0, value))
#' check_targets(tar.list, seed2)
#'
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

  if(length(tar.list) <= 1){
    target.checks.op <- list()
  } else {
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
               Check_trigger = abs(Check_value) > max.error,
               Check_trigger = ifelse(is.na(Check_trigger), FALSE, Check_trigger)) %>%
        select(-.dftype)

      names(combo.tar)[names(combo.tar) == "TarA"] <- tara
      names(combo.tar)[names(combo.tar) == "TarB"] <- tarb

      return(combo.tar)
    }

    target.checks <- purrr::pmap(list(a = tar.combo$TarA, b = tar.combo$TarB),
                                 function(a, b){combine_tars_a(a, b, tar.list)})

    target.checks.names <- tar.combo %>% mutate(.name = paste(TarA, " & ", TarB)) %>% pull
    names(target.checks) <- target.checks.names

    #Only keep dfs with violations
    target.checks.op <- target.checks[purrr::map_lgl(target.checks, function(x){any(x$Check_trigger, na.rm=TRUE)})]

    if(length(target.checks.op) == 0) {
      message("\nTargets are good! No issues here.\n===================================")
    } else {
      message("\nAt least one violation has been found within the targets. See output.\n===================================")
    }
  } #End target check


  ####
  # Check the targets against the seed ----
  ####

  if(is.null(seed)){ seed.checks.op <- list()} else {

    message("Checking each target against the seed... This will look for 0 or NA values over seed subtotals and compare them with the targets.\nIf the seed has a 0 subtotal, then the matching target should also be 0 (IPF cannot scale zero to a non-zero).")

    check_seed_a <- function(TarA, SeedA){
      dims.in.tar <- names(TarA)
      dims.in.tar <- dims.in.tar[!(dims.in.tar %in% c(target.value.names))]

      names(SeedA)[names(SeedA) == seed.value.name] <- ".value"
      names(TarA)[names(TarA) %in% target.value.names] <- ".target"

      seed.collapse <- SeedA %>%
        mutate_if(is.factor, as.character) %>%
        group_by_(.dots = as.list(dims.in.tar)) %>%
        summarize(.seed = sum(.value, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(TarA %>% mutate_if(is.factor, as.character), by = dims.in.tar) %>%
        mutate(Check_trigger = (.seed == 0 | is.na(.seed)) & (.target > 0 | !is.na(.target)))

    }

    seed.checks <- purrr::map(tar.list, function(x){check_seed_a(x, seed)})

    names(seed.checks) <- paste(names(seed.checks), "& Seed")

    #Only keep dfs with violations
    seed.checks.op <- seed.checks[purrr::map_lgl(seed.checks, function(x){any(x$Check_trigger)})]

    #Output message
    if(length(seed.checks.op) == 0){
      message("\nThe seed and targets line up! No issues here.\n===================================")
    } else {
      message("\n!!! Zero subtotals found in seed where targets have values. IPF will not converge. See output.\n===================================")
    }

  } #End seed check

  check.op <- purrr::map(c(target.checks.op, seed.checks.op), function(x){
    x %>% filter(Check_trigger) %>% select(-Check_trigger)
  })

  return(check.op)

}
