#' Check the targets and seed for compatibility before running \code{ip_fit()}.
#'
#'
#'
#' Wrapper function to create seed and perform iterative proportion fitting from N targets.
#' Combines the \code{ip_create_seed()} and \code{ip_fit()} functions into a single step. Creates a generic unary seed.
#'
#' @param targets A list of data frames containing subtotal targets. All totals in each target should be equal.
#' Series supplied in each target will shape the final data frame.
#' @param seed An optional data frame of seed values containing all final dimensions expected as output of \code{ip_fit()}.
#' Leave as \code{NULL} if the targets will be run using \code{ip_expand()} or will be used with a generic seed.
#' @param target.value.names The names of the series in \code{targets} containing subtotals to scale. Can be string or array of strings.
#' @param max.error The maximum total absolute difference allowed between final scaled values and targets.
#' @return Messages in the console and an output data frame listing any incompitable line items in the seed and targets.
#' @examples
#' tar1 <- data.frame(x = letters[1:2], value = c(50, 50))
#' tar2 <- data.frame(y = letters[3:5], value = c(20, 40, 40))
#' tar3 <- data.frame(z = letters[6:10], value = c(10, 20, 30, 40, 10))
#'
#' tar.list <- list(tar1, tar2, tar3)
#' df <- ip_expand(tar.list)
#' @export
check_targets <- function(targets, seed = NULL,
                          target.value.names = "value",
                          max.error = 0.01, show.messages = TRUE) {

  #Warnings
  if(is.null(targets) | !is.list(targets) | !is.data.frame(targets[[1]])) {
    stop("Targets must be a list of data frames.")
  }

  #Single Target but no seed
  if(length(targets) == 1 & is.null(seed)) {
    stop("Check_targets() looks for compatibility issues between targets and the seed. If only one target is supplied, you must also supply a seed.
         Otherwise, no need to check.")
  } else {
    message(paste("Initializing IPF...", length(targets), "targets supplied."))
  }

  #Check targets
  target.checker <- vector(length = length(targets)) #Use Targets, not tar.list
  for(i in seq_along(targets)){
    x <- targets[[i]]
    target.checker[i] <- sum(x[, target.value.names], na.rm=T)
  }
  if(length(unique(round(target.checker, 4))) > 1){
    message("Warning: Supplied targets do not have the same totals. IPF will not converge.")
  }



}
