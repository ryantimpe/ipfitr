#' A scale the sum or product of a vector to another value
#'
#' @param input_array Vector of values to be scaled.
#' @param target Desired result for the sum or product of the scaled result.
#' @param method 'sum' to scale the sum of the totals, or 'product'.
#' @param reduce_target Logical. If \code{target} has more than one value, reduce it using the \code{method}
#' @return An array of same length as \code{input_array}
#' @examples
#' ia_scale(1:5, 20)
#'
#' ia_scale(1:5, 20, method = "product")
#' @export
#'
ia_scale <- function(input_array, target,
                    method = "sum", reduce_target = FALSE){

  if(!(method %in% c("sum", "product"))){
    stop("method must either be 'sum' or 'product'")
  }

  #Check input sizes
  if(length(target) > 1 & !reduce_target){
    warning("Length of `target` is greater than 1. Only first value will be used. Set reduce_target = TRUE to sum over target.")
    tar <- as.numeric(target[1])
  } else if(length(target) > 1 & reduce_target){
    tar <- case_when(
      method == "sum" ~ sum(target, na.rm=TRUE),
      method == "product" ~ prod(target, na.rm=TRUE)
    )
  } else {
    tar <- target
  }

  scale_length <- length(input_array)

  scale_fct <- case_when(
    method == "sum" ~ (tar / sum(input_array)),
    method == "product" ~ (tar^(1/scale_length)) / (prod(input_array)^(1/scale_length))
  )

  return(scale_fct * input_array)

}
