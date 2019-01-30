#' Group by all variables except those listed
#'
#' Opposite of dplyr::group_by()... for when it's more convenient to list the excluded variables
#'
#' @param .data a tbl
#' @param ... Variables NOT to group by. Duplicated groups will be silently dropped.
#' @return A grouped tbl
#' @export

group_by_not <- function(.data, ...){
  dont_group <- gsub("~", "", as.character(rlang::quos(...)))

  .data %>%
    dplyr::group_by_(.dots = names(.data)[!(names(.data) %in% dont_group)])
}
