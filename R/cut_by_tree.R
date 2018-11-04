
#' Title
#'
#' @param tree
#' @param x
#' @param variable
#'
#' @return
#'
#' @examples
#'
#'
cut_by_tree <- function(tree, x, variable) {

  trees_splits <- tree$splits[, 4]
  cut_points <- c(-Inf, trees_splits, Inf)

  cuted_variable <- cut(x[, variable], breaks = cut_points, include.lowest = TRUE)

}
