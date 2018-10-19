
#' Select variables which contain at least two unique values by \code{data.table} way
#'
#' @param x data.frame or matrix - data for cleaning
#'
#' @return This function return a \code{data.frame} or \code{matrix} which contains columns with at leat two uniques
#' values.
#' @export
#'
#' @import data.table
#'
#' @examples
#' df <- data.frame(v1 = 1:4, v2 = 1, v3 = LETTERS[1:4], v4 = c(1, 1, NA, NA))
#' df_unq <- select_unique(df)
#' df; df_unq
#'
#' @author Bartlomiej Karaban

select_unique_dt <- function(x) {

  if (class(x) == 'list' | is.vector(x) == TRUE){
    stop('x must be a matrix or data.frame class object')
  }

  x <- data.table::as.data.table(x)
  cols <- which(sapply(x, function(x){length(unique(x)) > 1}))
  x[, cols, with = FALSE]
}


