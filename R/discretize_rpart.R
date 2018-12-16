
#' Dicretizing Continous Values by Decision Tree
#'
#' @param x - data.frame with continues variables
#' @param y - Target binary variable
#' @param maxdepth - parameter from rpart::rpart.control. It sets number of expected bins.
#' @param minbucket - parameter from rpart::rpart.control. It sets minumum size of bin.
#'
#' @return
#' @export
#'
#' @examples
#'
discretize_rpart <- function(x, y, maxdepth = 2, minbucket = 10){

  results <- list()
  trees_results <- list()

  # selecting vairables without target, character and factors

  proper_vars <- base::setdiff(colnames(x), y)
  proper_vars <- proper_vars[sapply(x[, proper_vars],
                                    function(x){!is.factor(x) & !is.character(x) & length(unique(x)) > 1})]

  # chcecking initials conditions

  if(length(unique(x[, y])) != 2){
    stop("Target variable must has 2 levels !")
  }

  if(class(x[, y]) %in% c("numeric", "logical", "integer")){
    x[, y] <- as.factor(x[, y])
  }

  # discretizing by rpart trees

  for (i in proper_vars) {
    trees_results[[i]] <- rpart::rpart(paste0(y, " ~", i),
                                       data = x,
                                       control = rpart::rpart.control(minbucket = minbucket,
                                                                      cp = 0.0001,
                                                                      maxdepth = maxdepth)
                                       )
  }

  results$trees_results <- trees_results

  nodes_df <- data.frame(variable = character(),
                         purity = double(),
                         n = numeric(),
                         stringsAsFactors = FALSE
  )

  for (i in 1:length(trees_results)) {
    nodes_df[i, "variable"] <- names(trees_results[i])
    ix <- which.max(trees_results[[i]]$frame$yval2[, 4])
    nodes_df[i, "purity"] <- trees_results[[i]]$frame$yval2[ix, 4]
    nodes_df[i, "n"] <- trees_results[[i]]$frame[ix, 2]
  }

  results$nodes_df <- nodes_df

  discretized_df <- x

  for (i in names(results$trees_results)) {
    discretized_df[ ,i] <- cut_by_tree(results$trees_results[[i]], x ,i)
  }

  results$discretized_df <- discretized_df

  return(results)

}
