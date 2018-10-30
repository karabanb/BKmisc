
discretize_rpart <- function(x, y, maxdepth = 2, minbucket = 10){

  results <- list()
  trees_results <- list()

  for (i in base::setdiff(colnames(x),y)) {
    trees_results[[i]] <- rpart::rpart(paste0(y, " ~", i),
                                       data = x,
                                       control = rpart.control(minbucket = minbucket, cp = 0.001, maxdepth = maxdepth))
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

  return(results)

}
