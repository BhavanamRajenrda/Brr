#' Factor of Character Column
#
#' This is an example function named 'df.factor'
#' which convert a character column to factor.
#' @param x the dataframe
#' @return the dataframe character column now converted into factor
#' @export


df.factor <-
  function(x) {
    for (i in 1:ncol(x)) {
      if (typeof(x[, i]) == "character") {
        x[, i] <- as.factor(x[, i])
      }
    }
    return(x)
  }
