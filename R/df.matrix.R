#' Matrix of a Factor Column
#
#' This is an example function named 'df.matrix'
#' which convert a factor columns levels(k-1) to different columns with value 0 and 1.
# Then save the dataframe in to new dummy dataframe withour the initial factor column.
#' @param x a data frame to be converted into matrix
#' @param name the name of the new dummy dataframe
#' @param sep the delimeter to be used for seperating column and levels
#' @return the matrix dataframe of factor column
#' @export

df.matrix <-
  function(x, name = "dummy", sep = ":") {
    cn <- 0
    y <- data.frame()
    for (i in 1:ncol(x)) {
      if (class(x[, i]) == "factor") {
        cn <- trimws(sub("0,", "", paste(cn, colnames(x)[i], sep = ",")))
        for (j in 1:(nlevels(x[, i]) - 1)) {
          x[paste(colnames(x)[i], levels(x[, i])[j], sep = sep)] <-
            ifelse(x[, i] == levels(x[, i])[j], 1, 0)
        }
      }
    }
    cn <- as.vector(strsplit(cn, ","))
    for (k in cn) {
      y <- x[,!(colnames(x) %in% k)]
    }
    assign(paste(name),
           y,
           environment(df.matrix))
  }
