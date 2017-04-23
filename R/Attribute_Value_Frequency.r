#' The Attribute Value Frequency of Categorical variable
#'
#' This is an example function name df.AVF'
#' The function will calculate the frequency of each unique variable in every column over the row
#' and sum the total number of count for each unique value in row which then will be divided by the
#' total number of categorical variable
#' @param x a data frame to be which have both numerical and categorical variable
#' @return it returns a data frame with avf score for outliers detection
#' @export

df.AVF <- function(x) {
  y <- 0
  for (i in seq_along(x)) {
    if (class(x[, i]) == "factor") {
      y <- y + length(x[, i])
    }
  }
  cn <- 0
  z <- data.frame()
  for (i in seq_along(x)) {
    if (class(x[, i]) == "factor") {
      cn <- trimws(sub("0,", "", paste(cn, colnames(x)[i], sep = ",")))
    }
  }
  cn <- as.vector(strsplit(cn, ","))
  for (k in cn) {
    z <- x[, (colnames(x) %in% k)]
  }
  freq_matrix <- table(unlist(unname(z)))
  z[, "AVF_Score"] <-
    apply(z, 1, function(x) {
      sum(freq_matrix[x]) / y
    })
  for (l in cn) {
    d <- x[, !(colnames(x) %in% l)]
  }
  w <- cbind(d, z)
  return(w)
}
