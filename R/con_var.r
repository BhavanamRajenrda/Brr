#' Data Frame of Numeric Variable
#
#' This is an example function named 'con_var'
#' which extracts numeric column of a data frame and then create a new data frame.
#' The data frame can be given any name using name attribute, by default the name is "Numeric_VAR".
#' @param x a data frame
#' @return a dataframe of numeric column
#' @export

con_var <- function(x) {
  cn <- 0
  z <- data.frame()
  for (i in seq_along(x)) {
    if (class(x[, i]) == "numeric") {
      cn <- trimws(sub("0,", "", paste(cn, colnames(x)[i],
                                       sep = ",")))
    }
  }
  cn <- as.vector(strsplit(cn, ","))
  for (k in cn) {
    z <- x[, (colnames(x) %in% k)]
  }
  return(z)
}
