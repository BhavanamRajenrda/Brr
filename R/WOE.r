#' Data Frame with Weight Of Evidence values from Information package
#'
#' This is an function name 'DF.Replace.WOE', which will extract the values of WOE from Information
#' list which can be generated using information package and replace the original values with the
#' WOE values.
#' @param X is a data sets which is used to create the Information table using information package.
#' @param y is the Information table which is created using information package.
#' @param Dependent is the dependent binary variable which is used to classify good and bad.
#' @return It will return the data frame with WOE values
#' @export

DF.Replace.WOE <-
  function(X,
           y,
           Dependent = NULL) {
    z <- 0
    cz <- 0
    D <- X[, Dependent]
    x <- X[,-which(names(X) == Dependent)]
    if (class(y) == "Information") {
      for (i in 1:ncol(x)) {
        if (class(x[, i]) == "factor") {
          for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
            x[, i] <- as.character(x[, i])
            if (is.na(y[[1]][i][[1]][1][[1]][j])) {
              x[[i]][which(is.na(x[, i]))] <-
                y[[1]][i][[1]][4][[1]][which(is.na(y[[1]][i][[1]][1][[1]]))]
            }
            else {
              x[[i]][which(x[, i] == y[[1]][i][[1]][1][[1]][j])] <-
                y[[1]][i][[1]][4][[1]][j]
            }
          }
        }
        else {
          for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
            cz <-
              as.vector(strsplit(gsub(
                "[]]", "", gsub("[[]", "", y[[1]][i][[1]][1][[1]])
              ), ","))
            if (is.na(y[[1]][i][[1]][1][[1]][j])) {
              x[[i]][which(is.na(x[, i]))] <-
                y[[1]][i][[1]][4][[1]][which(is.na(y[[1]][i][[1]][1][[1]]))]
            }
            else {
              x[[i]][which(x[, i] >= cz[[j]][1] & x[, i] <= cz[[j]][2])] <-
                y[[1]][i][[1]][4][[1]][j]
            }
          }
        }
      }
      z <- cbind(x, D)
      colnames(z)[which(names(X) == Dependent)] <- Dependent
    }
    return(z)
  }
