#' Create Binning variable from  Information Table
#'
#' The Function DF.Replace.Bin will create a data frame of binning variables as seen in Information table,
#' it takes the argument dataframe, information table and dependent variable and then create seperate df with all
#' the binning variables.
#' @param X it's a data frame that is used to create the Information table from Information package.
#' @param y it's a information table that is created by information package.
#' @param Dependent it's a dependent variable that is used to classify good or bad.
#' @return it will return a data frame with binning variables from information table.
#' @export

DF.Replace.Bin <-
  function(X,
           y,
           Dependent = NULL) {
    z <- 0
    cz <- 0
    D <- X[, Dependent]
    x <- X[,-which(names(X) == Dependent)]
    cn <- names(x)
    if (class(y) == "Information") {
      for (i in 1:ncol(x)) {
        if (class(x[, i]) == "factor") {
          for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
            x[, i] <- as.character(x[, i])
            if (is.na(y[[1]][i][[1]][1][[1]][j])) {
              x[which(is.na(x[, i])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][1][[1]][which(is.na(y[[1]][i][[1]][1][[1]]))]
            }
            else {
              x[which(x[, i] == y[[1]][i][[1]][1][[1]][j]), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][1][[1]][j]
            }
          }
        }
        else {
          for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
            cz <-
              as.vector(strsplit(gsub(
                "[]]", "", gsub("[[]", "", y[[1]][i][[1]][1][[1]])
              ), ","))
            if (y[[1]][i][[1]][1][[1]][j] == "NA") {
              x[which(is.na(x[, i])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][1][[1]][which(y[[1]][i][[1]][1][[1]][j] == "NA")]
            }
            else {
              x[which(x[, i] >= as.double(cz[[j]][1]) &
                        x[, i] <= as.double(cz[[j]][2])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][1][[1]][j]
            }
          }
        }
      }
    }
    z <- cbind(x, D)
    colnames(z)[which(names(z) == "D")] <- Dependent
    z <- z[, -which(names(x) == cn)]
    return(z)
  }
