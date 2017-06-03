#' Kolmogorov-Smirnov test FKA ks-test function
#'
#' This function will calculate the KS value of the given propability against the true label and also provide the decile under which it falls
#' @param True_Label this the actual target variable of the data
#' @param Prob this the predicted probability of the target variable using some model
#' @param Plot this is a logical value which tell to plot the ks graph in the console
#' @return this function will return a list of values like the ks table, decile and the ks ststistics
#' @import graphics
#' @import stats
#' @export

df.ks_stats <-
  function(True_Label = NULL,
           Prob = NULL,
           Plot = TRUE) {
    if (is.null(True_Label)) {
      stop("ERROR: true label is not provided")
    }
    if (is.null(Prob)) {
      stop("ERROR: predicted probability cannot be null")
    }
    if (is.null(Plot)) {
      stop("ERROR: Plot must be logical")
    }
    if (is.factor(True_Label) == FALSE) {
      stop("ERROR: True_Label must be of factor class")
    }
    if (is.double(Prob) == FALSE) {
      stop("ERROR: predicted probability must be in percentage of decimal")
    }
    if (is.logical(Plot) == FALSE) {
      stop("ERROR: Plot must be logical")
    }
    n <- length(Prob) #count of rows
    x <-
      cbind.data.frame(True_Label, Prob) #combining the true lable and predicted probability into a data frame
    x <-
      x[order(-x[, 2]),] #sorting the data frame with the predicted probability
    x[, 3] <-
      (1:n / (n / 10)) + 1 #creating a column with the decile
    x[, 3] <-
      as.integer(ifelse(x[, 3] > 10, 10, x[, 3])) #restricting the highest decile value to 10
    x[, 4] <-
      ifelse(x[, 1] == 0, 1, 0) #creating a new column with the has 1 for for every good observation if 0 is good
    x[, 5] <-
      ifelse(x[, 1] == 1, 1, 0) #creating a new column with the has 1 for for every bad observation if 1 is bad
    y <-
      aggregate.data.frame(x[, c(4, 5)], by = list(x[, 3]), FUN = sum) #we will use the decile to aggregate base on good and bad
    y[, 4] <-
      (100 * (y[, 2] / sum(y[, 2]))) #calculation the % of good in a particular decile
    y[, 5] <-
      (100 * (y[, 3] / sum(y[, 3]))) #calculation the % of bad in a particular decile
    y[, 6] <- cumsum(y[, 4]) #calculation the cumulative % of good
    y[, 7] <- cumsum(y[, 5]) #calculation the cumulative % of bad
    y[, 8] <-
      abs(y[, 6] - y[, 7]) #calculating the cumulative % of good - cumulative % of bad
    ks <-
      max(y[, 8]) #now the ks statistits will be the maximum difference between cum % of good and cum % of bad
    d <-
      y[which(y[, 8] == ks), 1] #calculating the decile in which max ks statistics falls
    #below is the condition to plot the ks statistics
    if (Plot == TRUE) {
      plot(
        c(1, min(c(y[, 6], y[, 7]))),
        c(10, 100),
        type = "n",
        xlab = "Decile",
        ylab = "Cumulative_%"
      )
      lines(y[, 1], y[, 6], col = "darkgreen", lwd = 2)  #cumalative % of good
      lines(y[, 1], y[, 7], col = 2, lwd = 2) #cumalative % of bad
      lines(y[, 1], y[, 8], col = 4, lwd = 2) #difference of cum % of good and bad
      legend(
        1,
        100,
        col = c("darkgreen", 2, 4),
        lwd = c(2, 2, 2, 2),
        c("cum_%_of_good", "cum_%_of_bad", "ks_statistics")
      )
    }
    colnames(y) <-
      c(
        "Decile",
        "Count_of_Good",
        "Count_of_Bad",
        "%_of_Good",
        "%_of_Bad",
        "Cum_%_of_Good",
        "Cum_%_of_Bad",
        "KS_statistics"
      )
    return(list(
      KS_statistics = ks,
      KS_Table = y,
      Decile = d
    ))
  }
