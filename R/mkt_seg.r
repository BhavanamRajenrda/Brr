#' Segmentation of Market based on Profit
#'
#' This function is created to calculate the top market segments base on minumum covariance of the profit made on each segments
#' @param x it is the data frame to calculate the market segments
#' @param c it is the column such as sales, quantity, profit etc which is required to aggregate based on date
#' @param d it is the date column for time series
#' @param m it is the no of months that needs to be seperated for validation purpose
#' @param v it is the profit column or can be any column based on which covariance is to be calculated
#' @param n it is the top n segments that is require for predicting sales and quantity for that segments
#' @return this will return a list of top segments, the train and test time series
#' @import stats
#' @export


imp_seg <-
  function(x = "Dataframe",
           c = "aggregate_column",
           d = "Date_col",
           m = "no of month to forecast",
           v = "Profit column for covariance calculation",
           n = "Top n profitable segments") {
    if (is.null(x)) {
      stop("ERROR: no dataframe provided")
    }
    if (is.null(c)) {
      stop("ERROR: no aggregate column provided")
    }
    if (is.null(d)) {
      stop("ERROR: no date column provided")
    }
    if (is.null(m)) {
      stop("ERROR: months to forecast not provided")
    }
    if (is.null(v)) {
      stop("ERROR: column name on which covariance is to be calculated is not provided")
    }
    if (is.null(n)) {
      stop("ERROR: n top segments based on minimum covariance is not provided")
    }
    if (is.data.frame(x) == FALSE) {
      stop("ERROR: x is not a dataframe")
    }
    if (is.character(c) == FALSE) {
      stop("ERROR: c is not a list of column name")
    }
    if (is.character(d) == FALSE) {
      stop("ERROR: d is not a column name")
    }
    if (is.integer(integer(m)) == FALSE) {
      stop("ERROR: m is not an integer")
    }
    if (is.character(v) == FALSE) {
      stop("ERROR: v is not a column name")
    }
    if (is.integer(integer(n)) == FALSE) {
      stop("ERROR: n is not an integer")
    }
    for (i in ncol(x)) {
      if (class(x[, i]) == "factor") {
        vec <- character(length(levels(i)))
        p <- character(length(levels(i)))
        t <- list()
        a <- list()
        for (j in levels(x[, i])) {
          k1 <- subset(x, x[, i] == j)
          k2 <-
            aggregate.data.frame(k1[, c], by = list(k1[, d]), FUN = "sum")
          k2 <- k2[order(k2[, 1]),]
          k2[, "Month"] <- seq(1, nrow(k2), 1)
          k2_train <- k2[1:(nrow(k2) - m), ]
          k2_test <- k2[(nrow(k2) - (m - 1)):nrow(k2), ]
          cv <- sd(k2_train[, v] / mean(k2_train[, v]))
          p <- c(p, j)
          vec <- c(vec, cv)
        }
        vic <- as.data.frame(cbind(p, vec))
        colnames(vic) <- c("Segments_Name", "Covariance")
        vic <- vic[order(vic[, 2]), ]
        vic <- vic[1:n, ]
        {
          for (i in ncol(x)) {
            if (class(x[, i]) == "factor") {
              for (j in levels(x[, i])) {
                if (j %in% vic[, 1]) {
                  k1 <- subset(x, x[, i] == j)
                  k2 <-
                    aggregate.data.frame(k1[, c], by = list(k1[, d]), FUN = "sum")
                  k2 <- k2[order(k2[, 1]),]
                  k2[, "Month"] <- seq(1, nrow(k2), 1)
                  k2_train <- k2[1:(nrow(k2) - m), ]
                  k2_test <- k2[(nrow(k2) - (m - 1)):nrow(k2), ]
                  tna <- paste(j, "Train", sep = "_")
                  ana <- paste(j, "Test", sep = "_")
                  t[[tna]] <- k2_train
                  a[[ana]] <- k2_test
                }
              }
            }
          }
        }
        return(list(
          Top_Seg = vic,
          Train_Series = t,
          Test_series = a
        ))
      }
    }
  }
