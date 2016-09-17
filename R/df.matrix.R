df.matrix <-
function(x,name="dummy")
    for (i in 1:ncol(x)) {
      if (class(x[, i]) == "factor") {
        y <- NA
        y <- data.frame(rep(0, nrow(x)))
        for (j in 1:(nlevels(x[, i])-1)) {
          y[, j] <- rep(0, nrow(x))
          colnames(y)[j] <-
            paste(colnames(x)[i], levels(x[, i])[j], sep = " : ")
          y[which(x[, i] == levels(x[, i])[j]), j] <- 1
        }
        assign(paste(name, colnames(x[i]), sep = "_"),
               y,
               environment(df.matrix))
      }
    }
