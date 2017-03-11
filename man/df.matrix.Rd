\name{df.matrix}
\alias{df.matrix}

\title{
Create Data Frame of Numeric Vector
}
\description{
The function "df.matrix" takes "x" (data frame) as argument and an option argument "name" which is to specify the intial name of the dummy data frame, it then search for the class factor in the given data frame(x) and if it match it store the attribute name and count the number of factor levels, the function then create a data frame with name provided by the user and adding that name by "_" with the attribute name store by the function, it then creates columns as per the number of levels (which is number of levels n - 1). The values of each column is either 0 or 1 which can for building a model.
}
\usage{
df.matrix(x, name = "dummy", sep = ":")
}

\arguments{
  \item{x}{
A Data Frame
}
  \item{name}{
A name assign by user for new Dummy Data Frame
}
  \item{sep}{
The delimeter to be used for seperation of column and level
}
}

\value{
The Data Frame of value 0 and 1
}

\author{
Prabhakar Srivastav
}





\examples{
## x <- df.factor(dataframe)
## df.matrix(x, name = "dummy", sep = ":")
}

