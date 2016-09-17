\name{AtConP-package}
\alias{AtConP-package}
\alias{AtConP}
\docType{package}
\title{
\packageTitle{AtConP}
}
\description{
\packageDescription{AtConP}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{AtConP}
\packageIndices{AtConP}
The "AtConP" Attribute Convertion package was created to minimize the repetition of some task which I was facing while doing my PG in Data Analytics from IIIT-B and UpGrad. This package includes two function initially, df.factor and df.matrix. The function df.factor takes the argument of a data frame and find for any character attributes and if it match the attributes is then converted to class factor, the output of the function "df.factor" is a data frame and can be strove in a new data frame. The other function "df.matrix" takes the "x" (data frame) as argument and an option argument "name" which is to specify the intial name of the dummy data frame, after providing the argument the function "df.matrix" search for the class factor in the given data frame(x) and if it match it store the attribute name and count the number of factor levels, the function then create a data frame with name provided by the user and adding that name by "_" with the attribute name store by the function, and then it creates columns as per the number of levels (which is number of levels n - 1). The next step it store the values of each column as 0 and 1 for building a model.
}
\author{
\packageAuthor{AtConP}

Maintainer: \packageMaintainer{AtConP}
}
