\name{df.matrix}
\alias{df.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Data Frame of Numeric Vector
}
\description{
The function "df.matrix" takes "x" (data frame) as argument and an option argument "name" which is to specify the intial name of the dummy data frame, it then search for the class factor in the given data frame(x) and if it match it store the attribute name and count the number of factor levels, the function then create a data frame with name provided by the user and adding that name by "_" with the attribute name store by the function, it then creates columns as per the number of levels (which is number of levels n - 1). The values of each column is either 0 or 1 which can for building a model.
}
\usage{
df.matrix(x, name = "dummy", sep = ":")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A Data Frame
}
  \item{name}{
A name assign by user for new Dummy Data Frame
  \item{sep}{
The delimeter to be used for seperation of column and level
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The Data Frame of value 0 and 1
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Prabhakar Srivastav
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## x <- df.factor(dataframe)
## df.matrix(x, name = "dummy", sep = ":")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
