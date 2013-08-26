\name{WhereXY}
\alias{WhereXY}
\title{
Count points in a scatter plot
}
\description{
count the number of points that fall into various sized subparts of a scatter plot. The graphing region can be split into cells based on a uniform or normal marginal distribution for both x and y variables.
}
\usage{
WhereXY(x, y = NULL, grid = c(3, 3), Dist = "uniform")
}
\arguments{
  \item{x,y}{vectors of x coordinates. If y is not specified, the function expects x to be a two-column matrix with x and y values in columns 1 and 2 respectively. }
  \item{grid}{pair of values to specify the way the graph is to be split into parts. Specify x and then y.}
  \item{Dist}{the distribution the variables might be expected to follow. The default is to consider uniformly distributed but any alternative text will lead ot an assumption of both margins being normally distributed.}
}
\value{
A text descriptionof the number of points in each subregion of the scatter plot. The table of counts can then be compared to the expected number of points in each subregion.
}
\author{
Jonathan Godfrey
}
\examples{
x=rnorm(50)
y=rnorm(50)
WhereXY(x,y)
WhereXY(x,y, c(3,4))
WhereXY(x,y, Dist="other")
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
