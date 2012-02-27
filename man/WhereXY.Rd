\name{WhereXY}
\alias{WhereXY}
\title{
Count points in a scatter plot
}
\description{
count the number of points that fall into various sized subparts of a scatter plot. The graphing region can be split into cells based on a uniform or normal marginal distribution for both x and y variables.
}
\usage{
WhereXY(x, y, grid = c(3, 3), Dist = "uniform")
}
\arguments{
  \item{x}{vector of x coordinates.}
  \item{y}{vector of y coordinates}
  \item{grid}{pair of values to split the graph into how many parts.}
  \item{Dist}{the distribution the variables might be expected to follow
}
}
\value{
A text descriptionof the number of points in each subregion of the scatter plot. The table of counts should be compared to the expected number of points in each subregion.
}
\author{
Jonathan Godfrey
}
\examples{
x=rnorm(50)
y=rnorm(50)
WhereXY(x,y)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
