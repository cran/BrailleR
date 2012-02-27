\name{UniDesc}
\alias{UniDesc}
\title{
Descriptive statistics and graphs for univariate data
}
\description{
This function is a convenience function fir analyzing univariate data. It provides histograms, density plots and tabulated results for normality tests as well as those for skewness and kurtosis. The intended use of this function is principally for a blind user of R who also has the advantage of retrieving textual descriptions of the graphs created along the way, via the \code{VI} methods.
}
\usage{
UniDesc(ResponseName, Response = NULL, Basic = TRUE, Graphs = TRUE, Normality = TRUE, Tests = TRUE, Folder = "", VI = TRUE)
}
\arguments{
  \item{ResponseName}{
the name of the variable to be analyzed. 
}
  \item{Response}{a numeric vector. If the default NULL is left in operation, the function uses the ResponseName given above to retrieve the named vector.}
  \item{Basic}{logical asking for basic numeric summary measures}
  \item{Graphs}{logical, indicating if the graphs are to be created. These will be eps files suitable for insertion in LaTeX documents.}
  \item{Normality}{logical asking if the various normality tests offered in the nortest package should be used}
  \item{Tests}{
%%     ~~Describe \code{Tests} here~~
}
  \item{Folder}{
%%     ~~Describe \code{Folder} here~~
}
  \item{VI}{
%%     ~~Describe \code{VI} here~~
}
}
\value{
Returns a text file and associated graph files in eps format (if requested) in (optinally: a subfolder of) the current working directory.
}
\author{Jonathan Godfrey \email{a.j.godfrey@massey.ac.nz}}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
