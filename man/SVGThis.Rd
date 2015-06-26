\name{SVGThis}
\alias{SVGThis}
\alias{SVGThis.default}
\alias{SVGThis.histogram}
\alias{MakeTigerReady}
\alias{addInfo}

\title{Save commonly used graphs as structured SVG files.}
\description{Converts a graph object (as long as it has a class assigned) or the current graph window to an SVG file that can be viewed using the  Tiger Player software availabel from ViewPlus Inc. At present, the SVG needs manual editing using the Tiger Transformer software before viewing.}
\usage{
SVGThis(x, file = "test.svg")
}
\arguments{
  \item{x}{a graph object for which a method exists, or the current graphics device if set to NULL.}
  \item{file}{The SVG file to be created.}
}
\details{
The Cairo SVG device found in the gr.devices package does not create a structured SVG file that includes the semantics of the graphic being displayed. The SVG created by the gridSVG package does meet this need, but only works on graphs drawn using the grid package. Any graph created using functions from the more common graphics package can be converted to the grid package system using the gridGraphics package.}
\value{NULL. This function is solely for the purpose of creating SVG files in thne current working directory or in a path of the user's choosing.}
\references{
Need Murrell for gridGraphics and Murrell and Potter for gridSVG.
}
\author{A. Jonathan R. Godfrey and Paul Murrell}
\examples{
x=rnorm(1000)
#SVGThis(hist(x))
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
