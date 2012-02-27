\name{VI}
\alias{VI}
\alias{VI.default}
\alias{VI.data.frame}
\alias{VI.histogram}
\alias{VI.hist}
\alias{VI.list}
\alias{VI.matrix}
\title{
Methods to help vision impaired useRs
}
\description{
A set of methods that extract the most relevant information from a graphical object (or implied set of graphical objects) and display the interpreted results in text form.

The method includes representations of summary methods that are more suitable for blind useRs. For example, the method for a data.frame uses a single line for each variable instead of the normal column layout used by the \code{summary} method.
}
\usage{
VI(x)

\method{VI}{histogram}(x)
}
\arguments{
  \item{x}{any \R object}
}
\details{
This is the general help page for the VI() functionality. Some specific pages exist where some ability to alter the outcome through user input have warranted their own help pages. See below for more detail on these.


Further methods can be written by useRs. Please submit to the package maintainer for possible inclusion in subsequent releases of the package.
}
\value{
This will vary according to the needs of vision impaired useRs and the specific objects that need to be interpretted.

In general, the output is a series of text strings printed in the console/terminal window in addition to the embedded command's normal functionality.

These functions do not create objects as do many \R commands. Manipulations on the objects created by regular \R expressions will need those regular expressions issued in addition to those of the VI family of functions.
}
\author{
Jonathan Godfrey
}

\examples{
RandomX=rnorm(500)
PlottedFig=hist(RandomX)
rm(RandomX)
VI(PlottedFig)
rm(PlottedFig)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
