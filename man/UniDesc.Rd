\name{UniDesc}
\alias{UniDesc}
\title{
Descriptive statistics and graphs for univariate data
}
\description{
This function is a convenience function for analyzing univariate data. It provides histograms, boxplots and tabulated results for normality tests as well as those for skewness and kurtosis. The intended use of this function is principally for a blind user of R who also has the advantage of retrieving textual descriptions of the graphs created along the way, via the \code{VI} methods.
}
\usage{
UniDesc(Response = NULL, ResponseName = as.character(match.call()$Response), 
    Basic = TRUE, Graphs = TRUE, Normality = TRUE, Tests = TRUE, 
    Filename = NULL, Folder = ResponseName, VI = TRUE, Latex = TRUE, View=TRUE)
}
\arguments{
  \item{Response}{The numeric vector to be analyzed. This must be specified as a variable that is directly available in the workspace, not as a data.frame$variable construct.}
  \item{ResponseName}{This is the same as Response but use quote marks around it. Exactly one of Response or ResponseName must be specified.}
  \item{Basic}{logical, asking for basic numeric summary measures}
  \item{Graphs}{logical, indicating if the graphs are to be created. These will be eps files suitable for insertion in LaTeX documents, pdf files for more general use, and SVG for easier use by blind users.}
  \item{Normality}{logical, asking if the various normality tests offered in the nortest package should be used}
  \item{Tests}{logical, should skewness and kurtosis tests be performed.}
  \item{Filename}{Specify the name of the R markdown and html files (without extensions).}
  \item{Folder}{the folder where results and graph files will be saved.}
  \item{VI}{logical, should the VI method be used to give added text descriptions of graphs.}
  \item{Latex}{logical, Should the xtable package be used to convert the tabulated results into LaTeX tables? Currently set to TRUE but not actually implemented yet.}
  \item{View}{logical, should the resulting HTML file  be opened in a browser.}
}

\value{
Saves an R markdown file, an R script file,  and an html file (which may be opened automatically) in the current working folder. Graphs are saved in png, eps, pdf, and SVG formats (if requested) in (optionally) a subfolder of the current working directory.
}
\author{A. Jonathan R. Godfrey \email{a.j.godfrey@massey.ac.nz}}
\examples{
Ozone=airquality$Ozone
UniDesc(Ozone, View=FALSE)
rm(Ozone)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
