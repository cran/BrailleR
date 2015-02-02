\name{OneFactor}
\alias{OneFactor}
\title{Analysis for a continuous response for one group factor}
\description{
A convenience funcion that creates an analysis for a continuous response variable with one grouping factor. The function creates a number of graphs and tables relevant for the analysis.}
\usage{
OneFactor(Response, Factor, Data = NULL, HSD = TRUE, AlphaE = 0.05, 
Filename = NULL, Folder = NULL, 
VI = getOption("BrailleR.VI"), Latex = getOption("BrailleR.Latex"), 
View = getOption("BrailleR.View"))
}
\arguments{
  \item{Response}{A continuous variable.}
  \item{Factor}{The grouping factor.}
  \item{Data}{The data.frame that contains both the response and hte factor.}
  \item{HSD}{Logical. Should the Tukey HSD analysis be performed.}
  \item{AlphaE}{The familywise level of significance used for the Tukey HSD analysis.}
  \item{Filename}{The filename used for the markdown and html files.}
  \item{Folder}{The folder in which files for graphs and tables will be saved.}
  \item{VI}{Logical. Should the VI method be employed? This is set using the package options.}
  \item{Latex}{Logical. Should tables be created using the \pkg{xtable} package? This is set using the package options.}
  \item{View}{Logical. Should the resulting HTML file be opened for viewing? This is set using the package options.}
}
\details{
to do
}
\value{
to do
}
\author{
A. Jonathan R. Godfrey
}

\seealso{
to do
}
\examples{
data(airquality)

# the following line  returns an error:
## OneFactor("Ozone", "Month", airquality, View=FALSE)
# so we make a copy of the data.frame, and fix that:

airquality2 = airquality
airquality2$Month = as.factor(airquality$Month)
# and now all is good to try:
OneFactor("Ozone", "Month", airquality2)
# N.B. Various files and a folder were created in the working directory. 
# Please investigate them to see how this function worked.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
