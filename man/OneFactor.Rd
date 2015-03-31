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
  \item{Response}{Name of the continuous response variable.}
  \item{Factor}{The grouping factor.}
  \item{Data}{The data.frame that contains both the response and the factor.}
  \item{HSD}{Logical: Should Tukey's HSD be evaluated for the data?}
  \item{AlphaE}{The family-wise Type I error rate for Tukey's HSD calculations.}
  \item{Filename}{Name of the Rmarkdown and HTML files to be created. A default will be created that uses the names of the variables if this is left set to NULL.}
  \item{Folder}{Name of the folder to store graph and LaTeX files. A default will be created based on the name of the data.frame being used.}
  \item{VI}{Logical: Should the VI method for blind users be employed?}
  \item{Latex}{Logical: Should the tabulated sections be saved in LaTeX format?}
  \item{View}{Logical: Should the HTML file be opened for inspection?}
}
\details{to complete}
\value{to complete}
\author{A. Jonathan R. Godfrey and Timothy P. Bilton}

\seealso{to complete}
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
