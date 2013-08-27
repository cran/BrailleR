\name{OneWayANOVA}
\alias{OneWayANOVA}
\title{
Convenience function for generating output and graphs for a one-way ANOVA
}
\description{
This function is a convenience function for creating the standard ANOVA table as well as optionally saving various commonly sought information for a one way analysis of variance. 
The intended use of this function is principally for a blind user of R who also has the advantage of retrieving textual descriptions of the graphs created along the way, via the \code{VI} methods.
}
\usage{
OneWayANOVA(DataName, ResponseName, FactorName, HSD=TRUE, AlphaE=0.05, Folder="", VI=TRUE)
}
\arguments{
\item{DataName}{the name of the data.frame being used for the analysis.}
  \item{ResponseName}{
the name of the variable to be analyzed. 
}
  \item{FactorName}{
the name of the predictor factor to be used in the analysis. It must be able to be converted to a factor if it is not one already.
}
\item{HSD}{should the TukeyHSD function be employed. Must be logical.}
\item{AlphaE}{the familywise error rate to be used if HSD=TRUE.}
  \item{Folder}{the folder where results and graph files will be saved.}
  \item{VI}{should the VI method be used to give added text descriptions of graphs.}
}
\value{
Returns a text file and associated graph files in eps and pdf formats (if requested) in (optionally) a subfolder of the current working directory.
}
\author{Jonathan Godfrey \email{a.j.godfrey@massey.ac.nz}}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
