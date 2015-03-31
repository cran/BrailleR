\name{SetOptions}
\alias{SetAuthor}
\alias{SetSigLevel}
\title{Functions for setting package options.}
\description{
Some package options have arguments which need validation. Setting the default significance level for analyses is the first function of this kind. Setting the name of the user to be inserted into documents is the second.}
\usage{
SetAuthor(name = "BrailleR", Permanent = TRUE)

SetSigLevel(alpha)
}
\arguments{
  \item{alpha}{The level of alpha to be used for analyses. Must be between zero and one or a warning is given and the option is not changed.}
  \item{name}{a character string to be used for author details in various file writing functions.}
  \item{Permanent}{Should the change be made permanent? Set to FALSE for a temporary change.}
}
\details{More convenience functions for BrailleR users.}
\author{A. Jonathan R. Godfrey}
\examples{
# SetSigLevel(5) # not a valid alpha
SetSigLevel(0.05) # valid alpha value
SetAuthor("Jonathan Godfrey")
SetAuthor()
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
