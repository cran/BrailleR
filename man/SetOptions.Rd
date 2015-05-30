\name{SetOptions}
\alias{ChooseStyle}
\alias{SetAuthor}
\alias{SetPValDigits}
\alias{SetSigLevel}
\title{Functions for setting package options.}
\description{
Some package options have arguments which need validation. Setting the default significance level for analyses is the first function of this kind. Setting the name of the user to be inserted into documents is the second. Others are detailed further below.}
\usage{
ChooseStyle(css="BrailleR.css", Permanent=TRUE)

SetAuthor(name = "BrailleR", Permanent = TRUE)

SetPValDigits(digits, Permanent = TRUE)

SetSigLevel(alpha, Permanent = TRUE)
}
\arguments{
  \item{alpha}{The level of alpha to be used for analyses. Must be between zero and one or a warning is given and the option is not changed.}
  \item{css}{a cascading style sheet file to be inserted in HTML documents created by convenience functions. The file must be placed in the css folder within the BrailleR package folder for this to work.}
  \item{digits}{The number of decimal places to display. Must be an integer greater than one or a warning is given and the option is not changed.}
  \item{name}{a character string to be used for author details in various file writing functions.}
  \item{Permanent}{Should the change be made permanent? Set to FALSE for a temporary change.}
}
\details{More convenience functions for BrailleR users.

SetPValDigits() is used for rounding purposes to avoid the use of scientific notation.}
\author{A. Jonathan R. Godfrey}
\examples{
# SetSigLevel(5) # not a valid alpha
SetSigLevel(0.05) # valid alpha value
SetAuthor()
SetAuthor("Jonathan Godfrey")
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
