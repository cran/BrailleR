\name{SetOptions}
\alias{ChooseEmbosser}
\alias{ChooseStyle}
\alias{ResetDefaults}
\alias{SetAuthor}
\alias{SetBRLPointSize}
\alias{SetPaperHeight}
\alias{SetPaperWidth}
\alias{SetPValDigits}
\alias{SetSigLevel}
\title{Functions for setting package options.}
\description{
Some package options have arguments which need validation. Setting the default significance level for analyses was the first function of this kind. Setting the name of the user to be inserted into documents was the second. Others are detailed further below.}
\usage{
ChooseEmbosser(Embosser = "none", Permanent = TRUE)

ChooseStyle(css = "BrailleR.css", Permanent = TRUE)

ResetDefaults()

SetAuthor(name = "BrailleR", Permanent = TRUE)

SetBRLPointSize(pt, Permanent=FALSE)
SetPaperHeight(Inches, Permanent=FALSE)
SetPaperWidth(Inches, Permanent=FALSE)

SetPValDigits(digits, Permanent = TRUE)

SetSigLevel(alpha, Permanent = TRUE)
}
\arguments{
  \item{alpha}{The level of alpha to be used for analyses. Must be between zero and one or a warning is given and the option is not changed.}
  \item{css}{a cascading style sheet file to be inserted in HTML documents created by convenience functions. The file must be placed in the css folder within the BrailleR package folder for this to work.}
  \item{digits}{The number of decimal places to display. Must be an integer greater than one or a warning is given and the option is not changed.}
  \item{Embosser}{the name of the embosser to be used for tactile images. Not all embossers will be immediately supported by the package. The supported embossers are listed in the relevant section below. Please contact the package maintainer to introduce a new embosser to the list of supported models.}
  \item{Inches}{The size of the area to use for embossing. This should be the size of the embossed area not the actual size of the paper itself.}
  \item{name}{a character string to be used for author details in various file writing functions.}
  \item{Permanent}{Should the change be made permanent? Set to FALSE for a temporary change.}
\item{pt}{The point size of the chosen braille font.}
}
\details{More convenience functions for BrailleR users. Most are self explanatory, but the following details should be noted.

The Choose...() functions are used for establishing default parameters for other details. The ChooseStyle() command can be used to alter the appearance of HTML output by way of cascading style sheets. You can create your own css file and add it to the package before calling this function.

The ChooseEmbosser() will look for the default settings recommended for particular types of embosser. Initial testing was done on a Tiger Premiere 100 embosser manufactured by ViewPlus Inc. The default paper size is 11 by 11.5 inches, but the recommended embossing area for graphics is 10 by 10 inches.


The Set..() commands will let the user specify any desired value for the options as long as it is valid: Options assumed to be character strings are checked to be so, integers must be integers and a proportion must be between zero and one.  

SetPaperHeight and SetPaperWidth are temporary changes by default because some types of images are not meant to use the maximum area set down by the original default settings for an embosser. Careful experimentation may be required to get optimal results. If permanent changes are desired, then please contact the package maintainer to explain why you have made these changes so that we can help other users get the best from a wide range of embossers.

SetPValDigits() is used for rounding purposes to avoid the use of scientific notation. It is not used for determining significance.}
\author{A. Jonathan R. Godfrey}
\examples{
# SetSigLevel(5) # not a valid alpha
SetSigLevel(0.05) # valid alpha value
SetAuthor()
SetAuthor("Jonathan Godfrey")
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
