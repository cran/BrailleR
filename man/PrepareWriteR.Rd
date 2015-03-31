\name{PrepareWriteR}
\alias{PrepareWriteR}
\alias{WriteR}
\title{Getting started with WriteR}
\description{The WriteR application was written in wxPython so that blind users can process Rmarkdown documents. This functionality is because RStudio is not an accessible application for screen reader users.}
\usage{
PrepareWriteR(Author = getOption("BrailleR.Author"))
}
\arguments{
  \item{Author}{Your name as you want it to appear in the default text that starts your Rmarkdown documents.}
}
\details{This function writes the settings file (called WriteR.init) for WriteR and copies the files that were part of the BrailleR installation into the current working directory. You will be able to run the WriteR application from there, or move to a folder of your choosing.}
\value{NULL. This function is for your convenience and not for doing any work inside an R session.}
\author{A. Jonathan R. Godfrey}
\note{You must have Python 2.7 and the associated wxPython installation on your system to use the WriteR application.}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
