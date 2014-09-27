\name{boxplot}
\alias{boxplot}
\title{Create a standard boxplot with a few extra elements added to the output object}
\description{This function is a wrapper to the standard hist() function in the graphics package. It adds detail to the stored object so that a better text description can be formulated using the VI() method in the BrailleR package.}
\usage{
boxplot(x, ...)
}
\arguments{
  \item{x}{a numeric variable.}
  \item{\dots}{
additional arguments passed on to the plotting function.}
}
\details{
This function masks the function of the same name in the graphics package. The base R implementation does create an object, but does not give it a class attribute, the object does not store all graphical arguments that are passed to the boxplot() function. The functionality should be no different at all for anyone who is not using the VI() function to gain a more detailed text description of the boxplot. See the help page for the graphics::boxplot() function to get a more complete description of boxplot creation.}
\value{
An object of class boxplot.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
The problem of not including class attributes for graphs was identified in: 
Godfrey, A.J.R. (2013) \sQuote{Statistical Software from a Blind Person's Perspective: R is the Best, but we can make it better}, The R Journal 5(1), pp73-80.
}
\author{
A. Jonathan R. Godfrey
}
\note{
I would love to see this function become redundant. This will happen if the extra functionality is included in the boxplot() function in the graphics package. This should be possible as the user experience will not be any different, no matter if the user is blind or sighted.
}

\seealso{
The base R implementation of the \code{\link{boxplot}} function should be consulted; see the entry in the \code{\link{graphics}} package
}
\examples{
# the stamdard boxplot function returns
MyBoxplot=graphics::hist(rnorm(1000), xlab="random normal values", main="Example histogram")
#dev.off()
MyBoxplot

# while this version returns
MyBoxplot=hist(rnorm(1000), xlab="random normal values", main="Example histogram")
#dev.off()
MyBoxplot

# The VI() method uses the extra information stored
VI(MyBoxplot)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
