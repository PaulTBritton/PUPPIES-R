\name{pplot}
\alias{pplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate scatterbar plot from puppies results
}
\description{
generate scatterbar plot from puppies results
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pplot(plotname = "pplot.tiff", envir = parent.frame(), rmarg = 8,
	stats = c(2, 0, 2, 2), prec = 2, legendpos, units = "Probability",
	xscale = "log", xnotation = sciNotation, filter, plist, maintitle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plotname}{
file name for plot
%%     ~~Describe \code{plotname} here~~
}
  \item{envir}{
puppies results
%%     ~~Describe \code{envir} here~~
}
  \item{rmarg}{
right margin
%%     ~~Describe \code{rmarg} here~~
}
  \item{stats}{
vector containing information on which samples status to print on plot
%%     ~~Describe \code{stats} here~~
}
  \item{prec}{
number of significant digits
%%     ~~Describe \code{prec} here~~
}
  \item{legendpos}{
position of legend on plot
%%     ~~Describe \code{legendpos} here~~
}
  \item{units}{
units label for the x-axis
%%     ~~Describe \code{units} here~~
}
  \item{xscale}{
log or linear scale for x-axis
%%     ~~Describe \code{xscale} here~~
}
  \item{xnotation}{
number format for x-axis
%%     ~~Describe \code{xnotation} here~~
}
%%  \item{xmarks}{
%%number vector containing location of x-axis tick marks
%%     ~~Describe \code{xmarks} here~~
%%}
%%  \item{range}{
%%number vector describing the range of the x-axis
%%     ~~Describe \code{range} here~~
%%}
  \item{filter}{
wildcard filter for which objects to plot
%%     ~~Describe \code{filter} here~~
}
  \item{plist}{
explicit plotlist of objects to plot
%%     ~~Describe \code{lst} here~~
}
  \item{maintitle}{
main title to be printed above plot
%%     ~~Describe \code{maintitle} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Paul Thomas Britton
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{evalp}}, \code{\link{puppies}}
}
\examples{
library(pradist)
library(numform)
library(scatterbar)
library(puppies)
modeldef <- expression(T1<-betaD(3,4),T2<-lognD(mean=.01,EF=5),T3<-T1|T2)
Y <- evalp(name="Simple in-line model",iterations=707,model=modeldef)
pplot(plotname="Simpleplot.tiff",envir=Y)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ setattr }
\keyword{ puppies }
\keyword{ pplot }
\keyword{ evalp }
