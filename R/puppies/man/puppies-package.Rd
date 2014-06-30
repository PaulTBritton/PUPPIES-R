\name{puppies-package}
\alias{puppies-package}
\alias{puppies}
\docType{package}
\title{
Programmable Uncertain Parameter Propagation Into Equations Software
}
\description{
Programmable Uncertain Parameter Propagation Into Equations Software
}
\details{
\tabular{ll}{
Package: \tab puppies\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-06-29\cr
License: \tab GPL-2\cr
}
General purpose monte carlo simulation.
}
\author{
Paul Thomas Britton

Maintainer: Paul Thomas Britton <paul.t.britton@nasa.gov>
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\keyword{ puppies }
\keyword{ evalp }
\keyword{ pplot }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
library(pradist)
library(numform)
library(scatterbar)
library(puppies)
modeldef <- expression(T1<-betaD(3,4),T2<-lognD(mean=.01,EF=5),T3<-T1|T2)
Y <- evalp(name="Simple in-line model",iterations=707,model=modeldef)
pplot(plotname="Simpleplot.tiff",envir=Y)
}
