\name{setattr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set attributes for puppies model results environment
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setattr(p=parent.frame(), name, iterations, seed, wildcard, verbose)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
PUPPIES results environment
%%     ~~Describe \code{filter} here~~
}
  \item{name}{
Model Name
%%     ~~Describe \code{name} here~~
}
  \item{iterations}{
Default number of iterations for the monte carlo simulation
%%     ~~Describe \code{iterations} here~~
}
  \item{seed}{
Seed value for the simulation.
%%     ~~Describe \code{seed} here~~
}
  \item{wildcard}{
Type of wildcard pattern matching
%%     ~~Describe \code{wildcard} here~~
}
  \item{verbose}{
level of printed feedback
%%     ~~Describe \code{verbose} here~~
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
\code{\link{puppies}}, \code{\link{pradist}}, \code{\link{numform}}, \code{\link{scatterbar}}, \code{\link{commoncause}}
}
\examples{
library(pradist)
library(numform)
library(scatterbar)
library(puppies)
#modeldef <- expression(T1<-betaD(3,4),T2<-lognD(mean=.01,EF=5),T3<-T1|T2)
#Y <- evalp(name="Simple in-line model",iterations=707,model=modeldef)
#pplot(plotname="Simpleplot.tiff",envir=Y)
E <- as.environment(dirichlet(40,4,5,6))
setattr(p=E,name="E model results")
pplot(envir=E)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ setattr }
\keyword{ evalp }
\keyword{ pplot }
\keyword{ puppies }
