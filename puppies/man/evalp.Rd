\name{evalp}
\alias{evalp}
\alias{appendp}
\alias{clonep}
\alias{spawnp}
\alias{superevalp}
\alias{superappendp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluate puppies models
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
evalp(name = "PUPPIES Model", iterations = 10, seed = NULL, wildcard = "aglob", model = "", verbose = 1)
appendp(p = parent.frame(), model = "")
clonep(p = parent.frame(), name = "Copy Model")
spawnp(p, name = "PUPPIES Model", model = "")
superevalp(name = "PUPPIES Model", iterations = 10, seed = NULL, wildcard = "aglob", verbose = 1, filter = "")
superappendp(p, filter = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filter}{
%%     ~~Describe \code{filter} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
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
  \item{model}{
an object of class "expression"
%%     ~~Describe \code{model} here~~
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
\code{\link{puppies}}, \code{\link{pradist}}, \code{\link{numform}}, \code{\link{scatterbar}}
}
\examples{
library(pradist)
library(numform)
library(scatterbar)
library(puppies)
modeldef <- expression(T1<-betaD(alpha=3,beta=4),T2<-lognD(mean=.01,EF=5),T3<-T1|T2)
Y <- evalp(name="Simple in-line model",iterations=707,model=modeldef)
pplot(plotname="Simpleplot.tiff",envir=Y)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (name = "PUPPIES Model", iterations = 10, seed = NULL, 
    wildcard = "aglob", model = "", verbose = 1) 
{
    p <- new.env(parent = puppiesenv)
    attr(p, "VerboseLevel") <- verbose
    attr(p, "modelname") <- name
    attr(p, "N") <- iterations
    attr(p, "saveseed") <- seed
    attr(p, "wildcardclass") <- wildcard
    pexpr <- topexpr(model)
    eval(pexpr, p)
    y <- attr(p, "modelname")
    z <- attr(p, "VerboseLevel")
    class(p) <- c("environment", "puppies")
    if (z >= 2) 
        print(paste("evalp() complete on:", y))
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ setattr }
\keyword{ evalp }
\keyword{ pplot }
\keyword{ puppies }
