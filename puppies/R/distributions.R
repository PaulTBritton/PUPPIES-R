##########
# Distributions:
# theoretical distributions functions
# emperical distribution procedure
# Author: Paul Thomas Britton
##########

library(pradist)

#############################################################
# various theoretical distributions
#

# uniform distribution
puppiesenv$unifD <- function(N=attr(parent.frame(),"N"),a=0,b=1) {
#	N <- get("N",parent.frame())
	
	return(runif(N,a,b))
}

# create a vector of N trianglular samples
# a = min
# b = mode
# c = max
puppiesenv$triaD <- function(N=attr(parent.frame(),"N"),a,b,c) {
#	N <- get("N",parent.frame())
#	N <- attr(parent.frame(),"N")
	return(triadist(N,a,b,c))
}

# N beta samples
puppiesenv$betaD <- function(N=attr(parent.frame(),"N"),alpha,beta) {
#	N <- get("N",parent.frame())
#	N <- attr(parent.frame(),"N")
	return(rbeta(N,alpha,beta))
}

# N gamma samples
puppiesenv$gammD <- function(N=attr(parent.frame(),"N"),shape,rate) {
#	N <- get("N",parent.frame())
#	N <- attr(parent.frame(),"N")
	return(rgamma(N,shape,rate))
}

puppiesenv$lognD <- function(N=attr(parent.frame(),"N"),...) {
#	N <- get("N",parent.frame())
#	N <- attr(parent.frame(),"N")
	return(logndist(N,...))
}

####################################################
# create a vector of N samples based on an emperical distribution
# (first fit a cdf based on data then spline fit the inverse cdf)
puppiesenv$empeD <- function(N=attr(parent.frame(),"N"),empfile=NULL) {
#	N <- get("N",parent.frame())
#	N <- attr(parent.frame(),"N")
	VerboseLevel <- attr(parent.frame(),"VerboseLevel")
	if (VerboseLevel >= 2) print(paste("Loading Emperical Data:",empfile))
	res <- 10*N + 1
	X <- seq(0,1,length.out=res)
	if (file.access(empfile,mode=4)) {
		stop(paste("File access error:",empfile))
	}
	D <- scan(empfile)
	Qfun <- splinefun(ecdf(D)(X),X)
	if (VerboseLevel >= 2) print(paste("Piece-wise spline fit complete:",
					empfile))
	return(Qfun(runif(N,0,1)))
}

# k dimensional dirichlet probability distribution
# returns an array of k vectors of size N
# each of the k vectors corresponds to the margins of
# the dirichlet distribution
puppiesenv$diriD <- function(N=attr(parent.frame(),"N"),...) {
#	N <- get("N",parent.frame())
#	N <- attr(parent.frame(),"N")
	return(dirichlet(N,...))
}
