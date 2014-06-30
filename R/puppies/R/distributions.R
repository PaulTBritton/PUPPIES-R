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
unifD <- function(a=0,b=1) {
#	N <- get("N",parent.frame())
	N <- attr(parent.frame(),"N")
	return(runif(N,a,b))
}

# create a vector of N trianglular samples
# a = min
# b = mode
# c = max
triaD <- function(a,b,c) {
#	N <- get("N",parent.frame())
	N <- attr(parent.frame(),"N")
	return(triadist(N,a,b,c))
}

# N beta samples
betaD <- function(alpha,beta) {
#	N <- get("N",parent.frame())
	N <- attr(parent.frame(),"N")
	return(rbeta(N,alpha,beta))
}

# N gamma samples
gammD <- function(shape,rate) {
#	N <- get("N",parent.frame())
	N <- attr(parent.frame(),"N")
	return(rgamma(N,shape,rate))
}

lognD <- function(...) {
#	N <- get("N",parent.frame())
	N <- attr(parent.frame(),"N")
	return(logndist(N,...))
}

####################################################
# create a vector of N samples based on an emperical distribution
# (first fit a cdf based on data then spline fit the inverse cdf)
empeD <- function(empfile) {
#	N <- get("N",parent.frame())
	N <- attr(parent.frame(),"N")
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
diriD <- function(...) {
#	N <- get("N",parent.frame())
	N <- attr(parent.frame(),"N")
	return(dirichlet(N,...))
}
