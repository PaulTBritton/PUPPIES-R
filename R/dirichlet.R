#
# Author: Paul Thomas Britton
#
# k dimensional dirichlet probability distribution
# returns an array of k vectors of size N
# each of the k vectors corresponds to the margins of
# the dirichlet distribution

dirichlet <- function(N,...) {
	AlphaVector <- list(...)
	k <- length(AlphaVector)
	Y <- list()
	V <- rep(0,N)
	n <- rep("",k)
	for (i in 1:k) {
		Y[[i]] <- rgamma(N,shape=AlphaVector[[i]],rate=1)
		V <- V + Y[[i]]
		n[i] <- paste("a",i,sep="")
	}
	dividebyV <- function(X) X/V
	diri <- lapply(Y,dividebyV)
	names(diri) <- n
	return(diri)
}
