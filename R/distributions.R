##########
# Distributions:
# theoretical distributions functions
# emperical distribution procedure
# Author: Paul Thomas Britton
##########

source("dirichlet.R")

#############################################################
# various theoretical distributions
#

# constant vector
#consV <- function(c=0) rep(c,times=N)

# uniform distribution
unifD <- function(a=0,b=1) {
	N <- get("N",parent.frame())
	return(runif(N,a,b))
}

# create a vector of N trianglular samples
# a = min
# b = mode
# c = max
triaD <- function(a,b,c) {
	if (!((a<=b)&&(b<=c))) {
		stop("triaD: Invalid Triangle Distribution Parameters")
	}
	N <- get("N",parent.frame())
	U <- runif(N,0,1)
	x <- (b-a)/(c-a)
	Tri <- U <= x
	Tri[Tri==TRUE] <- a+sqrt(U[U<=x]*(c-a)*(b-a))
	Tri[Tri==FALSE] <- c-sqrt((1-U[U>x])*(c-a)*(c-b))
	return(Tri)
}

# N beta samples
betaD <- function(alpha,beta) {
	N <- get("N",parent.frame())
	return(rbeta(N,alpha,beta))
}

# N gamma samples
gammD <- function(shape,rate) {
	N <- get("N",parent.frame())
	return(rgamma(N,shape,rate))
}

# create a vector of N lognormal samples
# mean = mean of lognormal
# median = 50th of lognormal
# EF = 95th/50th (of lognormal)
# mu = mean of underlying normal
# sigma = standard dev of underlying normal
#
# user must provide either: mean & EF, median & EF, or mu & sigma
lognD <- function(EF=exp(qnorm(0.95)),sigma=log(EF)/qnorm(0.95),
		mean=1,median=exp(log(mean)-(sigma^2)/2),
		mu=log(median)) {
	N <- get("N",parent.frame())
#	print("calling heirarchy")
#	print(ls(parent.frame()))
#	print(ls(parent.env(parent.frame())))
	return(rlnorm(N,mu,sigma))
}

####################################################
# create a vector of N samples based on an emperical distribution
# (first fit a cdf based on data then spline fit the inverse cdf)
empeD <- function(empfile) {
	N <- get("N",parent.frame())
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

# k dimensional dirichlet probability distribution
# returns an array of k vectors of size N
# each of the k vectors corresponds to the margins of
# the dirichlet distribution
diriD <- function(...) {
	N <- get("N",parent.frame(1))
	return(dirichlet(N,...))
}
