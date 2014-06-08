##########
# Distributions:
# theoretical distributions functions
# emperical distribution procedure
# Author: Paul Thomas Britton
##########

#############################################################
# various theoretical distributions
#

# constant vector
#consV <- function(c=0) rep(c,times=N)

# uniform distribution
unifD <- function(a=0,b=1) runif(N,a,b)

# create a vector of N trianglular samples
# a = min
# b = mode
# c = max
triaD <- function(a,b,c) {
#	if (!((0<=a)&&(a<=b)&&(b<=c)&&(c<=1))) {
#		stop("triaD: Invalid Triangle Distribution Parameters")
#	}
	U <- runif(N,0,1)
	x <- (b-a)/(c-a)
	Tri <- U <= x
	Tri[Tri==TRUE] <- a+sqrt(U[U<=x]*(c-a)*(b-a))
	Tri[Tri==FALSE] <- c-sqrt((1-U[U>x])*(c-a)*(c-b))
	return(Tri)
}

# N beta samples
betaD <- function(alpha,beta) {
	return(rbeta(N,alpha,beta))
}

# N gamma samples
gammD <- function(shape,rate) {
	return(rgamma(N,shape,rate))
}

# create a vector of N lognormal samples
# median = 50th
# EF = 95th/50th
lognD <- function(EF=exp(qnorm(0.95)),sigma=log(EF)/qnorm(0.95),
		mean=1,median=exp(log(mean)-(sigma^2)/2),
		mu=log(median)) {
#	mu <- log(median)
#	sigma <- log(EF)/qnorm(0.95)
	return(rlnorm(N,mu,sigma))
}

####################################################
# create a vector of N samples based on an emperical distribution
# (first fit a cdf based on data then spline fit the inverse cdf)
empeD <- function(empfile) {
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
	AlphaVector <- list(...)
	k <- length(AlphaVector)
	Y <- array(0,c(N,k))
	for (i in 1:k) {
print(AlphaVector[[i]])
		Y[,i] <- rgamma(N,shape=AlphaVector[[i]],rate=1)
	}
	V <- rowSums(Y)
	dividebyV <- function(X) X/V
	return(apply(Y,2,dividebyV))
}
