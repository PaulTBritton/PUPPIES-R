#
# ************UNDER DEVELOPMENT******************
# Common Cause Module
#
# Author: Paul Thomas Britton
#
###################################

dirichletD <- function(Z,P) {
	AlphaVector <- scan(text=as.character(P$f),sep=",")
	k <- length(AlphaVector)
	Y <- array(0,c(N,k))
	for (i in 1:k) {
		set.seed(seed)
		Y[,i] <- rgamma(N,shape=AlphaVector[i],rate=1)
		seed <- seed + 1
	}
	V <- rowSums(Y)
	return(Y[,P$a]/V)
#	dividebyV <- function(X) X/V
#	return(apply(Y,2,dividebyV))
}

alphaT <- function(...) {
	AlphaVector <- list(...)
	tmp <- 0
	for (i in 1:length(AlphaVector)) {
		tmp <- tmp + i * AlphaVector[[i]]
	}
	return(tmp)
}

GlobalAlpha <- function(M,...) {
	AlphaVector <- list(...)
	k <- length(AlphaVector)
	tmp <- 0
	aT <- alphaT(...)
	for (j in M:k) {
		tmp <- tmp + (k * AlphaVector[[j]])/aT
	}
	return(tmp)
}

GA <- function(Z,P) {
	N <- nrow(Z)
	AlphaVector <- scan(text=as.character(P$f),sep=",")
	k <- length(AlphaVector)
	Y <- array(0,c(N,k))
	for (i in 1:k) {
		set.seed(seed)
		Y[,i] <- rgamma(N,shape=AlphaVector[i],rate=1)
		seed <- seed + 1
	}
	V <- rowSums(Y)
	dividebyV <- function(X) X/V
	Dir <- apply(Y,2,dividebyV)
	aT <- 0
	for (i in 1:k) {
		aT <- aT + i * Dir[,i]
	}
	tmp <- 0
	for (j in P$a:k) {
		tmp <- tmp + (k * Dir[,j])/aT
	}
	return(tmp)
}
