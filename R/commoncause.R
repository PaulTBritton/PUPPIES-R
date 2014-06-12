#
#
# Common Cause Module
#
# Author: Paul Thomas Britton
#
###################################

alphaT <- function(diri) {
	k <- length(diri)
	tmp <- 0
	for (i in 1:k) {
		tmp <- tmp + i * diri[[i]]
	}
	return(tmp)
}

GlobalAlpha <- function(M,diri) {
	k <- length(diri)
	tmp <- 0
	aT <- alphaT(diri)
	for (j in M:k) {
		tmp <- tmp + (k * diri[[j]])/aT
	}
	return(tmp)
}
