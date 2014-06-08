#
#
# Common Cause Module
#
# Author: Paul Thomas Britton
#
###################################

alphaT <- function(Dir) {
	k <- length(Dir)
	tmp <- 0
	for (i in 1:k) {
		tmp <- tmp + i * Dir[[i]]
	}
	return(tmp)
}

GlobalAlpha <- function(M,Dir) {
	k <- length(Dir)
	tmp <- 0
	aT <- alphaT(Dir)
	for (j in M:k) {
		tmp <- tmp + (k * Dir[[j]])/aT
	}
	return(tmp)
}
