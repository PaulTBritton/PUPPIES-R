##########################################################
#
# Boolean Operators
#
# Author: Paul Thomas Britton
#
###################################

#################
# Logical operations for the PUPPIES equations (*.pe) files
#

NOT <- function(X) (1-X)

ORgate <- function(...) {
	dots <- list(...)
	X <- simplify2array(dots)
	return(1-apply(1-X,1,prod))
}

ANDgate <- function(...) {
	dots <- list(...)
	X <- simplify2array(dots)
	return(apply(X,1,prod))
}

MN <- function(M,N,X) {
	return(pbinom(N-M,N,1-X))
}

