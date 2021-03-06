##########################################################
#
# Boolean Operators
#
# Author: Paul Thomas Britton
#
###################################


#################
# Logical operations for vectors of probabilities
#

puppiesenv$NOT <- function(X) (1-X)

#ORgate <- function(...) {
puppiesenv$'|' <- function(...) {
	dots <- list(...)
	X <- simplify2array(dots)
	return(1-apply(1-X,1,prod))
}

#ANDgate <- function(...) {
puppiesenv$'&' <- function(...) {
	dots <- list(...)
	X <- simplify2array(dots)
	return(apply(X,1,prod))
}

puppiesenv$MN <- function(M,N,X) {
	return(pbinom(N-M,N,1-X))
}

#################
# Advanced Logical operation filters for vectors of probabilities
#

# logical OR operation filter for probabilities
puppiesenv$OR <- function(filter) {
	e <- parent.frame(1)
	Z <- as.data.frame(as.list(e))
#	class(filter) <- get("wildcardclass",e)
	class(filter) <- attr(e,"wildcardclass")
	X <- filterdata(torx(filter),Z)
	return(1-apply(1-X,1,prod))
}

# logical AND operation filter for probabilities
puppiesenv$AND <- function(filter) {
	e <- parent.frame(1)
	Z <- as.data.frame(as.list(e))
#	class(filter) <- get("wildcardclass",e)
	class(filter) <- attr(e,"wildcardclass")
	X <- filterdata(torx(filter),Z)
	return(apply(X,1,prod))
}

