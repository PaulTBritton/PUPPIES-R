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

NOT <- function(X) (1-X)

#ORgate <- function(...) {
'|' <- function(...) {
	dots <- list(...)
	X <- simplify2array(dots)
	return(1-apply(1-X,1,prod))
}

#ANDgate <- function(...) {
'&' <- function(...) {
	dots <- list(...)
	X <- simplify2array(dots)
	return(apply(X,1,prod))
}

MN <- function(M,N,X) {
	return(pbinom(N-M,N,1-X))
}

#################
# Advanced Logical operation filters for vectors of probabilities
#

# logical OR operation filter for probabilities
OR <- function(filter) {
	class(filter) <- wildcardclass
	e <- parent.frame(3)$e
	ne <- as.list(e)
	Z <- as.data.frame(ne)
	X <- filterdata(torx(filter),Z)
	return(1-apply(1-X,1,prod))
}

# logical AND operation filter for probabilities
AND <- function(filter) {
	class(filter) <- wildcardclass
	e <- parent.frame(3)$e
	ne <- as.list(e)
	Z <- as.data.frame(ne)
	X <- filterdata(torx(filter),Z)
	return(apply(X,1,prod))
}

