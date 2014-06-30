##########################################################
#
# Boolean Operators
#
# Author: Paul Thomas Britton
#
###################################

source("filter.R")

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
	e <- parent.frame(1)
	Z <- as.data.frame(as.list(e))
#	class(filter) <- get("wildcardclass",e)
	class(filter) <- attr(e,"wildcardclass")
	X <- filterdata(torx(filter),Z)
	return(1-apply(1-X,1,prod))
}

# logical AND operation filter for probabilities
AND <- function(filter) {
	e <- parent.frame(1)
	Z <- as.data.frame(as.list(e))
#	class(filter) <- get("wildcardclass",e)
	class(filter) <- attr(e,"wildcardclass")
	X <- filterdata(torx(filter),Z)
	return(apply(X,1,prod))
}

