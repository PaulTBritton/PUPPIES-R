##########################################################
#
# Meta Data manipulation functions:
#
# Author: Paul Thomas Britton
#
###################################

#set.iterations <- function(x) N <<- x

#set.modelname <- function(x) pname <<- x

set.puppies <- function(name,iterations,seed,wildcard) {
	if (!missing(name)) pname <<- name
	if (!missing(iterations)) N <<- iterations
	if (!missing(seed)) if (!is.null(seed)) set.seed(seed)
	if (!missing(wildcard)) wildcardclass <<- wildcard
}
