##########################################################
#
# Meta Data manipulation functions:
#
# Author: Paul Thomas Britton
#
###################################

setpuppies <- function(name,iterations,seed,wildcard) {
	if (!missing(name)) modelname <<- name
	if (!missing(iterations)) N <<- iterations
	if (!missing(seed)) {
		saveseed <<- seed
		if (!is.null(seed)) set.seed(seed)
	}
	if (!missing(wildcard)) wildcardclass <<- wildcard
}
