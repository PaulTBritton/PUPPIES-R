##########################################################
#
# Meta Data manipulation functions:
#
# Author: Paul Thomas Britton
#
###################################

setpuppies <- function(name,iterations,seed,wildcard) {
	m <- parent.frame(2)
	if (!missing(name)) m$modelname <- name
	if (!missing(iterations)) m$N <- iterations
	if (!missing(seed)) {
		m$saveseed <- seed
		if (!is.null(seed)) set.seed(seed)
	}
	if (!missing(wildcard)) m$wildcardclass <- wildcard
}
