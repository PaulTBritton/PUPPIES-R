##########################################################
#
# Meta Data manipulation functions:
#
# Author: Paul Thomas Britton
#
###################################

# set puppies model meta data values inside the meta environment
# of the model
metadata <- function(name,iterations,seed,wildcard) {
	m <- parent.env(parent.frame())
	if (!missing(name)) m$modelname <- name
	if (!missing(iterations)) m$N <- iterations
	if (!missing(seed)) {
		m$saveseed <- seed
		if (!is.null(seed)) set.seed(seed)
	}
	if (!missing(wildcard)) m$wildcardclass <- wildcard
}
