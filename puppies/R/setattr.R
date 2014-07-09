##########################################################
#
# Meta Data manipulation functions:
#
# Author: Paul Thomas Britton
#
###################################

# set puppies model meta data values inside the meta environment
# of the model
#metadata <- function(name,iterations,seed,wildcard) {
#	m <- parent.env(parent.frame())
#	if (!missing(name)) m$modelname <- name
#	if (!missing(iterations)) m$N <- iterations
#	if (!missing(seed)) {
#		m$saveseed <- seed
#		if (!is.null(seed)) set.seed(seed)
#	}
#	if (!missing(wildcard)) m$wildcardclass <- wildcard
#}
#puppiesenv$setattr <- function(p=parent.frame(),name,iterations,seed,wildcard,
setattr <- function(p=parent.frame(),name,iterations,seed,wildcard,
			verbose)
{
	if (!missing(name)) attr(p,"modelname") <- name
	if (!missing(iterations)) attr(p,"N") <- iterations
	if (!missing(seed)) {
		attr(p,"saveseed") <- seed
		if (!is.null(seed)) set.seed(seed)
	}
	if (!missing(wildcard)) attr(p,"wildcardclass") <- wildcard
	if (!missing(verbose)) attr(p,"VerboseLevel") <- verbose
}
