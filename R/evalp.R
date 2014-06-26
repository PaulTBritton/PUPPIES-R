##########################################################
#
# Evaluate monte carlo simulation functions:
#
# Author: Paul Thomas Britton
#
###################################

# initialize meta data environment for
# PUPPIES models
#metaenv <- function(name,iterations,seed,wildcard) {
#	m <- new.env(parent=puppiesenv)
#	m$N <- iterations
#	m$saveseed <- seed	# save seed
#	if (!is.null(seed)) set.seed(seed)
#	m$modelname <- name
#	m$wildcardclass <- wildcard
#	return(m)
#}

# evaluate a PUPPIES model with N random iterations
# return the model results p
evalp <- function(name="PUPPIES Model",iterations=10,seed=NULL,
		wildcard="aglob",model="")
{
	p <- new.env(parent=puppiesenv)
	attr(p,"modelname") <- name
	attr(p,"N") <- iterations
	attr(p,"saveseed") <- seed
	attr(p,"wildcardclass") <- wildcard
	pexpr <- topexpr(model)
	eval(pexpr,p)
#	y <- get("modelname",p)
	y <- attr(p,"modelname")
	class(p) <- c("environment","puppies")
	if (VerboseLevel >= 2) print(paste("evalp() complete on:",y))
	return(p)
}

# propagate results p from one PUPPIES model into
# another PUPPIES model and append the new results to p
appendp <- function(p=parent.frame(),model="") {
	pexpr <- topexpr(model)
	eval(pexpr,p)
}

# return a clone e of the model results p
# set the parent of e to be the parent of p
clonep <- function(p=parent.frame(),name="Copy Model") {
	pp <- parent.env(p)
	ppp <- parent.env(pp)
	e <- as.environment(as.list(p,all.names=TRUE))
	parent.env(e) <- as.environment(as.list(pp,all.names=TRUE))
	x <- parent.env(e)
        parent.env(x) <- ppp
        x$modelname <- name
	class(e) <- class(p)
	return(e)
}

# propagate results p from from one PUPPIES model into
# another PUPPIES model and return the only the new results
spawnp <- function(p,name="PUPPIES Model",model="") {
	pexpr <- topexpr(model)
	e <- clonep(p,name)
	eval(pexpr,e)
	rm(list=ls(p),envir=e)	# keep only the new results
	return(e)
}

# evaluate several PUPPIES models (described by filter)
# with N random iteration by appending them into one set of results
# return a list containing the model name and the (combine) model results
superevalp <- function(name="PUPPIES Model",iterations=10,seed=NULL,
		wildcard="aglob",filter="")
{
	if (VerboseLevel > 0) print(paste("superevalp() matching:",filter))
	class(filter) <- wildcard
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	if (VerboseLevel > 0) print(paste("superevalp() found:",LF[1]))
	x <- evalp(name=name,iterations=iterations,seed=seed,
		wildcard=wildcard,model=LF[1])
	for (i in LF[-1]) {
		if (VerboseLevel > 0) print(paste("superevalp() found:",i))
		appendp(p=x,model=i)
	}
	if (VerboseLevel >= 2) print(paste("superevalp() complete on:",filter))
	return(x)
}

# propagate results x from one PUPPIES model into several other
# PUPPIES models (described by filter) and append the new results to x
superappendp <- function(p,filter="") {
	if (VerboseLevel > 0) print(paste("superappendp() matching:",filter))
#	wildcardclass <- get("wildcardclass",p)
	wildcardclass <- attr(p,"wildcardclass")
	class(filter) <- wildcardclass
#	LF <- list.files(pattern=torx(filter),full.names=TRUE)
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	for (i in LF) {
		if (VerboseLevel > 0) print(paste("superappendp() found:",i))
		appendp(p=p,model=i)
	}
	if (VerboseLevel >= 2) print(paste("superappendp() complete on:",filter))
}
