##########################################################
#
# monte carlo simulation functions:
#
# Author: Paul Thomas Britton
#
###################################

# initialize evaluation environment for
# PUPPIES models
initevalenv <- function(p,N,seed,pname,wildcard) {
	p$N <- N
	p$seed <- seed	# save seed
	if (!is.null(seed)) set.seed(seed)
	p$pname <- pname
	p$wildcardclass <- wildcard
	sys.source("Modules/metadata.R",envir=p)
	sys.source("Modules/boolean.R",envir=p)
	sys.source("Modules/distributions.R",envir=p)
	sys.source("Modules/cram.R",envir=p)
	sys.source("Modules/commoncause.R",envir=p)
}

# evaluate a PUPPIES model with N random iterations
# return the model results
evalp <- function(N=1,seed=NULL,pname="PUPPIES Model",model="",
		wildcard="aglob")
{
	p <- new.env()
	initevalenv(p,N,seed,pname,wildcard)
	e <- new.env(parent=p)
	pexpr <- topexpr(model)
	eval(pexpr,e)
	y <- get("pname",e)
	if (VerboseLevel >= 2) print(paste("evalp() complete on:",y))
	return(e)
}

# propagate results x from one PUPPIES model into
# another PUPPIES model and append the new results to x
appendp <- function(p,model="") {
	pexpr <- topexpr(model)
	eval(pexpr,p)
}

# return a clone e of the model results p
# set the parent of e to be the parent of p
clonep <- function(p) {
	e <- as.environment(as.list(p,all.names=TRUE))
	parent.env(e) <- parent.env(p)
	return(e)
}

# propagate results x from from one PUPPIES model into
# another PUPPIES model and return the only the new results
spawnp <- function(p,pname="PUPPIES Model",model="") {
	pexpr <- topexpr(model)
	e <- clonep(p)
	eval(pexpr,e)
	rm(list=ls(p),envir=e)	# keep only the new results
	return(e)
}

# evaluate several PUPPIES models (described by filter)
# with N random iteration by appending them into one set of results
# return a list containing the model name and the (combine) model results
superevalp <- function(N,seed=NULL,pname="PUPPIES Model",filter="",
		wildcard="aglob") {
	if (VerboseLevel > 0) print(paste("superevalp() matching:",filter))
	class(filter) <- wildcard
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	if (VerboseLevel > 0) print(paste("superevalp() found:",LF[1]))
	x <- evalp(N=N,seed=seed,pname=pname,model=LF[1])
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
	wildcardclass <- get("wildcardclass",p)
	class(filter) <- wildcardclass
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	for (i in LF) {
		if (VerboseLevel > 0) print(paste("superappendp() found:",i))
		appendp(p=p,model=i)
	}
	if (VerboseLevel >= 2) print(paste("superappendp() complete on:",filter))
}
