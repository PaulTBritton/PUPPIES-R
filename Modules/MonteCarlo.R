##########################################################
#
# monte carlo simulation functions:
#
# Author: Paul Thomas Britton
#
###################################

# initialize evaluation environment for evaluating
# PUPPIES models
initevalenv <- function(p) {
	p$testname <- "default"
	eval(expression(setname <- function(x) {print(paste("setname:",testname))
						print(paste("setname:",x))
						testname <<- x}),p)
	sys.source("Modules/Boolean.R",envir=p)
	sys.source("Modules/Dist.R",envir=p)
	sys.source("Modules/CRAM.R",envir=p)
	sys.source("Modules/CommonCause.R",envir=p)
}

# evaluate a PUPPIES model with N random iterations
# return a list containing the model name and the model results
evalp <- function(N,seed=NULL,pname="PUPPIES Model",
		pfile="",pexpr=parse(file=pfile))
{
	if (!is.null(seed)) set.seed(seed)
	p <- new.env()
	p$N <- N
	initevalenv(p)
	e <- new.env(parent=p)
	eval(pexpr,e)
y <- get("testname",e)
print(paste("get:",y))
	if (VerboseLevel >= 2) print(paste("evalp() complete on:",pname))
	return(list(n=pname,r=e))
}

# propagate results x from one PUPPIES model into
# another PUPPIES model and append the new results to x
appendp <- function(x,pfile="",pexpr=parse(file=pfile)) {
	eval(pexpr,x$r)
}

# return a clone of a list of model name and model results
clonep <- function(p) {
	e <- as.environment(as.list(p$r,all.names=TRUE))
	parent.env(e) <- parent.env(p$r)
	return(list(n=p$n,r=e))
}

# propagate results x from from one PUPPIES model into
# another PUPPIES model and return the only the new results
spawnp <- function(p,pname="PUPPIES Model",pfile="",
		pexpr=parse(file=pfile)) {
	e <- clonep(p)
	eval(pexpr,e$r)
	rm(list=ls(p$r),envir=e$r)	# keep only the new results
	return(list(n=pname,r=e$r))
}

# evaluate several PUPPIES models (described by filter)
# with N random iteration by appending them into one set of results
# return a list containing the model name and the (combine) model results
superevalp <- function(N,seed=NULL,pname="PUPPIES Model",filter) {
	if (VerboseLevel > 0) print(paste("superevalp() matching:",filter))
	class(filter) <- wildcardclass
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	if (VerboseLevel > 0) print(paste("superevalp() found:",LF[1]))
	x <- evalp(N=N,seed=seed,pname=pname,pfile=LF[1])
	for (i in LF[-1]) {
		if (VerboseLevel > 0) print(paste("superevalp() found:",i))
		appendp(x=x,pfile=i)
	}
	if (VerboseLevel >= 2) print(paste("superevalp() complete on:",filter))
	return(x)
}

# propagate results x from one PUPPIES model into several other
# PUPPIES models (described by filter) and append the new results to x
superappendp <- function(x,filter) {
	if (VerboseLevel > 0) print(paste("superappendp() matching:",filter))
	class(filter) <- wildcardclass
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	for (i in LF) {
		if (VerboseLevel > 0) print(paste("superappendp() found:",i))
		appendp(x,i)
	}
	if (VerboseLevel >= 2) print(paste("superappendp() complete on:",filter))
}
