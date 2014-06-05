##########################################################
#
# monte carlo functions:
#
# Author: Paul Thomas Britton
#
###################################


#montecarlo_read <- function(mfile) {
#	if (VerboseLevel > 0) print(paste("montecarlo_read() reading:",mfile))
#	if (file.access(mfile,mode=4)) {
#		stop(paste("File access error:",mfile))
#	}
#	modeldef <- read.csv(mfile,sep=args$fieldsep,quote=NULL)#,row.names=1)
#	if (VerboseLevel > 0) print(modeldef)
#	return(modeldef)
#}

# Create a new PUPPIES model
# sample the distributions and propagate uncertainty according
# the to model definition
newpm <- function(N,seed=NULL,pmname="PUPPIES Model",
		pmfile="",mexpr=parse(file=pmfile))
{
	if (!is.null(seed)) set.seed(seed)
	p <- new.env()
	p$N <- N
print(getwd())
	sys.source("Modules/Boolean.R",envir=p)
	sys.source("Modules/Dist.R",envir=p)
	sys.source("Modules/CRAM.R",envir=p)
	sys.source("Modules/CommonCause.R",envir=p)
	e <- new.env(parent=p)
	eval(mexpr,e)
#	Z <- as.data.frame(as.list(e))
	if (VerboseLevel >= 2) print(paste("newpm() complete on:",pmname))
#	return(Z)
	return(list(n=pmname,m=e))
}

appendpm <- function(pm,pmfile="",mexpr=parse(file=pmfile)) {
	eval(mexpr,pm$m)
}

spawnpm <- function(pm,pmname="PUPPIES Model",
		pmfile="",mexpr=parse(file=pmfile)) {
	e <- as.environment(as.list(pm$m,all.names=TRUE))
	eval(mexpr,e)
	return(list(n=pmname,m=e))
}

montecarlo_batch <- function(N,filter) {
	if (VerboseLevel > 0) print(paste("montecarlo() matching:",filter))
	class(filter) <- wildcardclass
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	for (i in LF) {
		if (VerboseLevel > 0) print(paste("montecarlo() found:",i))
		newone <- montecarlo(N,i)
		if (exists("X")) X <- cbind(newone,X[!(X %in% newone)])
		else X <- newone
	}
	if (VerboseLevel >= 2) print(paste("montecarlo() complete on:",filter))
	return(X)
}
