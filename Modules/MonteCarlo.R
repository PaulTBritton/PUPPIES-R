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

# evaluate a PUPPIES model with N random iteration
evalpm <- function(N,seed=NULL,pmname="PUPPIES Model",
		pmfile="",mexpr=parse(file=pmfile))
{
	if (!is.null(seed)) set.seed(seed)
	p <- new.env()
	p$N <- N
	#print(getwd())
	sys.source("Modules/Boolean.R",envir=p)
	sys.source("Modules/Dist.R",envir=p)
	sys.source("Modules/CRAM.R",envir=p)
	sys.source("Modules/CommonCause.R",envir=p)
	e <- new.env(parent=p)
	eval(mexpr,e)
	if (VerboseLevel >= 2) print(paste("evalpm() complete on:",pmname))
	return(list(n=pmname,r=e))
}

# propagate results from from one PUPPIES model into
# another PUPPIES model and append the new results to the old results
appendpm <- function(pm,pmfile="",mexpr=parse(file=pmfile)) {
	eval(mexpr,pm$r)
}

# propagate results from from one PUPPIES model into
# another PUPPIES model and return the only the new results
spawnpm <- function(pm,pmname="PUPPIES Model",pmfile="",
		mexpr=parse(file=pmfile)) {
	e <- as.environment(as.list(pm$r,all.names=TRUE))
	parent.env(e) <- parent.env(pm$r)
	eval(mexpr,e)
	rm(list=ls(pm$r),envir=e)
	return(list(n=pmname,r=e))
}

batch_evalpm <- function(N,seed=NULL,pmname="PUPPIES Model",filter) {
	if (VerboseLevel > 0) print(paste("batch_evalpm() matching:",filter))
	class(filter) <- wildcardclass
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	model <- evalpm(N,seed,pmname,pmfile=LF[1])
	for (i in LF[-1]) {
		if (VerboseLevel > 0) print(paste("batch_evalpm() found:",i))
		appendpm(pm=model,pmfile=i)
#		if (exists("X")) X <- cbind(newone,X[!(X %in% newone)])
#		else X <- newone
	}
	if (VerboseLevel >= 2) print(paste("batch_evalpm() complete on:",filter))
	return(model)
}

batch_appendpm <- function(pm,filter) {
	if (VerboseLevel > 0) print(paste("batch_appendpm() matching:",filter))
	class(filter) <- wildcardclass
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	for (i in LF) {
		if (VerboseLevel > 0) print(paste("batch_appendpm() found:",i))
		appendpm(pm,i)
	}
}
