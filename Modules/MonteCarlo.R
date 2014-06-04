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

# sample the distributions and propagate uncertainty according
# the to model definition
montecarlo <- function(N,mfile="",mexpr)
{
	pe <- new.env()
	pe$N <- N
	source("Modules/Boolean.R",local=pe)
	source("Modules/Dist.R",local=pe)
	source("Modules/CRAM.R",local=pe)
	source("Modules/CommonCause.R",local=pe)
	e <- new.env(parent=pe)
	if (missing(mexpr)) eval(parse(file=mfile),e)
	else eval(mexpr,e)
	Z <- as.data.frame(as.list(e))
	if (VerboseLevel >= 2) print(paste("montecarlo() complete on:",mfile))
	return(Z)
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
