##########################################################
#
# monte carlo functions:
#
# Author: Paul Thomas Britton
#
###################################


montecarlo_read <- function(mfile) {
	if (VerboseLevel > 0) print(paste("montecarlo_read() reading:",mfile))
	if (file.access(mfile,mode=4)) {
		stop(paste("File access error:",mfile))
	}
	modeldef <- read.csv(mfile,sep=args$fieldsep,quote=NULL)#,row.names=1)
	if (VerboseLevel > 0) print(modeldef)
	return(modeldef)
}

# sample the distributions defined by the parameter data
montecarlo <- function(N,mfile)
{
	modeltmp <- montecarlo_read(mfile)
	I <- as.vector(modeltmp[,1]) # save the order of the data
	modeldef <- rowrevorder(modeltmp)
	# define and initialize the data structure that will
	# contain the vectors of monte carlo samples for the
	# parameters
	size <- nrow(modeldef)
	Z <- as.data.frame(array(0,c(N,size)))
	pe <- new.env()
	pe$N <- N
	source("Modules/Boolean.R",local=pe)
	source("Modules/Dist.R",local=pe)
	source("Modules/CRAM.R",local=pe)
	source("Modules/CommonCause.R",local=pe)
	e <- new.env(parent=pe)
	for (i in 1:size) {
		code <- parse(text=as.character(modeldef[[i,2]]))
		assign(as.character(modeldef[[i,1]]),eval(code,e),e)
	}
#	print(as.list(e))
	Z <- as.data.frame(as.list(e))
#	print(Z)
	if (VerboseLevel >= 2) print(paste("montecarlo() complete on:",mfile))
	return(Z[,I])
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
