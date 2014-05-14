##########################################################
#
# propagate function
#
# Author: Paul Thomas Britton
#
###################################

propagate <- function(filter,psamples) {
	if (VerboseLevel > 0) print(paste("propagate() matching:",filter))
	if (wildcardlevel == 0) filter <- aglob2rx(filter)
	LF <- list.files(pattern=filter,recursive=TRUE,full.names=TRUE)
	for (i in LF) {
		if (VerboseLevel > 0) print(paste("propagate() found:",i))
		newone <- propagate2(i,psamples)
		if (exists("Y")) Y <- cbind(newone,Y[!(Y %in% newone)])
		else Y <- newone
	}
	if (VerboseLevel >= 2) print(paste("propagate() complete on:",filter))
	return(Y)
}

propagate2 <- function(efile,psamples) {
	if (VerboseLevel > 0) print(paste("propagate() reading:",efile))
	if (grepl(".*\\.pf",efile)) {
		if (VerboseLevel > 0) print("propagate(): Calling ft2eqn()")
		EQNS <- ft2eqn(efile)
	} else {
		if (file.access(efile,mode=4)) {
			stop(paste("File access error:",efile))
		}
		EQNS <- read.csv(efile,sep=args$fieldsep,row.names=1)
	}
	if (VerboseLevel > 0) print(EQNS)
	Z <- propagate3(EQNS,psamples)
	if (VerboseLevel >= 2) print(paste("propagate() complete on:",efile))
#	return(cbind(Z,psamples))
	return(Z)
}

# propagate uncertainty into the expressions and evaluate
propagate3 <- function(EquationsList,ParamSamples) {
	# save R language defaults to keep functionality for
	# if-statements, etc. (?forced to do this because
	# locally defined functions seem to persist globally!)
	defaultOR <- '|'
	defaultAND <- '&'
	# overload logical operators to operate on
	# probability distrubutions
	'|' <- ORgate
	'&' <- ANDgate

	EquationsList <- rowrevorder(EquationsList)
	ParamSamples <- revorder(ParamSamples)

	# create objects for each parameter
	for (indexParam in names(ParamSamples)) {
		newsample <- ParamSamples[[indexParam]]
		newcode <- paste(indexParam,"<- newsample")
		eval(parse(text=newcode))
	}

	# initialize data structure for results
	ResultsList <- as.data.frame(array(0,c(nrow(ParamSamples),
			nrow(EquationsList))))
	names(ResultsList) <- row.names(EquationsList)

	# create objects for each expression evaluation result
	for (indexEQN in names(ResultsList)) {
		ithEquation <- as.character(EquationsList[[indexEQN,1]])
		newResult <- eval(parse(text=ithEquation))
		ResultsList[indexEQN] <- newResult
		newcode <- paste(indexEQN,"<- newResult")
		eval(parse(text=newcode))
	}
	# explicity remove overloaded operators
	rm('|')
	rm('&')
	# restore R language defaults
	'|' <- defaultOR
	'&' <- defaultAND
	return(revorder(ResultsList))
}
