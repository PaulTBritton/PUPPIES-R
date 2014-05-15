#
# Collection of misc user defined functions
#
# Author: Paul Thomas Britton
#
###################################


# advanced glob pattern to regular expression converter
aglob2rx <- function(pattern,trim.head=TRUE,trim.tail=TRUE) {
    p <- gsub("\\.", "\\\\.", paste("^", pattern, "$",sep=""))
    p <- gsub("\\?", ".", gsub("\\*", ".*", p))
#    p <- gsub("([^\\])\\(", "\\1\\\\(", p)
#    p <- gsub("([^\\])\\[", "\\1\\\\[", p)
    p <- gsub("([^\\])\\{", "\\1\\\\{", p)
    if (trim.tail) 
        p <- sub("\\.\\*\\$$", "", p)
    if (trim.head) 
        p <- sub("\\^\\.\\*", "", p)
    return(p)
}

# trim white space characters from a string
trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# return a subset of data based on a pass through filter in
# regex format
filterdata <-function(filter,data) data[grep(filter,names(data))]

#########################################
# reverse the order of Parameters and Equations to place them to an order 
# such that any data dependencies will coincide with the for-loop control
# logic of the script
#

# reverse the order of the rows in a data frame
rowrevorder <- function(df) {
	size <- nrow(df)
	tmp <- df
	rnames <- row.names(df)
	tmpnames <- rnames
	for (i in 1:size) {
		df[i,] <- tmp[size+1-i,]
		rnames[i] <- tmpnames[size+1-i]
	}
	row.names(df) <- rnames
#	if (is.data.frame(df)) print("is data.frame")
	return(df)
}

revorder <- function(lst) {
	size <- length(lst)
	tmp <-lst 
	enames <- names(lst)
	tmpnames <- enames
	for (i in 1:size) {
		lst[i] <- tmp[size+1-i]
		enames[i] <- tmpnames[size+1-i]
	}
	names(lst) <- enames
	return(lst)
}


# partially correlate lognormally distributed failure rates
# according to a correlation coefficient matrix
#Correlate <- function(M,FR,muFR,sigmaFR,corrfile) {
#	# correlation matrix (see illustration below)
#	#
#	#       BU PDCU CCSE
#	# BU
#	# PDCU    (Correlation
#	# CCSE     Coefficients)
#	#
#	Correl <- read.csv(corrfile,header=FALSE)
#	# cholesky matrix
#	U <- chol(as.matrix(Correl))
#
#	N <- length(FR[1,])
#	# log of FPMH
#	LFR <- array(0,c(M,N))
#	# correlated log of FPMH
#	LFRU <- array(0,c(M,N))
#	# correlated FPMH
#	CFR <- array(0,c(M,N))
#
#	for (i in 1:M) {
#		LFR[i,] <- (log(FR[i,])-muFR[i])/sigmaFR[i]
#	}
#	LFRU <- t(t(LFR[1:M,]) %*% U)
#	for (i in 1:M) {
#		CFR[i,] <- exp(LFRU[i,] * sigmaFR[i] + muFR[i])
#	}
#	return(CFR)
#}

# parse args.txt to extract global configuration arguments
# and place them in a list data structure
#parseArgsLines <- function(ArgsLines) {
#	return(list(N = as.integer(sub("N=","",
#			ArgsLines[grep("N=",ArgsLines)])),
#		seed = suppressWarnings(as.integer(sub("seed=","",
#			ArgsLines[grep("seed=",ArgsLines)]))),
#		PlotDir = sub("PlotDir=","",
#			ArgsLines[grep("PlotDir=",ArgsLines)]),
#		f = "",
#		d = "INPUTS/descriptions.csv",
#		p = "INPUTS/parameters.csv",
#		e = "INPUTS/equations.csv",
#		mode = "p",
#		fieldsep = ";"
##		fieldsep = sub("fieldsep=","",
##			ArgsLines[grep("fieldsep=",ArgsLines)]),
#		)
#	)
#}
#
#parseCargsLines <- function(cargs) {
#	dargs <- parseArgsLines("Modules/args.txt")
#	args <- list(f="",d="INPUTS/descriptions.csv",fieldsep=";",
#		plot="plot.tiff",p="INPUTS/parameters.csv",
#		e="INPUTS/equations.csv",N=100,s=NULL,mode="p")
#	if (max(grepl("^plot=",cargs))) {
#		args$plot <- sub("plot=","",cargs[grep("^plot=",cargs)])
#	}
#	if (max(grepl("^f=",cargs))) {
#		args$f <- sub("f=","",cargs[grep("^f=",cargs)])
#	}
#	if (max(grepl("^p=",cargs))) {
#		args$p <- sub("p=","",cargs[grep("^p=",cargs)])
#	}
#	if (max(grepl("^e=",cargs))) {
#		args$e <- sub("e=","",cargs[grep("^e=",cargs)])
#	}
#	# number of samples from command line
#	if (max(grepl("^N=",cargs))) {
#		args$N <- as.integer(sub("N=","",cargs[grep("^N=",cargs)]))
#	}
#	if (max(grepl("^s=",cargs))) {
#		args$s <- suppress.Warnings(as.integer(sub("s=","",
#				cargs[grep("^s=",cargs)])))
#	}
#	if (max(grepl("^mode=",cargs))) {
#		args$mode <- sub("mode=","",
#			cargs[grep("^mode=",cargs)])
#	}
#	return(args)
#}

