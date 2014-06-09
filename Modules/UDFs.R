#
# Collection of misc user defined functions
#
# Author: Paul Thomas Britton
#
###################################

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

