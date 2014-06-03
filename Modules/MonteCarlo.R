##########################################################
#
# monte carlo functions:
#
# theoretical distributions functions
# emperical distribution procedure
# user defined "Call Functions"
#	-Logic gates:
#		- AND, OR
#	-Math operations <- replace with EXPR?
#		- MULT, ADD, POW, EXPR(trumps old TRAN)
#			 [old - TRAN(combination of all three)]
#	-Expression evaluator (takes advantage of propagate module)
#		- EXPR
# Monte Carlo engine: montecarlo(),UDist(),CallFunc()
#
# Author: Paul Thomas Britton
#
###################################


#############################################################
# various theoretical distributions
#

# constant vector
#consV <- function(c=0) rep(c,times=N)

# uniform distribution
unifD <- function(a=0,b=1) runif(N,a,b)

# create a vector of N trianglular samples
# a = min
# b = mode
# c = max
triaD <- function(a,b,c) {
	if (!((0<=a)&&(a<=b)&&(b<=c)&&(c<=1))) {
		stop(paste(row.names(P),"tD:",
			"Invalid Triangle Districution Parameters"))
	}
	U <- runif(N,0,1)
	x <- (b-a)/(c-a)
	Tri <- U <= x
	Tri[Tri==TRUE] <- a+sqrt(U[U<=x]*(c-a)*(b-a))
	Tri[Tri==FALSE] <- c-sqrt((1-U[U>x])*(c-a)*(c-b))
	return(Tri)
}

##################################################
#
# Various Operations on Event Distributions
#


#################
# Advanced Logical operation filters for probabilities
#

# logical OR operation filter for probabilities
#OR <- function(filter) {
#	class(filter) <- wildcardclass
#	X <- filterdata(torx(filter),Z)
#	return(1-apply(1-X,1,prod))
#}

# logical AND operation filter for probabilities
#AND <- function(filter) {
#	class(filter) <- wildcardclass
#	X <- filterdata(torx(filter),Z)
#	return(apply(X,1,prod))
#}


#######################################
#
# evaluate an expressions within the parameter file 
#
#EXPR <- function(Z,P) {
#	EXPRdf <- as.data.frame(list(expression=as.character(P$f)))
#	row.names(EXPRdf) <- list("ExpressionString")
#	ExprResult <- propagate3(EXPRdf,Z)
#	return(ExprResult[,1])
#}

#################
# access script information
#
#getN <- function(Z,P) {
#	return(N)
#}


##########################################################
# main monte carlo functions
#

# call the function f that matches the string 'func'.
# The remaining arguments are passed to f.
#CallFunc <- function(func,...) {
#	if (VerboseLevel == 3) {
#		dots <- list(...)
#		print("Inside CallFunc for:")
#		print(dots[2])
#	}
#	fun <- as.character(func)
#	if (VerboseLevel == 3) {
#		print(paste("Attempting to find function:",fun))
#	}
#	f <- match.fun(fun)
#	if (VerboseLevel == 3) {
#		print(paste("Exiting CallFunc, ",
#		 "and attempting to call the function: ",fun,sep=""))
#	}
#	return(f(...))
#}

# UDist need to eliminate this in order to allow for more flexibility
# in applying multiple missions times to the same failure rate sample
#
# Generate uncertainty distributions from parameter data.
# The function along with CallFunc act like a switchboard
# from the parameter data inputfile to the uncertainty distribution functions
#UDist <- function(N,P) {
#	S <- CallFunc(P$dist,N,P)
#	X <- switch(as.character(P$calc),
#		I = S,			# identity: use samples as is
#		e = { if (P$t == 0) warning(paste(row.names(P),": t = 0"))
#		pexp(P$t,S) },	# constant rate: apply exponential cdf
#		w = { if (P$t == 0) warning(paste(row.names(P),": t = 0"))
#		pweibull(P$t,P$c,1/S) } # variable rate: apply weibull cdf
#	)
#	return(X)
#}

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

montecarlo_read <- function(mfile) {
	if (VerboseLevel > 0) print(paste("montecarlo_read() reading:",mfile))
	if (file.access(mfile,mode=4)) {
		stop(paste("File access error:",mfile))
	}
	modeldef <- read.csv(mfile,sep=args$fieldsep,quote=NULL)#,row.names=1)
#	if (VerboseLevel > 0) print(modeldef)
#	Z <- montecarlo3(N,modeldef)
#	if (VerboseLevel >= 2) print(paste("montecarlo() complete on:",mfile))
print(tail(modeldef,-1))
	return(tail(modeldef,-1))
}

# sample the distributions defined by the parameter data
montecarlo <- function(N,mfile)
{
# N beta samples
betaD <- function(alpha,beta) {
	return(rbeta(N,alpha,beta))
}

# N gamma samples
gammD <- function(shape,rate) {
	return(rgamma(N,shape,rate))
}

# create a vector of N lognormal samples
# median = 50th
# EF = 95th/50th
lognD <- function(median,EF) {
	mu <- log(median)
	sigma <- log(EF)/qnorm(0.95)
	return(rlnorm(N,mu,sigma))
}

####################################################
# create a vector of N samples based on an emperical distribution
# (first fit a cdf based on data then spline fit the inverse cdf)
empeD <- function(empfile) {
	if (VerboseLevel >= 2) print(paste("Loading Emperical Data:",empfile))
	res <- 10*N + 1
	X <- seq(0,1,length.out=res)
	if (file.access(empfile,mode=4)) {
		stop(paste("File access error:",empfile))
	}
	D <- scan(empfile)
	Qfun <- splinefun(ecdf(D)(X),X)
	if (VerboseLevel >= 2) print(paste("Piece-wise spline fit complete:",
					empfile))
	return(Qfun(runif(N,0,1)))
}

	# save R language defaults to keep functionality for
	# if-statements, etc. (?forced to do this because
	# locally defined functions seem to persist globally!)
	defaultOR <- '|'
	defaultAND <- '&'
	# overload logical operators to operate on
	# probability distrubutions
	'|' <- ORgate
	'&' <- ANDgate

	modeldef <- rowrevorder(montecarlo_read(mfile))
print(modeldef)
	# define and initialize the data structure that will
	# contain the vectors of monte carlo samples for the
	# parameters
	size <- nrow(modeldef)
print(size)
	Z <- as.data.frame(array(0,c(N,size)))
#	names(Z) <- row.names(Params)
	e <- new.env()
	e$N <- N
	for (i in 1:size) {
print(modeldef[[i,1]])
print(modeldef[[i,2]])
#	Z <- as.data.frame(as.list(e))
		code <- parse(text=as.character(modeldef[[i,2]]))
		assign(as.character(modeldef[[i,1]]),eval(code,e),e)
	}

	# consider replacing switch statement with generic functions
	#
	# sample uncertainty distributions from parameter data
#	for (i in names(Z)) {
#		P <- Params[i,]
#		Z[i] <- switch(as.character(P$dist),
#			CF={ j <- match(i,names(Z))
#			#call function
#			CallFunc(P$calc,Z[1:ifelse(j>1,j-1,j)],P)},
#			#uncertainty distribution
#			{ if (!is.null(seed)) {
#				if (VerboseLevel == 3) {
#					print(paste("Setting seed =",seed))
#				}
#				set.seed(seed)		#set seed
#				seed <- seed + 1}	#proactively update seed
#			UDist(N,P)})			#uncertainty dist
#	}
#	return(revorder(Z))
	# explicity remove overloaded operators
	rm('|')
	rm('&')
	# restore R language defaults
	'|' <- defaultOR
	'&' <- defaultAND
	print(as.list(e))
	Z <- as.data.frame(as.list(e))
	print(Z)
	return(revorder(Z))
}
