##########################################################
#
# monte carlo functions:
#
# theoretical distributions functions
# emperical distribution procedure
# user defined "Call Functions"
#	-Logic gates:
#		- AND, OR
#	-Math operations
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

# uniform distribution
uD <- function(N,P) runif(N,as.numeric(P$a),as.numeric(P$b))

# create a vector of N trianglular samples
# a = min
# c = mode
# b = max
tD <- function(N,P) {
	a <- as.numeric(P$a)
	c <- as.numeric(P$b)
	b <- as.numeric(P$c)
	if (!((0<=a)&&(a<=c)&&(c<=b)&&(b<=1))) {
		stop(paste(row.names(P),"tD:",
			"Invalid Triangle Distribution Parameters"))
	}
	U <- runif(N,0,1)
	x <- (c-a)/(b-a)
	Tri <- U <= x
	Tri[Tri==TRUE] <- a+sqrt(U[U<=x]*(b-a)*(c-a))
	Tri[Tri==FALSE] <- b-sqrt((1-U[U>x])*(b-a)*(b-c))
	return(Tri)
}

# N beta samples
bD <- function(N,P) {
	a<- as.numeric(P$a)
	b<- as.numeric(P$b)
	return(rbeta(N,a,b))
}

# N gamma samples
gD <- function(N,P) {
	shape <- as.numeric(P$a)
	rate <- as.numeric(P$b)
	return(rgamma(N,shape,rate))
}

# create a vector of N lognormal samples
# MED = median
# EF = 95th/50th
lD <- function(N,P) {
	MED <- as.numeric(P$a)
	EF <- as.numeric(P$b)
	mu <- log(MED)
	sigma <- log(EF)/qnorm(0.95)
	return(rlnorm(N,mu,sigma))
}

####################################################
# create a vector of N samples based on an emperical distribution
# (first fit a cdf based on data then spline fit the inverse cdf)
eD <- function(N,P) {
	empfile <- as.character(P$f)
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

##################################################
#
# Various Operations on Event Distributions
#


#######################
# ? Load data from file and apply it directly in the simulation
#DATA <- function(Z,P) scan(as.character(P$f))

#################
# Logical operations
#

# logical OR operation for compound event distributions
OR <- function(Z,P) {
	filter <- as.character(P$f)
	filter <- switch(wildcardlevel,newglob(filter),newaglob(filter),filter)
	X <- filterdata(torx(filter),Z)
	return(1-apply(1-X,1,prod))
}

# logical AND operation for compound event distributions
AND <- function(Z,P) {
	filter <- as.character(P$f)
	filter <- switch(wildcardlevel,newglob(filter),newaglob(filter),filter)
	X <- filterdata(torx(filter),Z)
	return(apply(X,1,prod))
}

#################
# Mathematical operations
#

# multiply by constant: a*X
MULT <- function(Z,P) {
	a <- as.numeric(P$a)
	X <- Z[[as.character(P$f)]]
	return(a*X)
}

# add constant: X+b
ADD <- function(Z,P) {
	b <- as.numeric(P$b)
	X <- Z[[as.character(P$f)]]
	return(X+b)
}

# raise to power: X^c
POW <- function(Z,P) {
	c <- as.numeric(P$c)
	X <- Z[[as.character(P$f)]]
	return(X^c)
}

# Note: this TRAN is trumped by EXPR below.  Additionally, TRAN is used
# represent Transfer Gate in the PUPPIES fault tree format.
# Apply the transformation: T(X)=a*X^c+b
#TRAN <- function(Z,P) {
#	a <- as.numeric(P$a)
#	b <- as.numeric(P$b)
#	c <- as.numeric(P$c)
#	X <- Z[[as.character(P$f)]]
#	return(a*X^c+b)
#}

#######################################
#
# evaluate an expressions within the parameter file 
#
EXPR <- function(Z,P) {
	EXPRdf <- as.data.frame(list(expression=as.character(P$f)))
	row.names(EXPRdf) <- list("ExpressionString")
	ExprResult <- propagate3(EXPRdf,Z)
	return(ExprResult[,1])
}

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
CallFunc <- function(func,...) {
	if (VerboseLevel == 3) {
		dots <- list(...)
		print("Inside CallFunc for:")
		print(dots[2])
	}
	fun <- as.character(func)
	if (VerboseLevel == 3) {
		print(paste("Attempting to find function:",fun))
	}
	f <- match.fun(fun)
	if (VerboseLevel == 3) {
		print(paste("Exiting CallFunc, ",
		 "and attempting to call the function: ",fun,sep=""))
	}
	return(f(...))
}

# Generate uncertainty distributions from parameter data.
# The function along with CallFunc act like a switchboard
# from the parameter data inputfile to the uncertainty distribution functions
UDist <- function(N,P) {
	S <- CallFunc(P$dist,N,P)
	X <- switch(as.character(P$calc),
		I = S,			# identity: use samples as is
		e = { if (P$t == 0) warning(paste(row.names(P),": t = 0"))
		pexp(P$t,S) },	# constant rate: apply exponential cdf
		w = { if (P$t == 0) warning(paste(row.names(P),": t = 0"))
		pweibull(P$t,P$c,1/S) } # variable rate: apply weibull cdf
	)
	return(X)
}

montecarlo <- function(N,filter) {
	if (VerboseLevel > 0) print(paste("montecarlo() matching:",filter))
# removing wildcardlevel from global config
#	if (wildcardlevel == 0) filter <- aglob2rx(filter)
	filter <- switch(wildcardlevel,newglob(filter),newaglob(filter),filter)
	LF <- list.files(pattern=torx(filter),recursive=TRUE,full.names=TRUE)
	for (i in LF) {
		if (VerboseLevel > 0) print(paste("montecarlo() found:",i))
		newone <- montecarlo2(N,i)
		if (exists("X")) X <- cbind(newone,X[!(X %in% newone)])
		else X <- newone
	}
	if (VerboseLevel >= 2) print(paste("montecarlo() complete on:",filter))
	return(X)
}

montecarlo2 <- function(N,pfile) {
	if (VerboseLevel > 0) print(paste("montecarlo() reading:",pfile))
	if (file.access(pfile,mode=4)) {
		stop(paste("File access error:",pfile))
	}
	Params <- read.csv(pfile,sep=args$fieldsep,row.names=1)
	if (VerboseLevel > 0) print(Params)
	Z <- montecarlo3(N,Params)
	if (VerboseLevel >= 2) print(paste("montecarlo() complete on:",pfile))
	return(Z)
}

# sample the distributions defined by the parameter data
montecarlo3 <- function(N,Params)
{
	Params <- rowrevorder(Params)
	# define and initialize the data structure that will
	# contain the vectors of monte carlo samples for the
	# parameters
	Z <- as.data.frame(array(0,c(N,nrow(Params))))
	names(Z) <- row.names(Params)

	# sample uncertainty distributions from parameter data
	for (i in names(Z)) {
		P <- Params[i,]
		Z[i] <- switch(as.character(P$dist),
			CF={ j <- match(i,names(Z))
			#call function
			CallFunc(P$calc,Z[1:ifelse(j>1,j-1,j)],P)},
			#uncertainty distribution
			{ if (!is.null(seed)) {
				if (VerboseLevel == 3) {
					print(paste("Setting seed =",seed))
				}
				set.seed(seed)		#set seed
				seed <- seed + 1}	#proactively update seed
			UDist(N,P)})			#uncertainty dist
	}
	return(revorder(Z))
}
