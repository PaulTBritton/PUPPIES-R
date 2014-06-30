#
# Collection of file input and output procedures
#
# Author: Paul Thomas Britton
#
###################################


#######################################
# Model input functions
#

ft2eqn <- function(filename,VerboseLevel=1) {
	if (file.access(filename,mode=4)) {
		stop(paste("File access error:",filename))
	}
	if (VerboseLevel > 0) {
		print(paste("ft2eqn: Converting fault tree,",
			filename,", into equations"))
	}
	X <- paste(readLines(filename),collapse="")
	if (VerboseLevel >= 2) {
		X <- scan(text=X,what="character",sep="$")
		print(paste("ft2eqn() parsing",filename,
				"into subtrees"))
		print(X)
	} else {
		X <- scan(text=X,what="character",sep="$",quiet=TRUE)
	}
	size <- length(X)
	I <- array("",c(size))
	for (i in 1:size) {
		if (VerboseLevel >= 2) {
			Y <- scan(text=X[i],what="character",sep=";")
			print(paste("ft2eqn() converting",Y[1],
			"into a boolean expression"))
		} else {
			Y <- scan(text=X[i],what="character",sep=";",
				quiet=TRUE)
		}
		if (Y[2] != "EXPR") {
			Y[3] <- gsub("NOT ([^,\n]*)","NOT(\\1)",Y[3])
			Y[3] <- gsub(
				"([[:digit:]]+)\\/([[:digit:]]+) ([^,\n]*)",
				"MN(\\1#\\2#\\3)",Y[3])
		}
		Y[1] <- trim(Y[1]) #gsub("\\s","",Y[1])
		I[i] <- Y[1]
		Y[3] <- gsub("\\s","",Y[3])
		if (Y[2] == "OR")
			Y[3] <- gsub(",","|",Y[3])
		if (Y[2] == "AND")
			Y[3] <- gsub(",","\\&",Y[3])
		if (Y[2] == "TRAN")
			Y[3] <- as.character(ft2eqn(as.character(Y[3]))[[1]])
		Y[3] <- gsub("#",",",Y[3])
		X[i] <- paste("(",Y[3],")",sep="")
	}
	names(X) <- I
	return(X)
}

ft2pexpr <- function(file) {
	X <- revorder(ft2eqn(file))
	expr <- ""
	for (i in names(X)) {
		expr <- paste(expr,i,"<-",X[[i]],"\n")
	}
	pexpr <- parse(text=expr)
	return(pexpr)
}

topexpr <- function(x) {
	if (class(x) == "expression") return(x)
	else if (grepl(".*\\.pf",x)) return(ft2pexpr(x))
	else return(parse(file=x))
}

#topexpr <- function(modeldef) UseMethod("toexpr")
#topexpr.ft <- function(modeldef) ft2pexpr(file=modeldef)
#topexpr.pfile <- function(modeldef) parse(file=modeldef)
#toexpr.default <- function(modeldef) modeldef
