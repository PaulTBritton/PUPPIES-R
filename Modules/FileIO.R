#
# Collection of file input and output procedures
#
# Author: Paul Thomas Britton
#
###################################

# data dump procedure
dumpdata <- function(Z,ddDir,filter) {
	if (VerboseLevel > 0) print("Attempting to dumpdata")
	class(filter) <- wildcardclass
	Z <- filterdata(torx(filter),Z)
	for (j in names(Z)) {
		filename <- paste(ddDir,names(Z[j]),".dat",sep="")
		if (VerboseLevel >= 2) print(paste("Attempting to write:",
					filename))
		write(Z[[j]],file=filename)
	}
	if (VerboseLevel > 0) print("dumpdata Complete")
}

# load data function
loaddata <- function(DataDir,filter) {
	class(filter) <- wildcardclass
	LF <- list.files(path=DataDir,pattern=torx(filter))
	size <- length(LF)
	if (VerboseLevel >= 2) print(paste("reading: ",DataDir,LF[1],sep=""))
	lfile <- paste(DataDir,LF[1],sep="")
	if (file.access(lfile,mode=4)) {
		stop(paste("File access error:",lfile))
	}
	X <- scan(lfile)
	N <- length(X)
	Z <- as.data.frame(array(0,c(N,size)))
	Labels <- array("",c(size))
	Z[1] <- X
# need to test new regex to strip extention
	Labels[1] <- sub("[.].*$","",LF[1])
	if (size > 1) {
		for (i in 2:size) {
			if (VerboseLevel >= 2) print(paste("reading: ",
						DataDir,LF[i],sep=""))
			lfile <- paste(DataDir,LF[i],sep="")
			if (file.access(lfile,mode=4)) {
				stop(paste("File access error:",lfile))
			}
			X <- scan(lfile)
			Z[i] <- X
			Labels[i] <- sub("[.].*$","",LF[i])
		}
	}
	names(Z) <- Labels
	DLF <- paste(DataDir,LF,sep="")
	times <- file.info(DLF)$ctime
	tmp <- cbind(Labels,times)
	row.names(tmp) <- Labels
	O <- tmp[order(tmp[,"times"])]	# recover original order of data
					# using ctime since list.files lists
					# files lexicographically
	return(Z[,O])
}

#superft2pm <- function(filter) {
#}

ft2pm <- function(filename) {
	X <- revorder(ft2eqn(filename))
	expr <- ""
	for (i in names(X)) {
		expr <- paste(expr,i,"<-",X[[i]],"\n")
	}
	pexpr <- parse(text=expr)
	return(pexpr)
}

ft2eqn <- function(filename) {
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

