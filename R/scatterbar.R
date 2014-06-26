library(numform)

# a more intuitive name for alist()
plotlist <- alist

# switch board for different number formats
notation <- function(num,prec,form) {
	return(switch(as.character(form),
		"1"=percentNotation(num,prec),
		"2"=sciNotation(num,prec),
		"3"=oneinNotation(num,prec),
		"4"=bothNotation(num,prec),
		warning("Not a valid stats format")))
}

setnames <- function(lst,envir) {
	myget <- function(x) get(as.character(x),envir)

	savenames <- names(lst)
	if (is.null(savenames)) {
		newlst <- lapply(lst,myget)
		names(newlst) <- lst
	} else {
		i = 1
		newnames <- savenames
		newlst <- list()
		for (n in savenames) {
			if (n == "") {
				newnames[i] <- as.character(lst[[i]])
			}
			newlst[[i]] <- myget(lst[[i]])
			i = i + 1
		}
		names(newlst) <- newnames
	}
	return(newlst)
}

# front-end to the plotting routine scatterbar2
# more WYSIWYG than scatterbar2
scatterbar <- function(plotname="plot.tiff",envir=parent.frame(),
	rmarg=8,filter, stats=c(2,0,2,2),prec=2,maintitle,
	legendpos,units="Probability", xscale="log",
	xnotation=sciNotation,xmarks,range,lst)
{
	if (missing(filter)) filter <- ".*"
	else class(filter) <- get("wildcardclass",envir)
	modelname <- get("modelname",envir)
	N <- get("N",envir)
	if (missing(lst)) lst <- ls(envir,pattern=torx(filter))
#	newlst <- setnames(lst,envir)
	Data <- setnames(lst,envir)
	if (missing(maintitle)) maintitle <-
		paste(modelname,": Monte Carlo Results (",N," Iterations)",
			sep="")
	logaxis <- switch(xscale,log="x",linear="")
	if (VerboseLevel >= 2) print(paste("Creating scatterbar plot:",
				plotname))
	scatterbar2(plotname,Data,logaxis,rmarg,xnotation,stats,prec,
		maintitle,legendpos,units)
}


calcxmarks <- function(leftm,rightm,logaxis) {
	if (VerboseLevel >= 2) print("Determining x-axis marks")
	xmarks <- switch(logaxis,x= {
		if (leftm <= 0) stop("Can't plot zero's on a log scale")
		10^seq(floor(log10(leftm))-1,
		ceiling(log10(rightm))+1,by=2) },
		seq(floor(leftm)-10,ceiling(rightm)+10,by=20))
	return(xmarks)
}

calcrange <- function(leftm,rightm,logaxis) {
	if (VerboseLevel >= 2) print("Determining x-axis range")
	pad <- (0.1)*(rightm - leftm)
	range <- switch(logaxis,x= {
		if (leftm <= 0) stop("Can't plot zero's on a log scale")
		c(logfloor(leftm)/10,logceil(rightm)*10) },
		c(leftm-pad,rightm+pad))
	return(range)
}

# the scatterbar2 drawing routine
# less WYSIWYG than the front-end scatterbar
scatterbar2 <- function(file,X,logaxis,rmarg,xnotation,stats,prec,
		maintitle,lpos,units,xmarks,range,tsize,tpos)
{
	ULX <- unlist(X)
	rightm <- (max(ULX))
	leftm <-(min(ULX))
	if(VerboseLevel == 3) {
		print(paste("Calculated Right Margin =",rightm))
		print(paste("Calculated Left Margin =",leftm))
	}
	# xmarks: need to test the linear scale case
	if (missing(xmarks)) xmarks <- calcxmarks(leftm,rightm,logaxis)
	if (missing(range)) range <- calcrange(leftm,rightm,logaxis)

	L <- names(X)
	M <- length(L)
	T <- switch(as.character(M),"1"=1,"2"=1,"3"=1,"4"=2,"5"=2,3)
	if(missing(tsize)) tsize <- switch(T,1,.87,.75)
	if(missing(tpos)) tpos <- switch(T,c(.3,.45),c(.33,.48),c(.35,.55))

	if (VerboseLevel > 0) print(paste("scatterbar() opening:",file))
	tiff(file,width=11,height=8,units="in",bg="white",res=300)
	par(mar=c(6,2,2,rmarg))
	plot.new()
	plot.window(log=logaxis,xlim=range,ylim=c(.5,M+.5),pch=20,
		col="black",cex=.7)
		#,axes=FALSE ,xlab="",ylab="",frame.plot=TRUE)
	box()
	axis(4,1:M,labels = L,hadj=0,las=1)
	axis(side=1,at=xmarks,labels=xnotation(xmarks),las=0)
	xscale <- switch(logaxis,x=" (log scale)","(linear scale)")
	mtext(paste(units,xscale),font=2,side=1,line=3)
	if (!missing(maintitle)) {
		title(main=maintitle)
	}
	par(cex=tsize)
#	N <- length(X[[1]])

	# draw grid
#	grid(12,(M+1),lwd=1.2,lty=1,col="gray")
	abline(h=1:(M),lwd=1.2,lty=1,col="gray")
	abline(v=xmarks,lwd=1.2,lty=1,col="gray")

	i <- 0
	for (v in X) {
		i <- i + 1
		# compute sample stats
		#
		Fifth <- quantile(v,0.05)
		Fiftyith <- quantile(v,0.5)
		Mean <- mean(v)
		Nfifth <- quantile(v,0.95)

		# color the samples gray as a function of cumulative
		# probability
		#
		for (j in v) {
			prob <- plnorm(j,log(Fiftyith),
				log(Nfifth/Fiftyith)/qnorm(0.95),
				lower.tail=TRUE)
			sign <- ifelse(prob <= 0.5,-1,1)
			maxshade <- 0.5
			shade <- sign*2*maxshade*prob - sign*maxshade
			segments(j,i-.09,j,i+.09,lwd=.1,col=gray(shade))
		}

		# place sample stats on band-aid
		#
		if (Mean < Fiftyith) {
			madj <- c(1,1)
			fadj <- c(0,1)
		} else {
			fadj <- c(1,1)
			madj <- c(0,1)
		}
		if (stats[1] != 0) text(Fifth,i+tpos[1],labels=paste("5th: ",
			notation(Fifth,prec,stats[1])), adj=c(1,1))
		if (stats[2] != 0) text(Fiftyith,i+tpos[2],
			labels=paste("Median: ",notation(Fiftyith,prec,
			stats[2])),adj=fadj)
		if (stats[3] != 0) text(Mean,i+tpos[2],labels=paste("Mean: ",
			notation(Mean,prec,stats[3])), adj=madj)
		if (stats[4] != 0) text(Nfifth,i+tpos[1],labels=paste("95th: ",
			notation(Nfifth,prec,stats[4])), adj=c(0,1))
	#	text(max(X[i,]),i,labels=paste("Max: ",
	#		notation(max(X[i,]),2)), adj=c(0,0))

		# draw 5th - 95th box with mean and median bars
		offcenter <- .18
		MM <- 0.3
		lw <- 1.9
		segments(Fifth,i+offcenter,Nfifth,i+offcenter,col=12,lwd=lw)
		segments(Fifth,i-offcenter,Nfifth,i-offcenter,col=12,lwd=lw)
		segments(Fifth,i+offcenter,Fifth,i-offcenter,col=12,lwd=lw)
		segments(Nfifth,i+offcenter,Nfifth,i-offcenter,col=12,lwd=lw)
		segments(Fiftyith,i - MM,Fiftyith,i + MM,
			lwd=lw,col="darkorange1")
		segments(Mean,i - MM,Mean,i + MM,lwd=lw,col="red3")

	}

	if (!missing(lpos)) {
		par(cex=1)
		legend(lpos[1],lpos[2],
			c("Samples","Median","Mean","5th - 95th"),
			lty=c(1,1,1,0),pch=c(NA,NA,NA,0),pt.cex=2,
			lwd=c(2,2,2,2),
			col=c(gray(.3),"darkorange1","red3","blue"))
	}
	junk <- dev.off()
	if (VerboseLevel > 0) print(paste("scatterbar() completed:",file))
}
