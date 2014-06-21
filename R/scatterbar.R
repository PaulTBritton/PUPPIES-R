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

# front-end to the plotting routine scatterbar2
# more WYSIWYG than scatterbar2
scatterbar <- function(plotname="plot.tiff",envir=parent.frame(),
	rmarg=8,filter, stats=c(2,0,2,2),prec=2,maintitle,
	legendpos,units="Probability", xscale="log",
	xnotation=sciNotation,xmarks,range,lst=ls(envir))
#	xnotation=sciNotation,xmarks,range,lst=as.list(envir,all.names=TRUE))
{
	if (missing(filter)) filter <- ".*"
	else class(filter) <- get("wildcardclass",envir)
	modelname <- get("modelname",envir)
	N <- get("N",envir)
	savenames <- names(lst)
	myget <- function(x) get(as.character(x),envir)
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
#print("newlist")
#print(newlst)
#print("ls filter newlist")
#print(ls(newlst,pattern=torx(filter)))
	Data <- ls(newlst,pattern=torx(filter))
#print("Data")
#print(Data)
#print("as.list Data")
#print(as.list(Data))
	Data <- filterdata(torx(filter),newlst)
#print("filter Data")
#print(Data)
#print(paste("plotname: ",plotname))
#print(paste("modelname: ",modelname))
	if (missing(maintitle)) maintitle <-
		paste(modelname," Monte Carlo Results (",N," Iterations)",sep="")
	T <- switch(as.character(M),"1"=1,"2"=1,"3"=1,"4"=2,"5"=2,3)
	switch(T,
		{	tsize <- 1
			tpos <- c(.3,.45)
			padper <- .15
		},
		{	tsize <- .87
			tpos <- c(.33,.48)
			padper <- .1
		},
		{	tsize <- 0.75
			tpos <- c(.35,.55)
			padper <- .1
		}
	)
	X <- unlist(Data)
	rightm <- (max(X))
	leftm <-(min(X))
	if(VerboseLevel == 3) {
		print(paste("Calculated Right Margin =",rightm))
		print(paste("Calculated Left Margin =",leftm))
	}
	# xmarks: need to test the linear scale case
	if (missing(xmarks)) {
		if (VerboseLevel >= 2) print("Determining x-axis marks")
		xmarks <- switch(xscale,log= {
			if (leftm <= 0) stop("Can't plot zero's on a log scale")
			10^seq(floor(log10(leftm))-1,
			ceiling(log10(rightm))+1,by=2) },
			linear= seq(floor(leftm)-10,ceiling(rightm)+10,by=20))
	}
	if (missing(range)) {
		if (VerboseLevel >= 2) print("Determining x-axis range")
		pad <- pad <- padper*(rightm - leftm)
		range <- switch(xscale,log= {
			if (leftm <= 0) stop("Can't plot zero's on a log scale")
			c(logfloor(leftm)/10,logceil(rightm)*10) },
			linear=c(leftm-pad,rightm+pad))
	}
	logaxis <- switch(xscale,log="x",linear="")
	if (VerboseLevel >= 2) print(paste("Creating scatterbar plot:",
				plotname))
	scatterbar2(plotname,Data,logaxis,
		tsize,tpos,rmarg,xmarks,xnotation,stats,prec,maintitle,
		legendpos,units,range)
}

# the scatterbar2 drawing routine
# less WYSIWYG than the front-end scatterbar
scatterbar2 <- function(name,X,logaxis,tsize,tpos,rmarg,xmarks,
	xnotation,stats,prec,maintitle,lpos,units,range)
{
	L <- names(X)
	M <- length(L)
	if (VerboseLevel > 0) print(paste("scatterbar() opening:",name))
	tiff(name,width=11,height=8,units="in",bg="white",res=300)
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
	N <- length(X[[1]])

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
		prob <- rep(0,N) #array(0,c(N))
		shade <- rep(0,N) #array(0,c(N))
		shadeLogic <- rep(FALSE,N) #array(FALSE,c(N))
		prob <- plnorm(v,log(Fiftyith),
			log(Nfifth/Fiftyith)/qnorm(0.95), lower.tail=TRUE)
		shadeLogic <- prob <= 0.5
		shade[shadeLogic==TRUE] <- -1.3*prob[prob <= 0.5] + .65
		shade[shadeLogic==FALSE] <- 1.3*prob[prob > 0.5] - .65
		for (j in 1:N) {
			segments(v[j],i-.09,v[j],i+.09,lwd=.5,
			col=gray(shade[j]))
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
		segments(Fifth,i+.2,Nfifth,i+.2,col=12,lwd=2)
		segments(Fifth,i-.2,Nfifth,i-.2,col=12,lwd=2)
		segments(Fifth,i+.2,Fifth,i-.2,col=12,lwd=2)
		segments(Nfifth,i+.2,Nfifth,i-.2,col=12,lwd=2)
		segments(Fiftyith,i - .35,Fiftyith,i + .35,
			lwd=2,col="darkorange1")
		segments(Mean,i - .35,Mean,i + .35,lwd=2,col="red3")

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
	if (VerboseLevel > 0) print(paste("scatterbar() completed:",name))
}
