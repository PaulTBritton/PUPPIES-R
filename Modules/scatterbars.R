#
# Plot Scatter Bars (Uncertainty Bars [band-aids]) #
# Turn sample data into scatter bars
#
# Author: Paul Thomas Britton
#
##############################################

##############################################
# draw the picture
#

# batch version front-end to the plotting routine scatterbars
# ? consider removing desc file feature: superseeded by pbfile
scatterbars_batch <- function(pbfile,Data)
{
	Ords <- paste(readLines(pbfile),collapse="")
	if (VerboseLevel >= 2) {
		Ords <- scan(text=Ords,what="character",quote=NULL,sep="$")
		print(paste("scatterbars_batch() parsing",pbfile,
				"into plot tasks"))
		print(Ords)
	} else {
		Ords <- scan(text=Ords,what="character",sep="$",
			quote=NULL,quiet=TRUE)
	}
	size <- length(Ords)
	for (i in 1:size) {
		Y <- scan(text=Ords[i],what="character",sep=";",
			quote=NULL,quiet=TRUE)
		patterns <- scan(text=Y[2],what="character",sep=",",
			quote=NULL,quiet=TRUE)
		ptable <- strsplit(patterns,"=")
#print(ptable)
		for (pat in ptable) {
			len <- length(pat)
			filter <- pat[1]
			class(filter) <- wildcardclass
			filter <- torx(filter)
			if (exists("Dindex") & exists("desc2")) {
				Dindex <- c(Dindex,grep(filter,names(Data)))
				if (len == 2) {
					desc2 <- sub(filter,pat[2],desc2)
				}
				
			} else if (exists("Dindex")) {
				Dindex <- c(Dindex,grep(filter,names(Data)))
				if (len == 2) {
					desc2 <- sub(filter,pat[2],names(Data))
				}
			} else {
				Dindex <- grep(filter,names(Data))
				if (len == 2) {
					desc2 <- sub(filter,pat[2],names(Data))
				}
			}
		}
#print(Dindex)
		if (exists("desc2")) {
			desc3 <- rbind(names(Data),desc2)
			plotcommand <-
			paste("scatterbars(Data=Data[Dindex],desc2=desc3,",
				Y[1],")")
			rm(desc2)
		} else {
			plotcommand <-
			paste("scatterbars(Data=Data[Dindex],",Y[1],")")
		}
print(plotcommand)
#print(parse(text=plotcommand))
		eval(parse(text=plotcommand))
		rm(Dindex)
	}
}

# front-end to the plotting routine scatterbars2
# more WYSIWYG than scatterbars3
scatterbars <- function(plotname="plot.tiff",Data,rmarg=8,filter,
	stats=c(2,0,2,2),prec=2,desc,desc2,maintitle,legendpos,
	units="Probability", xscale="log",xnotation=sciNotation,
	xmarks,range)
{
	if (missing(maintitle)) maintitle <- paste("Monte Carlo Results (",
					nrow(Data)," Samples)",sep="")
# removing wildcardlevel from global config
	if (missing(filter)) filter <- ".*"
	else class(filter) <- wildcardclass
	Data <- filterdata(torx(filter),Data)
	Labels <- names(Data)
	M <- length(Labels)
	if (!missing(desc)) {
		if (VerboseLevel > 0) print(paste("scatterbars()",
					"reading:",desc))
		if (file.access(desc,mode=4)==-1) {
			stop(paste("File access error:",desc))
		}
		D <- read.csv(desc,sep=args$fieldsep,row.names=1)
		if (VerboseLevel > 0) print(D)
		tmp <- Labels
		for (i in row.names(D)) {
			j <- grep(i,tmp)
			Labels[j] <- as.character(D[[i,1]])
		}
	}
	if (!missing(desc2)) {
		tmp <- Labels
		for (i in 1:ncol(desc2)) {
			j <- grep(desc2[1,i],tmp)
			Labels[j] <- as.character(desc2[2,i])
		}
	}
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
	X <- t(as.matrix(Data))
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
	scatterbars2(plotname,M,rowrevorder(X),revorder(Labels),logaxis,
		tsize,tpos,rmarg,xmarks,xnotation,stats,prec,maintitle,
		legendpos,units,range)
}

# the scatterbars2 drawing routine
# less WYSIWYG than the front-end scatterbars
scatterbars2 <- function(name,M,X,L,logaxis,tsize,tpos,rmarg,xmarks,
	xnotation,stats,prec,maintitle,lpos,units,range)
{
	if (VerboseLevel > 0) print(paste("scatterbars() opening:",name))
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
	N <- length(X[1,])
	prob <- array(0,c(N))
	shade <- array(0,c(N))

	# draw grid
#	grid(12,(M+1),lwd=1.2,lty=1,col="gray")
	abline(h=1:(M),lwd=1.2,lty=1,col="gray")
	abline(v=xmarks,lwd=1.2,lty=1,col="gray")

	for (i in 1:M) {
		# compute sample stats
		#
		Fifth <- quantile(X[i,],0.05)
		Fiftyith <- quantile(X[i,],0.5)
		Mean <- mean(X[i,])
		Nfifth <- quantile(X[i,],0.95)

		# color the samples gray as a function of cumulative
		# probability
		#
		prob <- array(0,c(N))
		shade <- array(0,c(N))
		shadeLogic <- array(FALSE,c(N))
		prob <- plnorm(X[i,],log(Fiftyith),
			log(Nfifth/Fiftyith)/qnorm(0.95), lower.tail=TRUE)
		shadeLogic <- prob <= 0.5
		shade[shadeLogic==TRUE] <- -1.3*prob[prob <= 0.5] + .65
		shade[shadeLogic==FALSE] <- 1.3*prob[prob > 0.5] - .65
		for (j in 1:N) {
			segments(X[i,j],i-.09,X[i,j],i+.09,lwd=.5,
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
	if (VerboseLevel > 0) print(paste("scatterbars() completed:",name))
}
