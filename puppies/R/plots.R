library(numform)
library(scatterbar)


# puppies front-end to the plotting routine scatterbar
pplot <- function(plotname="pplot.tiff",envir=parent.frame(),
	rmarg=8,stats=c(2,0,2,2),prec=2,legendpos=NULL,units="Probability",
	xscale="log",xnotation=sciNotation,filter,plist,maintitle)
{
	if (missing(filter)) filter <- ".*"
	else class(filter) <- attr(envir,"wildcardclass")
	VerboseLevel <- attr(envir,"VerboseLevel")
	if (is.null(VerboseLevel)) VerboseLevel <- 1
	if (missing(maintitle)) {
		modelname <- attr(envir,"modelname")
		if (is.null(modelname)) modelname <- "PUPPIES Model"
		N <- attr(envir,"N")
		Ntext <- ifelse(is.null(N),"",paste(" (",N," Iterations)",
				sep=""))
		maintitle <- paste(modelname,": Monte Carlo Results",
				Ntext,sep="")
	}
	logaxis <- switch(xscale,log="x",linear="")
	if (VerboseLevel >= 2) print(paste("Creating scatterbar plot:",
				plotname))
	stiff(file=plotname)
	if (missing(plist)) {
		scatterbar(envir=envir,filter=torx(filter),
			logaxis=logaxis,rmarg=rmarg,xnotation=xnotation,
			prec=prec,stats=stats,maintitle=maintitle,
			lpos=legendpos,units=units,sbox=TRUE,stext=TRUE)
	} else {
		scatterbar(envir=envir,plist=plist,
			logaxis=logaxis,rmarg=rmarg,xnotation=xnotation,
			prec=prec,stats=stats,maintitle=maintitle,
			lpos=legendpos,units=units,sbox=TRUE,stext=TRUE)
	}
	junk <-dev.off()
	if (VerboseLevel > 0) print(paste("scatterbar() completed:",plotname))
}
