library(numform)
library(scatterbar)

# puppies front-end to the plotting routine scatterbar
pplot <- function(plotname="pplot.tiff",envir=parent.frame(),
	rmarg=8,stats=c(2,0,2,2),prec=2,legendpos,units="Probability",
	xscale="log",xnotation=sciNotation,xmarks,range,
	filter,lst,maintitle)
{
	if (missing(filter)) filter <- ".*"
	else class(filter) <- attr(envir,"wildcardclass")
	modelname <- attr(envir,"modelname")
	N <- attr(envir,"N")
	if (missing(maintitle)) maintitle <-
		paste(modelname,": Monte Carlo Results (",N," Iterations)",
			sep="")
	logaxis <- switch(xscale,log="x",linear="")
	if (VerboseLevel >= 2) print(paste("Creating scatterbar plot:",
				plotname))
if (missing(lst)) {
	scatterbar(file=plotname,envir=envir,filter=torx(filter),
		logaxis=logaxis,rmarg=rmarg,xnotation=xnotation,prec=prec,
		stats=stats,maintitle=maintitle,lpos=legendpos,units=units,
		sbox=TRUE,stext=TRUE)
} else {
	scatterbar(file=plotname,envir=envir,lst=lst,
		logaxis=logaxis,rmarg=rmarg,xnotation=xnotation,prec=prec,
		stats=stats,maintitle=maintitle,lpos=legendpos,units=units,
		sbox=TRUE,stext=TRUE)
}
	junk <- dev.off()
	if (VerboseLevel > 0) print(paste("scatterbar() completed:",plotname))
}
