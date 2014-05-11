###############################################
# number formats and rounding
#
# Author: Paul Thomas Britton
#
###################################


logfloor <- function(x) 10^floor(log10(x))

logceil <- function(x) 10^ceiling(log10(x))

# switch board for different number formats
notation <- function(num,prec,form) {
	return(switch(as.character(form),
		"1"=percentNotation(num,prec),
		"2"=sciNotation(num,prec),
		"3"=oneinNotation(num,prec),
		"4"=bothNotation(num,prec),
		warning("Not a valid stats format")))
}

percentNotation <- function(x, digits = 1) {
	return(paste(100*signif(x,digits),"%",sep=""))
}

sciNotation <- function(x, digits = 1) {
	F <- paste("%",1,".",digits-1,"e",sep="")
	return(sprintf(F,signif(x,digits)))
}

oneinNotation <- function(x, digits = 1) {
	return(paste("1 in ",formatC(signif(1/x,digits),big.mark=",",
		format="d",digits=0),sep=""))
}

bothNotation <- function(x, digits = 1) {
	return(paste(sciNotation(x,digits),", ",
		oneinNotation(x,digits),sep=""))
}


#sciNotation <- function(x, digits = 1) {
#	if (length(x) > 1) {
#		return(append(sciNotation(x[1]), sciNotation(x[-1])))
#	}
#	if (!x) return(0)
#	exponent <- floor(log10(x))
#	base <- signif(x / 10^exponent, digits)
#	return(sprintf("%ge%d",base,exponent))
#	F <- paste("%1.",digits-1,"fe%d",sep="")
#	return(sprintf(F,base,exponent))
#	F <- paste("%",1,".",digits-1,"e",sep="")
#	return(sprintf(F,signif(x,digits)))
#}
