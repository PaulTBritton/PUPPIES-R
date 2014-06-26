#######################################
#
# SimpleModel.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("../../R/puppies.R",chdir=TRUE)

#VerboseLevel <- 3	# default = 1, choose from 0 to 3

modeldef <- expression(T1<-betaD(3,4),T2<-lognD(mean=.01,EF=5),T3<-T1|T2)

Y <- evalp(name="Simple in-line model",iterations=707,model=modeldef)

pplot(plotname="Simpleplot.tiff",envir=Y)
#print(class(Y))
#print(str(Y))
#print(Y)

print("SimpleModel.R Complete")	# status print statement demonstration
