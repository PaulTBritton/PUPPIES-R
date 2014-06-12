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
#seed <- 1		# global variable

modeldef <- expression(T1<-betaD(3,4),T2<-lognD(.01,5),T3<-T1|T2)

Y <- evalp(name="Simple in-line model",N=700,model=modeldef)

#scatterbar(plotname="Simpleplot.tiff",PM=Y)
scatterbars(plotname="Simpleplot.tiff",PM=Y)
print(class(Y))
print(str(Y))
print(Y)

print("SimpleModel.R Complete")	# status print statement demonstration
