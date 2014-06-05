#######################################
#
# EasyModel.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("PUPPIES.R")

VerboseLevel <- 3	# default = 1, choose from 0 to 3
#seed <- 1		# global variable

# generate monte carlo samples from model definition file
X <- newpm(N=100,pmfile="INPUTS/test.pp")
scatterbars(plotname="OUTPUTS/testplot.tiff",
	Data=as.data.frame(as.list(X$m)),filter="M?|S?")

modeldef <- expression(T1<-betaD(3,4),T2<-lognD(.01,5))
Y <- newpm(N=100,mexpr=modeldef)
scatterbars(plotname="OUTPUTS/mexprplot.tiff",
	Data=as.data.frame(as.list(Y$m)))


# propagate uncertainty from X into another model
appendpm(X,pmfile="INPUTS/append.pp")
scatterbars(plotname="OUTPUTS/appendplot.tiff",
	Data=as.data.frame(as.list(X$m)),
	desc="INPUTS/Easy.pd",filter="S4")

print("EasyModel.R Complete")	# status print statement demonstration
