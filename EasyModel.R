#######################################
#
# EasyModel.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("Modules/PUPPIES.R",chdir=TRUE)

VerboseLevel <- 3	# default = 1, choose from 0 to 3
#seed <- 1		# global variable

# generate monte carlo samples from model definition file
X <- evalpm(N=100,pmfile="INPUTS/test.pp")
scatterbars(plotname="OUTPUTS/testplot.tiff",PM=X,filter="M?|S?")
print("scatterbar")

modeldef <- expression(T1<-betaD(3,4),T2<-lognD(.01,5))
Y <- evalpm(N=100,mexpr=modeldef)
scatterbars(plotname="OUTPUTS/mexprplot.tiff",PM=Y)
print("scatterbar")


# propagate uncertainty from X into another model
appendpm(pm=X,pmfile="INPUTS/append.pp")
print("appendpm")
scatterbars(plotname="OUTPUTS/appendplot.tiff",PM=X,
	desc="INPUTS/Easy.pd",filter="S4")

print("EasyModel.R Complete")	# status print statement demonstration
