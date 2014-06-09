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
X <- evalp(N=500,pfile="INPUTS/test.pp")

modeldef <- expression(T1<-betaD(3,4),T2<-lognD(.01,5))
Y <- evalp(N=500,pname="in-line model",pexpr=modeldef)
scatterbars(plotname="OUTPUTS/mexprplot.tiff",PM=Y)


# propagate uncertainty from X into another model
appendp(x=X,pfile="INPUTS/append.pp")
scatterbars(plotname="OUTPUTS/appendplot.tiff",PM=X,
	desc="INPUTS/Easy.pd",filter="S4")

print("EasyModel.R Complete")	# status print statement demonstration
