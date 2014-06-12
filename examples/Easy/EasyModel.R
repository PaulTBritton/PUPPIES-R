#######################################
#
# EasyModel.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("../../R/puppies.R",chdir=TRUE)

VerboseLevel <- 3	# default = 1, choose from 0 to 3
#seed <- 1		# global variable

# define a PUPPIES Model from a PUPPIES file
#modeldef <- pfile("INPUTS/Easy.pp")

# generate monte carlo samples from model definition file
#X <- evalp(model="EasyModel.p",N=20,seed=2)
X <- evalp(model="EasyModel.p")

#print((as.list(X))[c("B2","S2","M2","A3")][[2]])
#print(as.data.frame(as.list(X))[c("B2","S2","M2","A3")][[2]])
# propagate uncertainty from X into another model
#appendp(x=X,pfile="INPUTS/append.pp")
#scatterbars(plotname="OUTPUTS/appendplot.tiff",PM=X,
#	desc="INPUTS/Easy.pd",filter="S4")

