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
X <- montecarlo(N=1000,"INPUTS/new-pp.pp")
# propagate uncertainty from X into another model
#Y <- propagate("equations.pe",X)
#Z <- cbind(Y,X)	# combine (column-wise) Y and X
scatterbars(plotname="OUTPUTS/Easyplot.tiff",Data=X,filter="M?|S?")

#scatterbars(plotname="OUTPUTS/OneEasyplot.tiff",Data=Z,
#	desc="INPUTS/Easy.pd",filter="S4")

print("EasyModel.R Complete")	# status print statement demonstration
