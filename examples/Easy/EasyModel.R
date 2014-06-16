#######################################
#
# EasyModel.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("../../R/puppies.R",chdir=TRUE)

#VerboseLevel <- 3	# default = 1, choose from 0 to 3
#seed <- 1		# global variable

# generate monte carlo samples from model definition file
#X <- evalp(model="EasyModel.p",N=20,seed=2)
X <- evalp(model="EasyModel.p")

# propagate uncertainty from X into another model
appendp(p=X,model="append.p")
scatterbar(plotname="appendplot.tiff",envir=X,lst=plotlist("Single V4"=V4))

