#######################################
#
# EasyModel.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

library(pradist)
library(numform)
library(scatterbar)
library(puppies)
#source("../../R/puppies.R",chdir=TRUE)

#VerboseLevel <- 3	# default = 1, choose from 0 to 3

# generate monte carlo samples from model definition file
X <- evalp(model="EasyModel.p")
#for (i in ls(X)) print(i)

# propagate uncertainty from X into another model
appendp(p=X,model="append.p")
pplot(plotname="appendplot.tiff",envir=X,plist=plotlist("Single V4"=V4))

