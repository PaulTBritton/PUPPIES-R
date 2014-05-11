#######################################
#
# MyModel.R
#	- Example Model Script Demo
#
#
source("PUPPIES.R")

#VerboseLevel <- 2	# default = 1, choose from 0 to 3
N <- 1000
print(paste("Number of Samples =",N))
seed <- 1

# samples from Vehicle system distributions,Engine Param, Alpha Factors
X <- montecarlo(N,"[VEA].pp")
# propagate uncertainty into vehicle models
Y <- propagate("V..pf",X)
YX <- cbind(Y,X)	# combine (column-wise) Y and X

scatterbars(pofile="INPUTS/V.po",YX,prec=4)

#print("running test")
#test <-propagate("V.pe",Y)
#scatterbars2(plotname="OUTPUTS/testplot.tiff",Data=test,
#	filter="AVE|alpha")


print("MyModel.R Complete")	# status print statement demonstration
#rm(list=ls())			# removes all objects from the current
				# R workspace/environment
