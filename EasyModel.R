#######################################
#
# EasyModel.R
#	- Example Model Script Demo
#
#
source("PUPPIES.R")

#VerboseLevel <- 3	# default = 1, choose from 0 to 3
N <- 555
print(paste("Number of Samples =",N))
seed <- 1		# global variable

# samples from Vehicle system distributions,Engine Param, Alpha Factors
X <- montecarlo(N,"parameters.pp")
# propagate uncertainty into vehicle models
Y <- propagate("equations.pe",X)
Z <- cbind(Y,X)	# combine (column-wise) Y and X
scatterbars(plotname="OUTPUTS/Easyplot.tiff",Data=Z,filter="V[4-7]|S2")

print("EasyModel.R Complete")	# status print statement demonstration
#rm(list=ls())			# removes all objects from the current
				# R workspace/environment
