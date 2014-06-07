#######################################
#
# MyModel.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("PUPPIES.R")

#devise default dir scheme
#INPUTS <- "INPUTS"
#OUTPUTS <- "OUTPUTS"
#DUMP <- "DUMP"

#VerboseLevel <- 3	# default = 1, choose from 0 to 3

#wildcardclass <- "regex"

#N <- 500
#print(paste("Number of Samples =",N))
seed <- 1

# samples from Vehicle system distributions,Engine Param, Alpha Factors
X <- batch_evalpm(N=50,seed=1,filter="[VEA].pp")
# propagate uncertainty into vehicle models
Y <- batch_appendpm(pm=X,filter="V?.pf")
#YX <- cbind(Y,X)	# combine (column-wise) Y and X
#dumpdata(YX,"DUMP/")

#YX <- loaddata("DUMP/")
#scatterbars_batch(pbfile="INPUTS/V-load.pb",Data=YX)
scatterbars_batch(pbfile="INPUTS/V.pb",PM=Y)

#Z <-spawnpm(pm=Y,"V.pe")
#scatterbars(plotname="OUTPUTS/Zplot.tiff",Data=Z[c("AVE_Engine","AVE_LOC")])


print("MyModel.R Complete")	# status print statement demonstration
