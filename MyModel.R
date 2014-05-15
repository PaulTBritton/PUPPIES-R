#######################################
#
# MyModel.R
#	- Example Model Script Demo
#
#
source("PUPPIES.R")

#VerboseLevel <- 3	# default = 1, choose from 0 to 3
#wildcardlevel <- 1	# default = 0, chose 0 or 1
N <- 500
print(paste("Number of Samples =",N))
seed <- 1

# samples from Vehicle system distributions,Engine Param, Alpha Factors
X <- montecarlo(N,"[VEA].pp")
# propagate uncertainty into vehicle models
Y <- propagate("V?.pf",X)
YX <- cbind(Y,X)	# combine (column-wise) Y and X
#dumpdata(YX,"DUMP/")

#YX <- loaddata("DUMP/")
#scatterbars_batch(pbfile="INPUTS/V-load.pb",Data=YX)
scatterbars_batch(pbfile="INPUTS/V.pb",Data=YX)

Z <-propagate("V.pe",Y)
scatterbars(plotname="OUTPUTS/Zplot.tiff",Data=Z[c("AVE_Engine","AVE_LOC")])


print("MyModel.R Complete")	# status print statement demonstration
#rm(list=ls())			# removes all objects from the current
				# R workspace/environment
