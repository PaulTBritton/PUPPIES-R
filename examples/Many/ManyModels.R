#######################################
#
# ManyModels.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("../../R/puppies.R",chdir=TRUE)
source("cram.R")
#source("commoncause.R")
library(commoncause)

#VerboseLevel <- 3	# default = 1, choose from 0 to 3

# samples from Vehicle systems, Engine Parameters,
# and Alpha Factor distributions
X <- superevalp(name="Many Models",iterations=400,seed=1,filter="[VEA].pp")
print("X")
# propagate uncertainty into a vehicle fault tree model
Y <- spawnp(p=X,model="INPUTS/V.pf")
print("Y")
scatterbar(plotname="OUTPUTS/vehiclemodel.tiff",envir=Y,filter="V_*")
appendp(p=X,model="INPUTS/V_5segBooster.pf")
print("append")
superappendp(p=X,filter="V?.pf")
scatterbar(plotname="OUTPUTS/VLOMLOC.tiff",envir=X,filter="V?_LO?")
#dumpdata(YX,"DUMP/")

#YX <- loaddata("DUMP/")
#scatterbars_batch(pbfile="INPUTS/V-load.pb",Data=YX)
#scatterbars_batch(pbfile="INPUTS/V.pb",PM=Y)
#scatterbars_batch(pbfile="INPUTS/V.pb",PM=X)

#Z <-spawnpm(p=Y,"V.pe")
#scatterbars(plotname="OUTPUTS/Zplot.tiff",Data=Z[c("AVE_Engine","AVE_LOC")])

print("ManyModels.R Complete")	# status print statement demonstration
