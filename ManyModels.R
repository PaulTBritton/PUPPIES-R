#######################################
#
# ManyModels.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("Modules/PUPPIES.R",chdir=TRUE)

#VerboseLevel <- 3	# default = 1, choose from 0 to 3
#wildcardclass <- "regex"

# samples from Vehicle systems, Engine Parameters, and Alpha Factor distributions
X <- superevalp(N=600,seed=1,pname="Many Models",filter="[VEA].pp")
# propagate uncertainty into a vehicle fault tree model
Y <- spawnp(p=X,model="INPUTS/V.pf")
scatterbars(plotname="OUTPUTS/vehiclemodel.tiff",PM=Y,filter="V_*")
appendp(p=X,model="INPUTS/V_5segBooster.pf")
superappendp(p=X,filter="V?.pf")
scatterbars(plotname="OUTPUTS/VLOMLOC.tiff",PM=X,filter="V?_LO?")
#dumpdata(YX,"DUMP/")

#YX <- loaddata("DUMP/")
#scatterbars_batch(pbfile="INPUTS/V-load.pb",Data=YX)
#scatterbars_batch(pbfile="INPUTS/V.pb",PM=Y)
#scatterbars_batch(pbfile="INPUTS/V.pb",PM=X)

#Z <-spawnpm(p=Y,"V.pe")
#scatterbars(plotname="OUTPUTS/Zplot.tiff",Data=Z[c("AVE_Engine","AVE_LOC")])

print("ManyModels.R Complete")	# status print statement demonstration
