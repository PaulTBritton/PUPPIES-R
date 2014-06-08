#######################################
#
# MyModel.R
#	- Example Model Script Demo
#
#

rm(list=ls())			# removes all objects from the current
				# R workspace/environment

source("Modules/PUPPIES.R",chdir=TRUE)

#devise default dir scheme
#INPUTS <- "INPUTS"
#OUTPUTS <- "OUTPUTS"
#DUMP <- "DUMP"

#VerboseLevel <- 3	# default = 1, choose from 0 to 3

#wildcardclass <- "regex"

#print(ft2eqn("INPUTS/V.pf"))
#cat(ft2pm("INPUTS/V.pf"))

# samples from Vehicle system distributions,Engine Param, Alpha Factors
#X <- batch_evalpm(N=50,seed=1,filter="[VEA].pp")
X <- superevalp(N=500,seed=1,filter="[VEA].pp")
scatterbars(plotname="OUTPUTS/A.tiff",PM=X,filter="*_?_[4]")
# propagate uncertainty into vehicle models
Y <- spawnp(p=X,pexpr=ft2pm("INPUTS/V.pf"))
scatterbars(plotname="OUTPUTS/mymodel.tiff",PM=Y,filter="V_*")
#Y <- batch_appendpm(pm=X,filter="V?.pf")
#YX <- cbind(Y,X)	# combine (column-wise) Y and X
#dumpdata(YX,"DUMP/")

#YX <- loaddata("DUMP/")
#scatterbars_batch(pbfile="INPUTS/V-load.pb",Data=YX)
#scatterbars_batch(pbfile="INPUTS/V.pb",PM=Y)
#scatterbars_batch(pbfile="INPUTS/V.pb",PM=X)

#Z <-spawnpm(pm=Y,"V.pe")
#scatterbars(plotname="OUTPUTS/Zplot.tiff",Data=Z[c("AVE_Engine","AVE_LOC")])


print("MyModel.R Complete")	# status print statement demonstration
