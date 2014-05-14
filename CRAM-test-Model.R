#######################################
#
# CRAM-test-Model.R
#	- Example Model Script Demo
#
#
source("PUPPIES.R")

#VerboseLevel <- 3	# default = 1, choose from 0 to 3
wildcardlevel <- 1	# 1 = regular expressions
N <- 555
print(paste("Number of Samples =",N))
seed <- 1

# samples from Vehicle system distributions,Engine Param, Alpha Factors
X <- montecarlo(N,"[VEA].pp")
# propagate uncertainty into vehicle models
Y <- propagate("V.[.]pf",X)
YX <- cbind(Y,X)	# combine (column-wise) Y and X

print("running summary test")
test <-propagate("V[.]pe",Y)
scatterbars(plotname="OUTPUTS/testplot.tiff",Data=test,filter="AVE")

f <- c("LOM|LOC","CS","Booster","Engine")
p <- c("plotV.tiff","plotCS.tiff","plotBooster.tiff","plotEngine.tiff")
p <- paste("OUTPUTS/",p,sep="")

for (i in 1:length(f)) {
	Z <- filterdata(f[i],YX)
	scatterbars(plotname=p[i],Data=Z,stats=c(2,0,4,2))
}

I <- filterdata("Engine",YX)
J <- filterdata("Booster",YX)
scatterbars(plotname="OUTPUTS/prop-plot.tiff",Data=cbind(I,J),stats=c(2,0,4,2))


print("CRAM-test-Model.R Complete")	# status print statement demonstration
#rm(list=ls())			# removes all objects from the current
				# R workspace/environment
