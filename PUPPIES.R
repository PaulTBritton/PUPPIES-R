#
# PUPPIES.R beta version 1.9
# 04-28-2014
#
# PUPPIES:
#	Programmable
#	Uncertain
#	Parameter
#	Propagation
#	Into
#	Equations
#	Scripts
#
# Performs Customizable Monte Carlo Simulations and
# Plots Scatter Bars (uncertainty bars)
#
# Author: Paul Thomas Britton
#
#####################################################

#options(warn=-1)

#####################################################
#load code modules
source("Modules/FileIO.R")
source("Modules/Number.R")
source("Modules/Boolean.R")
source("Modules/UDFs.R")
source("Modules/MonteCarlo.R")
source("Modules/Propagate.R")
source("Modules/scatterbars.R")
source("Modules/CRAM.R")
source("Modules/CommonCause.R")

#####################################################
# define global configuration arguments
args <- list(fieldsep=";")

#
# VerboseLevel -
#	print script status and feedback to console to
#	aid touble shooting or to facilitate learning
#
#
# Levels:	0 = minimal status printed to console (usually none)
#		1 = moderate	"	"	" (echos major inputs)	
#		2 = maximal	"	"	" (moderate + more)
#		3 = extreme debugging mode (minutiae level)
#

# set default VerboseLevel
VerboseLevel <- 1

#
# set default wildcardlevel
# 1 = glob
# 2 = advanced glob
# 3 = regular expression
wildcardlevel <- 2	# default = 2, chose 1-3

# seed for random number generator
# NULL means rely on the CPU clock for seed generation
seed <- NULL
