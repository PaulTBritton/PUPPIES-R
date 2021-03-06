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
#	Software
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
#print(getwd())
#source("filter.R")
#source("fileio.R")
#source("numform.R")
#source("udfs.R")
#source("evalp.R")
#source("scatterbar.R")
#source("plots.R")

#####################################################
# define global configuration arguments
#args <- list(fieldsep=";")

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
#VerboseLevel <- 1

#
# set default wildcardclass
# "regex" = regular expression
# "aglob" = advanced glob
# "glob" = glob
#wildcardclass <- "aglob"

# seed for random number generator
# NULL means rely on the CPU clock for seed generation
#seed <- NULL

puppiesenv <- new.env()
#sys.source("setattr.R",envir=puppiesenv)
#sys.source("boolean.R",envir=puppiesenv)
#sys.source("distributions.R",envir=puppiesenv)
