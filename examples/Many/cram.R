#
# CRAM - Conceptual Risk Assessment Modeler
#
# ***********UNDER DEVELOPMENT********************
#
# Author: Paul Thomas Britton
#
###################################

##################################################3
#
# Engine Model
#

ThrottleFactor <- function(PL,RF) {
	return(exp((PL - 1)*100/RF))
}

CERisk <- function(BT,beta,eta) {
	return(pweibull(BT,beta,eta))
}

UERisk <- function(BT,lambdaTI,lambdaTS,PL,RF) {
	return(pexp(BT,lambdaTI)+pexp(BT,ThrottleFactor(PL,RF)*lambdaTS))
}

