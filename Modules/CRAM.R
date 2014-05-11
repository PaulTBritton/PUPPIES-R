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

#EngineRisk <- function(BT,beta,eta,lambdaTI,lambdaTS,PL,RF) {
#	return(CERisk(BT,beta,eta) + UERisk(BT,lambdaTI,lambdaTS,PL,RF))
#}

##################################################3
#
# Stage Model
#
#
#MPSRisk <- function() {
#}
#
#APURisk <- function() {
#}
#
#TVCRisk <- function() {
#}
#
#StageRisk <- function() {
#}
#
###################################################3
##
## Booster Model
##
#
#BoosterRisk <- function() {
#}
#
###################################################3
##
## Vehicle Model
##
#
#RCSRisk <- function() {
#}
#
#SWRisk <- function() {
#}
#
#VehicleRisk <- function() {
#}
