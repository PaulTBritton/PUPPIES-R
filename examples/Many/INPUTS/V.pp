TPS <- betaD(2,100)
AVI_FR <- gammD(1,500)
Abortability <- triaD(.1,.6,.9)

V_Total_ICPS <- betaD(2,10)
pplot(plotname="OUTPUTS/ICPS.tiff",filter="*ICPS")

B_Motor <- lognD(median=3e-4,EF=3)
B_SEP <- lognD(median=1e-5,EF=5)
B_TVC <- 1-exp(-gammD(1,19)*.02)
B_TPS <- TPS
B_AVI <- 1-exp(-AVI_FR*.01)
B_STR <- lognD(median=1e-7,EF=7)
pplot(plotname="OUTPUTS/booster.tiff",filter="B_*")

CS_TVC <- 1-exp(-gammD(1,1900)*.02)
CS_TPS <- TPS
CS_MPS <- 1-exp(-gammD(1,10000)*.05)
CS_AVI <- 1-exp(-AVI_FR*.05)
CS_RCS <- 1-exp(-gammD(2,20000)*.05)
CS_STR <- lognD(median=1e-5,EF=5)
CS_SEP <- lognD(median=1e-5,EF=5)
pplot(plotname="OUTPUTS/corestage.tiff",filter="CS_*")
