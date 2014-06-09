pname <<- "Overide default name"
# setname("Test Model Name")
E5 <- unifD(4,6)
E3 <- 3
A3 <- triaD(2,3,4)
B5 <- betaD(alpha=A3,beta=6)
B4 <- empeD("INPUTS/B4.dat")
#B3 <- lognD(mu=log(7.00E-05)-(1e-4^2)/2,sigma=1e-4)
B3 <- lognD(mu=-9.57,sigma=1e-4)
B2 <- lognD(mean=6.00E-04,EF=E5)
B1 <- lognD(median=B2,EF=E5)
S5 <- B1&B2&B3
S4 <- B3&(NOT(B5)|(B2&B1))
S3 <- AND("B[1-3]")
S2 <- OR("B[1-3]")
S1 <- MN(3,10,S2)
M2 <- NOT(cos(S3))+sin(S1)+S2
M1 <- S1*5.00E-01
scatterbars(plotname="OUTPUTS/testplot.tiff",filter="M?|S?")
print("scatterbar")
