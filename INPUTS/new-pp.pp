index;expr
M1;S1*5.00E-01
M2;NOT(cos(S3))+sin(S1)+S2
S1;MN(3,10,S2)
S2;B1|B2
S3;B1&B3
S4;B3&(NOT(B5)|(B2&B1))
B1;lognD(median=6.00E-04,EF=E5)
B2;lognD(median=6.00E-04,EF=E5)
B3;lognD(median=7.00E-05,EF=3)
B4;empeD(empfile="INPUTS/B4.dat")
B5;betaD(alpha=3,beta=6)
E5;5
