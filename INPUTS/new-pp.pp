index;udist;up;calc;cp
M1;CF;MULT;S1;5.00E-01;0;0;0
M2;CF;EXPR;NOT(cos(S3))+sin(S1)+S2;0;0;0;0
S1;CF;EXPR;MN(3,10,S2);3;10;0;0
S2;CF;OR;B[1-3];0;0;0;0
S3;CF;AND;B[1-3];0;0;0;0
S4;CF;EXPR;B3&(NOT(B5)|(B2&B1));0;0;0;0
B1;lD;median=6.00E-04,EF=5;I;0
B2;lD;mean=6.00E-04,EF=5;I;0
B3;lD;median=7.00E-05,EF=3;e;t=0.5
B4;eD;INPUTS/B4.dat;I;0
B5;bD;alpha=3,beta=6;I;0