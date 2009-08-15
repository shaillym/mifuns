$PROB RUN# 1001 ESTIMATE PK W/COVARIATE RELATIONSHIPS
$INPUT C ID DV AMT II ADDL TIME RATE HT WT CLCR SEX AGE
$DATA  ../1001.csv IGNORE=C 
$SUB ADVAN1 TRANS2 INFN=../MIfunsRunlogNM6.for
$ABB COMRES=4
$PK
TVCL=THETA(1)
CL=TVCL*EXP(ETA(1))
TVV=THETA(2)
V=TVV*EXP(ETA(2))
S1=V
$ERROR
Y=F + F*ERR(1) +ERR(2)
IPRED=F
"  LAST 
"  COM(1)=G(1,1) 
"  COM(2)=G(2,1) 
"  COM(3)=HH(1,1) 
"  COM(4)=HH(2,1)
$THETA
 (0, 13) ;1. CL
 (0, 75);2. V
$OMEGA BLOCK(2)
(0.04) ;1. CL VAR
(0.02) (0.04) ;2. V VAR
$SIGMA 
0.04
1
;$MSFI=1001.MSF
$ESTIMATION MAXEVAL=9999 PRINT=10 POSTHOC MSFO=1001.MSF
$TABLE ID TIME EVID IPRED NOPRINT ONEHEADER FILE=1001.TAB
$TABLE ID CL V EVID ETA1 ETA2 NOPRINT
 ONEHEADER FILE=1001par.TAB
$TABLE ID TIME COM(1)=G11 COM(2)=G21 COM(3)=H11
COM(4)=H21 IPRED MDV NOPRINT ONEHEADER FILE=cwtab1.deriv
