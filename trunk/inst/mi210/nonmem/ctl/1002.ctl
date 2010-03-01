$PROB 1002 phase1 2 CMT
$INPUT C ID TIME EVID AMT DV
$DATA ../../data/derived/phase1.csv IGNORE=C
$SUBROUTINE ADVAN4 TRANS4
$PK
 CL=THETA(1)*EXP(ETA(1))
 V2 =THETA(2)*EXP(ETA(2))
 KA=THETA(3)*EXP(ETA(3))
 Q  =THETA(4)*EXP(ETA(4))
 V3=THETA(5) *EXP(ETA(5))
 S2=V2
 
$ERROR
 Y=F*EXP(ERR(1)) * ERR(2)
 IND = IREP

$THETA 
(10)     ;CL
(10)      ;V
(0.2)     ;KA
(10)      ;Q
(10)      ;V3

$OMEGA 0.09 0.09 0.09 0.09 0.09 
$SIGMA 0.09 0.1
$ESTIMATION MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTER MSFO=./1001.msf
$TABLE NOPRINT FILE=./1001.tab ONEHEADER ID AMT TIME EVID PRED IPRE
$TABLE NOPRINT FILE=./1001par.tab ONEHEADER ID TIME CL Q V2 V3 KA ETA1 ETA2 ETA3 ETA4 ETA5