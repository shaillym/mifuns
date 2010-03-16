$PROB 1005 phase1 2 CMT like 1004 but diff. initial on V3
$INPUT C ID TIME SEQ=DROP EVID AMT DV SUBJ HOUR TAFD TAD LDOS MDV HEIGHT WT SEX AGE DOSE FED
$DATA ../../data/ph1/derived/phase1.csv IGNORE=C
$SUBROUTINE ADVAN4 TRANS4
$PK
 CL=THETA(1)*EXP(ETA(1)) * THETA(6)**SEX * (WT/70)**THETA(7)
 V2 =THETA(2)*EXP(ETA(2))
 KA=THETA(3)*EXP(ETA(3))
 Q  =THETA(4);*EXP(ETA(4)) *(WT/70)**THETA(7)
 V3=THETA(5); *EXP(ETA(5))
 S2=V2
 
$ERROR
 Y=F*EXP(ERR(1)); + ERR(2)
 IPRE=F

$THETA 
(0,10,50)     ;CL        <parameter name='P1' label='CL'>clearance</parameter>
(0,10,100)    ;V         <parameter name='P2' label='V2'>central volume</parameter>
(0,0.2, 5)    ;KA        <parameter name='P3' label='Ka'>absorption constant</parameter>
(0,10,50)     ;Q         <parameter name='P4' label='Q' >intercompartmental clearance</parameter>
(0,100,1000)  ;V3        <parameter name='P5' label='V3'>peripheral volume</parameter>
(0,1,2)       ;SEX       <parameter name='P6' label='Male.CL'>male effect on clearance</parameter>
(0,0.75,3)    ;WT on CL  <parameter name='P7' label='WT.CL'>weight effect on clearance</parameter>

$OMEGA 0.09 0.09 0.09 ;0.09 0.09
;<parameter name='P8' label='IIV.CL'>interindividual variability on clearance</parameter>
;<parameter name='P9' label='CL.V2'>covariance of clearance and central volume</parameter>
;<parameter name='P10' label='IIV.V2'>interindividual variability on central volume</parameter>
;<parameter name='P11' label='CL.Ka'>covariance of clearance and Ka</parameter>
;<parameter name='P12' label='V2.Ka'>covariance of central volume and Ka</parameter>
;<parameter name='P13' label='IIV.Ka'>interindividual variability on Ka</parameter>
$SIGMA 0.09 ;0.1
;<parameter name='P14' label='ERR'>proportional error</parameter>
$ESTIMATION MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTER MSFO=./1005.msf
$COV PRINT=E
$TABLE NOPRINT FILE=./1005.tab ONEHEADER ID AMT TIME EVID PRED IPRE CWRES
$TABLE NOPRINT FILE=./1005par.tab ONEHEADER ID TIME CL Q V2 V3 KA ETA1 ETA2 ETA3
