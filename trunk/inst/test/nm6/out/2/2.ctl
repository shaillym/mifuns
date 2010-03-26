$PROB RUN# 2 Parent-Metabolite-Urine Model
$DATA ../../data/2.csv IGNORE=C
$INPUT C ID TIME AMT DV CRCL SEX WT AGE CMT EVID
$SUB ADVAN5 TRANS1 INFN=../../infn/MIfunsRunlogNM6.for
$ABB COMRES=12
$MODEL
    NCOMPARTMENTS=4
    COMP=(GUT,DEFDOSE)    ;GUT COMPARTMENT
    COMP=(CZX)            ;CENTRAL COMPARTMENT FOR CZX
    COMP=(HCZX)           ;CENTRAL COMPARTMENT FOR HCZX
    COMP=(URINE)          ;HCZX URINE
$PK
    K12=THETA(1)*EXP(ETA(1))
    
    TCLM=THETA(2)*(WT/70)**THETA(7)
    CLCM=TCLM*EXP(ETA(2))        ;CYP2E1 CL CZX
    TCLB=THETA(3)*(WT/70)**THETA(8)
    CLCB=TCLB*EXP(ETA(6))        ;BILIARY & OTHER CL CZX
    
    CLC=CLCB+CLCM                ;TOTAL CLEARANCE CZX
    FM=CLCM/CLC                  ;FRACTION METABOLIZED
    
    TCLH=THETA(4)*(CRCL/55)**THETA(9)
    CLHC=TCLH*EXP(ETA(3))    ;TOTAL CLEARANCE OF HCZX
    TVV2=THETA(5)
    V2=TVV2*EXP(ETA(4))
    
    TVV3=THETA(6)
    V3= TVV3*EXP(ETA(5))
    K23=CLCM/V2
    K20=CLCB/V2
    K34=CLHC/V3
    S2=V2
    S3=V3
    S4=1
$ERROR
    C2=A(2)/V2
    C3=A(3)/V3
    C4=A(4)
    IPRED=F
    IND2=0
    IND3=0
    IND4=0
    IF (CMT.EQ.2) IND2=1
    IF (CMT.EQ.3) IND3=1
    IF (CMT.EQ.4) IND4=1
    Y3=F*(1+ERR(3))+ERR(4)
    Y2=F*(1+ERR(1))+ERR(2)
    Y4=F*(1+ERR(5))+ERR(6)
    Y=Y3*IND3+Y2*IND2+Y4*IND4
"  LAST 
"  COM(1)=G(1,1) 
"  COM(2)=G(2,1) 
"  COM(3)=G(3,1) 
"  COM(4)=G(4,1) 
"  COM(5)=G(5,1) 
"  COM(6)=G(6,1) 
"  COM(7)=HH(1,1) 
"  COM(8)=HH(2,1)
"  COM(9)=HH(3,1)
"  COM(10)=HH(4,1)
"  COM(11)=HH(5,1)
"  COM(12)=HH(6,1)
$THETA   
    (0, 0.493)  ;1. ABSORPTION RATE CONSTANT
    (0, 12.8)   ;2. CLCM
    (0, 5.7)    ;3. CLCB
    (0, 18.4)   ;4. CLHC
    (0, 7.3)    ;5. V2
    (0, 17)     ;6. V3
    (0,0.560)     ;7. WT ON CLCM
    (0,1.30)     ;8. WT ON CLCB
    (0,2.16)     ;9. CRCL ON CLH
   
$OMEGA
0.009
0.0219
0.034
0.0267
0.0184
0.0742
$SIGMA
   0.071
   100
   0.0452
   0.142
   0.06
   10
$ESTIMATION PRINT=2 MAX=9999 SIG=3 METH=0 POSTHOC NOABORT MSFO=./2.msf
$COVARIANCE PRINT=E
$TABLE ID TIME EVID IPRED NOPRINT ONEHEADER FILE=./2.tab
$TABLE ID CLCM CLCB CLHC V2 V3 EVID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 
NOPRINT ONEHEADER FILE=./2par.tab
$TABLE ID TIME 
COM(1)=G11 COM(2)=G21 COM(3)=G31
COM(4)=G41 COM(5)=G51 COM(6)=G61
COM(7)=H11 COM(8)=H21 COM(9)=H31
COM(10)=H41 COM(11)=H51 COM(12)=H61
IPRED MDV NOPRINT ONEHEADER FILE=./cwtab1.deriv       