      MODULE NMPRD4P
      USE SIZES, ONLY: DPSIZE
      USE NMPRD4,ONLY: VRBL
      IMPLICIT NONE
      SAVE
      REAL(KIND=DPSIZE), DIMENSION (:),POINTER ::COM
      REAL(KIND=DPSIZE), POINTER ::TVCL,CL,TVV,V,S1,Y,IPRED,A00072
      REAL(KIND=DPSIZE), POINTER ::A00074,A00076,A00078,A00079,A00080
      REAL(KIND=DPSIZE), POINTER ::D00001,D00002,D00003,D00004,D00005
      REAL(KIND=DPSIZE), POINTER ::D00079,D00087,D00088,D00078,D00084
      REAL(KIND=DPSIZE), POINTER ::C00071,D00090,D00089,C00072
      CONTAINS
      SUBROUTINE ASSOCNMPRD4
      COM=>VRBL
      TVCL=>COM(00001);CL=>COM(00002);TVV=>COM(00003);V=>COM(00004)
      S1=>COM(00005);Y=>COM(00006);IPRED=>COM(00007)
      A00072=>COM(00008);A00074=>COM(00009);A00076=>COM(00010)
      A00078=>COM(00011);A00079=>COM(00012);A00080=>COM(00013)
      D00001=>COM(00014);D00002=>COM(00015);D00003=>COM(00016)
      D00004=>COM(00017);D00005=>COM(00018);D00079=>COM(00019)
      D00087=>COM(00020);D00088=>COM(00021);D00078=>COM(00022)
      D00084=>COM(00023);C00071=>COM(00024);D00090=>COM(00025)
      D00089=>COM(00026);C00072=>COM(00027)
      END SUBROUTINE ASSOCNMPRD4
      END MODULE NMPRD4P
      SUBROUTINE PK(ICALL,IDEF,THETA,IREV,EVTREC,NVNT,INDXS,IRGG,GG,NETAS)    
      USE NMPRD4P
      USE SIZES,     ONLY: DPSIZE,ISIZE
      USE NMPRD_REAL,ONLY: ETA,EPS                                            
      USE NMPRD_INT, ONLY: MSEC=>ISECDER,MFIRST=>IFRSTDER,COMACT,COMSAV
      USE NMPRD_INT, ONLY: IQUIT
      USE PRCM_INT,  ONLY: MC0000=>PRMC,ME0000=>PRME                          
      USE PRCM_INT,  ONLY: MG0000=>PRMG,MT0000=>PRMT                          
      USE PROCM_INT, ONLY: NEWIND=>PNEWIF                                     
      USE NMBAYES_REAL, ONLY: LDF                                             
      IMPLICIT REAL(KIND=DPSIZE) (A-Z)                                        
      REAL(KIND=DPSIZE) :: EVTREC                                             
      SAVE
      INTEGER(KIND=ISIZE) :: ICALL,IDEF,IREV,NVNT,INDXS,IRGG,NETAS            
      DIMENSION :: IDEF(7,*),THETA(*),EVTREC(IREV,*),INDXS(*),GG(IRGG,71,*)
      IF (ICALL <= 1) THEN                                                    
      CALL ASSOCNMPRD4
      MC0000(1)=30
      ME0000(1)=70
      MG0000(1)=080
      MT0000(1)=70
      IDEF(1,001)= -9
      IDEF(1,002)= -1
      IDEF(1,003)=  0
      IDEF(1,004)=  0
      IDEF(2,003)=  0
      IDEF(2,004)=  0
      IDEF(3,001)=  3
      CALL GETETA(ETA)                                                        
      IF (IQUIT == 1) RETURN                                                  
      RETURN                                                                  
      ENDIF                                                                   
      IF (NEWIND /= 2) THEN
       IF (ICALL == 4) THEN
        CALL SIMETA(ETA)
       ELSE
        CALL GETETA(ETA)
       ENDIF
       IF (IQUIT == 1) RETURN
      ENDIF
      TVCL=THETA(01) 
      B00001=DEXP(ETA(01)) 
      CL=TVCL*B00001 
!                      A00072 = DERIVATIVE OF CL W.R.T. ETA(01)
      A00072=TVCL*B00001 
      TVV=THETA(02) 
      B00003=DEXP(ETA(02)) 
      V=TVV*B00003 
!                      A00076 = DERIVATIVE OF V W.R.T. ETA(02)
      A00076=TVV*B00003 
      S1=V 
!                      A00079 = DERIVATIVE OF S1 W.R.T. ETA(02)
      A00079=A00076 
      GG(01,1,1)=CL    
      GG(01,02,1)=A00072
      GG(02,1,1)=V     
      GG(02,03,1)=A00076
      GG(03,1,1)=S1    
      GG(03,03,1)=A00079
      IF (MSEC == 1) THEN
!                      A00074 = DERIVATIVE OF A00072 W.R.T. ETA(01)
      A00074=TVCL*B00001 
!                      A00078 = DERIVATIVE OF A00076 W.R.T. ETA(02)
      A00078=TVV*B00003 
!                      A00080 = DERIVATIVE OF A00079 W.R.T. ETA(02)
      A00080=A00078 
      GG(01,02,02)=A00074
      GG(02,03,03)=A00078
      GG(03,03,03)=A00080
      ENDIF
      RETURN
      END
      SUBROUTINE ERROR (ICALL,IDEF,THETA,IREV,EVTREC,NVNT,INDXS,F,G,HH)       
      USE NMPRD4P
      USE SIZES,     ONLY: DPSIZE,ISIZE
      USE NMPRD_REAL,ONLY: ETA,EPS                                            
      USE NMPRD_INT, ONLY: MSEC=>ISECDER,MFIRST=>IFRSTDER,IQUIT
      USE NMPRD_INT, ONLY: NEWL2
      USE PRCM_INT,  ONLY: MC0000=>PRMC,ME0000=>PRME                          
      USE PRCM_INT,  ONLY: MG0000=>PRMG,MT0000=>PRMT                          
      USE PROCM_INT, ONLY: NEWIND=>PNEWIF                                     
      IMPLICIT REAL(KIND=DPSIZE) (A-Z)                                        
      REAL(KIND=DPSIZE) :: EVTREC                                             
      SAVE
      INTEGER(KIND=ISIZE) :: ICALL,IDEF,IREV,NVNT,INDXS                       
      DIMENSION :: IDEF(*),THETA(*),EVTREC(IREV,*),INDXS(*),G(70,*)
      DIMENSION :: HH(70,*)
      IF (ICALL <= 1) THEN                                                    
      CALL ASSOCNMPRD4
      MC0000(2)=30
      ME0000(2)=70
      MG0000(2)=080
      MT0000(2)=70
      IDEF(2)=-1
      IDEF(3)=00
      RETURN
      ENDIF
      IF (ICALL == 4) THEN
       IF (NEWL2 == 1) THEN
        CALL SIMEPS(EPS)
        IF (IQUIT == 1) RETURN
       END IF
      ENDIF
      D00001=G(01,1)
      D00002=G(02,1)
      Y=F+F*EPS(01)+EPS(02) 
!                      D00076 = DERIVATIVE OF Y W.R.T. ETA(02)
      D00076=D00002 
!                      D00077 = DERIVATIVE OF Y W.R.T. ETA(01)
      D00077=D00001 
!                      D00078 = DERIVATIVE OF Y W.R.T. ETA(02)
      D00078=EPS(01)*D00002+D00076 
!                      D00079 = DERIVATIVE OF Y W.R.T. ETA(01)
      D00079=EPS(01)*D00001+D00077 
!                      C00071 = DERIVATIVE OF Y W.R.T. EPS(01)
      C00071=F 
!                      C00072 = DERIVATIVE OF Y W.R.T. EPS(02)
      C00072=1.D0 
!                      D00089 = DERIVATIVE OF C00071 W.R.T. ETA(02)
      D00089=D00002 
!                      D00090 = DERIVATIVE OF C00071 W.R.T. ETA(01)
      D00090=D00001 
      IPRED=F 
      G(01,1)=D00079  
      G(02,1)=D00078  
      HH(01,1)=C00071 
      HH(02,1)=C00072 
      HH(01,02)=D00090
      HH(01,03)=D00089
      IF (MSEC == 1) THEN
      D00003=G(01,02)
      D00004=G(02,02)
      D00005=G(02,03)
!                      D00080 = DERIVATIVE OF D00076 W.R.T. ETA(02)
      D00080=D00005 
!                      D00081 = DERIVATIVE OF D00077 W.R.T. ETA(02)
      D00081=D00004 
!                      D00082 = DERIVATIVE OF D00077 W.R.T. ETA(01)
      D00082=D00003 
!                      D00083 = DERIVATIVE OF D00078 W.R.T. ETA(02)
      D00083=EPS(01)*D00005 
!                      D00084 = DERIVATIVE OF D00078 W.R.T. ETA(02)
      D00084=D00080+D00083 
!                      D00085 = DERIVATIVE OF D00079 W.R.T. ETA(02)
      D00085=EPS(01)*D00004 
!                      D00086 = DERIVATIVE OF D00079 W.R.T. ETA(01)
      D00086=EPS(01)*D00003 
!                      D00087 = DERIVATIVE OF D00079 W.R.T. ETA(01)
      D00087=D00082+D00086 
!                      D00088 = DERIVATIVE OF D00079 W.R.T. ETA(02)
      D00088=D00081+D00085 
      G(01,02)=D00087 
      G(02,02)=D00088 
      G(02,03)=D00084 
      ENDIF
      F=Y
      RETURN
      END
      SUBROUTINE MUMODEL2(THETA,MU_,ICALL,IDEF,NEWIND,&
      EVTREC,DATREC,IREV,NVNT,INDXS,F,G,H,IRGG,GG,NETAS)
      USE NMPRD4P
      USE SIZES,     ONLY: DPSIZE,ISIZE
      USE NMPRD_REAL,ONLY: ETA,EPS
      USE NMPRD_INT, ONLY: MSEC=>ISECDER,MFIRST=>IFRSTDER,COMACT,COMSAV
      USE NMPRD_INT, ONLY: IQUIT
      USE PRCM_INT,  ONLY: MC0000=>PRMC,ME0000=>PRME
      USE PRCM_INT,  ONLY: MG0000=>PRMG,MT0000=>PRMT
      USE NMBAYES_REAL, ONLY: LDF
      IMPLICIT REAL(KIND=DPSIZE) (A-Z)
      REAL(KIND=DPSIZE)   :: MU_(*)
      INTEGER NEWIND
      REAL(KIND=DPSIZE) :: EVTREC
      SAVE
      INTEGER(KIND=ISIZE) :: ICALL,IDEF,IREV,NVNT,INDXS,IRGG,NETAS
      DIMENSION :: IDEF(7,*),THETA(*),EVTREC(IREV,*),INDXS(*),GG(IRGG,71,*)
       RETURN
       END
