C Generates a running table file to summarize the output of NONMEM runs.        
                                                                                
      SUBROUTINE INFN(ICALL,THETA,DATREC,INDXS,NEWIND)                          
	  INTEGER ICALL,INDXS,NEWIND                                                   
      DOUBLE PRECISION THETA                                                    
      REAL DATREC                                                               
      DIMENSION THETA(*),DATREC(*),INDXS(*)                                     
	  if (ICALL.EQ.3) CALL runlog(NEWIND)                                          
      end                                                                       
	                                                                               
      subroutine runlog(NEWIND)                                                 
      include '/common/NONMEM/nm6osxbeta/SIZES'                                 
      common /cm1/ ntheta,nth                                                   
      common /cm2/ neta,neps,nre2                                               
	  common /cm34/ ie                                                             
	  common /rocm6/ thetaf(lth),omegaf(lvr,lvr),sigmaf(lvr,lvr)                   
	  common /rocm7/ seth(lth),seom(lvr,lvr),sesig(lvr,lvr)                        
	  common /rocm9/ iere,ierc                                                     
      common /cm18/ spec                                                        
      common /cm19/ AVAIL(11)                                                   
      common /cm10/ s(lpar), n99, il, iu                                        
      COMMON /CM12/ COV(LPAR3),INVCOV(LPAR3),STHTA(LTH),                        
     1              SEN(LVR,LVR),COR(LPAR3),NVLS,VLS(LPAR)                      
                                                                                
	  common /rocm8/ object                                                        
      double precision s, object                                                
                                                                                
      real*8 thetaf,omegaf,sigmaf                                               
      real seth,seom,sesig                                                      
                                                                                
      real sethl(lth),seoml(lvr,lvr),sesigl(lvr,lvr)                            
      CHARACTER(100) DESC                                                       
      integer unitp, ie, UNITO, IERE, IERC,J,I                                  
c      integer lth,lvr,lvr2,lpar,lpar2,lpar3,mthvr,no,lnp4,lws1,ladd,           
c     +        lsupp,lim1,lim2,lim3,lim4,lim5,lim6,lim7,lim8,lim11              
                                                                                
c      integer maxids                                                           
                                                                                
      double precision ETA(lvr)                                                 
      integer MODE                                                              
      integer NTH,NETA,NEPS,NTH1,NETA1,NEPS1,NEIG                               
                                                                                
      unitp = 42                                                                
      UNITO = 43                                                                
C Assign number of theta,eta,omega that will be written to the file:            
       NTH1 = ntheta                                                            
C       NTH1 = 10                                                               
       NOM1 = neta                                                              
C       NOM1 = 5                                                                
       NSI1 = neps                                                              
C       NSI1 = 4                                                                
                                                                                
C     This INFN provides var-cov results assuming full block omega and sigma    
C     elements that are not estimated are output as 0.10000E+11                 
       NTC = ntheta                                                             
       NEC = neta                                                               
       NSC = neps                                                               
       NPAR = NTC+((NSC*NSC)-NSC)/2+NSC+((NEC*NEC)-NEC)/2+NEC                   
       NPR = ((NPAR*NPAR)-NPAR)/2+NPAR                                          
       OPEN(70,FILE='varcov.est')                                               
                                                                                
       write(70,300) (COV(I),I=1,NPR)                                           
  300  format(E12.6,",")                                                        
                                                                                
       CLOSE(70)                                                                
                                                                                
C      IF (ICALL.EQ.3) THEN                                                     
       OPEN(50,FILE='cwtab1.est')                                               
         MODE=0                                                                 
         CALL PASS(MODE)                                                        
         MODE=1                                                                 
         WRITE(50,*) 'ETAS'                                                     
   20      CALL PASS(MODE)                                                      
         IF (MODE.EQ.0) GO TO 30                                                
         IF (NEWIND.NE.2) THEN                                                  
           CALL GETETA(ETA)                                                     
           WRITE (50,97) (ETA(I),I=1,NETA)                                      
         ENDIF                                                                  
         GO TO 20                                                               
   30    CONTINUE                                                               
         WRITE (50,*) 'THETAS'                                                  
         WRITE (50,99) (THETAF(J),J=1,NTH)                                      
         WRITE(50,*) 'OMEGAS'                                                   
         DO 7000 I=1,NETA                                                       
 7000    WRITE (50,99) (OMEGAF(I,J),J=1,NETA)                                   
         WRITE(50,*) 'SIGMAS'                                                   
         DO 7999 I=1,NEPS                                                       
 7999    WRITE (50,99) (SIGMAF(I,J),J=1,NEPS)                                   
         CLOSE(50)                                                              
                                                                                
   99    FORMAT (70E15.7)                                                       
   98    FORMAT (2I8)                                                           
   97    FORMAT (20E15.7)                                                       
                                                                                
       OPEN (UNITO, FILE='FCON2')                                               
		CALL FILES(UNITO)                                                             
		READ (UNITO,34) DESC                                                          
          OPEN(unitp,FILE='NonmemRunLog.csv',ACCESS='append')                   
	    call files(unitp)                                                          
		write(unitp,35) DESC,IERE,IERC,object                                         
   35 format(A100, ",,", I3, ",", I3,",",e15.7,",",$)                           
                                                                                
                                                                                
C write out theta estimates                                                     
      do 770 i=1,NTH1                                                           
	   write(unitp,780)thetaf(i)                                                   
  770 continue	                                                                 
                                                                                
  780	format(e12.4,",",$)                                                       
                                                                                
C write out omega estimates                                                     
      do 771 i=1,NOM1                                                           
       do 771 j=1,i                                                             
           write(unitp,780) omegaf(i,j)                                         
  771 continue                                                                  
                                                                                
C write out sigma estimates                                                     
      do 772 i=1,NSI1                                                           
	       write(unitp,780) sigmaf(i,i)                                            
C	  else                                                                        
C	       write(unitp,781) sigmaf(i,i)                                           
C	  endif	                                                                      
  772 continue	                                                                 
  781	format(e12.4,",")                                                         
C   /)                                                                          
C      CLOSE(unitp)                                                             
C  782 format(I1,",",$)                                                         
                                                                                
C write out eigenvalues                                                         
      NEIG = nvls                                                               
      do 872 i=1, NEIG                                                          
	  if(i.LT.NEIG) THEN                                                           
	       write(unitp,780) vls(i)                                                 
	  else                                                                         
	       write(unitp,881) vls(i)                                                 
	  endif	                                                                       
  872 continue	                                                                 
  881	format(e12.4,",")                                                         
                                                                                
      if(NEIG.GE.1) THEN                                                        
      CLOSE(unitp)                                                              
      else                                                                      
      write(unitp,*) ','                                                        
      CLOSE(unitp)                                                              
      endif                                                                     
  782 format(I1,",",$)                                                          
	                                                                               
C Write standard errors of the parameters                                       
      OPEN(unitp,FILE='NonmemRunLog.csv',ACCESS='append')                       
		write(unitp,36) DESC                                                          
   36 format(A100,",RSE,,,,",$)                                                 
                                                                                
C write out RSE's for theta estimates                                           
      do 990 i=1,NTH1                                                           
	  if(seth(i).NE.0) THEN                                                        
                   write(unitp,780) abs(100*seth(i)/thetaf(i))                  
         else                                                                   
                   write(unitp,782) 0                                           
         endif                                                                  
  990 continue	                                                                 
                                                                                
C write out RSE's for omega estimates                                           
	  do 991 i=1,NOM1                                                              
	   do 991 j=1,i                                                                
	  if(omegaf(i,j).NE.0) THEN                                                    
                   write(unitp,780) abs(100*seom(i,j)/omegaf(i,j))              
         else                                                                   
                   write(unitp,782) 0                                           
         endif                                                                  
  991 continue                                                                  
                                                                                
C write out RSE's for sigma estimates                                           
      do 992 i=1,NSI1                                                           
	  if(i.LT.NSI1) THEN                                                           
	  if(sigmaf(i,i).NE.0) THEN                                                    
	         write(unitp,780) abs(100*sesig(i,i)/sigmaf(i,i))                      
         else                                                                   
                   write(unitp,782) 0                                           
         endif                                                                  
      else                                                                      
       if(sigmaf(i,i).NE.0) THEN                                                
	         write(unitp,780) abs(100*sesig(i,i)/sigmaf(i,i))                      
         else                                                                   
                   write(unitp,782) 0                                           
         endif                                                                  
      endif                                                                     
  992 continue	                                                                 
                                                                                
C write out eigenvalue ratio of largest to smallest value                       
C this works because NONMEM orders eigenvalues from smallest to largest         
C       write(unitp,999) 'Eigen. Ratio'                                         
C  999 format(A15,",",$)                                                        
       if(NEIG.GE.1) THEN                                                       
       write(unitp,780) abs(vls(NEIG)/vls(1))                                   
C       vls(NEIG)/vls(1))                                                       
C	   write(unitp, FMT='( /)')                                                   
C	   CLOSE(unitp)                                                               
	                                                                               
C write out placeholder zero's below eigenvalues                                
                                                                                
      do 1002 i=1,NEIG-1                                                        
      write(unitp, 782) 0                                                       
 1002 continue                                                                  
      else                                                                      
       write(unitp,*) ','                                                       
      endif                                                                     
	   write(unitp, FMT='( /)')                                                    
	   CLOSE(unitp)                                                                
                                                                                
   34	FORMAT( / 8X, 1A)                                                         
C        ENDIF                                                                  
       RETURN	                                                                  
       END                                                                      
                                                                                
      SUBROUTINE PK(ICALL,IDEF,THETA,IREV,EVTREC,NVNT,INDXS,IRGG,GG,          
     X NETAS)
      IMPLICIT DOUBLE PRECISION (A-Z)                                         
      REAL EVTREC                                                             
      SAVE
      INTEGER ICALL,IDEF,IREV,NVNT,INDXS,IRGG,NETAS                           
      DIMENSION IDEF(7,*),THETA(*),EVTREC(IREV,*),INDXS(*),GG(IRGG,71,*)
      COMMON/PROCM1/NEWIND                                                    
      INTEGER NEWIND                                                          
      COMMON/NMPRD4/BBBBB0(0004),TVCL,CL,TVV,V,S1,Y,IPRED,A00072
      COMMON/NMPRD4/A00074,A00076,A00078,A00079,A00080,D00001,D00002
      COMMON/NMPRD4/D00003,D00004,D00005,D00079,D00087,D00088,D00078
      COMMON/NMPRD4/D00084,C00071,D00090,D00089,C00072,BBBBBB(29969)
      DIMENSION COM(030000)
      EQUIVALENCE(BBBBB0(1),COM(1))
      COMMON/NMPRD7/ETA(70),EPS(70)                                           
      COMMON/ROCM12/MSEC,MFIRST
      INTEGER MSEC,MFIRST
      COMMON/PRCM00/MC0000(6),ME0000(6),MG0000(6),MT0000(6)                   
      INTEGER MC0000,ME0000,MG0000,MT0000                                     
      IF (ICALL.LE.1) THEN                                                    
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
      RETURN                                                                  
      ENDIF                                                                   
      IF (NEWIND.NE.2) THEN
       IF (ICALL.EQ.4) THEN
        CALL SIMETA(ETA)
       ELSE
        CALL GETETA(ETA)
       ENDIF
      ENDIF
      TVCL=THETA(01) 
      B00001=DEXP(ETA(01)) 
      CL=TVCL*B00001 
C                      A00072 = DERIVATIVE OF CL W.R.T. ETA(01)
      A00072=TVCL*B00001 
      TVV=THETA(02) 
      B00003=DEXP(ETA(02)) 
      V=TVV*B00003 
C                      A00076 = DERIVATIVE OF V W.R.T. ETA(02)
      A00076=TVV*B00003 
      S1=V 
C                      A00079 = DERIVATIVE OF S1 W.R.T. ETA(02)
      A00079=A00076 
      GG(01,1,1)=CL    
      GG(01,02,1)=A00072
      GG(02,1,1)=V     
      GG(02,03,1)=A00076
      GG(03,1,1)=S1    
      GG(03,03,1)=A00079
      IF (MSEC.EQ.1) THEN
C                      A00074 = DERIVATIVE OF A00072 W.R.T. ETA(01)
      A00074=TVCL*B00001 
C                      A00078 = DERIVATIVE OF A00076 W.R.T. ETA(02)
      A00078=TVV*B00003 
C                      A00080 = DERIVATIVE OF A00079 W.R.T. ETA(02)
      A00080=A00078 
      GG(01,02,02)=A00074
      GG(02,03,03)=A00078
      GG(03,03,03)=A00080
      ENDIF
      RETURN
      END
      SUBROUTINE ERROR (ICALL,IDEF,THETA,IREV,EVTREC,NVNT,INDXS,F,G,HH )      
      IMPLICIT DOUBLE PRECISION (A-Z)                                         
      REAL EVTREC                                                             
      SAVE
      INTEGER ICALL,IDEF,IREV,NVNT,INDXS                                      
      DIMENSION IDEF(*),THETA(*),EVTREC(IREV,*),INDXS(*),G(70,*)
      DIMENSION HH(70,*)
      COMMON/PROCM1/NEWIND                                                    
      INTEGER NEWIND                                                          
      COMMON/PROCM4/A(30),DAETA(30,70),D2AETA(30,70,70)
      COMMON/NMPRD1/IERPRD,NETEXT                                             
      COMMON/NMPRD2/ETEXT(3)                                                  
      INTEGER IERPRD,NETEXT                                                   
      CHARACTER*132 ETEXT                                                     
 991  FORMAT (35F14.4)
 992  FORMAT (35E15.7)
 998  FORMAT (I8,E15.7/)            
      COMMON/ROCM35/NTHES_,NETAS_,NEPSS_
      INTEGER NTHES_,NETAS_,NEPSS_
      COMMON/ROCM44/PR_Y
      COMMON/ROCM45/PR_CT
      COMMON/NMPRD4/BBBBB0(0004),TVCL,CL,TVV,V,S1,Y,IPRED,A00072
      COMMON/NMPRD4/A00074,A00076,A00078,A00079,A00080,D00001,D00002
      COMMON/NMPRD4/D00003,D00004,D00005,D00079,D00087,D00088,D00078
      COMMON/NMPRD4/D00084,C00071,D00090,D00089,C00072,BBBBBB(29969)
      DIMENSION COM(030000)
      EQUIVALENCE(BBBBB0(1),COM(1))
      COMMON/NMPRD3/COMACT,COMSAV
      INTEGER COMACT,COMSAV
      COMMON/NMPRD7/ETA(70),EPS(70)                                           
      COMMON/NMPR10/RPTI,RPTO,RPTON,PRDFL
      INTEGER RPTI,RPTO,RPTON,PRDFL
      COMMON/ROCM11/MIXNUM,MIXEST                                             
      INTEGER MIXNUM,MIXEST                                                   
      COMMON/ROCM25/MIXP(10)                                                  
      DOUBLE PRECISION MIXP                                                   
      COMMON/ROCM12/MSEC,MFIRST
      INTEGER MSEC,MFIRST
      COMMON/ROCM10/NREP,IREP
      INTEGER NREP,IREP
      COMMON/ROCM14/NPROB,IPROB,S1NUM,S2NUM,S1NIT,S2NIT,S1IT,S2IT
      INTEGER NPROB,IPROB,S1NUM,S2NUM,S1NIT,S2NIT,S1IT,S2IT
      COMMON/ROCM22/OMEGA(70,70)       
      COMMON/ROCM17/NEWL2
      INTEGER NEWL2
      COMMON/ROCM31/TEMPLT(20)
      REAL TEMPLT
      COMMON/ROCM32/NIREC,NDREC
      INTEGER NIREC,NDREC
      COMMON/ROCM46/NINDR,INDR1,INDR2
      INTEGER NINDR,INDR1,INDR2
      COMMON/ROCM48/LIREC
      INTEGER LIREC
      COMMON/NMPR12/YLO,YUP
      COMMON/NMPR13/CTLO,DCTLO(70),DDCTLO(70,70)
      COMMON/NMPR14/CTUP,DCTUP(70),DDCTUP(70,70)
      COMMON/NMPR16/THSIMP(70),OMSIMP(70,70),SGSIMP(70,70)
      COMMON/ROCM18/DEN_,CDEN_(70)
      COMMON/ROCM50/IIDX(010000),CNTID(010000)
      INTEGER IIDX
      COMMON/CM7/NIND_7,IC0701(8)
      INTEGER NIND_7,IC0701
      COMMON/NMPR17/F_FLAG
      INTEGER F_FLAG
      COMMON/PRCM00/MC0000(6),ME0000(6),MG0000(6),MT0000(6)                   
      INTEGER MC0000,ME0000,MG0000,MT0000                                     
      COMMON/PROCMA/MNOW,MPAST(0:030),MNEXT(0:030)                      
      INTEGER MNOW,MPAST,MNEXT
      COMMON/PRDPK1/MTIME(0:030)                                        
      DOUBLE PRECISION MTIME
      COMMON/PRDPK2/MTDIFF                                                    
      INTEGER MTDIFF
      IF (ICALL.LE.1) THEN                                                    
      MC0000(2)=30
      ME0000(2)=70
      MG0000(2)=080
      MT0000(2)=70
      IDEF(2)=-1
      IDEF(3)=00
      RETURN
      ENDIF
      IF (ICALL.EQ.4) THEN
      IF (NEWL2.EQ.1) CALL SIMEPS(EPS)
      ENDIF
      D00001=G(01,1)
      D00002=G(02,1)
      Y=F+F*EPS(01)+EPS(02) 
C                      D00076 = DERIVATIVE OF Y W.R.T. ETA(02)
      D00076=D00002 
C                      D00077 = DERIVATIVE OF Y W.R.T. ETA(01)
      D00077=D00001 
C                      D00078 = DERIVATIVE OF Y W.R.T. ETA(02)
      D00078=EPS(01)*D00002+D00076 
C                      D00079 = DERIVATIVE OF Y W.R.T. ETA(01)
      D00079=EPS(01)*D00001+D00077 
C                      C00071 = DERIVATIVE OF Y W.R.T. EPS(01)
      C00071=F 
C                      C00072 = DERIVATIVE OF Y W.R.T. EPS(02)
      C00072=1.D0 
C                      D00089 = DERIVATIVE OF C00071 W.R.T. ETA(02)
      D00089=D00002 
C                      D00090 = DERIVATIVE OF C00071 W.R.T. ETA(01)
      D00090=D00001 
      IPRED=F 
      G(01,1)=D00079  
      G(02,1)=D00078  
      HH(01,1)=C00071 
      HH(02,1)=C00072 
      HH(01,02)=D00090
      HH(01,03)=D00089
      IF (MSEC.EQ.1) THEN
      D00003=G(01,02)
      D00004=G(02,02)
      D00005=G(02,03)
C                      D00080 = DERIVATIVE OF D00076 W.R.T. ETA(02)
      D00080=D00005 
C                      D00081 = DERIVATIVE OF D00077 W.R.T. ETA(02)
      D00081=D00004 
C                      D00082 = DERIVATIVE OF D00077 W.R.T. ETA(01)
      D00082=D00003 
C                      D00083 = DERIVATIVE OF D00078 W.R.T. ETA(02)
      D00083=EPS(01)*D00005 
C                      D00084 = DERIVATIVE OF D00078 W.R.T. ETA(02)
      D00084=D00080+D00083 
C                      D00085 = DERIVATIVE OF D00079 W.R.T. ETA(02)
      D00085=EPS(01)*D00004 
C                      D00086 = DERIVATIVE OF D00079 W.R.T. ETA(01)
      D00086=EPS(01)*D00003 
C                      D00087 = DERIVATIVE OF D00079 W.R.T. ETA(01)
      D00087=D00082+D00086 
C                      D00088 = DERIVATIVE OF D00079 W.R.T. ETA(02)
      D00088=D00081+D00085 
      G(01,02)=D00087 
      G(02,02)=D00088 
      G(02,03)=D00084 
      ENDIF
      F=Y
      COM(1)=G(1,1)
      COM(2)=G(2,1)
      COM(3)=HH(1,1)
      COM(4)=HH(2,1)
      RETURN
      END
