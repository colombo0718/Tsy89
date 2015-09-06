C--GRAF1-----------
      PROGRAM GRAF1
C
C   THIS IS AN EXAMPLE OF USING 'TRACE' SUBROUTINE
C    IN CASE OF EXPLICIT SPECIFICATION OF DIPOLE TILT ANGLE .
C    IT IS ASSUMED THAT WE KNOW THE GSM LATITUDE AND LONGITUDE OF THE
C    STARTING POINT AT THE EARTH'S SURFACE
C      ONE MORE ASSUMPTION IS THAT THE MAIN (INTERNAL) FIELD IS PURELY DIPOLAR
C
      COMMON AA(10),SPS,CPS,BB(3),PS,CC(11),KK(2),DD(8)
      DIMENSION XX(500),YY(500),ZZ(500)
c
c  do not forget to include the following EXTERNAL statement in your codes !
c    
      EXTERNAL EXTERN
C
C------------------------------
C     IOPT=1
      PRINT *, ' Enter Kp-index version number IOPT, according to:'
      print*,
     *'IOPT= 1       2         3         4         5        6       7'
      print*,
     *'KP=  0,0+  1-,1,1+   2-,2,2+   3-,3,3+   4-,4,4+  5-,5,5+   >=6-'
       read *,IOPT
C------------------------------
C      PS=0.
      PRINT * , ' Enter tilt angle in degs and maximum distance Rmax:'
      read *, tilt, Rmax
      ps=tilt*0.01745329
C
       SPS=SIN(PS)
       CPS=COS(PS)
C
C WE HAVE SPECIFIED THE DIPOLE TILT ANGLE (PS), ITS SINE (SPS) AND COSINE (CPS)
C------------------------------
      Rmin=1.     !minimum R
C     Rmax=60.    !maximum R
      IHarm=0     !max. order of spheric harmonics
      id=31       !free format output files fort.31, 32, ...
      id0=30      !unformat output file fort.30
C------------------------------
      PRINT *,' Enter KGSM = 0 : for input XLAT, XLAT, Rini, DIR'
      PRINT *,'       KGSM = 1 : for input XGSM, YGSM, ZGSM, DIR'
      print *,'where XLAT, XLON, Rini are latitude, longitude, andr' 
      print *,'      radius of the initial point'
      print *,'XGSM, YGSM, ZGSM are GSM coordinate of initial point'
      print *,'DIR = +1 :  (N-to-S) tracing along -B direction'
      print *,'DIR = -1 :  (S-to-N) tracing along +B direction'
      READ * , KGSM
   30 CONTINUE
      IF(KGSM.EQ.0) THEN
C------------------------------
C     XLAT=75.
C     XLON=180.
C
      print *, ' Enter XLAT,XLON,Rini,DIR: [ DIR=0 to stop] '
      READ *,XLAT,XLON,Rini, DIR
      if(dir.eq.0.) stop
C
c  we have defined the latitude (XLAT) and longitude (XLON) in the GSM system
C------------------------------
       T=(90.-XLAT)*.01745329
       XL=XLON*.01745329
      XGSM=Rini*SIN(T)*COS(XL)
      YGSM=Rini*SIN(T)*SIN(XL)
      ZGSM=Rini*COS(T)
C------------------------------
      ELSE
C------------------------------
      print *, ' Enter XGSM,YGSM,ZGSM,DIR: [ DIR=0 to stop]'
      READ *,XGSM,YGSM,ZGSM, DIR
      if(dir.eq.0.) stop
C------------------------------
      ENDIF
C------------------------------
C
c  we have defined the X, Y, Z coordinates in the GSM system
C------------------------------
C     CALL TRACE(XGSM,YGSM,ZGSM,1.,60.,1.,0,500,
C    *  IOPT,EXTERN,XF,YF,ZF,XX,YY,ZZ,M)
      CALL TRACE(XGSM,YGSM,ZGSM,DIR, Rmax, Rmin, IHarm, 500,
     *  IOPT,EXTERN,XF,YF,ZF,XX,YY,ZZ,M)
      print *,'NP=500, M=',M
      print *,'final position GSM coordinate XF, YF, ZF='
      print *, XF, YF, ZF
C
C    THE MEANING OF INPUT PARAMETERS FOR TRACE IS EXPLAINED IN
C     THE FILE  GEOPACK.TXT
C
C   WRITE THE RESULTS IN THE DATA FILE:
C
      WRITE(id,*) 'input parameters IOPT, tilt=', IOPT, tilt
      WRITE(id0) IOPT,tilt
      WRITE(id,*) 'initial tracing point and dirction'
      IF(KGSM.EQ.0) THEN
      WRITE(id,*) ' XLAT, XLON, Rini, DIR='
      WRITE(id,*) XLAT, XLON, Rini, DIR
      WRITE(id0) KGSM, XLAT, XLON, Rini, DIR, XF, YF, ZF
      ELSE
      WRITE(id,*) ' XGSM, YGSM, ZGSM, DIR='
      WRITE(id,*) XGSM, YGSM, ZGSM, DIR
      WRITE(id0) KGSM, XGSM, YGSM, ZGSM, DIR, XF, YF, ZF
      ENDIF
      WRITE(id,*) 'final position GSM coordinate XF, YF, ZF='
      WRITE(id,*) XF, YF, ZF
      WRITE(id,*) 'tracing field-line GSM coordinates XX, YY, ZZ='
      DO L=1,M
      WRITE(id,*) XX(L),YY(L),ZZ(L)
      ENDDO
      id=id+1
      write(id0) M, (XX(L), YY(L), ZZ(L), L=1,M)
C      OPEN(UNIT=1, FILE='LINTEST1.DAT')
C 1   WRITE (1,20) (XX(L),YY(L),ZZ(L),L=1,M)
C20   FORMAT((2X,4(3F6.2,1X)))
C      CLOSE(UNIT=1)
      GO TO 30
C     STOP
      END
