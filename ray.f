C--GRAF1-----------
      PROGRAM GRAF1
C
C THIS IS AN EXAMPLE OF USING 'TRACE' SUBROUTINE
C IN CASE OF EXPLICIT SPECIFICATION OF DIPOLE TILT ANGLE .
C IT IS ASSUMED THAT WE KNOW THE GSM LATITUDE AND LONGITUDE OF THE
C STARTING POINT AT THE EARTH'S SURFACE
C ONE MORE ASSUMPTION IS THAT THE MAIN (INTERNAL) FIELD IS PURELY DIPOLAR
C
      COMMON AA(10),SPS,CPS,BB(3),PS,CC(11),KK(2),DD(8)
      DIMENSION XX(500),YY(500),ZZ(500)
c
c  do not forget to include the following EXTERNAL statement in your codes !
c    
      EXTERNAL EXTERN
      integer mm
C
C------------------------------
C     IOPT=1
      PRINT *, ' Enter Kp-index version number IOPT, according to:'
      print*,
     *'IOPT= 1       2         3         4         5        6       7'
      print*,
     *'KP=  0,0+  1-,1,1+   2-,2,2+   3-,3,3+   4-,4,4+  5-,5,5+   >=6-'
!       read *,IOPT
      iopt=6
C------------------------------
C      PS=0.
      PRINT * , ' Enter tilt angle in degs and maximum distance Rmax:'
!      read *, tilt, Rmax
      tilt=0
      ps=tilt*0.01745329
C
       SPS=SIN(PS)
       CPS=COS(PS)
C
C WE HAVE SPECIFIED THE DIPOLE TILT ANGLE (PS), ITS SINE (SPS) AND COSINE (CPS)
C------------------------------
      Rmin=1.     !minimum R
      Rmax=100.    !maximum R
      IHarm=0     !max. order of spheric harmonics
C------------------------------
      PRINT *,' Enter KGSM = 0 : for input XLAT, XLAT, Rini, DIR'
      PRINT *,'       KGSM = 1 : for input XGSM, YGSM, ZGSM, DIR'
      print *,' XLAT, XLON, Rini are latitude, longitude, and radius' 
      print *,' XGSM, YGSM, ZGSM are GSM coordinate'
      print *,'      radius of the initial point'
      print *,'DIR = +1 :  (N-to-S) tracing along -B direction'
      print *,'DIR = -1 :  (S-to-N) tracing along +B direction'
!      READ * , KGSM
      kgsm=0


      open(unit=10,file='fieldLine.js')
      write(10,*)'field=['
    1 continue
!      XLAT=70
!      XLON=60*mm
!      Rini=1.
!      DIR=1

        print *, ' Enter XLAT,XLON,Rini,DIR: [ DIR=0 to stop] '
        READ *,XLAT,XLON,Rini, DIR
        print*,XLAT,XLON,Rini, DIR
        if(dir.eq.0.)goto 2
        T=(90.-XLAT)*.01745329
        XL=XLON*.01745329
        XGSM=Rini*SIN(T)*COS(XL)
        YGSM=Rini*SIN(T)*SIN(XL)
        ZGSM=Rini*COS(T)
        print *, 'initial XGSM,YGSM,ZGSM ='
        print *,XGSM,YGSM,ZGSM

      CALL TRACE(XGSM,YGSM,ZGSM,DIR,Rmax,Rmin,IHarm,500,
     |  IOPT,EXTERN,XF,YF,ZF,XX,YY,ZZ,M)
      print *,'NP=500, M=',M
      print *,'final position GSM coordinate XF, YF, ZF='
      print *, XF, YF, ZF

      write(10,*)'['
      WRITE(10,"(3(a1,f7.2),a2)")
     |('[',XX(L),',',YY(L),',',ZZ(L),'],',L=1,M)
      write(10,*)'],'

      goto 1
    2 write(10,*)']'
      END
