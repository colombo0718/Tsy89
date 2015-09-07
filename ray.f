C--GRAF1-----------
      PROGRAM GRAF1
C
C THIS IS AN EXAMPLE OF USING 'TRACE' SUBROUTINE
C IN CASE OF EXPLICIT SPECIFICATION OF DIPOLE TILT ANGLE .
C IT IS ASSUMED THAT WE KNOW THE GSM LATITUDE AND LONGITUDE OF THE
C STARTING POINT AT THE EARTH'S SURFACE
C ONE MORE ASSUMPTION IS THAT THE MAIN (INTERNAL) FIELD IS PURELY DIPOLAR
C
      DIMENSION XX(500),YY(500),ZZ(500)
c
c  do not forget to include the following EXTERNAL statement in your codes !
c    
      EXTERNAL EXTERN
      integer aa
C
C------------------------------
C     IOPT=1
      PRINT *, ' Enter Kp-index version number IOPT, according to:'
      print*,
     *'IOPT= 1       2         3         4         5        6       7'
      print*,
     *'KP=  0,0+  1-,1,1+   2-,2,2+   3-,3,3+   4-,4,4+  5-,5,5+   >=6-'
!       read *,IOPT
      iopt=5
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
      do aa=0,5
      XLAT=-20
      XLON=aa*60
      Rini=1.05
      DIR=1

        print *, ' Enter XLAT,XLON,Rini,DIR: [ DIR=0 to stop] '
        print*,XLAT
!        READ *,XLAT,XLON,Rini, DIR
        if(dir.eq.0.) stop
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

      DO L=1,M
      WRITE(10,"('[',3(f6.2,','),'],')") XX(L),YY(L),ZZ(L)
      ENDDO

      end do 
      write(10,*)']'
      END
