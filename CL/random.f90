
!******************************************************************
    FUNCTION SPLIT(MU,SIGMA_up,SIGMA_dn) !LHS sigma1, RHS sigma2
!*
!*       Returns a random number Normally distributed with mean
!*       MU and standard deviation |SIGMA|, using the Box-Muller
!*       algorithm
!*
       DOUBLE PRECISION THETA,R,ZBQLNOR,ZBQLU01,PI,MU,SIGMA_up,SIGMA_dn
       DOUBLE PRECISION SPARE,SPLIT
       INTEGER STATUS
       SAVE STATUS,SPARE,PI
       DATA STATUS /-1/

       IF (STATUS.EQ.-1) PI = 4.0D0*DATAN(1.0D0)

       IF (STATUS.LE.0) THEN
       THETA = 2.0D0*PI*ZBQLU01(0.0D0)
       R = DSQRT( -2.0D0*DLOG(ZBQLU01(0.0D0)) )
       ZBQLNOR = (R*DCOS(THETA))
       SPARE = (R*DSIN(THETA))
       STATUS = 1
       ELSE
       ZBQLNOR = SPARE
       STATUS = 0
       ENDIF
      
 if(ZBQLNOR.lt.0)then
       SPLIT = MU + (SIGMA_dn*ZBQLNOR)
 elseif(ZBQLNOR.ge.0)then
       SPLIT = MU + (SIGMA_up*ZBQLNOR)
 endif
 END
!******************************************************************


!******************************************************************
       FUNCTION ZBQLNOR(MU,SIGMA)
!*
!*       Returns a random number Normally distributed with mean
!*       MU and standard deviation |SIGMA|, using the Box-Muller
!*       algorithm
!*
       DOUBLE PRECISION THETA,R,ZBQLNOR,ZBQLU01,PI,MU,SIGMA
       DOUBLE PRECISION SPARE
       INTEGER STATUS
       SAVE STATUS,SPARE,PI
       DATA STATUS /-1/

       IF (STATUS.EQ.-1) PI = 4.0D0*DATAN(1.0D0)

       IF (STATUS.LE.0) THEN
       THETA = 2.0D0*PI*ZBQLU01(0.0D0)
       R = DSQRT( -2.0D0*DLOG(ZBQLU01(0.0D0)) )
       ZBQLNOR = (R*DCOS(THETA))
       SPARE = (R*DSIN(THETA))
       STATUS = 1
       ELSE
       ZBQLNOR = SPARE
       STATUS = 0
       ENDIF
      
       ZBQLNOR = MU + (SIGMA*ZBQLNOR)

       END
!******************************************************************

!******************************************************************
       FUNCTION ZBQLU01(DUMMY)
!*
       DOUBLE PRECISION ZBQLU01,DUMMY,B,C,ZBQLIX(43),X,B2,BINV
       INTEGER CURPOS,ID22,ID43

       COMMON /ZBQL0001/ ZBQLIX,B,C
       SAVE /ZBQL0001/
       SAVE CURPOS,ID22,ID43
       DATA CURPOS,ID22,ID43 /1,22,43/

       B2 = B
       BINV = 1.0D0/B
 5     X = ZBQLIX(ID22) - ZBQLIX(ID43) - C
       IF (X.LT.0.0D0) THEN
       X = X + B
       C = 1.0D0
       ELSE
       C = 0.0D0
       ENDIF
       ZBQLIX(ID43) = X
!*
!*     Update array pointers. Do explicit check for bounds of each to
!*     avoid expense of modular arithmetic. If one of them is 0 the others
!*     won't be
!*
       CURPOS = CURPOS - 1
       ID22 = ID22 - 1
       ID43 = ID43 - 1
       IF (CURPOS.EQ.0) THEN
       CURPOS=43
       ELSEIF (ID22.EQ.0) THEN
       ID22 = 43
       ELSEIF (ID43.EQ.0) THEN
       ID43 = 43
       ENDIF
!*
!*     The integer arithmetic there can yield X=0, which can cause 
!*     problems in subsequent routines (e.g. ZBQLEXP). The problem
!*     is simply that X is discrete whereas U is supposed to 
!*     be continuous - hence if X is 0, go back and generate another
!*     X and return X/B^2 (etc.), which will be uniform on (0,1/B). 
!*
       IF (X.LT.BINV) THEN
       B2 = B2*B
       GOTO 5
       ENDIF

       ZBQLU01 = X/B2

       END
!******************************************************************

! Uniform eaandom number between A and B
!******************************************************************
      FUNCTION ZBQLUAB(A,B)
!*
!*       Returns a random number uniformly distributed on (A,B)
!*
      DOUBLE PRECISION A,B,ZBQLU01,ZBQLUAB
      
!*
!*       Even if A > B, this will work as B-A will then be -ve
!*
      IF (A.NE.B) THEN
       ZBQLUAB = A + ( (B-A)*ZBQLU01(0.0D0) )
      ELSE
       ZBQLUAB = A
       WRITE(*,1)
      ENDIF
 1    FORMAT(/5X,'****WARNING**** (function ZBQLUAB) Upper and lower limits on uniform',/5X,'distribution are identical',/)
      END
!******************************************************************



      BLOCK DATA ZBQLBD01
      COMMON /ZBQL0001/ ZBQLIX,B,C
      DOUBLE PRECISION ZBQLIX(43),B,C
      INTEGER I
 DATA (ZBQLIX(I),I=1,43) /8.001441D7,5.5321801D8, &
     +1.69570999D8,2.88589940D8,2.91581871D8,1.03842493D8, &
     +7.9952507D7,3.81202335D8,3.11575334D8,4.02878631D8, &
     +2.49757109D8,1.15192595D8,2.10629619D8,3.99952890D8, &
     +4.12280521D8,1.33873288D8,7.1345525D7,2.23467704D8, &
     +2.82934796D8,9.9756750D7,1.68564303D8,2.86817366D8, &
     +1.14310713D8,3.47045253D8,9.3762426D7 ,1.09670477D8, &
     +3.20029657D8,3.26369301D8,9.441177D6,3.53244738D8, &
     +2.44771580D8,1.59804337D8,2.07319904D8,3.37342907D8, &
     +3.75423178D8,7.0893571D7 ,4.26059785D8,3.95854390D8, &
     +2.0081010D7,5.9250059D7,1.62176640D8,3.20429173D8, &
     +2.63576576D8/
      DATA B / 4.294967291D9 /
      DATA C / 0.0D0 /
      END

      SUBROUTINE ZBQLINI(SEED)
      INTEGER LFLNO
      PARAMETER (LFLNO=80)
      INTEGER SEED,SS,MM,HH,DD,FILNO,I
      INTEGER INIT
      DOUBLE PRECISION ZBQLIX(43),B,C
      DOUBLE PRECISION TMPVAR1,DSS,DMM,DHH,DDD

      COMMON /ZBQL0001/ ZBQLIX,B,C
      SAVE INIT

      IF (INIT.GE.1) THEN
       IF(INIT.EQ.1) THEN
        WRITE(*,1)
        INIT = 2
       ENDIF
       RETURN
      ELSE
       INIT = 1
      ENDIF
      IF (SEED.EQ.0) THEN
       CALL SYSTEM(' date +%S%M%H%j > zbql1234.tmp')
       FILNO = LFLNO
 10    OPEN(FILNO,FILE='zbql1234.tmp',ERR=11)
       GOTO 12
 11    FILNO = FILNO + 1
       IF (FILNO.GT.999) THEN
        WRITE(*,2)
        RETURN
       ENDIF
       GOTO 10
 12    READ(FILNO,'(3(I2),I3)') SS,MM,HH,DD
       CLOSE(FILNO)
       CALL SYSTEM('rm zbql1234.tmp')
       DSS = DINT((DBLE(SS)/6.0D1) * B)
       DMM = DINT((DBLE(MM)/6.0D1) * B)
       DHH = DINT((DBLE(HH)/2.4D1) * B)
       DDD = DINT((DBLE(DD)/3.65D2) * B)
       TMPVAR1 = DMOD(DSS+DMM+DHH+DDD,B)
      ELSE
       TMPVAR1 = DMOD(DBLE(SEED),B)
      ENDIF
      ZBQLIX(1) = TMPVAR1
      DO 100 I = 2,43
       TMPVAR1 = ZBQLIX(I-1)*3.0269D4
       TMPVAR1 = DMOD(TMPVAR1,B)       
       ZBQLIX(I) = TMPVAR1
 100  CONTINUE

 1    FORMAT(//5X,'****WARNING****',//)
 2    FORMAT(//5X,'**** ERROR ****',//)
      END
!******************************************************************
