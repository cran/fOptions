
C This library is free software; you can redistribute it and/or
C modify it under the terms of the GNU Library General Public
C License as published by the Free Software Foundation; either
C version 2 of the License, or (at your option) any later version.
C
C This library is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
C GNU Library General Public License for more details.
C
C You should have received a copy of the GNU Library General 
C Public License along with this library; if not, write to the 
C Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
C MA  02111-1307  USA


COPYRIGHT: DIETHELM WUERTZ, SEPT. 2002


C------------------------------------------------------------------------------

C     INITHALTON (DIMEN, QUASI, BASE, OFFSET)
C     NEXTHALTON (DIMEN, QUASI, BASE, OFFSET)
C     HALTON (QN, N, DIMEN, QUASI, BASE, OFFSET, INIT, TRANSFORM)
C     REAL*8 FUNCTION HQNORM(P)

C------------------------------------------------------------------------------


      SUBROUTINE INITHALTON(DIMEN, QUASI, BASE, OFFSET)

C     INITIALIZE THE HALTON LOW DISCREPANCY SEQUENCE.
C     THE BASE IS CALCULATED FROM PRIMES

      INTEGER DIMEN, BASE(DIMEN), ITER(DIMEN), OFFSET, DIGIT
      REAL*8 QUASI(DIMEN), HALF
      INTRINSIC MOD
 
C     INIT BASE FROM PRIMES - THIS IMPLEMENTS A SIMMPLE SIEVE:
      BASE(1) = 2
      BASE(2) = 3
      N = 3
      NC = 2
      DO WHILE(NC.LT.DIMEN)
	  M = N/2
	  K = 0
	  IF (MOD(N,2).NE.0.AND.MOD(N,3).NE.0) THEN
	     DO I = 5, M
               IF(MOD(N,I).EQ.0) K = K + 1
         ENDDO
	     IF (K.EQ.0) THEN
	        NC = NC + 1
	        BASE(NC) = N
	     ENDIF
	  ENDIF
	  N = N + 1
      ENDDO
      
C     NOW CREATE THE FIRST QUASI RANDOM NUMBER:
      OFFSET = 0
      DO NB = 1, DIMEN	      
	  ITER(NB) = OFFSET
	  QUASI(NB) = 0.0D0
	  HALF = 1.0D0 / BASE(NB)
	  DO WHILE (ITER(NB).NE.0)
	     DIGIT = MOD ( ITER(NB), BASE(NB) )
	     QUASI(NB) = QUASI(NB) + DIGIT * HALF
	     ITER(NB) = ( ITER(NB) - DIGIT ) / BASE(NB)
	     HALF = HALF / BASE(NB)
	  ENDDO 
      ENDDO

C     SET THE COUNTER:
      OFFSET = OFFSET + 1
      
      RETURN
      END
  

C------------------------------------------------------------------------------

  
      SUBROUTINE NEXTHALTON(DIMEN, QUASI, BASE, OFFSET) 

C     GENERATE THE NEXT POINT IN HALTON'S LOW DISCREPANCY SEQUENCE
C     NOTE, THAT WE HAVE ALREADY "OFFSET" POINTS GENERATED.

      INTEGER DIMEN, BASE(DIMEN), ITER(DIMEN), OFFSET, DIGIT
      REAL*8 QUASI(DIMEN), HALF
      INTRINSIC MOD
  	  	   
      DO NB = 1, DIMEN      
	  ITER(NB) = OFFSET
	  QUASI(NB) = 0.0D0
	  HALF = 1.0 / BASE(NB)
	  DO WHILE (ITER(NB).NE.0)
	     DIGIT = MOD ( ITER(NB), BASE(NB) )
	     QUASI(NB) = QUASI(NB) + DIGIT * HALF
	     ITER(NB) = ( ITER(NB) - DIGIT ) / BASE(NB)
	     HALF = HALF / BASE(NB)
	  ENDDO 
      ENDDO	      

C     INCREASE THE COUNTER BY ONE:
      OFFSET = OFFSET + 1	 

      RETURN
      END


C------------------------------------------------------------------------------   


      SUBROUTINE HALTON(QN, N, DIMEN, BASE, OFFSET, INIT, TRANSFORM)

C     THIS IS AN INTERFACE TO CREATE "N" POINTS IN "DIMEN" DIMENSIONS
C     ARGUMENTS:
C       QN        - THE QUASI NUMBERS, A "N" BY "DIMEN" ARRAY
C       N         - NUMBERS OF POINTS TO GENERATE
C       DIMEN     - THE DIMENSION
C       QUASI     - THE LAST POINT IN THE SEQUENDE
C       BASE      - THE PRIME BASE, A VECTOR OF LENGTH "DIMEN"
C       OFFSET    - THE OFFSET OF POINTS IN THE NEXT FUNCTION CALL
C       INIT      - IF ONE, WE INITIALIZE
C       TRANSFORM - A FLAG, 0 FOR UNIFORM, 1 FOR NORMAL DISTRIBUTION

      INTEGER N, DIMEN, OFFSET, INIT, TRANSFORM
      INTEGER BASE(DIMEN)
      REAL*8 QN(N,DIMEN), QUASI(DIMEN)

C     IF REQUESTED, INITIALIZE THE GENERATOR:
      IF (INIT.EQ.1) THEN
	  CALL INITHALTON(DIMEN, QUASI, BASE, OFFSET)	 
      ENDIF

C     GENERATE THE NEXT "N" QUASI RANDOM NUMBERS:
      DO I=1, N
         CALL NEXTHALTON(DIMEN, QUASI, BASE, OFFSET)
	  IF (TRANSFORM.EQ.1) THEN
	     DO J = 1, DIMEN
            QN(I, J) = HQNORM(QUASI(J))
	     ENDDO
	  ELSE
         DO J = 1, DIMEN
            QN(I, J) = QUASI(J)	       
	     ENDDO
	  ENDIF
      ENDDO

      RETURN
      END


C -----------------------------------------------------------------------------


      REAL*8 FUNCTION HQNORM(P)

C     USED TO CALCULATE HALTON NORMAL DEVIATES:
      REAL*8 P,R,T,A,B, EPS
      DATA P0,P1,P2,P3,P4, Q0,Q1,Q2,Q3,Q4
     &   /-0.322232431088E+0, -1.000000000000E+0, -0.342242088547E+0, 
     &    -0.204231210245E-1, -0.453642210148E-4, +0.993484626060E-1,
     &    +0.588581570495E+0, +0.531103462366E+0, +0.103537752850E+0,  
     &    +0.385607006340E-2  /

C     NOTE, IF P BECOMES 1, THE PROGRAM FAILS TO CALCULATE THE
C     NORMAL RDV. IN THIS CASE WE REPLACE THE LOW DISCREPANCY 
C     POINT WITH A POINT FAR IN THE TAILS.
      EPS = 1.0D-6
      IF (P.GE.(1.0D0-EPS)) P = 1.0d0 - EPS
      IF (P.LE.EPS) P = EPS
      IF (P.NE.0.5D0) GOTO 150
 50   HQNORM = 0.0D0
      RETURN
150   R = P 
      IF (P.GT.0.5D0) R = 1.0 - R
      T = DSQRT(-2.0*DLOG(R))
      A = ((((T*P4 + P3)*T+P2)*T + P1)*T + P0)
      B = ((((T*Q4 + Q3)*T+Q2)*T + Q1)*T + Q0)
      HQNORM = T + (A/B)
      IF (P.LT.0.5D0) HQNORM = -HQNORM
      
      RETURN
      END 


C -----------------------------------------------------------------------------


      SUBROUTINE TESTHALTON()
      
      INTEGER N1,N2,DIMEN,OFFSET,TRANSFORM
      PARAMETER (N1=20,N2=N1/2,DIMEN=5)
      INTEGER BASE(DIMEN)
      REAL*8 QN1(N1,DIMEN),QN2(N2,DIMEN)

      TRANSFORM = 0
      
C     FIRST TEST RUN:
      INIT = 1
      OFFSET = 0
      CALL HALTON(QN1,N1,DIMEN,BASE,OFFSET,INIT,TRANSFORM)

      WRITE (*,*) 
      WRITE (*,*) "HALTON SEQUENCE: 1-20"  
      WRITE (*,*) 
      WRITE (*,7) "N/DIMEN:", (J, J=1,DIMEN,INT(DIMEN/5))   
      DO I=1, N1, INT(N1/(2*10))
         WRITE (*,8) I, (QN1(I,J), J=1, DIMEN, INT(DIMEN/5))
      ENDDO

C     SECOND TEST RUN:
      INIT=1
      OFFSET = 0
      CALL HALTON(QN2,N2,DIMEN,BASE,OFFSET,INIT,TRANSFORM)
      WRITE (*,*) 
      WRITE (*,*) "HALTON SEQUENCE: 1-10 RE-INITIALIZED"  
      WRITE (*,*)  
      WRITE (*,7) "N/DIMEN:", (J, J=1,DIMEN,INT(DIMEN/5))   
      DO I=1, N2, INT(N2/10)
         WRITE (*,8) I, (QN2(I,J), J=1, DIMEN, INT(DIMEN/5))
      ENDDO

      INIT = 0
      CALL HALTON(QN2,N2,DIMEN,BASE,OFFSET,INIT,TRANSFORM)
      WRITE (*,*) 
      WRITE (*,*) "HALTON SEQUENCE: 11-20 CONTINUED"  
      WRITE (*,*) 
      WRITE (*,7) "N/DIMEN:", (J, J=1,DIMEN,INT(DIMEN/5))   
      DO I=1, N2, INT(N2/10)
         WRITE (*,8) I+N2, (QN2(I,J), J=1, DIMEN, INT(DIMEN/5))
      ENDDO

 7    FORMAT(1H ,A8, 10I10)
 8    FORMAT(1H ,I8, 10F10.6)
      
      RETURN
      END


C -----------------------------------------------------------------------------


c      program mainhalton
c      call testhalton
c      end
  

C -----------------------------------------------------------------------------

