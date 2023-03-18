! =============================================================================
!
! MIT License
!
! Copyright (c) 2023 Patrizia Favaron
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
      MODULE SPLINES
!-----------------------------------------------------------------------
!     Last change:  JEK  11 Mar 99    1:09 pm
!     Module containing interfaces for the Fortran 90 versions of
!     the Duck series. It will eventually include UGLYDK, EVALDK,
!     DRIVDK and INTEDK.
!-----------------------------------------------------------------------

         INTERFACE UGLYDK
            SUBROUTINE UGLYDK(NCL,NCR,XIN,YIN,ESL,ESR,CUBIC)
               REAL, DIMENSION(:), INTENT(IN) :: XIN,YIN
               REAL, DIMENSION(:,:), INTENT(OUT) :: CUBIC
               REAL, INTENT(IN) :: ESL,ESR
               INTEGER, INTENT(IN) :: NCL,NCR
            END SUBROUTINE UGLYDK
         END INTERFACE

         INTERFACE EVALDK
            SUBROUTINE EVALDKA(X,Y,CUBIC) !array version
               REAL, DIMENSION(:), INTENT(IN)  :: X
               REAL, DIMENSION(:), INTENT(OUT) :: Y
               REAL, DIMENSION(:,:), INTENT(IN) :: CUBIC
            END SUBROUTINE EVALDKA
            SUBROUTINE EVALDKP(X,Y,CUBIC) !point version
               REAL, INTENT(IN)  :: X
               REAL, INTENT(OUT) :: Y
               REAL, DIMENSION(:,:), INTENT(IN) :: CUBIC
               END SUBROUTINE EVALDKP
         END INTERFACE

         INTERFACE ISPAN
            INTEGER FUNCTION ISPAN(X,CUBIC)
               REAL, INTENT(IN) :: X
               REAL, DIMENSION(:,:), INTENT(IN) :: CUBIC
            END FUNCTION ISPAN
         END INTERFACE

         INTERFACE INTDK1
            SUBROUTINE INTDK1(XL,XU,YDX,CUBIC)
               REAL,  INTENT(IN) :: XL,XU  ! Upper and lower limits of integral
               REAL, DIMENSION(:,:), INTENT(INOUT) :: CUBIC  ! Spline cubic array
               REAL, INTENT(OUT) :: YDX   ! Integral frol XL to XU
            END SUBROUTINE INTDK1
         END INTERFACE

      END MODULE SPLINES


      SUBROUTINE UGLYDK(NCL,NCR,XIN,YIN,ESL,ESR,CUBIC)
!-----------------------------------------------------------------------
!     Last change:  JEK   5 Mar 98    2:25 pm
!     Fortran 90 version of original Duck Series written by J.E.Kerwin
!    Arguments:
!    NCL  - Integer describing the left end condition for the spline
!           0-Second derivative specified in ESL
!           1-Rate of change of curvature is continuous at second input
!             point(best option if you do not know what NCL should be)
!           2-First derivative specified in ESL
!   NCR   - Same as for NCL but for right end of spline
!   XIN,YIN - Arrays of input point pairs(NIN in each array)
!   ESL   - First or second derivative at left end of curve (if NCL=0,2)
!   ESR   - Same as ESL but for right end of spline
!   NOTE!!- Positive slope at left end is the spline angling UP from
!            left to right.  Negative slope at the right end is the
!            spline angling DOWN from left to right.
!   CUBIC - Array of dimension 5*(NIN-1) which will contain the
!           cubic coefficients on completion of the subroutine,
!           as well as the first NIN-1 x coordinates of the base points.
!-----------------------------------------------------------------------

!------------------ Declare the variables ------------------------------
      IMPLICIT NONE
      REAL, DIMENSION(:), INTENT(IN) :: XIN,YIN
      REAL, DIMENSION(:,:), INTENT(OUT) :: CUBIC
      REAL, INTENT(IN) :: ESL,ESR
      INTEGER, INTENT(IN) :: NCL,NCR
      REAL, DIMENSION(:), ALLOCATABLE :: H,D,AU,AM,AL,X,S
      REAL, PARAMETER :: HALF=0.5E00, TWO=2.0E00, THREE=3.0E00
      REAL, PARAMETER :: SIX=6.0E00
      REAL HFACT
      INTEGER NIN,K,L,N

!-----Allocate the local arrays for the coeffcinet matrix, RHS, solution
      NIN=SIZE(XIN)
      ALLOCATE (H(NIN-1),D(NIN-1),AU(NIN-3),AM(NIN-2),AL(NIN-3),        &
     &          X(NIN-2),S(NIN))

!-----Compute the intervals, H, and the divided differences, D ---------
      DO N=1,NIN-1
         H(N)=XIN(N+1)-XIN(N)
         D(N)=(YIN(N+1)-YIN(N))/H(N)
      END DO

!-----Set up the principal diagonal (AM) and right hand side (S)-----
      DO N=1,NIN-2
         AM(N)=TWO*(H(N)+H(N+1))
         S(N)=SIX*(D(N+1)-D(N))
      END DO
!-----Set up the upper (AU) and lower (AL) diagonals--------------------
      DO N=1,NIN-3
         AL(N)=H(N+1)
         AU(N)=H(N+1)
      END DO

!-----Modify the first equation based on the left end condition---------
      IF(NCL.EQ.0) THEN         ! Second derivative specified as ESL
         S(1)=S(1)-ESL*H(1)
      ELSE IF(NCL.EQ.1) THEN    ! Extrapolated curvature end condition--
         AM(1)=AM(1)+H(1)*(H(1)+H(2))/H(2)
         AU(1)=AU(1)-H(1)**2/H(2)
      ELSE IF(NCL.EQ.2) THEN    ! First derivative specified as ESL
         AM(1)=AM(1)-HALF*H(1)
         S(1)=S(1)-THREE*(D(1)-ESL)
      END IF

!-----Modify the last equation based on the right end condition--------
      IF(NCR.EQ.0) THEN         ! Second derivative specified as ESR
         S(NIN-2)=S(NIN-2)-ESR*H(NIN-1)
      ELSE IF(NCR.EQ.1) THEN    ! Extrapolated curvature end condition--
         AM(NIN-2)=AM(NIN-2)+H(NIN-1)*(H(NIN-2)+H(NIN-1))/H(NIN-2)
         AL(NIN-3)=AL(NIN-3)-H(NIN-1)**2/H(NIN-2)
      ELSE IF(NCR.EQ.2) THEN    ! First derivative specified as ESR
         AM(NIN-2)=AM(NIN-2)-HALF*H(NIN-1)
         S(NIN-2)=S(NIN-2)+THREE*(D(NIN-1)-ESR)
      END IF

!-----Solve the tri-diagonal system: First pass eliminates lower diag---
      DO K=2,NIN-2
         AL(K-1)=AL(K-1)/AM(K-1)
         AM(K)=AM(K)-AL(K-1)*AU(K-1)
         S(K)=S(K)-AL(K-1)*S(K-1)
      END DO
!-----Second pass back substitutes along principal diagonal-------------
      X(NIN-2)=S(NIN-2)/AM(NIN-2)
      DO L=2,NIN-2
         K=NIN-L-1
         X(K)=(S(K)-AU(K)*X(K+1))/AM(K)
      END DO

!-----Generate array of second derivatives at base points S(N)----------
!-----First get S(1) from the left end condition------------------------
      IF(NCL.EQ.0) THEN
         S(1)=ESL
      ELSE IF(NCL.EQ.1) THEN
         HFACT=H(1)/H(2)
         S(1)=(1.0+HFACT)*X(1)-HFACT*X(2)
      ELSE IF(NCL.EQ.2) THEN
         S(1)=-HALF*X(1)+THREE*(D(1)-ESL)/H(1)
      END IF

!-----Copy the interior values from the solution X----------------------
      DO N=2,NIN-1
         S(N)=X(N-1)
      END DO

!-----Finally, get S(NIN) from the right end condition------------------
      IF(NCR.EQ.0) THEN
         S(NIN)=ESR
      ELSE IF(NCR.EQ.1) THEN
         HFACT=H(NIN-1)/H(NIN-2)
         S(NIN)=(1.0+HFACT)*S(NIN-1)-HFACT*S(NIN-2)
      ELSE IF(NCR.EQ.2) THEN
         S(NIN)=-HALF*S(NIN-1)-THREE*(D(NIN-1)-ESR)/H(NIN-1)
      END IF

!-----Form the output CUBIC array---------------------------------------
      DO N=1,NIN-1
         CUBIC(N,1)=(S(N+1)-S(N))/(SIX*H(N))
         CUBIC(N,2)=HALF*S(N)
         CUBIC(N,3)=D(N)-H(N)*(TWO*S(N)+S(N+1))/SIX
         CUBIC(N,4)=YIN(N)
         CUBIC(N,5)=XIN(N)
      END DO

      DEALLOCATE ( H,D,AU,AM,AL,X,S )

      RETURN
      END SUBROUTINE UGLYDK


      SUBROUTINE EVALDKA(X,Y,C)
!-----------------------------------------------------------------------
!     Last change:  JEK  25 Feb 98    8:39 am
!     Fortran 90 version of original Duck series. Evaluates a spline
!     Arguments:
!     X -  Array of length NOUT containing desired x coordinates
!     Y -  Array of length NOUT : EVALDK will return values of spline
!     C -  Array of size (NIN-1,5) containing spline cubic from UGLYDK
!          Note that the 5th column of C contains the x coordinates of
!          the original base points passed to UGLYDK.
!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL, DIMENSION(:), INTENT(IN) :: X
      REAL, DIMENSION(:), INTENT(OUT) :: Y
      REAL, DIMENSION(:,:), INTENT(IN) :: C
      REAL P
      INTEGER NIN,NOUT,N,J,JI
      NIN=SIZE(C,1)+1
      NOUT=SIZE(X)

      DO  N=1,NOUT
         IF(X(N).LE.C(1,5)) THEN
            Y(N) = C(1,4)
            CYCLE
         ELSE IF(X(N).LE.C(2,5)) THEN
            JI=1
         ELSE IF(X(N).GE.C(NIN-1,5)) THEN
            JI=NIN-1
            Y(N) = C(JI,4)
            CYCLE
         ELSE
            DO J=2,NIN-1
               JI=J
               IF(X(N).GE.C(J,5).AND.X(N).LT.C(J+1,5))   EXIT
            END DO
         END IF
         P=X(N)-C(JI,5)
         Y(N)=C(JI,4)+P*(C(JI,3)+P*(C(JI,2)+P*C(JI,1)))
      END DO

      RETURN
      END SUBROUTINE EVALDKA


      SUBROUTINE EVALDKP(X,Y,C)
!-----------------------------------------------------------------------
!     Last change:  CLW made into one point version 4/21/98
!     Fortran 90 version of original Duck series. Evaluates a spline
!     Arguments:
!     X -  Point containing desired x coordinates
!     Y -  Point  : EVALDK will return the one value of spline
!     C -  Array of size (NIN-1,5) containing spline cubic from UGLYDK
!          Note that the 5th column of C contains the x coordinates of
!          the original base points passed to UGLYDK.
!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL,  INTENT(IN) :: X
      REAL, INTENT(OUT) :: Y
      REAL, DIMENSION(:,:), INTENT(IN) :: C
      REAL P
      INTEGER NIN,J,JI
      NIN=SIZE(C,1)+1

      IF(X.LE.C(2,5)) THEN
         JI=1
      ELSE IF(X.GE.C(NIN-1,5)) THEN
         JI=NIN-1
      ELSE
         DO J=2,NIN-1
            JI=J
            IF(X.GE.C(J,5).AND.X.LT.C(J+1,5))  EXIT
         END DO
      END IF
      P=X-C(JI,5)
      Y=C(JI,4)+P*(C(JI,3)+P*(C(JI,2)+P*C(JI,1)))

      RETURN
      END SUBROUTINE EVALDKP

      SUBROUTINE INTDK1(XL,XU,YDX,CUBIC)
      USE splines, ONLY : ISPAN
      IMPLICIT NONE
!------------------- Declare the arguments -------------------------------------
      REAL,  INTENT(IN) :: XL,XU  ! Upper and lower limits of integral
      REAL, DIMENSION(:,:), INTENT(INOUT) :: CUBIC  ! Spline cubic array
      REAL, INTENT(OUT) :: YDX   ! Integral frol XL to XU
!------------------- Declare the local variables -------------------------------
      INTEGER :: JU,JL,J
      REAL H1,H2,H3,H4

      JL=ISPAN(XL,CUBIC)
      JU=ISPAN(XU,CUBIC)
!--------------- Evaluate integral at the lower limit --------------------------
      H1=XL-CUBIC(JL,5)
      H2=H1**2
      H3=H1*H2
      H4=H2**2
      YDX=-CUBIC(JL,1)/4.0*H4-CUBIC(JL,2)/3.0*H3-CUBIC(JL,3)/2.0*H2            &
          -CUBIC(JL,4)*H1
!--------------- Evaluate integral at the upper limit --------------------------
      H1=XU-CUBIC(JU,5)
      H2=H1**2
      H3=H1*H2
      H4=H2**2
      YDX=YDX+CUBIC(JU,1)/4.0*H4+CUBIC(JU,2)/3.0*H3+CUBIC(JU,3)/2.0*H2         &
          +CUBIC(JU,4)*H1
!--------------- Evaluate integral over intermediate spans, if any -------------
      IF(JU>JL) THEN
         DO J=JL,JU-1
            H1=CUBIC(J+1,5)-CUBIC(J,5)
            H2=H1**2
            H3=H1*H2
            H4=H2**2
            YDX=YDX+CUBIC(J,1)/4.0*H4+CUBIC(J,2)/3.0*H3+CUBIC(J,3)/2.0*H2      &
                +CUBIC(J,4)*H1
         END DO
      END IF
      RETURN
      END SUBROUTINE INTDK1

      INTEGER FUNCTION ISPAN(X,CUBIC)
      IMPLICIT NONE
      REAL, INTENT(IN) :: X
      REAL, DIMENSION(:,:), INTENT(IN) :: CUBIC
      INTEGER NM,NLOW,NHIGH,MID
      NM=SIZE(CUBIC,1)
      IF(X<CUBIC(2,5)) THEN
         MID=1     ! X is in the first span, or out of range to the left.
      ELSE IF(X>=CUBIC(NM,5)) THEN
         MID=NM
      ELSE
         NLOW=2    ! Do binary search for the span index
         NHIGH=NM
         MID=(NLOW+NHIGH)/2
         DO WHILE (X<CUBIC(MID,5).OR.X>=CUBIC(MID+1,5))
            IF(X<CUBIC(MID,5)) THEN
               NHIGH=MID
            ELSE
               NLOW=MID
            END IF
            MID=(NLOW+NHIGH)/2
         END DO
      END IF
      ISPAN=MID
      RETURN
      END FUNCTION ISPAN

