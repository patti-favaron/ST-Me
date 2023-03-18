! ABL_physics
!
! This module contains all data and procedures related to "ABL physics"
! (in a very broad sense).
!
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
MODULE ABL_physics

    USE SPLINES
    USE Statistics
    USE NaN_support

    IMPLICIT NONE

    PRIVATE

    ! Public interface
    ! -1- Procedures
    PUBLIC  :: IsDiurnal
    PUBLIC  :: FillWindGaps
    PUBLIC  :: FillScalarGaps


CONTAINS

    ! Check whether a given time is diurnal or not
    FUNCTION IsDiurnal(iDayInYear, iHour, rLat, rLon, iFuse) RESULT(lDiurnal)

        ! Routine arguments
        INTEGER, INTENT(IN) :: iDayInYear
        INTEGER, INTENT(IN) :: iHour
        REAL, INTENT(IN)    :: rLat
        REAL, INTENT(IN)    :: rLon
        INTEGER, INTENT(IN) :: iFuse
        LOGICAL             :: lDiurnal

        ! Locals
        REAL            :: rDeclination
        REAL            :: rSinEps
        REAL, PARAMETER :: PI     = 3.1415927
        REAL, PARAMETER :: TO_RAD = PI/180.

        ! Check whether daytime
        rDeclination = 0.409*COS(2.*PI*(iDayInYear-173)/365.25)
        rSinEps = SIND(rLat)*SIN(rDeclination) - &
				  COSD(rLat)*COS(rDeclination)*COS((PI*(iHour-iFuse)/12.) - &
				  rLon*TO_RAD)
        lDiurnal = (rSinEps > 0.)

    END FUNCTION IsDiurnal


    ! Fill gaps in wind series
    FUNCTION FillWindGaps(rvTime, rvVel, rvDir, lvOriginal) RESULT(iRetCode)

        ! Routine arguments
        REAL, DIMENSION(:), INTENT(IN)      	:: rvTime
        REAL, DIMENSION(:), INTENT(INOUT)   	:: rvVel
        REAL, DIMENSION(:), INTENT(INOUT)   	:: rvDir
		logical, dimension(:), intent(inout)	:: lvOriginal
        INTEGER                             	:: iRetCode

        ! Locals
        INTEGER                                 :: iNumData
        REAL, DIMENSION(:), ALLOCATABLE         :: rvU
        REAL, DIMENSION(:), ALLOCATABLE         :: rvV
        REAL, DIMENSION(:), ALLOCATABLE         :: rvValTime
        REAL, DIMENSION(:), ALLOCATABLE         :: rvNewU
        REAL, DIMENSION(:), ALLOCATABLE         :: rvNewV
        REAL, DIMENSION(:), ALLOCATABLE         :: rvNewTime
        REAL, DIMENSION(:,:), ALLOCATABLE       :: rmCubicU
        REAL, DIMENSION(:,:), ALLOCATABLE       :: rmCubicV
        INTEGER                                 :: i
        INTEGER                                 :: iData
        INTEGER                                 :: iValidData
        INTEGER                                 :: iInvalidData
        INTEGER                                 :: iNonData
        INTEGER									:: iErrCode

        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Check input parameters are compatible
        iNumData = SIZE(rvTime)
        IF(iNumData <= 0) THEN
            iRetCode = 1
            RETURN
        END IF
        IF(SIZE(rvVel) /= iNumData .AND. SIZE(rvDir) /= iNumData) THEN
            iRetCode = 2
            RETURN
        END IF
        IF(iNumData < 2) THEN
            iRetCode = 2
            RETURN
        END IF

        ! Count valid and invalid data. This information is immediately used
        ! to reserve workspace.
        iValidData = 0
        DO i = 1, iNumData
            IF(.NOT.ISNAN(rvVel(i)) .AND. .NOT.ISNAN(rvDir(i))) THEN
                iValidData = iValidData + 1
            END IF
        END DO
        IF(iValidData < 4) THEN
            iRetCode = 4
            RETURN
        END IF
        iInvalidData = iNumData - iValidData
        IF(iInvalidData <= 0) THEN
            ! Nothing to do
            RETURN
        END IF
        ALLOCATE(rvValTime(iValidData), rvU(iValidData), rvV(iValidData))
        ALLOCATE(rmCubicU(iValidData-1,5), rmCubicV(iValidData-1,5))
        ALLOCATE(rvNewTime(iInvalidData), rvNewU(iInvalidData), rvNewV(iInvalidData))

        ! Dispatch all valid data to the "experimental data set", all invalid
        ! to the "prediction" data set.
        iData = 0
        iNonData = 0
        DO i = 1, iNumData
            IF(.NOT.ISNAN(rvVel(i)) .AND. .NOT.ISNAN(rvDir(i))) THEN
                iData = iData + 1
                rvValTime(iData) =  rvTime(i)
                rvU(iData)       = -rvVel(i)*SIND(rvDir(i))
                rvV(iData)       = -rvVel(i)*COSD(rvDir(i))
            ELSE
                iNonData = iNonData + 1
                rvNewTime(iNonData) = rvTime(i)
            END IF
        END DO

        ! Compute interpolating spline coefficients for U and V
        ! separately.
        CALL UGLYDK(1, 1, rvValTime, rvU, 0., 0., rmCubicU)
        CALL UGLYDK(1, 1, rvValTime, rvV, 0., 0., rmCubicV)

        ! Evaluate the spline functions for U and V at prediction times,
        ! meanwhile changing back to wind polar form.
        CALL EVALDKA(rvNewTime, rvNewU, rmCubicU)
        CALL EVALDKA(rvNewTime, rvNewV, rmCubicV)
        iData = 0
        DO i = 1, iNumData
            IF(ISNAN(rvVel(i)) .OR. ISNAN(rvDir(i))) THEN
                iData = iData + 1
                rvVel(i) = SQRT(rvNewU(iData)**2+rvNewV(iData)**2)
                rvDir(i) = ATAN2D(-rvNewU(iData),-rvNewV(iData))
                IF(rvDir(i) <   0.) rvDir(i) = rvDir(i) + 360.
                IF(rvDir(i) > 360.) rvDir(i) = rvDir(i) - 360.
                lvOriginal(i) = .false.
            END IF
        END DO

        ! Leave
        DEALLOCATE(rvNewTime, rvNewU, rvNewV)
        DEALLOCATE(rmCubicU, rmCubicV)
        DEALLOCATE(rvValTime, rvU, rvV)

    END FUNCTION FillWindGaps


    ! Fill gaps in wind series
    FUNCTION FillScalarGaps(rvTime, rvVal, lvOriginal) RESULT(iRetCode)

        ! Routine arguments
        REAL, DIMENSION(:), INTENT(IN)      	:: rvTime
        REAL, DIMENSION(:), INTENT(INOUT)   	:: rvVal
		logical, dimension(:), intent(inout)	:: lvOriginal
        INTEGER                             	:: iRetCode

        ! Locals
        INTEGER                                 :: iNumData
        REAL, DIMENSION(:), ALLOCATABLE         :: rvOldVal
        REAL, DIMENSION(:), ALLOCATABLE         :: rvNewVal
        REAL, DIMENSION(:), ALLOCATABLE         :: rvOldTime
        REAL, DIMENSION(:), ALLOCATABLE         :: rvNewTime
        REAL, DIMENSION(:,:), ALLOCATABLE       :: rmCubic
        INTEGER                                 :: i
        INTEGER                                 :: iData
        INTEGER                                 :: iValidData
        INTEGER                                 :: iInvalidData
        INTEGER                                 :: iNonData
        INTEGER									:: iErrCode

        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Check input parameters are compatible
        iNumData = SIZE(rvTime)
        IF(iNumData <= 0) THEN
            iRetCode = 1
            RETURN
        END IF
        IF(SIZE(rvVal) /= iNumData) THEN
            iRetCode = 2
            RETURN
        END IF

        iValidData = 0

        ! Count valid and invalid data. This information is immediately used
        ! to reserve workspace.
        iValidData = 0
        DO i = 1, iNumData
            IF(.NOT.ISNAN(rvVal(i))) THEN
                iValidData = iValidData + 1
            END IF
        END DO
        IF(iValidData < 3) THEN
            iRetCode = 4
            RETURN
        END IF
        iInvalidData = iNumData - iValidData
        IF(iInvalidData <= 0) THEN
            ! Nothing to do
            RETURN
        END IF
        ALLOCATE( &
			rvOldTime(iValidData), &
			rvOldVal(iValidData), &
			rvNewTime(iInvalidData), &
			rvNewVal(iInvalidData) &
		)
        ALLOCATE(rmCubic(iValidData-1,5))

        ! Dispatch all valid data to the "experimental data set", all invalid
        ! to the "prediction" data set.
        iData = 0
        iNonData = 0
        DO i = 1, iNumData
            IF(.NOT.ISNAN(rvVal(i))) THEN
                iData = iData + 1
                rvOldTime(iData) = rvTime(i)
                rvOldVal(iData) = rvVal(i)
            ELSE
                iNonData = iNonData + 1
                rvNewTime(iNonData) = rvTime(i)
            END IF
        END DO

        ! Compute interpolating spline coefficients for U and V
        ! separately.
        CALL UGLYDK(1, 1, rvOldTime, rvOldVal, 0., 0., rmCubic)

        ! Evaluate the spline function at prediction times.
        CALL EVALDKA(rvNewTime, rvNewVal, rmCubic)
        iData = 0
        DO i = 1, iNumData
            IF(ISNAN(rvVal(i))) THEN
                iData = iData + 1
                rvVal(i) = rvNewVal(iData)
                lvOriginal(i) = .false.
            END IF
        END DO

        ! Leave
        DEALLOCATE(rmCubic)
        DEALLOCATE(rvOldTime, rvOldVal, rvNewTime, rvNewVal)

    END FUNCTION FillScalarGaps


    ELEMENTAL FUNCTION SIND(a) RESULT(val)

		! Routine arguments
		REAL, INTENT(IN)	:: a
		REAL				:: val

		! Locals
		! --none--

		! Compute the quantity desired
		val = SIN(3.1415927/180.0*a)

    END FUNCTION SIND


    ELEMENTAL FUNCTION COSD(a) RESULT(val)

		! Routine arguments
		REAL, INTENT(IN)	:: a
		REAL				:: val

		! Locals
		! --none--

		! Compute the quantity desired
		val = COS(3.1415927/180.0*a)

    END FUNCTION COSD


    ELEMENTAL FUNCTION ATAN2D(a, b) RESULT(val)

		! Routine arguments
		REAL, INTENT(IN)	:: a
		REAL, INTENT(IN)	:: b
		REAL				:: val

		! Locals
		! --none--

		! Compute the quantity desired
		val = 180.0/3.1415927*ATAN2(a, b)

    END FUNCTION ATAN2D

END MODULE ABL_physics
