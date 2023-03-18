! ST_SODAR_Data - Support read access to SODAR data in ST standard form
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
MODULE ST_SODAR_Data

    IMPLICIT NONE
    
    PRIVATE
    
    ! Public interface
    ! -1- Data types
    PUBLIC  :: SODAR_Data
    PUBLIC  :: SODAR_Plex
    ! -1- Procedures
    PUBLIC  :: ReadSodarData
    PUBLIC  :: GetSodarDataClosestInTime
    
    ! Data types
    
    TYPE SODAR_Plex
        REAL, DIMENSION(:), ALLOCATABLE :: rvHeight
        REAL, DIMENSION(:), ALLOCATABLE :: rvVel
        REAL, DIMENSION(:), ALLOCATABLE :: rvDir
        REAL, DIMENSION(:), ALLOCATABLE :: rvTemp
    END TYPE SODAR_Plex
    
    TYPE SODAR_Data
        INTEGER, DIMENSION(:), ALLOCATABLE          :: ivTime
        INTEGER, DIMENSION(:), ALLOCATABLE          :: ivDataSize
        TYPE(SODAR_Plex), DIMENSION(:), ALLOCATABLE :: tvData
    END TYPE SODAR_Data

CONTAINS

    ! Read an ST-formatted SODAR data file, reserving workspace for it
    ! (only the relevant columns are read, to save memory space).
    !
    ! This routine has been designed to be called first than any others
    ! in this module, and once only. I can afford this, as I assume
    ! to be one of the few users, all internal to ST, and then keen to
    ! read comments.
    FUNCTION ReadSodarData(iLUN, sDataFile, tSodarData) RESULT(iRetCode)
    
        USE Calendar
    
        ! Routine arguments
        INTEGER, INTENT(IN)             :: iLUN
        CHARACTER(LEN=*), INTENT(IN)    :: sDataFile
        TYPE(SODAR_DATA), INTENT(INOUT) :: tSodarData
        INTEGER                         :: iRetCode
        
        ! Locals
        INTEGER :: iErrCode
        INTEGER :: iNumData
        INTEGER :: iData
        INTEGER :: iYear
        INTEGER :: iMonth
        INTEGER :: iDay
        INTEGER :: iHour
        INTEGER :: iMinute
        INTEGER :: iSecond
        INTEGER :: iTime
        INTEGER :: iNumLevels
        INTEGER :: iLevel
        INTEGER :: iNumPlexes
        INTEGER :: iPlex
        REAL    :: rHeight
        REAL    :: rVel
        REAL    :: rDir
        REAL    :: rTemp
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Clean workspace (this is a prudential measure,
        ! and should not be essential if this routine is
        ! called as expected)
        IF(ALLOCATED(tSodarData % tvData)) THEN
            iNumData = SIZE(tSodarData % tvData)
            DO iData = 1, iNumData
                DEALLOCATE(tSodarData % tvData(iData) % rvHeight, STAT=iErrCode)
                DEALLOCATE(tSodarData % tvData(iData) % rvVel,    STAT=iErrCode)
                DEALLOCATE(tSodarData % tvData(iData) % rvDir,    STAT=iErrCode)
                DEALLOCATE(tSodarData % tvData(iData) % rvTemp,   STAT=iErrCode)
            END DO
        END IF
        DEALLOCATE(tSodarData % ivTime, tSodarData % ivDataSize, STAT=iErrCode)
        
        ! Step 1: Count number of plexes in input file
        OPEN(iLUN, FILE=sDataFile, STATUS='OLD', ACTION='READ', IOSTAT=iErrCode)
        IF(iErrCode /= 0) THEN
            iRetCode = 1
            RETURN
        END IF
        iNumPlexes = 0
        DO
            READ(iLUN, *, IOSTAT=iErrCode) iYear, iMonth, iDay, iHour, iMinute, iSecond, iNumLevels
            IF(iErrCode /= 0) EXIT
            iNumPlexes = iNumPlexes + 1
            DO iLevel = 1, iNumLevels
                READ(iLUN,*,IOSTAT=iErrCode) rHeight, rVel, rDir, rTemp
                IF(iErrCode /= 0) THEN
                    iRetCode = 2
                    CLOSE(iLUN)
                    RETURN
                END IF
            END DO
        END DO
        IF(iNumPlexes <= 0) THEN
            iRetCode = 3
            CLOSE(iLUN)
            RETURN
        END IF
        ALLOCATE(tSodarData % ivTime(iNumPlexes),tSodarData % ivDataSize(iNumPlexes),tSodarData % tvData(iNumPlexes))
        print *, "st_me:: info: ", iNumPlexes, " SODAR data records found"

        ! Step 2: Gather actual data
        REWIND(iLUN)
        DO iPlex = 1, iNumPlexes
            READ(iLUN, *) iYear, iMonth, iDay, iHour, iMinute, iSecond, iNumLevels
            CALL PackTime(tSodarData % ivTime(iPlex), iYear, iMonth, iDay, iHour, iMinute, iSecond)
            ALLOCATE(tSodarData % tvData(iPlex) % rvHeight(iNumLevels))
            ALLOCATE(tSodarData % tvData(iPlex) % rvVel(iNumLevels))
            ALLOCATE(tSodarData % tvData(iPlex) % rvDir(iNumLevels))
            ALLOCATE(tSodarData % tvData(iPlex) % rvTemp(iNumLevels))
            DO iLevel = 1, iNumLevels
                READ(iLUN,*) &
                    tSodarData % tvData(iPlex) % rvHeight(iLevel), &
                    tSodarData % tvData(iPlex) % rvVel(iLevel), &
                    tSodarData % tvData(iPlex) % rvDir(iLevel), &
                    tSodarData % tvData(iPlex) % rvTemp(iLevel)
            END DO
        END DO
        CLOSE(iLUN)
        
    END FUNCTION ReadSodarData

	
	! Warning: SODAR data are time-stamped on period *end*, not beginning. On call, date and time should be converted
	! to end-period convention if necessary.
    FUNCTION GetSodarDataClosestInTime(tSodarData, iTime, iNumHeights, rvHeight, rvVel, rvDir, rvTemp) RESULT(iRetCode)
    
        ! Routine arguments
        TYPE(SODAR_Data), INTENT(IN)    :: tSodarData
        INTEGER, INTENT(IN)             :: iTime
        INTEGER, INTENT(OUT)            :: iNumHeights
        REAL, DIMENSION(:), INTENT(OUT) :: rvHeight
        REAL, DIMENSION(:), INTENT(OUT) :: rvVel
        REAL, DIMENSION(:), INTENT(OUT) :: rvDir
        REAL, DIMENSION(:), INTENT(OUT) :: rvTemp
        INTEGER                         :: iRetCode
        
        ! Locals
        INTEGER					:: iErrCode
        INTEGER, PARAMETER      :: MAX_DELTA_T = 1800   ! Seconds
        INTEGER, DIMENSION(1)   :: ivClosestPosition
        INTEGER                 :: iClosestTime
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Ensure the SODAR data are really available
        IF(.NOT.ALLOCATED(tSodarData % tvData)) THEN
            iRetCode = 1
            RETURN
        END IF
        ! Post-condition: At this point, SODAR data are surely available
        
        ! Lookup the time stamp in SODAR data structure nearest to 'iTime'
        ivClosestPosition = MINLOC(ABS(tSodarData%ivTime - iTime))
        iClosestTime = tSodarData % ivTime(ivClosestPosition(1))
        IF(ABS(iClosestTime - iTime) <= MAX_DELTA_T) THEN
            iNumHeights = SIZE(tSodarData % tvData(ivClosestPosition(1)) % rvHeight)
            IF(iNumHeights > 0) THEN
				rvHeight(1:iNumHeights) = tSodarData % tvData(ivClosestPosition(1)) % rvHeight(1:iNumHeights)
				rvVel(1:iNumHeights)    = tSodarData % tvData(ivClosestPosition(1)) % rvVel(1:iNumHeights)
				rvDir(1:iNumHeights)    = tSodarData % tvData(ivClosestPosition(1)) % rvDir(1:iNumHeights)
				rvTemp(1:iNumHeights)   = tSodarData % tvData(ivClosestPosition(1)) % rvTemp(1:iNumHeights)
				IF(rvVel(1) < -9000.0 .OR. rvDir(1) < -9000.0) THEN
					! Rare case, but may happen, and in the case there is no way to estimate
					! ground-based gaps reliably
					iNumHeights = 0
					rvHeight = -9999.9
					rvVel    = -9999.9
					rvDir    = -9999.9
					rvTemp   = -9999.9
				END IF
				iErrCode = FillVerticalGaps(iNumHeights, rvHeight, rvVel, rvDir, rvTemp)
			ELSE
				iNumHeights = 0	! Might have been < 0, this condition prevents this
				rvHeight = -9999.9
				rvVel    = -9999.9
				rvDir    = -9999.9
				rvTemp   = -9999.9
				RETURN
            END IF
        ELSE
            iNumHeights = 0
            rvHeight = -9999.9
            rvVel    = -9999.9
            rvDir    = -9999.9
            rvTemp   = -9999.9
            RETURN
        END IF
        
    END FUNCTION GetSodarDataClosestInTime
    
    
    ! *********************
    ! * Internal routines *
    ! *********************
    
    ! Fill vertical gaps in U, V, Temp by linear interpolation
    FUNCTION FillVerticalGaps(iNumHeights, rvHeight, rvVel, rvDir, rvTemp) RESULT(iRetCode)
    
		! Routine arguments
		INTEGER, INTENT(IN)				   :: iNumHeights
        REAL, DIMENSION(:), INTENT(IN)     :: rvHeight
        REAL, DIMENSION(:), INTENT(INOUT)  :: rvVel
        REAL, DIMENSION(:), INTENT(INOUT)  :: rvDir
        REAL, DIMENSION(:), INTENT(INOUT)  :: rvTemp
        INTEGER							   :: iRetCode
        
        ! Locals
        INTEGER				:: i, n
        REAL, DIMENSION(44)	:: rvU
        REAL, DIMENSION(44)	:: rvV
        
        ! Routine arguments
        iRetCode = 0
        
        ! Fill wind speed and direction
        WHERE(rvVel(1:iNumHeights) > -9000.0 .AND. rvVel(1:iNumHeights) > -9000.0)
			rvU(1:iNumHeights) = rvVel(1:iNumHeights)*sin(3.1415927/180.0*rvDir(1:iNumHeights))
			rvV(1:iNumHeights) = rvVel(1:iNumHeights)*cos(3.1415927/180.0*rvDir(1:iNumHeights))
		ELSEWHERE
			rvU = -9999.9
			rvV = -9999.9
		ENDWHERE
		CALL Fill(rvHeight, rvU, iNumHeights)
		CALL Fill(rvHeight, rvV, iNumHeights)
		rvVel(1:iNumHeights) = SQRT(rvU(1:iNumHeights)**2+rvV(1:iNumHeights)**2)
		rvDir(1:iNumHeights) = ATAN2(rvU(1:iNumHeights), rvV(1:iNumHeights))*180.0/3.1415927
		WHERE(rvDir(1:iNumHeights) < 0.)
			rvDir(1:iNumHeights) = rvDir(1:iNumHeights) + 360.0
		ENDWHERE
		
		! Fill temperature
		CALL Fill(rvHeight, rvTemp, iNumHeights)
        
    END FUNCTION FillVerticalGaps
    
    
	! Fill individual vector, having at least one element, and whose first and last elements are valid
    SUBROUTINE Fill(rvHeight, rvX, n)
    
		! Routine arguments
		REAL, DIMENSION(:), INTENT(IN)		:: rvHeight
		REAL, DIMENSION(:), INTENT(INOUT)	:: rvX
		integer, intent(in)					:: n
		
		! Locals
		integer		:: i
		real		:: rHgt0, rHgt1
		real		:: rX0, rX1
		integer		:: j, iPre, iPost
		
		! Main loop: find first gaps block from below
		i = 1
		DO WHILE (i <= n)
		
			! Find bounds
			iPre  = 0
			iPost = 0
			IF(rvX(i) < -9000.) THEN
				iPre = i - 1
				DO iPost = i, n
					IF(rvX(iPost) > -9000.0) EXIT
				END DO
			END IF
			
			! Fill by linear interpolation
			if(iPre > 0 .and. iPost > 0) then
				rHgt0 = rvHeight(iPre)
				rHgt1 = rvHeight(iPost)
				rX0   = rvX(iPre)
				rX1   = rvX(iPost)
				do j = iPre+1, iPost-1
					rvX(j) = rX0 + (rvHeight(j)-rHgt0)/(rHgt1-rHgt0)*(rX1-rX0)
				end do
				i = iPost
			else
				i = i + 1
			end if
			
		END DO
		
    END SUBROUTINE Fill
    
END MODULE ST_SODAR_Data
