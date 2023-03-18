! Fortran module, supporting missing-data aware statistical functions
! of various kinds
!
! Copyright 2017 by Servizi Territorio srl
!                   All rights reserved
!
! By: Mauri Favaron
!
module Statistics

	use NaN_support
	
	implicit none
	
	private
	
	! Public interface
	public	:: DirectionalMean
	public	:: TypicalDay
	public	:: TypicalDayWind
	public	:: Anomaly
	public	:: FillGaps
	public	:: WindGaps
	public	:: EstimateAnomaly
	public	:: DailyMean
	public	:: DailyMin
	public	:: DailyMax
	public	:: DailySum
	public	:: DailyAvailability
	public	:: CountFilled
	
    ! Constants
	real, parameter	:: PI  = 3.1415927
    
contains

	! Compute directional mean.
	! Warning: because of this lack of controls, this routine is not
	!          meant for general use: it works within ST-Me only.
	function DirectionalMean( &
		iStampInDay, ivTimeStamp, rvDir, rvValue, &
		iDeltaTime, iDaysRadius, iNumDirClasses, rClassWidth, &
		rvMean &
	) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)								:: iStampInDay
		integer, dimension(:), intent(in)				:: ivTimeStamp
		real, dimension(:), intent(in)					:: rvDir
		real, dimension(:), intent(in)					:: rvValue
		integer, intent(in)								:: iDeltaTime
		integer, intent(in)								:: iDaysRadius
		integer, intent(in)								:: iNumDirClasses
		real, intent(in)								:: rClassWidth
		real, dimension(:), allocatable, intent(out)	:: rvMean
		integer											:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: iFirstItem
		integer								:: iLastItem
		integer								:: i, j
		integer								:: iDayStamp
		integer, dimension(:), allocatable	:: ivNumValues
		real, dimension(:), allocatable		:: rvSumValues
		real, dimension(:), allocatable		:: rvClassMin
		real, dimension(:), allocatable		:: rvClassMax
		integer								:: iNumValid
		real								:: rClassSpacing
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			iRetCode = 1
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			iRetCode = 2
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			iRetCode = 3
			return
		end if
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			iRetCode = 4
			return
		end if
		
		! Allocate output vector
		if(ALLOCATED(rvMean)) deallocate(rvMean)
		allocate(rvMean(iNumDirClasses), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		
		! Reserve temporary workspace
		allocate(& 
			ivNumValues(iNumDirClasses), &
			rvSumValues(iNumDirClasses), &
			rvClassMin(iNumDirClasses), &
			rvClassMax(iNumDirClasses), &
			STAT=iErrCode &
		)
		if(iErrCode /= 0) then
			iRetCode = 6
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
			
		! Delimit the time span over which directional mean is computed
		iFirstItem  = MAX(iFirstItemInDay - iDaysRadius * iNumItemsPerDay, 1)
		iLastItem   = MIN(iLastItemInDay  + iDaysRadius * iNumItemsPerDay, SIZE(rvValue))
		
		! Compute class limits
		rClassSpacing = 360. / iNumDirClasses
		rvClassMin = [(i*rClassSpacing - 0.5*rClassWidth, i=0, iNumDirClasses-1)]
		rvClassMax = [(i*rClassSpacing + 0.5*rClassWidth, i=0, iNumDirClasses-1)]
		where(rvClassMin < 0.0)
			rvClassMin = rvClassMin + 360.0
		end where
		where(rvClassMax > 360.0)
			rvClassMax = rvClassMax + 360.0
		end where
		
		! Compute directional mean
		ivNumValues = 0
		rvSumValues = 0.
		do i = iFirstItem, iLastItem
			if(.not.ISNAN(rvValue(i))) then
				do j = 1, iNumDirClasses
					if(rvClassMin(j) < rvClassMax(j)) then
						! Normal case
						if(rvDir(i) >= rvClassMin(j) .and. rvDir(i) <= rvClassMax(j)) then
							ivNumValues(j) = ivNumValues(j) + 1
							rvSumValues(j) = rvSumValues(j) + rvValue(i)
						end if
					else
						! Less usual case: class splits at 0
						if(rvDir(i) >= rvClassMax(j) .or. rvDir(i) <= rvClassMin(j)) then
							ivNumValues(j) = ivNumValues(j) + 1
							rvSumValues(j) = rvSumValues(j) + rvValue(i)
						end if
					end if
				end do
			end if
		end do
		do i = 1, SIZE(ivNumValues)
			if(ivNumValues(i) > 0) then
				rvMean(i) = rvSumValues(i) / ivNumValues(i)
			else
				rvMean(i) = NaN
			end if
		end do
				
		! Leave
		deallocate(ivNumValues, rvSumValues, rvClassMin, rvClassMax)
		
	end function DirectionalMean
	
	
	! Compute typical day.
	! Warning: because of this lack of controls, this routine is not
	!          meant for general use: it works within ST-Me only.
	function TypicalDay(iStampInDay, ivTimeStamp, rvValue, iDeltaTime, iDaysRadius, rvTypicalDay) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)								:: iStampInDay
		integer, dimension(:), intent(in)				:: ivTimeStamp
		real, dimension(:), intent(in)					:: rvValue
		integer, intent(in)								:: iDeltaTime
		integer, intent(in)								:: iDaysRadius
		real, dimension(:), allocatable, intent(out)	:: rvTypicalDay
		integer											:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: iFirstItem
		integer								:: iLastItem
		integer								:: i
		integer								:: iDayStamp
		integer								:: iIndex
		integer, dimension(:), allocatable	:: ivNumValues
		real, dimension(:), allocatable		:: rvSumValues
		real								:: rAnomaly
		integer								:: iNumValid
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			iRetCode = 1
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			iRetCode = 2
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			iRetCode = 3
			return
		end if
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			iRetCode = 4
			return
		end if
		
		! Allocate output vector
		if(ALLOCATED(rvTypicalDay)) deallocate(rvTypicalDay)
		allocate(rvTypicalDay(iNumItemsPerDay), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		
		! Reserve temporary workspace
		allocate(ivNumValues(iNumItemsPerDay), rvSumValues(iNumItemsPerDay), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 6
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
			
		! Delimit the time span over which typical day is computed
		iFirstItem  = MAX(iFirstItemInDay - iDaysRadius * iNumItemsPerDay, 1)
		iLastItem   = MIN(iLastItemInDay  + iDaysRadius * iNumItemsPerDay, SIZE(rvValue))
		
		! Compute typical day
		ivNumValues = 0
		rvSumValues = 0.
		do i = iFirstItem, iLastItem
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				ivNumValues(iIndex) = ivNumValues(iIndex) + 1
				rvSumValues(iIndex) = rvSumValues(iIndex) + rvValue(i)
			end if
		end do
		do i = 1, SIZE(ivNumValues)
			if(ivNumValues(i) > 0) then
				rvTypicalDay(i) = rvSumValues(i) / ivNumValues(i)
			else
				rvTypicalDay(i) = NaN
			end if
		end do
				
		! Leave
		deallocate(ivNumValues, rvSumValues)
		
	end function TypicalDay
	
	
	! Compute typical day of wind.
	! Warning: because of this lack of controls, this routine is not
	!          meant for general use: it works within ST-Me only.
	function TypicalDayWind( &
		iStampInDay, ivTimeStamp, rvVel, rvDir, iDeltaTime, iDaysRadius, &
		rvTypicalVel, rvTypicalDir &
	) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)								:: iStampInDay
		integer, dimension(:), intent(in)				:: ivTimeStamp
		real, dimension(:), intent(in)					:: rvVel
		real, dimension(:), intent(in)					:: rvDir
		integer, intent(in)								:: iDeltaTime
		integer, intent(in)								:: iDaysRadius
		real, dimension(:), allocatable, intent(out)	:: rvTypicalVel
		real, dimension(:), allocatable, intent(out)	:: rvTypicalDir
		integer											:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		real, dimension(:), allocatable		:: rvU, rvV
		real, dimension(:), allocatable		:: rvTypicalU
		real, dimension(:), allocatable		:: rvTypicalV
		
		! Constants
		real, parameter	:: PI = 3.1415927
		real, parameter	:: TO_RAD = PI / 180.0
		real, parameter	:: TO_DEG = 180.0 / PI
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Transform wind speed and direction to vector components
		if(ALLOCATED(rvU)) DEALLOCATE(rvU)
		ALLOCATE(rvU(SIZE(rvVel)), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		if(ALLOCATED(rvV)) DEALLOCATE(rvV)
		ALLOCATE(rvV(SIZE(rvVel)), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		rvU = rvVel * SIN(rvDir*TO_RAD)
		rvV = rvVel * COS(rvDir*TO_RAD)
		
		! Compute typical day of vector components
		iErrCode = TypicalDay(iStampInDay, ivTimeStamp, rvU, iDeltaTime, iDaysRadius, rvTypicalU)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		iErrCode = TypicalDay(iStampInDay, ivTimeStamp, rvV, iDeltaTime, iDaysRadius, rvTypicalV)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		
		! Convert back wind vectors from Cartesian to polar form
		if(ALLOCATED(rvTypicalVel)) DEALLOCATE(rvTypicalVel)
		ALLOCATE(rvTypicalVel(SIZE(rvTypicalU)), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		if(ALLOCATED(rvTypicalDir)) DEALLOCATE(rvTypicalDir)
		ALLOCATE(rvTypicalDir(SIZE(rvTypicalU)), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 6
			return
		end if
		rvTypicalVel = SQRT(rvTypicalU**2 + rvTypicalV**2)
		rvTypicalDir = ATAN2(rvTypicalU,rvTypicalV)*TO_DEG
		where(rvTypicalDir < 0.0)
			rvTypicalDir = rvTypicalDir + 360.0
		end where
		
	end function TypicalDayWind
	
	
	! Compute day anomaly with respect to typical day.
	! Warning: because of this lack of controls, this routine is not
	!          meant for general use: it works within ST-Me only.
	function Anomaly(iStampInDay, ivTimeStamp, rvValue, iDeltaTime, iDaysRadius, rStdDev, rMin, rMax) result(rAnomaly)
	
		! Routine arguments
		integer, intent(in)								:: iStampInDay
		integer, dimension(:), intent(in)				:: ivTimeStamp
		real, dimension(:), intent(in)					:: rvValue
		integer, intent(in)								:: iDeltaTime
		integer, intent(in)								:: iDaysRadius
		real, intent(out)								:: rStdDev
		real, intent(out)								:: rMin
		real, intent(out)								:: rMax
		real											:: rAnomaly
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: iFirstItem
		integer								:: iLastItem
		integer								:: i
		integer								:: iDayStamp
		integer								:: iIndex
		integer, dimension(:), allocatable	:: ivNumValues
		real, dimension(:), allocatable		:: rvSumValues
		real, dimension(:), allocatable		:: rvTypicalDay
		integer								:: iNumValid
		integer								:: iNumAnomaly
		real								:: rDelta
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			rAnomaly = NaN
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			rAnomaly = NaN
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			rAnomaly = NaN
			return
		end if
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			rAnomaly = NaN
			return
		end if
		
		! Allocate output vector
		allocate(rvTypicalDay(iNumItemsPerDay), STAT=iErrCode)
		if(iErrCode /= 0) then
			rAnomaly = NaN
			return
		end if
		
		! Reserve temporary workspace
		allocate(ivNumValues(iNumItemsPerDay), rvSumValues(iNumItemsPerDay), STAT=iErrCode)
		if(iErrCode /= 0) then
			rAnomaly = NaN
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
			
		! Delimit the time span over which typical day is computed
		iFirstItem  = MAX(iFirstItemInDay - iDaysRadius * iNumItemsPerDay, 1)
		iLastItem   = MIN(iLastItemInDay  + iDaysRadius * iNumItemsPerDay, SIZE(rvValue))
		
		! Compute typical day
		ivNumValues = 0
		rvSumValues = 0.
		do i = iFirstItem, iLastItem
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				ivNumValues(iIndex) = ivNumValues(iIndex) + 1
				rvSumValues(iIndex) = rvSumValues(iIndex) + rvValue(i)
			end if
		end do
		do i = 1, SIZE(ivNumValues)
			if(ivNumValues(i) > 0) then
				rvTypicalDay(i) = rvSumValues(i) / ivNumValues(i)
			else
				rvTypicalDay(i) = NaN
			end if
		end do
		
		! Compute anomaly (and its buddies) based on current and typical day
		rAnomaly    =  0.0
		rStdDev     =  0.0
		rMin        =  HUGE(rMin)
		rMax        = -HUGE(rMax)
		iNumAnomaly = 0
		do i = iFirstItemInDay, iLastItemInDay
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				rDelta   = rvValue(i) - rvTypicalDay(iIndex)
				rAnomaly = rAnomaly + rDelta
				rMin     = MIN(rMin, rDelta)
				rMax     = MAX(rMax, rDelta)
				iNumAnomaly = iNumAnomaly + 1
			end if
		end do
		if(iNumAnomaly > 0) then
		
			! Compute mean anomaly
			rAnomaly = rAnomaly / iNumAnomaly
			
			! Compute anomaly standard deviation
			do i = iFirstItemInDay, iLastItemInDay
				if(.not.ISNAN(rvValue(i))) then
					iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
					rDelta   = rvValue(i) - rvTypicalDay(iIndex)
					rStdDev = rStdDev + (rDelta - rAnomaly)**2
				end if
			end do
			rStdDev = SQRT(ABS(rStdDev / iNumAnomaly))
			
		else
		
			rAnomaly = NaN
			rMin     = NaN
			rMax     = NaN
			rStdDev  = NaN
			
		end if
				
		! Leave
		deallocate(ivNumValues, rvSumValues)
		deallocate(rvTypicalDay)
		
	end function Anomaly
	
	
	! Fill gaps using typical day. Input data are assumed to come from a
	! monotonic regular time series composed by entire days - this is not checked
	! however, to save some computing time, and also because this condition
	! is true by construction after data have been read within ST-Me.
	! Warning: because of this lack of controls, this routine is not
	!          meant for general use: it works within ST-Me only.
	function FillGaps(ivTimeStamp, rvValue, iDeltaTime, iDaysRadius, lvOriginal) result(iRetCode)
	
		! Routine arguments
		integer, dimension(:), intent(in)		:: ivTimeStamp
		real, dimension(:), intent(inout)		:: rvValue
		integer, intent(in)						:: iDeltaTime
		integer, intent(in)						:: iDaysRadius
		logical, dimension(:), intent(inout)	:: lvOriginal
		integer									:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: iFirstItem
		integer								:: iLastItem
		integer								:: i
		integer								:: iIndex
		integer, dimension(:), allocatable	:: ivNumValues
		real, dimension(:), allocatable		:: rvSumValues
		real								:: rAnomaly
		integer								:: iNumValid
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			iRetCode = 1
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			iRetCode = 2
			return
		end if
		
		! Reserve temporary workspace
		allocate(ivNumValues(iNumItemsPerDay), rvSumValues(iNumItemsPerDay), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		
		! Iterate over days
		do iCurDay = 1, iNumDays
		
			! Delimit day
			iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
			iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
			
			! Check whether something is to be made on this day
			if(any(ISNAN(rvValue(iFirstItemInDay:iLastItemInDay)))) then
			
				! Delimit the time span over which typical day is computed
				iFirstItem  = MAX(iFirstItemInDay - iDaysRadius * iNumItemsPerDay, 1)
				iLastItem   = MIN(iLastItemInDay  + iDaysRadius * iNumItemsPerDay, SIZE(rvValue))
				
				! Compute typical day
				ivNumValues = 0
				rvSumValues = 0.
				do i = iFirstItem, iLastItem
					if(.not.ISNAN(rvValue(i))) then
						iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
						ivNumValues(iIndex) = ivNumValues(iIndex) + 1
						rvSumValues(iIndex) = rvSumValues(iIndex) + rvValue(i)
					end if
				end do
				do i = 1, SIZE(ivNumValues)
					if(ivNumValues(i) > 0) then
						rvSumValues(i) = rvSumValues(i) / ivNumValues(i)
					else
						rvSumValues(i) = NaN
					end if
				end do
				
				! Estimate current day anomaly respect to typical day
				rAnomaly  = 0.
				iNumValid = 0
				do i = iFirstItem, iLastItem
					if(.not.ISNAN(rvValue(i))) then
						iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
						iNumValid = iNumValid + 1
						rAnomaly  = rAnomaly + rvValue(i) - rvSumValues(iIndex)
					end if
				end do
				
				! Fill gaps taking anomaly into account (if it can be computed)
				if(iNumValid > 0) then
					rAnomaly = rAnomaly / iNumValid
					do i = iFirstItem, iLastItem
						if(ISNAN(rvValue(i))) then
							iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
							rvValue(i) = rvSumValues(iIndex) + rAnomaly
							lvOriginal(i) = .false.
						end if
					end do
				else
					do i = iFirstItem, iLastItem
						if(ISNAN(rvValue(i))) then
							iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
							rvValue(i) = rvSumValues(iIndex)
							lvOriginal(i) = .false.
						end if
					end do
				end if
				
			end if
			
		end do
		
		! Leave
		deallocate(ivNumValues, rvSumValues)
		
	end function FillGaps
	
	
	! Fill wind gaps
	function WindGaps(ivTimeStamp, rvVel, rvDir, iDeltaTime, iDaysRadius, lvOriginal) result(iRetCode)
	
		! Routine arguments
		integer, dimension(:), intent(in)		:: ivTimeStamp
		real, dimension(:), intent(inout)		:: rvVel
		real, dimension(:), intent(inout)		:: rvDir
		integer, intent(in)						:: iDeltaTime
		integer, intent(in)						:: iDaysRadius
		logical, dimension(:), intent(inout)	:: lvOriginal
		integer									:: iRetCode
		
		! Locals
		integer								:: iErrCode
		real, dimension(size(ivTimeStamp))	:: rvU, rvV
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Compute horizontal wind components in flow reference (that is,
		! wind vector "points to" where wind is going). Gaps are notified, and not
		! covered.
		where(ISNAN(rvVel) .or. ISNAN(rvDir))
			rvU = NaN
			rvV = NaN
		elsewhere
			rvU = -rvVel * SIN(rvDir * PI / 180.)
			rvV = -rvVel * COS(rvDir * PI / 180.)
		endwhere
		
		! Fill gaps in components
		iErrCode = FillGaps(ivTimeStamp, rvU, iDeltaTime, iDaysRadius, lvOriginal)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		iErrCode = FillGaps(ivTimeStamp, rvV, iDeltaTime, iDaysRadius, lvOriginal)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		
		! Reconstruct wind vector in polar form
		where(ISNAN(rvDir))
			rvDir = ATAN2(-rvU,-rvV)*180.0/PI
		endwhere
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		endwhere
		where(ISNAN(rvVel))
			rvVel = SQRT(rvU**2 + rvV**2)
		endwhere
		
	end function WindGaps


	! Estimate *absolute* anomaly with respect to typical day. This calculation is done on some
	! specially interesting values (temperature, relative humidity, mixing height, ...)
	! after processing, and reported on a daily basis as a statistical comprehension
	! tool. Having data been already processed, they form a regular time series
	! made of entire days, with no gaps.
	!
	! To see why I've chosen absolute anomaly instead of usual anomaly, you may
	! have a look to my 
	function EstimateAnomaly(iStampInDay, ivTimeStamp, rvValue, iDeltaTime, iDaysRadius) result(rAnomaly)
	
		! Routine arguments
		integer, intent(in)					:: iStampInDay
		integer, dimension(:), intent(in)	:: ivTimeStamp
		real, dimension(:), intent(in)		:: rvValue
		integer, intent(in)					:: iDeltaTime
		integer, intent(in)					:: iDaysRadius
		real								:: rAnomaly
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iDayStamp
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: iFirstItem
		integer								:: iLastItem
		integer								:: i
		integer								:: iIndex
		integer, dimension(:), allocatable	:: ivNumValues
		real, dimension(:), allocatable		:: rvSumValues
		integer								:: iNumValid
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			rAnomaly = NaN
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			rAnomaly = NaN
			return
		end if
		
		! Reserve temporary workspace
		allocate(ivNumValues(iNumItemsPerDay), rvSumValues(iNumItemsPerDay), STAT=iErrCode)
		if(iErrCode /= 0) then
			rAnomaly = NaN
			return
		end if
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			rAnomaly = NaN
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			rAnomaly = NaN
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
	
		! Delimit the time span over which typical day is computed
		iFirstItem  = MAX(iFirstItemInDay - iDaysRadius * iNumItemsPerDay, 1)
		iLastItem   = MIN(iLastItemInDay  + iDaysRadius * iNumItemsPerDay, SIZE(rvValue))
			
		! Compute typical day
		ivNumValues = 0
		rvSumValues = 0.
		do i = iFirstItem, iLastItem
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				ivNumValues(iIndex) = ivNumValues(iIndex) + 1
				rvSumValues(iIndex) = rvSumValues(iIndex) + rvValue(i)
			end if
		end do
		do i = 1, SIZE(ivNumValues)
			if(ivNumValues(i) > 0) then
				rvSumValues(i) = rvSumValues(i) / ivNumValues(i)
			else
				rvSumValues(i) = NaN
			end if
		end do
			
		! Estimate current day anomaly respect to typical day
		rAnomaly  = 0.
		iNumValid = 0
		do i = iFirstItemInDay, iLastItemInDay
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				iNumValid = iNumValid + 1
				rAnomaly  = rAnomaly + (rvValue(i) - rvSumValues(iIndex))
			end if
		end do
		if(iNumValid > 0) then
			rAnomaly = rAnomaly / iNumValid
		else
			rAnomaly = NaN
		end if
				
		! Leave
		deallocate(ivNumValues, rvSumValues)
		
	end function EstimateAnomaly
	
	
	! Compute daily mean
	function DailyMean(iStampInDay, ivTimeStamp, rvValue, iDeltaTime) result(rMean)
	
		! Routine arguments
		integer, intent(in)					:: iStampInDay
		integer, dimension(:), intent(in)	:: ivTimeStamp
		real, dimension(:), intent(in)		:: rvValue
		integer, intent(in)					:: iDeltaTime
		real								:: rMean
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iDayStamp
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: i
		integer								:: iIndex
		integer								:: iNumValid
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			rMean = NaN
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			rMean = NaN
			return
		end if
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			rMean = NaN
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			rMean = NaN
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
	
		! Compute mean
		rMean = 0.
		iNumValid = 0
		do i = iFirstItemInDay, iLastItemInDay
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				iNumValid = iNumValid + 1
				rMean     = rMean + rvValue(i)
			end if
		end do
		if(iNumValid > 0) then
			rMean = rMean / iNumValid
		else
			rMean = NaN
		end if
			
	end function DailyMean
	
	
	! Compute daily mean
	function DailyMin(iStampInDay, ivTimeStamp, rvValue, iDeltaTime) result(rMean)
	
		! Routine arguments
		integer, intent(in)					:: iStampInDay
		integer, dimension(:), intent(in)	:: ivTimeStamp
		real, dimension(:), intent(in)		:: rvValue
		integer, intent(in)					:: iDeltaTime
		real								:: rMean
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iDayStamp
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: i
		integer								:: iIndex
		integer								:: iNumValid
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			rMean = NaN
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			rMean = NaN
			return
		end if
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			rMean = NaN
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			rMean = NaN
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
	
		! Compute mean
		rMean = HUGE(rMean)
		iNumValid = 0
		do i = iFirstItemInDay, iLastItemInDay
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				iNumValid = iNumValid + 1
				rMean     = MIN(rMean, rvValue(i))
			end if
		end do
		if(iNumValid <= 0) then
			rMean = NaN
		end if
			
	end function DailyMin
	
	
	! Compute daily mean
	function DailyMax(iStampInDay, ivTimeStamp, rvValue, iDeltaTime) result(rMean)
	
		! Routine arguments
		integer, intent(in)					:: iStampInDay
		integer, dimension(:), intent(in)	:: ivTimeStamp
		real, dimension(:), intent(in)		:: rvValue
		integer, intent(in)					:: iDeltaTime
		real								:: rMean
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iDayStamp
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: i
		integer								:: iIndex
		integer								:: iNumValid
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			rMean = NaN
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			rMean = NaN
			return
		end if
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			rMean = NaN
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			rMean = NaN
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
	
		! Compute mean
		rMean = -HUGE(rMean)
		iNumValid = 0
		do i = iFirstItemInDay, iLastItemInDay
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				iNumValid = iNumValid + 1
				rMean     = MAX(rMean, rvValue(i))
			end if
		end do
		if(iNumValid <= 0) then
			rMean = NaN
		end if
			
	end function DailyMax
	
	
	! Compute daily sum
	function DailySum(iStampInDay, ivTimeStamp, rvValue, iDeltaTime) result(rMean)
	
		! Routine arguments
		integer, intent(in)					:: iStampInDay
		integer, dimension(:), intent(in)	:: ivTimeStamp
		real, dimension(:), intent(in)		:: rvValue
		integer, intent(in)					:: iDeltaTime
		real								:: rMean
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iDayStamp
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: i
		integer								:: iIndex
		integer								:: iNumValid
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			rMean = NaN
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			rMean = NaN
			return
		end if
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			rMean = NaN
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			rMean = NaN
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
	
		! Compute mean
		rMean = 0.
		iNumValid = 0
		do i = iFirstItemInDay, iLastItemInDay
			if(.not.ISNAN(rvValue(i))) then
				iIndex = MOD(ivTimeStamp(i), ONE_DAY) / iDeltaTime + 1
				iNumValid = iNumValid + 1
				rMean     = rMean + rvValue(i)
			end if
		end do
		if(iNumValid <= 0) then
			rMean = NaN
		end if
			
	end function DailySum
	
	
	! Compute daily count of invalid data
	function DailyAvailability(iStampInDay, ivTimeStamp, rvValue, iDeltaTime) result(rAvailability)
	
		! Routine arguments
		integer, intent(in)					:: iStampInDay
		integer, dimension(:), intent(in)	:: ivTimeStamp
		real, dimension(:), intent(in)		:: rvValue
		integer, intent(in)					:: iDeltaTime
		real								:: rAvailability
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iDayStamp
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: i
		integer								:: iNumInvalid
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			rAvailability = NaN
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			rAvailability = NaN
			return
		end if
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			rAvailability = NaN
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			rAvailability = NaN
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
	
		! Compute mean
		iNumInvalid = 0
		do i = iFirstItemInDay, iLastItemInDay
			if(ISNAN(rvValue(i))) then
				iNumInvalid = iNumInvalid + 1
			end if
		end do
		if(iNumInvalid <= 0) then
			rAvailability = 100.0
		else
			rAvailability = 100.0 * (1.0 - FLOAT(iNumInvalid) / FLOAT(iNumItemsPerDay))
		end if
			
	end function DailyAvailability
	
	
	! Compute daily count of invalid data
	function CountFilled(iStampInDay, ivTimeStamp, lvOriginal, iDeltaTime) result(rFilled)
	
		! Routine arguments
		integer, intent(in)					:: iStampInDay
		integer, dimension(:), intent(in)	:: ivTimeStamp
		logical, dimension(:), intent(in)	:: lvOriginal
		integer, intent(in)					:: iDeltaTime
		real								:: rFilled
		
		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iDayStamp
		integer								:: iCurDay
		integer								:: iFirstItemInDay
		integer								:: iLastItemInDay
		integer								:: i
		integer								:: iNumFilled
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = 24*ONE_HOUR
		
		! How many data come in a day?
		if(iDeltaTime <= 0) then
			rFilled = NaN
			return
		end if
		iNumItemsPerDay = ONE_DAY / iDeltaTime
		
		! How many days in data set?
		iNumDays = (ivTimeStamp(size(ivTimeStamp)) - ivTimeStamp(1)) / ONE_DAY + 1
		if(iNumDays <= 0) then
			rFilled = NaN
			return
		end if
		
		! Check day's stamp to be within time series overall span
		if(iStampInDay < ivTimeStamp(1) .or. iStampInDay > ivTimeStamp(SIZE(ivTimeStamp))) then
			rFilled = NaN
			return
		end if
		
		! Round stamp in day so that it is expressed to beginning of day,
		! and not to something-within; this is not strictly indispensable
		! for the computer, but may make the analysts' life somewhat simpler,
		! together wit the assumption the time span  of data series starts on an
		! entire day.
		iDayStamp = iStampInDay - MOD(iStampInDay, ONE_DAY)
		
		! Find index corresponding to day whose stamp is passed
		iCurDay = (iDayStamp - ivTimeStamp(1)) / ONE_DAY + 1
		
		! Check again the current day is within expected limits
		if(iCurDay < 1 .or. iCurDay > iNumDays) then
			rFilled = NaN
			return
		end if
		
		! Delimit day
		iFirstItemInDay = (iCurDay - 1)*iNumItemsPerDay + 1
		iLastItemInDay  = iFirstItemInDay + iNumItemsPerDay - 1
	
		! Compute mean
		iNumFilled = 0
		do i = iFirstItemInDay, iLastItemInDay
			if(.not.lvOriginal(i)) then
				iNumFilled = iNumFilled + 1
			end if
		end do
		if(iNumFilled <= 0) then
			rFilled =   0.0
		else
			rFilled = 100.0 * FLOAT(iNumFilled) / FLOAT(iNumItemsPerDay)
		end if
			
	end function CountFilled
	
end module Statistics
