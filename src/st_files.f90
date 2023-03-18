module st_files

	use calendar
	use abl_physics
	use st_sodar_data
	use profiles
	use routines
	use statistics
	use logging

	implicit none
	
	private
	
	! Public interface
	! -1- Data types
	public	:: FileNamesType
	public	:: DataSetType
    ! -1- Functions and subroutines
	public	:: generateFileNames
	
	! Data types
	
	type FileNamesType
		character(len=256)	:: sInputFile
		character(len=256)	:: sLogFile
		character(len=256)	:: sSodarFile
		character(len=256)	:: sOutputFile
		character(len=256)	:: sPreWindRoseFile
		character(len=256)	:: sPostWindRoseFile
		character(len=256)	:: sDiurnalWindRoseFile
		character(len=256)	:: sNocturnalWindRoseFile
		character(len=256)	:: sConvectiveWindRoseFile1
		character(len=256)	:: sNeutralWindRoseFile1
		character(len=256)	:: sStableWindRoseFile1
		character(len=256)	:: sConvectiveWindRoseFile2
		character(len=256)	:: sNeutralWindRoseFile2
		character(len=256)	:: sStableWindRoseFile2
		character(len=256)	:: sDailyAvailFile
		character(len=256)	:: sDailyPostAvailFile
		character(len=256)	:: sDailyReportFile
		character(len=256)	:: sDiaReport
		character(len=256)	:: sCalpuffIsc
		character(len=256)	:: sCalpuffSrf
		character(len=256)	:: sCalpuffPrf
		character(len=256)	:: sCalmetSrf
		character(len=256)	:: sCalmetPrf
		character(len=256)	:: sCalmetSrf00
		character(len=256)	:: sCalmetPrf00
		character(len=256)	:: sAermet
		character(len=256)	:: sGral
		character(len=256)	:: sAustal2000
		character(len=256)	:: sTinyDispSrf
		character(len=256)	:: sTinyDispPrf
	contains
		procedure	:: generate   => GenerateFileNames
		procedure	:: setSodar   => SetSodarFile
		procedure	:: validate   => ValidateFileNames
	end type FileNamesType
	
	
	type DataSetType
		! Station context
		real	:: rLat
		real	:: rLon
		integer	:: iTimeZone
		real	:: rHeight
		real	:: rZ0
		real	:: rAlbedo
		real	:: rD
		real	:: rZr
		integer	:: iLandUse
		integer	:: iDateFormat
		integer	:: iDateMeaning
		! Actual data
		integer, dimension(:), allocatable		:: ivTimeStamp
		real, dimension(:), allocatable			:: rvVel
		real, dimension(:), allocatable			:: rvDir
		real, dimension(:), allocatable			:: rvTemp
		real, dimension(:), allocatable			:: rvTdew
		real, dimension(:), allocatable			:: rvUrel
		real, dimension(:), allocatable			:: rvN
		real, dimension(:), allocatable			:: rvP
		real, dimension(:), allocatable			:: rvCover
		real, dimension(:), allocatable			:: rvRg
		real, dimension(:), allocatable			:: rvRain
		real, dimension(:), allocatable			:: rvRn
		real, dimension(:), allocatable			:: rvUstar
		real, dimension(:), allocatable			:: rvTstar
		real, dimension(:), allocatable			:: rvH0
		real, dimension(:), allocatable			:: rvHLM
		real, dimension(:), allocatable			:: rvL
		real, dimension(:), allocatable			:: rvLalt1
		real, dimension(:), allocatable			:: rvLalt2
		real, dimension(:), allocatable			:: rvG0
		real, dimension(:), allocatable			:: rvApparentLAI
		real, dimension(:), allocatable			:: rvHe
		real, dimension(:), allocatable			:: rvZi
		real, dimension(:), allocatable			:: rvWs
		real, dimension(:), allocatable			:: rvSinPsi
		real, dimension(:), allocatable			:: rvPsi
		real, dimension(:), allocatable			:: rvPrecWater
		real, dimension(:), allocatable			:: rvRg2
		real, dimension(:), allocatable			:: rvRg3
		real, dimension(:), allocatable			:: rvRg4
		real, dimension(:), allocatable			:: rvKt
		real, dimension(:), allocatable			:: rvKtErr
		real, dimension(:), allocatable			:: rvRa
		real, dimension(:), allocatable			:: rvFcd
		real, dimension(:), allocatable			:: rvRn2
		real, dimension(:), allocatable			:: rvEsat
		real, dimension(:), allocatable			:: rvEa
		real, dimension(:), allocatable			:: rvETs
		real, dimension(:), allocatable			:: rvETt
		integer, dimension(:), allocatable		:: ivIstab
		integer, dimension(:), allocatable		:: ivLstab
		logical, dimension(:), allocatable		:: lvDiurnal
		logical, dimension(:), allocatable		:: lvOriginal
		logical, dimension(:), allocatable		:: lvOriginalWind
		logical, dimension(:), allocatable		:: lvOriginalTemp
		logical, dimension(:), allocatable		:: lvOriginalRelH
		integer, dimension(:), allocatable		:: ivNumHeights
		integer, dimension(:), allocatable		:: ivSodarNumHeights
		real, dimension(:,:), allocatable		:: rmSodarHeight
		real, dimension(:,:), allocatable		:: rmSodarVel
		real, dimension(:,:), allocatable		:: rmSodarDir
		real, dimension(:,:), allocatable		:: rmSodarTemp
		real, dimension(:), allocatable			:: rvZ
		real, dimension(:,:), allocatable		:: rmOutU
		real, dimension(:,:), allocatable		:: rmOutV
		real, dimension(:,:), allocatable		:: rmOutTemp
		real, dimension(:,:), allocatable		:: rmU
		real, dimension(:,:), allocatable		:: rmV
		real, dimension(:,:), allocatable		:: rmTemp
		! Inferred properties
		integer	:: iMinTimeStamp
		integer	:: iMaxTimeStamp
		integer	:: iMinDateStamp
		integer	:: iMaxDateStamp
		integer	:: iNumDays
		integer	:: iDeltaTime
		logical	:: lIsMonotonic		! Actually, time stamp vector is strictly increasing
		logical	:: lIsRegular		! That is, all time stamps are exact multiples of 'iDeltaTime'
	contains
		! High-level interface
		procedure	:: populate              => PopulateFromInput
		procedure	:: report                => ReportDataSet
		procedure	:: reportYear            => ReportDataSetYear
		procedure	:: reportTwo             => ReportTwoDataSet
		procedure	:: reportThree           => ReportThreeDataSet
		procedure	:: reportProfiles        => ReportProfiles
		procedure	:: reportH0Stability     => ReportH0Stability
		procedure	:: reportStability       => ReportStability
		procedure	:: reportTypicalDays     => ReportTypicalDays
		procedure	:: reportDirMean         => ReportDirectionalMean
		procedure	:: printRose             => PrintWindRose
		procedure	:: printStbRoses         => PrintStbRoses
		procedure	:: checkUseability       => CheckEnoughData
		procedure	:: prepareToPrint        => PrepareToPrint
		procedure	:: writeDataToFile       => WriteDataToFile
		procedure	:: WriteDailyReport      => WriteDailyReport
		procedure	:: WriteDailyAvail       => WriteDailyAvailability
		procedure	:: WriteDailyPostAvail   => WriteDailyPostAvailability
		procedure	:: writeDataToCIsc       => WriteDataToCalpuffIsc
		procedure	:: writeDataToPIsc       => WriteDataToCalpuffProfiled
		procedure	:: writeDataToPIsc7      => WriteDataToCalpuffProf7
		procedure	:: writeDataToCmet       => WriteDataToCalmet
		procedure	:: writeDataToCmet00     => WriteDataToCalmet00
		procedure	:: writeDataToAmet       => WriteDataToAermet
		procedure	:: writeDataToGral       => WriteDataToGral
		procedure	:: writeDataToAustal2000 => WriteDataToAustal2000
		procedure	:: writeDataToTinyDisp   => WriteDataToTinyDisp
		! Low-level routines
		procedure	:: getContext      => GetContextFromInput
		procedure	:: getData         => GetDataFromInput
	end type DataSetType
	
contains

	subroutine GenerateFileNames(this, sInputFile)
	
		! Routine arguments
		class(FileNamesType), intent(inout)	:: this
		character(len=256), intent(in)		:: sInputFile
		
		! Locals
		character(len=256)	:: sWithoutExtension
		
		! Clean file extension, then use it to generate all file names
		sWithoutExtension = cleanFileExtension(sInputFile)
		this % sInputFile = sInputFile
		write(this % sLogFile, "(a, 'log')") trim(sWithoutExtension)
		write(this % sOutputFile, "(a, 'csv')") trim(sWithoutExtension)
		write(this % sPreWindRoseFile, "(a, 'pre.rose')") trim(sWithoutExtension)
		write(this % sPostWindRoseFile, "(a, 'post.rose')") trim(sWithoutExtension)
		write(this % sDiurnalWindRoseFile, "(a, 'diurnal.rose')") trim(sWithoutExtension)
		write(this % sNocturnalWindRoseFile, "(a, 'nocturnal.rose')") trim(sWithoutExtension)
		write(this % sConvectiveWindRoseFile1, "(a, 'convective.rad.rose')") trim(sWithoutExtension)
		write(this % sNeutralWindRoseFile1, "(a, 'neutral.rad.rose')") trim(sWithoutExtension)
		write(this % sStableWindRoseFile1, "(a, 'stable.rad.rose')") trim(sWithoutExtension)
		write(this % sConvectiveWindRoseFile2, "(a, 'convective.turb.rose')") trim(sWithoutExtension)
		write(this % sNeutralWindRoseFile2, "(a, 'neutral.turb.rose')") trim(sWithoutExtension)
		write(this % sStableWindRoseFile2, "(a, 'stable.turb.rose')") trim(sWithoutExtension)
		write(this % sDailyReportFile, "(a, 'drf')") trim(sWithoutExtension)
		write(this % sDailyAvailFile, "(a, 'avl')") trim(sWithoutExtension)
		write(this % sDailyPostAvailFile, "(a, 'avp')") trim(sWithoutExtension)
		write(this % sDiaReport, "(a, 'report')") trim(sWithoutExtension)
		write(this % sCalpuffIsc, "(a, 'cpf')") trim(sWithoutExtension)
		write(this % sCalpuffSrf, "(a, 'csr')") trim(sWithoutExtension)
		write(this % sCalpuffPrf, "(a, 'cpr')") trim(sWithoutExtension)
		write(this % sCalmetSrf, "(a, 'msr')") trim(sWithoutExtension)
		write(this % sCalmetPrf, "(a, 'mpr')") trim(sWithoutExtension)
		write(this % sCalmetSrf00, "(a, '0sr')") trim(sWithoutExtension)
		write(this % sCalmetPrf00, "(a, '0pr')") trim(sWithoutExtension)
		write(this % sAermet, "(a, 'amt')") trim(sWithoutExtension)
		write(this % sGral, "(a, 'met')") trim(sWithoutExtension)
		write(this % sAustal2000, "(a, 'akt')") trim(sWithoutExtension)
		write(this % sTinyDispSrf, "(a, 'tsr')") trim(sWithoutExtension)
		write(this % sTinyDispPrf, "(a, 'tpr')") trim(sWithoutExtension)
		
	end subroutine GenerateFileNames
	
	
	subroutine SetSodarFile(this, sSodarFile)
	
		! Routine arguments
		class(FileNamesType), intent(inout)	:: this
		character(len=256), intent(in)		:: sSodarFile
		
		! Locals
		! -none-
		
		! Set SODAR file name
		this % sSodarFile = sSodarFile
		
	end subroutine SetSodarFile
	
	
	function ValidateFileNames(this) result(lIsValid)
	
		! Routine arguments
		class(FileNamesType), intent(inout)	:: this
		logical								:: lIsValid
		
		! Locals
		! -none-
		
		! Check file names are non-empty
		if( &
			this % sInputFile == " " .or. &
			this % sOutputFile == " " .or. &
			this % sPreWindRoseFile == " " .or. &
			this % sPostWindRoseFile == " " .or. &
			this % sDiaReport == " " &
		) then
			lIsValid = .false.
			return
		end if
		
		! Check file names are distinct
		if( &
			this % sInputFile == this % sOutputFile .or. &
			this % sInputFile == this % sPreWindRoseFile .or. &
			this % sInputFile == this % sPreWindRoseFile .or. &
			this % sInputFile == this % sDiaReport .or. &
			this % sOutputFile == this % sPreWindRoseFile .or. &
			this % sOutputFile == this % sPostWindRoseFile .or. &
			this % sOutputFile == this % sDiaReport .or. &
			this % sPreWindRoseFile == this % sPostWindRoseFile .or. &
			this % sPreWindRoseFile == this % sPostWindRoseFile .or. &
			this % sPostWindRoseFile == this % sDiaReport &
		) then
			lIsValid = .false.
			return
		end if
		
		! Check input file does really exist
		inquire(file=this % sInputFile, exist=lIsValid)
		
	end function ValidateFileNames
	
	
	function PopulateFromInput(this, tFiles, iSodarDateStart) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		type(FileNamesType), intent(in)		:: tFiles
		integer, intent(in)					:: iSodarDateStart
		integer								:: iRetCode
		
		! Locals
		integer	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Try connecting input file
		open(10, file=tFiles % sInputFile, status='old', action='read', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			call logError("Data read: Input file not found")
			return
		end if
		iErrCode = this % getContext()
		if(iErrCode /= 0) then
			iRetCode = 100 + iErrCode
			return
		end if
		iErrCode = this % getData(tFiles, iSodarDateStart)
		if(iErrCode /= 0) then
			iRetCode = 200 + iErrCode
			return
		end if
		close(10)
		
	end function PopulateFromInput
	

	function GetContextFromInput(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		integer				:: iErrCode
		character(len=512)	:: sBuffer
		integer				:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Skip first line
		rewind(10)
		read(10,"(a)",IOSTAT=iErrCode) sBuffer
		if(iErrCode /= 0) then
			close(10)
			iRetCode = 1
			call logError("Data read: Input file is empty or not readable as text")
			return
		end if
		
		! Replace commas with blanks
		do i = 1, len_trim(sBuffer)
			if(sBuffer(i:i) == ",") sBuffer(i:i) = ' '
		end do
		
		! Get station context data
		read(10,*,IOSTAT=iErrCode) &
			this % rLat, &			! Latitude
			this % rLon, &			! Longitude
			this % iTimeZone, &		! Time zone (1 for Italy, Germany, France)
			this % rHeight, &		! Altitude of station about msl (m)
			this % rZ0, &			! Aerodynamic roughness length (m)
			this % rAlbedo, &		! Albedo ratio
			this % rD, &			! Displacement height (m)
			this % rZr, &			! Anemometer height above ground (m)
			this % iLandUse, &		! Land use code: 1=Desert, 2=Rural dry areas, 3=Urban, 4=Mixed urban and suburban, 5=Forests and grassland, 6=Water bodies
			this % iDateFormat, &	! 0=European (dd/mm/yyyy hh), 1=USA (mm/dd/yyyy hh:mm), 2=ISO (yyyy-mm-dd hh:mm:ss)
			this % iDateMeaning		! 0=Time stamp refers to beginning of avg interval; 1=To end of interval
		if(iErrCode /= 0) then
			close(10)
			iRetCode = 2
			call logError("Data read: Line 2 (header) of PREMET2 file not read")
			return
		end if
		if(this%rLat < -90. .OR. this%rLat > +90.) then
			close(10)
			iRetCode = 3
			call logError("Data read: Latitude not in range [-90,+90]")
			return
		end if
		if(this%rLon < 0. .OR. this%rLon > 360.) then
			close(10)
			iRetCode = 4
			call logError("Data read: Longitude not in range [0,360]")
			return
		end if
		if(this%iTimeZone < -12 .OR. this%iTimeZone > 12) then
			close(10)
			iRetCode = 5
			call logError("Data read: Time zone not in range [-12,+12]")
			return
		end if
		if(this%rHeight <= 0) then
			close(10)
			iRetCode = 6
			call logError("Data read: Negative altitude above MSL")
			return
		end if
		if(this%rZ0 < 0.0001 .OR. this%rZ0 > 10.) then
			close(10)
			iRetCode = 7
			call logError("Data read: z0 not in range [0.0001,10.]")
			return
		end if
		if(this%rAlbedo < 0. .OR. this%rAlbedo > 1.) then
			close(10)
			iRetCode = 8
			call logError("Data read: Albedo not in range [0,1]")
			return
		end if
		if(this%rD < 0.) then
			close(10)
			iRetCode = 9
			call logError("Data read: Negative displacement height")
			return
		end if
		if(this%rZr < 0.1 .OR. this%rZr > 30.) then
			close(10)
			iRetCode = 10
			call logError("Data read: Zr not in range [0.1,30.]")
			return
		end if
		if(this%iDateFormat < 0 .OR. this%iDateFormat > 2) then
			close(10)
			iRetCode = 11
			call logError("Data read: Date format not 0 or 1")
			return
		end if
		if(this%iLandUse < 1 .or. this%iLandUse > 6) then
			close(10)
			iRetCode = 12
			call logError("Data read: Land use not in range [1,6]")
			return
		end if
		
		! Skip next two lines
		read(10,"(a)",IOSTAT=iErrCode) sBuffer
		if(iErrCode /= 0) then
			close(10)
			iRetCode = 13
			print *,iErrCode
			call logError("Data read: Input file line 3 is missing")
			return
		end if
		read(10,"(a)",IOSTAT=iErrCode) sBuffer
		if(iErrCode /= 0) then
			close(10)
			iRetCode = 14
			call logError("Data read: Input file line 4 is missing")
			return
		end if
		call logInfo("Data read: Input file context section successfully read")

	end function GetContextFromInput
	

	! Warning: must be called after 'GetContextFromInput'
	function GetDataFromInput(this, tFiles, iSodarDateStart) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		type(FileNamesType), intent(in)		:: tFiles
		integer, intent(in)					:: iSodarDateStart
		integer								:: iRetCode
		
		! Locals
		integer						:: iErrCode
		character(len=512)			:: sBuffer
		character(len=32)			:: sDateTime
		integer						:: iPos
		integer						:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer						:: iCurTime, iOldTime, iNumDeltas, iDelta
		integer						:: iNumData, i, iIndex, iBaseTime, iDayInYear
		logical						:: lIsFirst
		real						:: rVel, rDir, rTemp, rRelH, rRain, rCloudCover
		real						:: rRg, rRn, rUstar, rH0, rzL
		integer, dimension(1000000)	:: ivDelta
		real, dimension(60)			:: rvVel, rvDir, rvTemp, rvHeight
		type(SODAR_Data)			:: tSodarData
			
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! First pass: Get bounding time stamps (Note: station
		! context must have been read already, so file pointer
		! is past line 4 as a side effect: no skip to make)
		this % iMinTimeStamp =  HUGE(this % iMinTimeStamp)
		this % iMaxTimeStamp = -HUGE(this % iMaxTimeStamp)
		this % lIsMonotonic  = .true.
		lIsFirst   = .true.
		iOldTime   = -1
		iNumDeltas = 0
		ivDelta    = 0
		do
		
			! Get one line, and extract time stamp from it
			read(10, "(a)", iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			select case(this % iDateFormat)
			case(0)	! European form
			
				call logInfo("Data read: Date in European form expected (DD/MM/YYYY)")

				! Locate second comma (two fields used, one for date and one for hour),
				! and get date-time string up to there
				iPos = INDEX(sBuffer, ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 1
					call logError("Data read: No ',' separators")
					return
				end if
				iPos = iPos + INDEX(sBuffer(iPos+1:), ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 2
					call logError("Data read: Two fields only present, no information contents")
					return
				end if
				sDateTime = sBuffer(1:iPos-1)
				
				! Read date and time information
				call sanitizeString(sDateTime)
				read(sDateTime, *, iostat=iErrCode) iDay, iMonth, iYear, iHour
				if(iErrCode /= 0) then
					close(10)
					iRetCode = 3
					call logError("Data read: No date and time information parsed")
					return
				end if
				iMinute = 0
				iSecond = 0
				
			case(1)	! American form
			
				call logInfo("Data read: Date in American form expected (MM/DD/YYYY)")

				! Locate second comma (two fields used, one for date and one for hour),
				! and get date-time string up to there
				iPos = INDEX(sBuffer, ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 4
					call logError("Data read: No ',' separators")
					return
				end if
				iPos = iPos + INDEX(sBuffer(iPos+1:), ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 5
					call logError("Data read: Two fields only present, no information contents")
					return
				end if
				sDateTime = sBuffer(1:iPos-1)
				
				! Read date and time information
				call sanitizeString(sDateTime)
				read(sDateTime, *, iostat=iErrCode) iMonth, iDay, iYear, iHour
				if(iErrCode /= 0) then
					close(10)
					iRetCode = 6
					call logError("Data read: No date and time information parsed")
					return
				end if
				iMinute = 0
				iSecond = 0
				
			case(2)	! ISO form
			
				call logInfo("Data read: Date/time in ISO form expected (YYYY-MM-DD HH:MM:SS)")

				! Locate first comma (one field used for date and time),
				! and get date-time string up to there
				iPos = INDEX(sBuffer, ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 7
					call logError("Data read: No ',' separators")
					return
				end if
				sDateTime = sBuffer(1:iPos-1)
				
				! Read date and time information
				call sanitizeString(sDateTime)
				read(sDateTime, *, iostat=iErrCode) iYear, iMonth, iDay, iHour, iMinute, iSecond
				if(iErrCode /= 0) then
					close(10)
					iRetCode = 8
					call logError("Data read: No date and time information parsed")
					return
				end if
				
			end select
			
			! Adjust year if century was not specified (assume century 2000 in case)
			if(iYear < 100) iYear = iYear + 2000	! A year like "123" is taken face value
			
			! Convert date and time to epoch time stamp, and update time bounds:
			! this allows for non-full-year intervals
			call PackTime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			this % iMinTimeStamp = MIN(this % iMinTimeStamp, iCurTime)
			this % iMaxTimeStamp = MAX(this % iMaxTimeStamp, iCurTime)
			
			! Check current time is larger than previous
			if(iCurTime <= iOldTime) then
				this % lIsMonotonic = .false.
			end if
			
			! Compute time difference and add it to differences set
			this % lIsRegular = .true.
			if(lIsFirst) then
				lIsFirst = .false.
			else
				iDelta = iCurTime - iOldTime
				if(iDelta > 0) then
					if(iNumDeltas <= 0) then
						ivDelta(1) = iDelta
						iNumDeltas = 1
					else
						if(ALL(ivDelta(1:iNumDeltas) /= iDelta)) then
							iNumDeltas = iNumDeltas + 1
							ivDelta(iNumDeltas) = iDelta
						end if
					end if
				end if
			end if
			
			! Update old time for next step
			iOldTime = iCurTime
			
		end do
		
		! If no delta times have been found, then data is very strange, and not worth continuing
		if(iNumDeltas <= 0) then
			close(10)
			iRetCode = 10
			call logError("Data read: Time step not found in data set (maybe 1 data line only?)")
			return
		end if
		
		! If one delta time only exists, then it is the actual delta time for the whole data set;
		! otherwise, the delta time is the minimum delta time found, provided it divides any other
		! delta time exactly (which may happen, thanks to gaps as missing lines)
		if(iNumDeltas == 1) then
			if(ivDelta(1) <= 0) then
				close(10)
				iRetCode = 11
				call logError("Data read: Some time stamp goes backwards")
				return
			end if
			this % iDeltaTime = ivDelta(1)
		else
			iDelta = MINVAL(ivDelta(1:iNumDeltas))
			if(ANY(MOD(ivDelta(1:iNumDeltas), iDelta) /= 0)) then
				! One non-multiple delta time
				this % lIsRegular = .false.
			end if
			this % iDeltaTime = iDelta
		end if
		
		! A delta time is now assigned. But, does it correspond to
		! something easy to use? In particular, is it an exact divisor of 1 hour?
		if(MOD(3600, this % iDeltaTime) /= 0) then
			this % lIsRegular = .false.
		end if
		
		! In case of a posticipated time stamp, update minimum and maximum time
		! stamps so that they are expressed in anticipated convention
		if(this % iDateMeaning == 1) then
			this % iMinTimeStamp = this % iMinTimeStamp - this % iDeltaTime
			this % iMaxTimeStamp = this % iMaxTimeStamp - this % iDeltaTime
		end if
		
		! Build minimum and maximum date stamps: these will be the values actually used
		! when attempting to allocate workspace - this simplifies the treatment of entire
		! days
		this % iMinDateStamp = this % iMinTimeStamp - MOD(this % iMinTimeStamp, 86400)			! Beginning of first data day
		this % iMaxDateStamp = this % iMaxTimeStamp - MOD(this % iMaxTimeStamp, 86400) + 86400	! Beginning of the first day *next* to data
		this % iNumDays      = (this % iMaxDateStamp - this % iMinDateStamp) / 86400
		
		! Reserve workspace
		iNumData = (this % iMaxDateStamp - this % iMinDateStamp) / this % iDeltaTime
		allocate( &
			this % ivTimeStamp(iNumData), &
			this % rvVel(iNumData), &
			this % rvDir(iNumData), &
			this % rvTemp(iNumData), &
			this % rvTdew(iNumData), &
			this % rvUrel(iNumData), &
			this % rvN(iNumData), &
			this % rvP(iNumData), &
			this % rvCover(iNumData), &
			this % rvRg(iNumData), &
			this % rvRain(iNumData), &
			this % rvRn(iNumData), &
			this % rvUstar(iNumData), &
			this % rvTstar(iNumData), &
			this % rvH0(iNumData), &
			this % rvHLM(iNumData), &
			this % rvL(iNumData), &
			this % rvLalt1(iNumData), &
			this % rvLalt2(iNumData), &
			this % rvG0(iNumData), &
			this % rvApparentLAI(iNumData), &
			this % rvHe(iNumData), &
			this % rvZi(iNumData), &
			this % rvWs(iNumData), &
			this % rvSinPsi(iNumData), &
			this % rvPsi(iNumData), &
			this % rvPrecWater(iNumData), &
			this % rvRa(iNumData), &
			this % rvRg2(iNumData), &
			this % rvRg3(iNumData), &
			this % rvRg4(iNumData), &
			this % rvKt(iNumData), &
			this % rvKtErr(iNumData), &
			this % rvFcd(iNumData), &
			this % rvRn2(iNumData), &
			this % rvEsat(iNumData), &
			this % rvEa(iNumData), &
			this % rvETs(iNumData), &
			this % rvETt(iNumData), &
			this % ivIstab(iNumData), &
			this % ivLstab(iNumData), &
			this % lvDiurnal(iNumData), &
			this % lvOriginal(iNumData), &
			this % lvOriginalWind(iNumData), &
			this % lvOriginalTemp(iNumData), &
			this % lvOriginalRelH(iNumData), &
			stat = iErrCode &
		)
		if(iErrCode /= 0) then
			close(10)
			iRetCode = 12
			return
		end if
		call logInfo("Data read: Time stamps analysis completed successfully")
		
		! Fill workspace preliminarily with "missing values"; as data set
		! will be filled by indexing, this choice guarantees all missing
		! rows to become immediately evident (all data will be NaN then).
		this % rvVel          = NaN
		this % rvDir          = NaN
		this % rvTemp         = NaN
		this % rvTdew         = NaN
		this % rvUrel         = NaN
		this % rvRg           = NaN
		this % rvRain         = NaN
		this % rvRn           = NaN
		this % rvUstar        = NaN
		this % rvTstar        = NaN
		this % rvCover        = NaN
		this % rvH0           = NaN
		this % rvHLM          = NaN
		this % rvL            = NaN
		this % rvLalt1        = NaN
		this % rvLalt2        = NaN
		this % rvG0           = NaN
		this % rvApparentLAI  = NaN
		this % rvHe           = NaN
		this % rvZi           = NaN
		this % rvWs           = NaN
		this % rvSinPsi       = NaN
		this % rvPsi          = NaN
		this % rvPrecWater    = NaN
		this % rvRa           = NaN
		this % rvRg2          = NaN
		this % rvRg3          = NaN
		this % rvRg4          = NaN
		this % rvKt           = NaN
		this % rvKtErr        = NaN
		this % rvFcd          = NaN
		this % rvRn2          = NaN
		this % rvEsat         = NaN
		this % rvEa           = NaN
		this % rvETs          = NaN
		this % rvETt          = NaN
		this % ivIstab        = 0
		this % ivLstab        = 0
		this % lvOriginal     = .true.
		this % lvOriginalWind = .true.
		this % lvOriginalTemp = .true.
		this % lvOriginalRelH = .true.
		call logInfo("Data read: Data vectors initialization done")
		
		! Reposition to the beginning of data section in file
		rewind(10)
		do i = 1, 4
			read(10, "(a)") sBuffer
		end do
		
		! Second pass: actual data read
		do
		
			! Get one line, and extract time stamp from it
			read(10, "(a)", iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			select case(this % iDateFormat)
			case(0)	! European form
			
				! Locate second comma (two fields used, one for date and one for hour),
				! and get date-time string up to there
				iPos = INDEX(sBuffer, ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 1
					return
				end if
				iPos = iPos + INDEX(sBuffer(iPos+1:), ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 2
					return
				end if
				sDateTime = sBuffer(1:iPos-1)
				
				! Read date and time information
				call sanitizeString(sDateTime)
				read(sDateTime, *, iostat=iErrCode) iDay, iMonth, iYear, iHour
				if(iErrCode /= 0) then
					close(10)
					iRetCode = 3
					return
				end if
				iMinute = 0
				iSecond = 0
				
			case(1)	! American form
			
				! Locate second comma (two fields used, one for date and one for hour),
				! and get date-time string up to there
				iPos = INDEX(sBuffer, ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 4
					return
				end if
				iPos = iPos + INDEX(sBuffer(iPos+1:), ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 5
					return
				end if
				sDateTime = sBuffer(1:iPos-1)
				
				! Read date and time information
				call sanitizeString(sDateTime)
				read(sDateTime, *, iostat=iErrCode) iMonth, iDay, iYear, iHour
				if(iErrCode /= 0) then
					close(10)
					iRetCode = 6
					return
				end if
				iMinute = 0
				iSecond = 0
				
			case(2)	! ISO form
			
				! Locate first comma (one field used for date and time),
				! and get date-time string up to there
				iPos = INDEX(sBuffer, ",")
				if(iPos <= 0) then
					close(10)
					iRetCode = 7
					return
				end if
				sDateTime = sBuffer(1:iPos-1)
				
				! Read date and time information
				call sanitizeString(sDateTime)
				read(sDateTime, *, iostat=iErrCode) iYear, iMonth, iDay, iHour, iMinute, iSecond
				if(iErrCode /= 0) then
					close(10)
					iRetCode = 8
					return
				end if
				
			end select
			
			! Adjust year if century was not specified (assume century 2000 in case)
			if(iYear < 100) iYear = iYear + 2000	! A year like "123" is taken face value
			
			! Convert date and time to epoch time stamp:
			! this allows for non-full-year intervals
			call PackTime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			
			! Convert date-time to anticipated convention, if posticipated
			if(this % iDateMeaning == 1) then
				iCurTime = iCurTime - this % iDeltaTime
				call UnpackTime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			end if
			
			! Read data, and in case of success compute its index
			read(sBuffer(iPos+1:), *, iostat=iErrCode) &
				rVel, rDir, rTemp, rRelH, rRain, rCloudCover, rRg, rRn, rUstar, rH0, rzL
			if(iErrCode /= 0) cycle
			iIndex = (iCurTime - this % iMinDateStamp) / this % iDeltaTime + 1
			if(iIndex < 1 .or. iIndex > iNumData) cycle	! Should never happen given test made on delta time
			
			! Exclude data whose time stamp is not an integer multiple of time delta
			if(MOD(iCurTime, this % iDeltaTime) /= 0) cycle
			
			! Save data (invalidating them in case they are off-range)
			this % rvVel(iIndex)	= validateValue(rVel,           0.01,   60.0)
			this % rvDir(iIndex)	= validateValue(rDir,           0.00,  360.0)
			this % rvTemp(iIndex)	= validateValue(rTemp,        -40.00,   60.0)
			this % rvUrel(iIndex)	= validateValue(rRelH,          0.00,  100.0)
			this % rvCover(iIndex)	= validateValue(rCloudCover,    0.00,    1.0)
			this % rvRg(iIndex)		= validateValue(rRg,            0.00, 1600.0)
			this % rvRain(iIndex)	= validateValue(rRain,          0.00,  150.0)
			this % rvRn(iIndex)		= validateValue(rRn,         -900.00, 1400.0)
			this % rvUstar(iIndex)	= validateValue(rUstar,         0.02,    6.0)
			this % rvH0(iIndex)		= validateValue(rH0,         -900.00, 1400.0)
			this % rvHLM(iIndex)	= validateValue(rzL,         -900.00,  900.0)
			
			! Cross-invalidation of wind speed and direction
			if(ISNAN(this % rvVel(iIndex)) .or. ISNAN(this % rvDir(iIndex))) then
				this % rvVel(iIndex) = NaN
				this % rvDir(iIndex) = NaN
			end if
			
			! Reciprocal (in)validation of radiation-related quantities
			if(this % rvRn(iIndex) > this % rvRg(iIndex)) then
				this % rvRn(iIndex) = NaN
			end if
			if(this % rvH0(iIndex) > this % rvRg(iIndex)) then
				this % rvH0(iIndex) = NaN
			end if
        
		end do
		call logInfo("Data read: Surface data read successfully")
		
		! Assign time stamps to data and determine whether hours are diurnal or nocturnal
		do i = 1, iNumData
			this % ivTimeStamp(i) = this % iMinDateStamp + (i-1) * this % iDeltaTime
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			call PackTime(iBaseTime, iYear, 1, 1, 0, 0, 0)
			iDayInYear = (this % ivTimeStamp(i) - iBaseTime) / 86400 + 1
			this % lvDiurnal(i) = IsDiurnal(iDayInYear, iHour, this % rLat, this % rLon, this % iTimeZone)
		end do
		call logInfo("Data read: Diurnal/nocturnal discrimination done")
		
		! Reserve space for profiles at standard heights
		allocate( &
			this % ivNumHeights(iNumData), &
			this % ivSodarNumHeights(iNumData), &
			this % rmSodarHeight(iNumData,60), &
			this % rmSodarVel(iNumData,60), &
			this % rmSodarDir(iNumData,60), &
			this % rmSodarTemp(iNumData,60), &
			this % rvZ(60), &
			this % rmU(iNumData,60), &
			this % rmV(iNumData,60), &
			this % rmTemp(iNumData,60), &
			this % rmOutU(iNumData,60), &
			this % rmOutV(iNumData,60), &
			this % rmOutTemp(iNumData,60), &
			stat = iErrCode &
		)
		if(iErrCode /= 0) then
			iRetCode = 9
			return
		end if
		call logInfo("Data read: Profile workspace reserved successfully")
		
		! Read SODAR profiles, if any (if not, no real problem: profiles
		! will be generated synthetically)
		this % ivNumHeights      = 0
		this % ivSodarNumHeights = 0
		this % rmSodarHeight     = NaN
		this % rmSodarVel        = NaN
		this % rmSodarDir        = NaN
		this % rmSodarTemp       = NaN
        print *, "st_me:: info: SODAR data file is: ", trim(tFiles % sSodarFile)
		if(tFiles % sSodarFile /= " ") then
		
			! Get all SODAR data
			iErrCode = ReadSodarData(14, tFiles % sSODARFile, tSodarData)
            print *, "st_me:: info: SODAR file read completion code is: ", iErrCode
			if(iErrCode == 0) then
				do i = 1, iNumData
				
					if(iSodarDateStart == 0) then
						iErrCode = GetSodarDataClosestInTime( &
							tSodarData, &
							this % ivTimeStamp(i), &
							this % ivSodarNumHeights(i), &
							rvHeight, rvVel, rvDir, rvTemp &
						)
					elseif(iSodarDateStart == 1) then
						iErrCode = GetSodarDataClosestInTime( &
							tSodarData, &
							this % ivTimeStamp(i) + this % iDeltaTime, &
							this % ivSodarNumHeights(i), &
							rvHeight, rvVel, rvDir, rvTemp &
						)
					else
						iErrCode = 10
					end if
					if(iErrCode == 0) then
					
						! Get SODAR profile data
						this % rmSodarHeight(i,1:this % ivSodarNumHeights(i)) = rvHeight(1:this % ivSodarNumHeights(i))
						this % rmSodarVel(i,1:this % ivSodarNumHeights(i))    = rvVel(1:this % ivSodarNumHeights(i))
						this % rmSodarDir(i,1:this % ivSodarNumHeights(i))    = rvDir(1:this % ivSodarNumHeights(i))
						this % rmSodarTemp(i,1:this % ivSodarNumHeights(i))   = rvTemp(1:this % ivSodarNumHeights(i))
						
						! Convert invalid data to NaN for consistency with the remaining of ST-Me
						where(this % rmSodarVel(i,:) < -9000.0)
							this % rmSodarVel(i,:) = NaN
						end where
						where(this % rmSodarDir(i,:) < -9000.0)
							this % rmSodarDir(i,:) = NaN
						end where
						where(this % rmSodarTemp(i,:) < -9000.0)
							this % rmSodarTemp(i,:) = NaN
						end where
						
					else
					
						! Notify no SODAR data is available on this time
						this % ivSodarNumHeights(i) = 0
						call logInfo("Data read: No SODAR data available on read for this time:")
						call logInfo("Data read:    profiles, if needed, will be modeled")
						
					end if
				end do
			else
			
				! Notify no SODAR data is available
				this % ivSodarNumHeights = 0
				call logInfo("Data read: SODAR data file requested, but not found:")
				call logInfo("Data read:    profiles, if needed, will be modeled")
				
			end if
			
		else
		
			call logInfo("Data read: SODAR data file not requested:")
			call logInfo("Data read:    profiles, if needed, will be modeled")
		
		end if
		
	end function GetDataFromInput
	

	function ReportDataSet(this, tFiles) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		type(FileNamesType), intent(in)		:: tFiles
		integer								:: iRetCode
		
		! Locals
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write a synthetic summary of data read
		write(11, "('Data set read from file ', a)") trim(tFiles % sInputFile)
		write(11, "('===================================')")
		write(11, "(' ')")
		write(11, "('Station location: Lat=',f7.2,'  Lon=',f7.2,'  T.Zone=',i2)") this % rLat, this % rLon, this % iTimeZone
		write(11, "('Station altitude above mean sea level: ',f7.2)") this % rHeight
		write(11, "('Aerodynamic roughness length: ',f7.5,'  Displacement height: ',f5.3)") this % rZ0, this % rD
		write(11, "('Albedo: ',f4.2)") this % rAlbedo
		write(11, "('Anemometer height: ',f4.1)") this % rZr
		select case(this % iDateFormat)
		case(0)
			write(11, "('Date format: American')")
		case(1)
			write(11, "('Date format: European')")
		case(2)
			write(11, "('Date format: ISO')")
		end select
		write(11, "(' ')")
		call UnpackTime(this % iMinDateStamp, iYear, iMonth, iDay, iHour, iMinute, iSecond)
		write(11, "('Initial date-time (included): ',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") &
			iYear, iMonth, iDay, iHour, iMinute, iSecond
		call UnpackTime(this % iMaxDateStamp, iYear, iMonth, iDay, iHour, iMinute, iSecond)
		write(11, "('Final date-time (excluded):   ',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") &
			iYear, iMonth, iDay, iHour, iMinute, iSecond
		write(11, "(' ')")
		write(11, "('Total days in data set: ',i6)") this % iNumDays
		write(11, "(' ')")
		write(11, "('Averaging period:       ',i6)") this % iDeltaTime
		write(11, "(' ')")
		if(this % lIsMonotonic) then
			write(11, "('Time series is strictly increasing')")
		else
			write(11, "('Time series is not strictly increasing; this fact has been')")
			write(11, "('masked when reading data, yet it is advisable to check the data set')")
		end if
		write(11, "(' ')")
		if(this % lIsRegular) then
			write(11, "('Time series is regular')")
		else
			write(11, "('Time series is not regular; this fact has been')")
			write(11, "('masked when reading data, yet it is advisable to check the data set')")
		end if
		
		write(11, "(1x)")
		write(11, "(1x)")
		write(11, "(1x,'Basic statistics of data as read')")
		write(11, "(1x,'================================')")
		write(11, "(1x)")
		call DumpBasicStat(this)
    
	end function ReportDataSet
	
	
	function ReportDataSetYear(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		integer	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write a synthetic summary of data read
		write(11, "(1x)")
		write(11, "(1x)")
		write(11, "(1x,'Basic statistics of data as read, yearly')")
		write(11, "(1x,'========================================')")
		write(11, "(1x)")
		iErrCode = DumpBasicStatByYear(this)
		if(iErrCode /= 0) then
			iRetCode = 1
		end if
    
	end function ReportDataSetYear
	
	
	function ReportTwoDataSet(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		! -none-
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write a synthetic summary of data read
		write(11, "(1x)")
		write(11, "(1x)")
		write(11, "(1x,'Basic statistics of data after QC and gap filling')")
		write(11, "(1x,'=================================================')")
		write(11, "(1x)")
		call DumpBasicStat(this)
    
	end function ReportTwoDataSet
	
	
	function ReportThreeDataSet(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		! -none-
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write a synthetic summary of data read
		write(11, "(1x)")
		write(11, "(1x)")
		write(11, "(1x,'Basic statistics of data after physical processing')")
		write(11, "(1x,'==================================================')")
		write(11, "(1x)")
		call DumpBasicStat(this)
    
	end function ReportThreeDataSet
	
	
	function ReportH0Stability(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		integer									:: iErrCode
		integer, dimension(6,6)					:: imStabilityComparison
		integer, dimension(:,:), allocatable	:: imNumStab
		integer, dimension(:), allocatable		:: ivYear
		integer, dimension(:), allocatable		:: ivMonth
		integer									:: i
		integer									:: n
		integer									:: iStabRad
		integer									:: iStabTurb
		integer									:: iFirstIndex
		integer									:: iFirstYear
		integer									:: iFirstMonth
		integer									:: iLastIndex
		integer									:: iLastYear
		integer									:: iLastMonth
		integer									:: iNumTimes
		integer									:: iDay, iHour, iMinute, iSecond
		integer									:: iIndex
		integer									:: iCurYear
		integer									:: iCurMonth
		integer									:: iSumNum
		
		! Steering constants (change as appropriate)
		real, parameter	:: MIN_NEUTRAL_H0 = -1.5
		real, parameter	:: MAX_NEUTRAL_H0 =  3.0
		real, parameter	:: MIN_NEUTRAL_RN = -3.0
		real, parameter	:: MAX_NEUTRAL_RN =  5.0
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Compute monthly fraction table for radiometric stability categories
		n = SIZE(this % ivTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		call UnpackTime(this % ivTimeStamp(1), iFirstYear, iFirstMonth, iDay, iHour, iMinute, iSecond)
		call UnpackTime(this % ivTimeStamp(n), iLastYear,  iLastMonth,  iDay, iHour, iMinute, iSecond)
		iFirstIndex = iFirstYear*12 + iFirstMonth - 1
		iLastIndex  = iLastYear*12 + iLastMonth - 1
		iNumTimes   = iLastIndex - iFirstIndex + 1
		
		! Reserve workspace
		allocate(imNumStab(iNumTimes,3), ivYear(iNumTimes), ivMonth(iNumTimes), stat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		ivYear    = 0
		ivMonth   = 0
		
		! Fill year and month with relevant values by iterating the index
		i = 0
		do iIndex = iFirstIndex, iLastIndex
			i = i + 1
			iCurMonth = MOD(iIndex, 12)
			iCurYear  = (iIndex - iCurMonth) / 12
			ivYear(i) = iCurYear
			ivMonth(i) = iCurMonth
		end do
		
		! Iterate over H0-derived stability and time stamps; on each step,
		! increment the appropriate count
		imNumStab = 0
		do i = 1, n
			call UnpackTime(this % ivTimeStamp(i), iCurYear, iCurMonth, iDay, iHour, iMinute, iSecond)
			iIndex = iCurYear*12 + iCurMonth - 1 - iFirstIndex + 1
			if(this % rvH0(i) < MIN_NEUTRAL_H0) then
				imNumStab(iIndex,1) = imNumStab(iIndex,1) + 1
			elseif(this % rvH0(i) > MAX_NEUTRAL_H0) then
				imNumStab(iIndex,3) = imNumStab(iIndex,3) + 1
			else
				imNumStab(iIndex,2) = imNumStab(iIndex,2) + 1
			end if
		end do
		
		! Print stability percent table
		write(11, "(1x)")
		write(11, "(1x,'H0-based stability monthly fractions')")
		write(11, "(1x,'====================================')")
		write(11, "(1x)")
		write(11, "(1x,'Year, Month, Stab, Ntrl, Cnvc')")
		write(11, "(1x,'-----------------------------')")
		do iIndex = 1, iNumTimes
			iSumNum = SUM(imNumStab(iIndex,1:3))
			if(iSumNum > 0) then
				write(11, "(1x,i4, ',', i6, 3(',',f5.1))") &
					ivYear(iIndex), ivMonth(iIndex)+1, ( &
						100.0 * FLOAT(imNumStab(iIndex,i)) / iSumNum, &
						i = 1, 3 &
					)
			else
				write(11, "(1x,i4, ',', i6, 3(',',f5.1))") &
					ivYear(iIndex), ivMonth(iIndex)+1, ( &
						0.0, &
						i = 1, 3 &
					)
			end if
		end do
		write(11, "(1x)")
		
		! Iterate over Rn-derived stability and time stamps; on each step,
		! increment the appropriate count
		imNumStab = 0
		do i = 1, n
			call UnpackTime(this % ivTimeStamp(i), iCurYear, iCurMonth, iDay, iHour, iMinute, iSecond)
			iIndex = iCurYear*12 + iCurMonth - 1 - iFirstIndex + 1
			if(this % rvRn(i) < MIN_NEUTRAL_RN) then
				imNumStab(iIndex,1) = imNumStab(iIndex,1) + 1
			elseif(this % rvRn(i) > MAX_NEUTRAL_RN) then
				imNumStab(iIndex,3) = imNumStab(iIndex,3) + 1
			else
				imNumStab(iIndex,2) = imNumStab(iIndex,2) + 1
			end if
		end do
		
		! Print stability percent table
		write(11, "(1x)")
		write(11, "(1x,'Rn-based stability monthly fractions')")
		write(11, "(1x,'====================================')")
		write(11, "(1x)")
		write(11, "(1x,'Year, Month, Stab, Ntrl, Cnvc')")
		write(11, "(1x,'-----------------------------')")
		do iIndex = 1, iNumTimes
			iSumNum = SUM(imNumStab(iIndex,1:3))
			if(iSumNum > 0) then
				write(11, "(1x,i4, ',', i6, 3(',',f5.1))") &
					ivYear(iIndex), ivMonth(iIndex)+1, ( &
						100.0 * FLOAT(imNumStab(iIndex,i)) / iSumNum, &
						i = 1, 3 &
					)
			else
				write(11, "(1x,i4, ',', i6, 3(',',f5.1))") &
					ivYear(iIndex), ivMonth(iIndex)+1, ( &
						0.0, &
						i = 1, 3 &
					)
			end if
		end do
		write(11, "(1x)")
		
		! Leave
		deallocate(imNumStab, ivYear, ivMonth)
		
	end function ReportH0Stability
	
	
	function ReportStability(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		integer									:: iErrCode
		integer, dimension(6,6)					:: imStabilityComparison
		integer, dimension(:,:), allocatable	:: imNumStab
		integer, dimension(:), allocatable		:: ivYear
		integer, dimension(:), allocatable		:: ivMonth
		integer									:: i
		integer									:: n
		integer									:: iStabRad
		integer									:: iStabTurb
		integer									:: iFirstIndex
		integer									:: iFirstYear
		integer									:: iFirstMonth
		integer									:: iLastIndex
		integer									:: iLastYear
		integer									:: iLastMonth
		integer									:: iNumTimes
		integer									:: iDay, iHour, iMinute, iSecond
		integer									:: iIndex
		integer									:: iCurYear
		integer									:: iCurMonth
		integer									:: iSumNum
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Compute monthly fraction table for radiometric stability categories
		n = SIZE(this % ivTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		call UnpackTime(this % ivTimeStamp(1), iFirstYear, iFirstMonth, iDay, iHour, iMinute, iSecond)
		call UnpackTime(this % ivTimeStamp(n), iLastYear,  iLastMonth,  iDay, iHour, iMinute, iSecond)
		iFirstIndex = iFirstYear*12 + iFirstMonth - 1
		iLastIndex  = iLastYear*12 + iLastMonth - 1
		iNumTimes   = iLastIndex - iFirstIndex + 1
		
		! Reserve workspace
		allocate(imNumStab(iNumTimes,6), ivYear(iNumTimes), ivMonth(iNumTimes), stat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		ivYear    = 0
		ivMonth   = 0
		
		! Fill year and month with relevant values by iterating the index
		i = 0
		do iIndex = iFirstIndex, iLastIndex
			i = i + 1
			iCurMonth = MOD(iIndex, 12)
			iCurYear  = (iIndex - iCurMonth) / 12
			ivYear(i) = iCurYear
			ivMonth(i) = iCurMonth
		end do
		
		! Iterate over stability categories and time stamps; on each step,
		! increment the appropriate count
		imNumStab = 0
		do i = 1, n
			if(this % ivIstab(i) >= 1 .and. this % ivIstab(i) <= 6) then
				call UnpackTime(this % ivTimeStamp(i), iCurYear, iCurMonth, iDay, iHour, iMinute, iSecond)
				iIndex = iCurYear*12 + iCurMonth - 1 - iFirstIndex + 1
				imNumStab(iIndex,this % ivIstab(i)) = imNumStab(iIndex,this % ivIstab(i)) + 1
			end if
		end do
		
		! Print radiometric stability percent table
		write(11, "(1x)")
		write(11, "(1x,'Radiometric stability monthly fractions')")
		write(11, "(1x,'=======================================')")
		write(11, "(1x)")
		write(11, "(1x,'Year, Month,   A%,   B%,   C%,   D%,   E%,   F%')")
		write(11, "(1x,'-----------------------------------------------')")
		do iIndex = 1, iNumTimes
			iSumNum = SUM(imNumStab(iIndex,1:6))
			if(iSumNum > 0) then
				write(11, "(1x,i4, ',', i6, 6(',',f5.1))") &
					ivYear(iIndex), ivMonth(iIndex)+1, ( &
						100.0 * FLOAT(imNumStab(iIndex,i)) / iSumNum, &
						i = 1, 6 &
					)
			else
				write(11, "(1x,i4, ',', i6, 6(',',f5.1))") &
					ivYear(iIndex), ivMonth(iIndex)+1, ( &
						0.0, &
						i = 1, 6 &
					)
			end if
		end do
		write(11, "(1x)")
		
		! Iterate over stability categories and time stamps; on each step,
		! increment the appropriate count
		imNumStab = 0
		do i = 1, n
			if(this % ivLstab(i) >= 1 .and. this % ivLstab(i) <= 6) then
				call UnpackTime(this % ivTimeStamp(i), iCurYear, iCurMonth, iDay, iHour, iMinute, iSecond)
				iIndex = iCurYear*12 + iCurMonth - 1 - iFirstIndex + 1
				imNumStab(iIndex,this % ivLstab(i)) = imNumStab(iIndex,this % ivLstab(i)) + 1
			end if
		end do
		
		! Print turbulent stability percent table
		write(11, "(1x)")
		write(11, "(1x,'Turbulent stability monthly fractions')")
		write(11, "(1x,'=====================================')")
		write(11, "(1x)")
		write(11, "(1x,'Year, Month,   A%,   B%,   C%,   D%,   E%,   F%')")
		write(11, "(1x,'-----------------------------------------------')")
		do iIndex = 1, iNumTimes
			iSumNum = SUM(imNumStab(iIndex,1:6))
			if(iSumNum > 0) then
				write(11, "(1x,i4, ',', i6, 6(',',f5.1))") &
					ivYear(iIndex), ivMonth(iIndex)+1, ( &
						100.0 * FLOAT(imNumStab(iIndex,i)) / SUM(imNumStab(iIndex,1:6)), &
						i = 1, 6 &
					)
			else
				write(11, "(1x,i4, ',', i6, 6(',',f5.1))") &
					ivYear(iIndex), ivMonth(iIndex)+1, ( &
						0.0, &
						i = 1, 6 &
					)
			end if
		end do
		write(11, "(1x)")
		
		! Count and print stability comparison between radiometric (ISTAB7)
		! and turbulence (LSTAB) definitions of stability categories
		imStabilityComparison = 0
		do i = 1, SIZE(this % ivTimeStamp)
			iStabRad  = this % ivIstab(i)
			iStabTurb = this % ivLstab(i)
			if(iStabRad >= 1 .and. iStabRad <= 6 .and. iStabTurb >= 1 .and. iStabTurb <=6) then
				imStabilityComparison(iStabRad, iStabTurb) = imStabilityComparison(iStabRad, iStabTurb) + 1
			end if
		end do
		write(11, "(1x)")
		write(11, "(1x,'Comparison between radiometric and turbulent stability categories')")
		write(11, "(1x,'=================================================================')")
		write(11, "(1x)")
		write(11, "(1x,'         Turbulent')")
		write(11, "(1x,'         A      B      C      D      E      F')")
		write(11, "(1x,'R    A ',6i7)") (imStabilityComparison(1, iStabTurb),iStabTurb=1,6)
		write(11, "(1x,'a')")
		write(11, "(1x,'d    B ',6i7)") (imStabilityComparison(2, iStabTurb),iStabTurb=1,6)
		write(11, "(1x,'i')")
		write(11, "(1x,'o    C ',6i7)") (imStabilityComparison(3, iStabTurb),iStabTurb=1,6)
		write(11, "(1x,'m')")
		write(11, "(1x,'e    D ',6i7)") (imStabilityComparison(4, iStabTurb),iStabTurb=1,6)
		write(11, "(1x,'t')")
		write(11, "(1x,'r    E ',6i7)") (imStabilityComparison(5, iStabTurb),iStabTurb=1,6)
		write(11, "(1x,'i')")
		write(11, "(1x,'c    F ',6i7)") (imStabilityComparison(6, iStabTurb),iStabTurb=1,6)
		write(11, "(1x)")
		
		! Leave
		deallocate(imNumStab, ivYear, ivMonth)
		
	end function ReportStability
	
	
	function ReportTypicalDays(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		real, dimension(:), allocatable		:: rvTemp
		real, dimension(:), allocatable		:: rvRelH
		real, dimension(:), allocatable		:: rvVel
		real, dimension(:), allocatable		:: rvDir
		real, dimension(:), allocatable		:: rvUstar
		real, dimension(:), allocatable		:: rvRa
		real, dimension(:), allocatable		:: rvRg
		real, dimension(:), allocatable		:: rvRn
		real, dimension(:), allocatable		:: rvG0
		real, dimension(:), allocatable		:: rvH0
		real, dimension(:), allocatable		:: rvHE
		real, dimension(:), allocatable		:: rvZoverL
		real, dimension(:), allocatable		:: rvZi
		real, dimension(:), allocatable		:: rvETs
		real, dimension(:), allocatable		:: rvETt
		integer								:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer								:: i
		integer								:: iStampInDay
		integer								:: iTimeIndex
		integer								:: iCurTime
		integer								:: iErrCode
		integer								:: iBaseMonth
		integer								:: iEndMonth
		integer								:: iNumMonths
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Identify months in time series span
		call UnpackTime(this % ivTimeStamp(1), iYear, iMonth, iDay, iHour, iMinute, iSecond)
		iBaseMonth = 12*iYear + iMonth - 1
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), &
			iYear, iMonth, iDay, iHour, iMinute, iSecond &
		)
		iEndMonth = 12*iYear + iMonth - 1
		iNumMonths = iEndMonth - iBaseMonth + 1
		
		! Write a synthetic summary of data read
		write(11, "(1x)")
		write(11, "(1x)")
		write(11, "(1x,'Monthly typical days of selected quantities')")
		write(11, "(1x,'===========================================')")
		write(11, "(1x)")
		
		! Loop over months, compute typical days and write them to file
		do i = 1, iNumMonths
		
			! Determine approximate center-month date
			iYear = (iBaseMonth + i - 1) / 12
			iMonth = MOD(iBaseMonth + i - 1, 12) + 1
			iDay = 15
			iHour = 0
			iMinute = 0
			iSecond = 0
			call PackTime(iStampInDay, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			
			! Compute all interesting typical days
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvTemp, this % iDeltaTime, 15, rvTemp)
			if(iErrCode /= 0) then
				iRetCode = 1
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvUrel, this % iDeltaTime, 15, rvRelH)
			if(iErrCode /= 0) then
				iRetCode = 2
				return
			end if
			iErrCode = TypicalDayWind( &
				iStampInDay, this % ivTimeStamp, this % rvVel, this % rvDir, this % iDeltaTime, 15, &
				rvVel, rvDir &
			)
			if(iErrCode /= 0) then
				iRetCode = 3
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvUstar, this % iDeltaTime, 15, rvUstar)
			if(iErrCode /= 0) then
				iRetCode = 4
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvRa, this % iDeltaTime, 15, rvRa)
			if(iErrCode /= 0) then
				iRetCode = 5
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvRg, this % iDeltaTime, 15, rvRg)
			if(iErrCode /= 0) then
				iRetCode = 6
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvRn, this % iDeltaTime, 15, rvRn)
			if(iErrCode /= 0) then
				iRetCode = 7
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvG0, this % iDeltaTime, 15, rvG0)
			if(iErrCode /= 0) then
				iRetCode = 8
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvH0, this % iDeltaTime, 15, rvH0)
			if(iErrCode /= 0) then
				iRetCode = 9
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvHe, this % iDeltaTime, 15, rvHE)
			if(iErrCode /= 0) then
				iRetCode = 10
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvHLM, this % iDeltaTime, 15, rvZoverL)
			if(iErrCode /= 0) then
				iRetCode = 11
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvZi, this % iDeltaTime, 15, rvZi)
			if(iErrCode /= 0) then
				iRetCode = 12
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvETs, this % iDeltaTime, 15, rvETs)
			if(iErrCode /= 0) then
				iRetCode = 13
				return
			end if
			iErrCode = TypicalDay(iStampInDay, this % ivTimeStamp, this % rvETt, this % iDeltaTime, 15, rvETt)
			if(iErrCode /= 0) then
				iRetCode = 14
				return
			end if
			
			! Write typical days
			write(11, "('Year: ',i4,'   Month: ',i2)") iYear, iMonth
			write(11, "(' ')")
			write(11, "('Time,Temp,RelH,Vel,Dir,U.star,Ra,Rg,Rn,G0,H0,HE,z.over.L,Zi,ETs,ETt')")
			do iTimeIndex = 1, SIZE(rvZi)
			
				! Build the hour stamp. Here I use a little trick, based on the incidental fact
				! that "small" time stamps expressed in seconds signify a small Epoch value.
				! If, as in our case, all time stamps are smaller than one day, then by
				! the way Epoch time is defined all corresponding Epoch values will belong
				! somewhen in day 1970-01-01. But, I'm really interest to the time part,
				! not the date, and this will be the one I'll use when printing to file.
				iCurTime = (iTimeIndex - 1) * this % iDeltaTime
				call UnpackTime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
				
				! Write the row of typical days corresponding to current time
				write(11, "(i2.2,2(':',i2.2),15(',',f9.3))") &
					iHour, iMinute, iSecond, &
					rvTemp(iTimeIndex), rvRelH(iTimeIndex), &
					rvVel(iTimeIndex), rvDir(iTimeIndex), &
					rvUstar(iTimeIndex), &
					rvRa(iTimeIndex), rvRg(iTimeIndex), rvRn(iTimeIndex), rvG0(iTimeIndex), &
					rvH0(iTimeIndex), rvHE(iTimeIndex), rvZoverL(iTimeIndex), &
					rvZi(iTimeIndex), &
					rvETs(iTimeIndex), rvETt(iTimeIndex)
					
			end do
			write(11, "(' ')")
			write(11, "(' ')")
			
		end do
    
	end function ReportTypicalDays
	
	
	function ReportDirectionalMean(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		real, dimension(:), allocatable		:: rvVel
		real, dimension(:), allocatable		:: rvUstar
		real, dimension(:), allocatable		:: rvUstarOverVel
		real, dimension(:), allocatable		:: rvTemp
		real, dimension(:), allocatable		:: rvRelH
		integer								:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer								:: i
		integer								:: iStampInDay
		integer								:: iIndex
		integer								:: iCurTime
		integer								:: iErrCode
		integer								:: iBaseMonth
		integer								:: iEndMonth
		integer								:: iNumMonths
		real								:: rCenterAngle
		
		! Steering constants - change as appropriate
		integer, parameter	:: NUM_CLASSES	= 32
		real, parameter		:: CLASS_WIDTH	= 1.5	! Multiplier of 360/NUM_CLASSES used to obtain real class width; 0.5 for minimal-overlapping classes
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Identify months in time series span
		call UnpackTime(this % ivTimeStamp(1), iYear, iMonth, iDay, iHour, iMinute, iSecond)
		iBaseMonth = 12*iYear + iMonth - 1
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), &
			iYear, iMonth, iDay, iHour, iMinute, iSecond &
		)
		iEndMonth = 12*iYear + iMonth - 1
		iNumMonths = iEndMonth - iBaseMonth + 1
		
		! Write a synthetic summary of data read
		write(11, "(1x)")
		write(11, "(1x)")
		write(11, "(1x,'Monthly directional mean of selected quantities')")
		write(11, "(1x,'===============================================')")
		write(11, "(1x)")
		
		! Loop over months, compute typical days and write them to file
		do i = 1, iNumMonths
		
			! Determine approximate center-month date
			iYear = (iBaseMonth + i - 1) / 12
			iMonth = MOD(iBaseMonth + i - 1, 12) + 1
			iDay = 15
			iHour = 0
			iMinute = 0
			iSecond = 0
			call PackTime(iStampInDay, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			
			! Compute all interesting typical days
			iErrCode = DirectionalMean( &
				iStampInDay, &
				this % ivTimeStamp, this % rvDir, this % rvVel, &
				this % iDeltaTime, 15, NUM_CLASSES, CLASS_WIDTH*360.0/NUM_CLASSES, &
				rvVel &
			)
			if(iErrCode /= 0) then
				iRetCode = 1
				return
			end if
			iErrCode = DirectionalMean( &
				iStampInDay, &
				this % ivTimeStamp, this % rvDir, this % rvUstar, &
				this % iDeltaTime, 15, NUM_CLASSES, CLASS_WIDTH*360.0/NUM_CLASSES, &
				rvUstar &
			)
			if(iErrCode /= 0) then
				iRetCode = 2
				return
			end if
			iErrCode = DirectionalMean( &
				iStampInDay, &
				this % ivTimeStamp, this % rvDir, this % rvUstar / this % rvVel, &
				this % iDeltaTime, 15, NUM_CLASSES, CLASS_WIDTH*360.0/NUM_CLASSES, &
				rvUstarOverVel &
			)
			if(iErrCode /= 0) then
				iRetCode = 3
				return
			end if
			iErrCode = DirectionalMean( &
				iStampInDay, &
				this % ivTimeStamp, this % rvDir, this % rvTemp, &
				this % iDeltaTime, 15, NUM_CLASSES, CLASS_WIDTH*360.0/NUM_CLASSES, &
				rvTemp &
			)
			if(iErrCode /= 0) then
				iRetCode = 4
				return
			end if
			iErrCode = DirectionalMean( &
				iStampInDay, &
				this % ivTimeStamp, this % rvDir, this % rvUrel, &
				this % iDeltaTime, 15, NUM_CLASSES, CLASS_WIDTH*360.0/NUM_CLASSES, &
				rvRelH &
			)
			if(iErrCode /= 0) then
				iRetCode = 5
				return
			end if
			
			! Write typical days
			write(11, "('Year: ',i4,'   Month: ',i2)") iYear, iMonth
			write(11, "(' ')")
			write(11, "('Dir,Temp,RelH,Vel,U.star,U.star.over.Vel')")
			do iIndex = 1, SIZE(rvUstar)
			
				! Build class center angle
				rCenterAngle = (iIndex-1) * 360.0 / NUM_CLASSES
				
				! Write the row of typical days corresponding to current time
				write(11, "(f6.2,5(',',f11.5))") &
					rCenterAngle, &
					rvTemp(iIndex), rvRelH(iIndex), &
					rvVel(iIndex), &
					rvUstar(iIndex), rvUstarOverVel(iIndex)
					
			end do
			write(11, "(' ')")
			write(11, "(' ')")
			
		end do
    
	end function ReportDirectionalMean
	
	
	function ReportProfiles(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		! -none-
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write a synthetic summary of data read
		write(11, "(1x)")
		write(11, "(1x)")
		write(11, "(1x,'Some data about profiles')")
		write(11, "(1x,'========================')")
		write(11, "(1x)")
    
	end function ReportProfiles
	
	
	function CheckEnoughData(this) result(iRetCode)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		integer	:: iNumValues
		integer	:: iInvalidVel
		integer	:: iInvalidDir
		integer	:: iInvalidTemp
		integer	:: iInvalidUrel
		real	:: rInvalidVel
		real	:: rInvalidDir
		real	:: rInvalidTemp
		real	:: rInvalidUrel
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Count data
		iNumValues    = SIZE(this % ivTimeStamp)
		iInvalidVel   = COUNT(ISNAN(this % rvVel))
		iInvalidDir   = COUNT(ISNAN(this % rvDir))
		iInvalidTemp  = COUNT(ISNAN(this % rvTemp))
		iInvalidUrel  = COUNT(ISNAN(this % rvUrel))
		
		! Convert to data fraction
		rInvalidVel   = FLOAT(iInvalidVel)  / iNumValues
		rInvalidDir   = FLOAT(iInvalidDir)  / iNumValues
		rInvalidTemp  = FLOAT(iInvalidTemp) / iNumValues
		rInvalidUrel  = FLOAT(iInvalidUrel) / iNumValues
		
		! Check all invalid data fractions are sufficiently small
		if(rInvalidVel > 0.5 .or. rInvalidDir > 0.5 .or. rInvalidTemp > 0.5 .or. rInvalidUrel > 0.5) then
			iRetCode = 1
		end if
		
	end function CheckEnoughData
	
	
	subroutine PrintWindRose(this, sFileName)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		character(len=*), intent(in)	:: sFileName
		
		! Locals
		integer	:: i
		
		! Write wind rose data
		open(12, file=sFileName, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if(.not.ISNAN(this % rvVel(i)) .and. .not.ISNAN(this % rvDir(i))) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
    end subroutine PrintWindRose
    
    
	subroutine PrintStbRoses(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer	:: i
		
		! Diurnal
		open(12, file=tFiles % sDiurnalWindRoseFile, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if( &
				.not.ISNAN(this % rvVel(i)) .and. &
				.not.ISNAN(this % rvDir(i)) .and. &
				this % lvDiurnal(i) &
			) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
		! Nocturnal
		open(12, file=tFiles % sNocturnalWindRoseFile, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if( &
				.not.ISNAN(this % rvVel(i)) .and. &
				.not.ISNAN(this % rvDir(i)) .and. &
				.not.this % lvDiurnal(i) &
			) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
		! Radiometric, convective
		open(12, file=tFiles % sConvectiveWindRoseFile1, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if( &
				.not.ISNAN(this % rvVel(i)) .and. &
				.not.ISNAN(this % rvDir(i)) .and. &
				this % ivIstab(i) <= 3 &
			) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
		! Radiometric, neutral
		open(12, file=tFiles % sNeutralWindRoseFile1, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if( &
				.not.ISNAN(this % rvVel(i)) .and. &
				.not.ISNAN(this % rvDir(i)) .and. &
				this % ivIstab(i) == 4 &
			) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
		! Radiometric, stable
		open(12, file=tFiles % sStableWindRoseFile1, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if( &
				.not.ISNAN(this % rvVel(i)) .and. &
				.not.ISNAN(this % rvDir(i)) .and. &
				this % ivIstab(i) >= 5 &
			) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
		! Turbulent, convective
		open(12, file=tFiles % sConvectiveWindRoseFile2, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if( &
				.not.ISNAN(this % rvVel(i)) .and. &
				.not.ISNAN(this % rvDir(i)) .and. &
				this % ivLstab(i) <= 3 &
			) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
		! Turbulent, neutral
		open(12, file=tFiles % sNeutralWindRoseFile2, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if( &
				.not.ISNAN(this % rvVel(i)) .and. &
				.not.ISNAN(this % rvDir(i)) .and. &
				this % ivLstab(i) == 4 &
			) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
		! Turbulent, stable
		open(12, file=tFiles % sStableWindRoseFile2, status='unknown', action='write',decimal='comma')
		write(12,"('Dir;Vel')")
		do i = 1, size(this % ivTimeStamp)
			if( &
				.not.ISNAN(this % rvVel(i)) .and. &
				.not.ISNAN(this % rvDir(i)) .and. &
				this % ivLstab(i) >= 5 &
			) then
				write(12, "(1x,f7.3,';',f7.3)") this % rvDir(i), this % rvVel(i)
			end if
		end do
		close(12)
		
    end subroutine PrintStbRoses
    
    
	subroutine PrepareToPrint(this)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		
		! Locals
		integer	:: i
		
		! Predispose data for printing, by replacing NaNs to an invalid indicator
		do i = 1, SIZE(this % ivTimeStamp)
			if(ISNAN(this % rvRg(i)))     this % rvRg(i)     = -9999.9
			if(ISNAN(this % rvRain(i)))   this % rvRain(i)   = -9999.9
			if(ISNAN(this % rvRn(i)))     this % rvRn(i)     = -9999.9
			if(ISNAN(this % rvUstar(i)))  this % rvUstar(i)  = -9999.9
			if(ISNAN(this % rvH0(i)))     this % rvH0(i)     = -9999.9
			if(ISNAN(this % rvHLM(i)))    this % rvHLM(i)    = -9999.9
			if(ISNAN(this % rvCover(i)))  this % rvCover(i)  = -9999.9
			if(ISNAN(this % rvTstar(i)))  this % rvTstar(i)  = -9999.9
			if(ISNAN(this % rvZi(i)))     this % rvZi(i)     = -9999.9
			if(ISNAN(this % rvWs(i)))     this % rvWs(i)     = -9999.9
			if(ISNAN(this % rvSinPsi(i))) this % rvSinPsi(i) = -9999.9
		end do

	end subroutine PrepareToPrint
    
    
	subroutine WriteDataToFile(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer	:: i
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		character(len=32)	:: sDateTime
		character(len=512)	:: sData
		character(len=256)	:: sHeader1, sHeader2
		
		! Write data to file
		open(13, file=tFiles % sOutputFile, status='unknown', action='write')
		write(sHeader1, "('Time.Stamp,Vel,Dir,Temp,Rel.H,Prec,Rg,Rn,U.star,H0,HE,zL,L,zL1,zL2,Cloud.Cover,')")
		write(sHeader2, "('G0,LAI.A,T.star,Zi,Ws,Sin.Psi,Brunt.Vaisala,Pa,Tdew,Prec.Water," // &
						"Esat,Ea,ETs,ETt,Ra,Rg2,Rg3,Rg4,El.Ang,Cloudiness,Rn2,Kt,Kt.Err,Istab,Lstab," // &
						"Diurnal,Original.Wind,Original.Temp,Original.RelH,Original')")
		write(13,"(a,a)") trim(sHeader1), trim(sHeader2)
		do i = 1, SIZE(this % ivTimeStamp)
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			write(sData,"(38(',',f10.3),2(',',i5),5(',',l1))") &
				this % rvVel(i), this % rvDir(i), &
				this % rvTemp(i), this % rvUrel(i), this % rvRain(i), &
				this % rvRg(i), this % rvRn(i), this % rvUstar(i), &
				this % rvH0(i), this % rvHE(i), this % rvHLM(i), this % rvL(i), &
				this % rvLalt1(i), this % rvLalt2(i), this % rvCover(i), &
				this % rvG0(i), this % rvApparentLAI(i), &
				this % rvTstar(i), this % rvZi(i), this % rvWs(i), &
				this % rvSinPsi(i), this % rvN(i), this % rvP(i), this % rvTdew(i), &
				this % rvPrecWater(i), this % rvEsat(i), this % rvEa(i), &
				this % rvETs(i), this % rvETt(i), &
				this % rvRa(i), this % rvRg2(i), this % rvRg3(i), this % rvRg4(i), &
				this % rvPsi(i), this % rvFcd(i), this % rvRn2(i), &
				this % rvKt(i), this % rvKtErr(i), &
				this % ivIstab(i), this % ivLstab(i), this % lvDiurnal(i), &
				this % lvOriginalWind(i), this % lvOriginalTemp(i), this % lvOriginalRelH(i), &
				this % lvOriginal(i)
			write(13,"(a,a)") trim(sDateTime), trim(sData)
		end do
		close(13)

	end subroutine WriteDataToFile
    
    
	subroutine WriteDailyAvailability(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		type(FileNamesType), intent(in)		:: tFiles
		
		! Locals
		integer	:: i
		integer	:: j
		integer	:: iDayInYear
		integer	:: iDataPerDay
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		real	:: rHour
		real	:: rTempAvailability
		real	:: rRelHAvailability
		real	:: rVelAvailability
		real	:: rDirAvailability
		real	:: rCloudAvailability
		real	:: rPrecAvailability
		real	:: rRgAvailability
		real	:: rRnAvailability
		real	:: rUstarAvailability
		real	:: rH0Availability
		real	:: rLm1Availability
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = ONE_HOUR * 24
		
		! Compute the number of data per day (will be used as a skip term in the next "do" loop)
		iDataPerDay = ONE_DAY / this % iDeltaTime
		if(iDataPerDay <= 0) then
			return
		end if
		
		! Write daily data to file (for diagnostic purposes)
		open(13, file=tFiles % sDailyAvailFile, status='unknown', action='write')
		write(13, "('Date, Vel.Avail, Dir.Avail, Temp.Avail, RelH.Avail, " // &
					"Cloud.Avail, Rain.Avail, Rg.Avail, Rn.Avail, " // &
					"U.star.Avail, H0.Avail, zL.Avail" // &
					"')")
		do i = 1, SIZE(this % ivTimeStamp), iDataPerDay
		
			! Get date and time; date only will be used in this loop
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			
			! Compute PBL_MET sunrise and sunset
			iDayInYear        = J_DAY(iYear, iMonth, iDay)
			
			! Compute data statistics
			! -1- Temperature
			rTempAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvTemp, &
				this % iDeltaTime &
			)
			! -1- Relative humidity
			rRelHAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUrel, &
				this % iDeltaTime &
			)
			! -1- Wind speed
			rVelAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvVel, &
				this % iDeltaTime &
			)
			! -1- Wind direction
			rDirAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvDir, &
				this % iDeltaTime &
			)
			! -1- Cloud cover
			rCloudAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvCover, &
				this % iDeltaTime &
			)
			! -1- Precipitation
			rPrecAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvDir, &
				this % iDeltaTime &
			)
			! -1- Global radiation
			rRgAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRg, &
				this % iDeltaTime &
			)
			! -1- Net radiation
			rRnAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRn, &
				this % iDeltaTime &
			)
			! -1- Friction velocity
			rUstarAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUstar, &
				this % iDeltaTime &
			)
			! -1- Turbulent sensible heat flux
			rH0Availability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvH0, &
				this % iDeltaTime &
			)
			! -1- Stability parameter
			rLm1Availability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvHlm, &
				this % iDeltaTime &
			)
			
			! Write results
			write(13, "(i4.4,2('-',i2.2),11(',',f6.2))") &
				iYear, iMonth, iDay, &
				rVelAvailability, rDirAvailability, &
				rTempAvailability, rRelHAvailability, &
				rCloudAvailability, rPrecAvailability, &
				rRgAvailability, rRnAvailability, &
				rUstarAvailability, rH0Availability, rLm1Availability
			
		end do
		close(13)

	end subroutine WriteDailyAvailability
	
	
	subroutine WriteDailyPostAvailability(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(inout)	:: this
		type(FileNamesType), intent(in)		:: tFiles
		
		! Locals
		integer	:: i
		integer	:: j
		integer	:: iDayInYear
		integer	:: iDataPerDay
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		real	:: rHour
		real	:: rTempAvailability
		real	:: rRelHAvailability
		real	:: rVelAvailability
		real	:: rDirAvailability
		real	:: rWindFilled
		real	:: rTempFilled
		real	:: rRelHFilled
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = ONE_HOUR * 24
		
		! Compute the number of data per day (will be used as a skip term in the next "do" loop)
		iDataPerDay = ONE_DAY / this % iDeltaTime
		if(iDataPerDay <= 0) then
			return
		end if
		
		! Write daily data to file (for diagnostic purposes)
		open(13, file=tFiles % sDailyPostAvailFile, status='unknown', action='write')
		write(13, "('Date, Vel.Avail, Vel.Filled, Dir.Avail, Dir.Filled, " // &
					"Temp.Avail, Temp.Filled, RelH.Avail, RelH.Filled" // &
					"')")
		do i = 1, SIZE(this % ivTimeStamp), iDataPerDay
		
			! Get date and time; date only will be used in this loop
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			
			! Compute PBL_MET sunrise and sunset
			iDayInYear        = J_DAY(iYear, iMonth, iDay)
			
			! Compute data statistics
			! -1- Temperature
			rTempAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvTemp, &
				this % iDeltaTime &
			)
			rTempFilled = CountFilled( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % lvOriginalTemp, &
				this % iDeltaTime &
			)
			! -1- Relative humidity
			rRelHAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUrel, &
				this % iDeltaTime &
			)
			rRelHFilled = CountFilled( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % lvOriginalRelH, &
				this % iDeltaTime &
			)
			! -1- Wind speed and direction
			rVelAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvVel, &
				this % iDeltaTime &
			)
			rDirAvailability = DailyAvailability( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvDir, &
				this % iDeltaTime &
			)
			rWindFilled = CountFilled( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % lvOriginalWind, &
				this % iDeltaTime &
			)
			
			! Write results
			write(13, "(i4.4,2('-',i2.2),4(',',f6.2,',',f6.2))") &
				iYear, iMonth, iDay, &
				rVelAvailability, rWindFilled, rDirAvailability, rWindFilled, &
				rTempAvailability, rTempFilled, rRelHAvailability, rRelHFilled
			
		end do
		close(13)

	end subroutine WriteDailyPostAvailability
	
	
	subroutine WriteDailyReport(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer	:: i
		integer	:: j
		integer	:: iDayInYear
		integer	:: iDataPerDay
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		real	:: rSolarDeclination
		real	:: rSunRise, rSunSet
		real	:: rOldSinSolarElevation
		real	:: rSinSolarElevation
		real	:: rHour
		real	:: rTempAnomaly, rTempAnomalyStdDev, rTempAnomalyMin, rTempAnomalyMax, rAvgTemp, rMinTemp, rMaxTemp
		real	:: rRelHAnomaly, rRelHAnomalyStdDev, rRelhAnomalyMin, rRelHAnomalyMax, rAvgRelH, rMinRelH, rMaxRelH
		real	:: rVelAnomaly, rAvgVel, rMinVel, rMaxVel
		real	:: rRgAnomaly, rAvgRg, rMinRg, rMaxRg, rSumRg
		real	:: rRnAnomaly, rAvgRn, rMinRn, rMaxRn, rSumRn
		real	:: rUstarAnomaly, rAvgUstar, rMinUstar, rMaxUstar
		real	:: rH0Anomaly, rAvgH0, rMinH0, rMaxH0
		real	:: rLm1Anomaly, rAvgLm1, rMinLm1, rMaxLm1
		real	:: rZiAnomaly, rAvgZi, rMinZi, rMaxZi
		real	:: rTotalPrec
		logical	:: lIsFirst = .true.
		integer, dimension(2)	:: ivSignChangeIndex
		integer					:: iNumSignChanges
		real, dimension(2)		:: rvSunRiseSet
		
		! Steering parameters (change as appropriate)
		integer, parameter	:: DAYS_RADIUS = 15
		
		! Constants
		integer, parameter	:: ONE_HOUR = 3600
		integer, parameter	:: ONE_DAY  = ONE_HOUR * 24
		
		! Compute the number of data per day (will be used as a skip term in the next "do" loop)
		iDataPerDay = ONE_DAY / this % iDeltaTime
		if(iDataPerDay <= 0) then
			return
		end if
		
		! Write daily data to file (for diagnostic purposes)
		open(13, file=tFiles % sDailyReportFile, status='unknown', action='write')
		write(13, "('Date, DOY, Declination, Sun.Rise, Sun.Set, " // &
					"Vel.Min, Vel.Avg, Vel.Max, Vel.Anomaly, " // &
					"Temp.Min, Temp.Avg, Temp.Max, Temp.Anomaly, Temp.An.Sd, Temp.An.Min, Temp.An.Max, " // &
					"RelH.Min, RelH.Avg, RelH.Max, RelH.Anomaly, RelH.An.Sd, RelH.An.Min, RelH.An.Max, " // &
					"Rg.Min, Rg.Avg, Rg.Max, Rg.Anomaly, " // &
					"Rn.Min, Rn.Avg, Rn.Max, Rn.Anomaly, " // &
					"U.star.Min, U.star.Avg, U.star.Max, U.star.Anomaly, " // &
					"H0.Min, H0.Avg, H0.Max, H0.Anomaly, " // &
					"zL.Min, zL.Avg, zL.Max, zL.Anomaly, " // &
					"Zi.Min, Zi.Avg, Zi.Max, Zi.Anomaly, " // &
					"Prec.Total" // &
					"')")
		do i = 1, SIZE(this % ivTimeStamp), iDataPerDay
		
			! Get date and time; date only will be used in this loop
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			
			! Compute PBL_MET sunrise and sunset
			iDayInYear        = J_DAY(iYear, iMonth, iDay)
			rSolarDeclination = SolarDeclination(iYear, iMonth, iDay)
			rvSunRiseSet = SunRiseSunSet(iYear, iMonth, iDay, this % rLat, this % rLon, this % iTimeZone)
			
			! Compute data statistics
			! -1- Temperature
			rTempAnomaly = Anomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvTemp, &
				this % iDeltaTime, &
				DAYS_RADIUS, &
				rTempAnomalyStdDev, &
				rTempAnomalyMin, &
				rTempAnomalyMax &
			)
			rAvgTemp = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvTemp, &
				this % iDeltaTime &
			)
			rMinTemp = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvTemp, &
				this % iDeltaTime &
			)
			rMaxTemp = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvTemp, &
				this % iDeltaTime &
			)
			! -1- Relative humidity
			rRelHAnomaly = Anomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvTemp, &
				this % iDeltaTime, &
				DAYS_RADIUS, &
				rRelHAnomalyStdDev, &
				rRelHAnomalyMin, &
				rRelHAnomalyMax &
			)
			rAvgRelH = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUrel, &
				this % iDeltaTime &
			)
			rMInRelH = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUrel, &
				this % iDeltaTime &
			)
			rMaxRelH = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUrel, &
				this % iDeltaTime &
			)
			! -1- Wind speed
			rVelAnomaly = EstimateAnomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvVel, &
				this % iDeltaTime, &
				DAYS_RADIUS &
			)
			rAvgVel = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvVel, &
				this % iDeltaTime &
			)
			rMinVel = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvVel, &
				this % iDeltaTime &
			)
			rMaxVel = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvVel, &
				this % iDeltaTime &
			)
			! -1- Global radiation
			rRgAnomaly = EstimateAnomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRg, &
				this % iDeltaTime, &
				DAYS_RADIUS &
			)
			rAvgRg = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRg, &
				this % iDeltaTime &
			)
			rMinRg = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRg, &
				this % iDeltaTime &
			)
			rMaxRg = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRg, &
				this % iDeltaTime &
			)
			rSumRg = DailySum( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRg, &
				this % iDeltaTime &
			)
			! -1- Net radiation
			rRnAnomaly = EstimateAnomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRn, &
				this % iDeltaTime, &
				DAYS_RADIUS &
			)
			rAvgRn = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRn, &
				this % iDeltaTime &
			)
			rMinRn = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRn, &
				this % iDeltaTime &
			)
			rMaxRn = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRn, &
				this % iDeltaTime &
			)
			rSumRn = DailySum( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRn, &
				this % iDeltaTime &
			)
			! -1- Friction velocity
			rUstarAnomaly = EstimateAnomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUstar, &
				this % iDeltaTime, &
				DAYS_RADIUS &
			)
			rAvgUstar = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUstar, &
				this % iDeltaTime &
			)
			rMinUstar = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUstar, &
				this % iDeltaTime &
			)
			rMaxUstar = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvUstar, &
				this % iDeltaTime &
			)
			! -1- Turbulent sensible heat flux
			rH0Anomaly = EstimateAnomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvH0, &
				this % iDeltaTime, &
				DAYS_RADIUS &
			)
			rAvgH0 = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvH0, &
				this % iDeltaTime &
			)
			rMinH0 = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvH0, &
				this % iDeltaTime &
			)
			rMaxH0 = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvH0, &
				this % iDeltaTime &
			)
			! -1- Stability parameter
			rLm1Anomaly = EstimateAnomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvHlm, &
				this % iDeltaTime, &
				DAYS_RADIUS &
			)
			rAvgLm1 = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvHlm, &
				this % iDeltaTime &
			)
			rMinLm1 = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvHlm, &
				this % iDeltaTime &
			)
			rMaxLm1 = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvHlm, &
				this % iDeltaTime &
			)
			! -1- Mixing height
			rZiAnomaly = EstimateAnomaly( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvZi, &
				this % iDeltaTime, &
				DAYS_RADIUS &
			)
			rAvgZi = DailyMean( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvZi, &
				this % iDeltaTime &
			)
			rMinZi = DailyMin( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvZi, &
				this % iDeltaTime &
			)
			rMaxZi = DailyMax( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvZi, &
				this % iDeltaTime &
			)
			! -1- Precipitation
			rTotalPrec = DailySum( &
				this % ivTimeStamp(i), &
				this % ivTimeStamp, &
				this % rvRain, &
				this % iDeltaTime &
			)
			
			! Write results
			write(13, "(i4.4,2('-',i2.2),',',i3,',',f8.5,2(',',f5.2),43(',',f11.5))") &
				iYear, iMonth, iDay, &
				iDayInYear, rSolarDeclination, rvSunRiseSet, &
				rMinVel, rAvgVel, rMaxVel, rVelAnomaly, &
				rMinTemp, rAvgTemp, rMaxTemp, rTempAnomaly, rTempAnomalyStdDev, rTempAnomalyMin, rTempAnomalyMax, &
				rMinRelH, rAvgRelH, rMaxRelH, rRelHAnomaly, rRelHAnomalyStdDev, rRelhAnomalyMin, rRelHAnomalyMax, &
				rMinRg,   rMaxRg,   rSumRg,   rRgAnomaly, &
				rMinRn,   rMaxRn,   rSumRn,   rRnAnomaly, &
				rMinUstar, rAvgUstar, rMaxUstar, rUstarAnomaly, &
				rMinH0, rAvgH0, rMaxH0, rH0Anomaly, &
				rMinLm1, rAvgLm1, rMaxLm1, rLm1Anomaly, &
				rMinZi, rAvgZi, rMaxZi, rZiAnomaly, &
				rTotalPrec
			
		end do
		close(13)

	end subroutine WriteDailyReport
    
    
	subroutine WriteDataToCalpuffIsc(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer	:: i
		real	:: rDir
		real	:: rHLM, rL
		real	:: rZi
		real	:: rTa
		integer	:: iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
		integer	:: iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer	:: iPrecCode
		
		! Write data to file
		open(13, file=tFiles % sCalpuffIsc, status='unknown', action='write')
		! -1- Header
		call UnpackTime(this % ivTimeStamp(1), iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom)
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo)
		write(13,"('ISCMET.DAT      2.1             Processed data')")
		write(13,"('   1')")
		write(13,"('Prepared by ST-Me')")
		write(13,"('NONE')")
		if(this % iTimeZone > 0) then
			write(13, "('UTC+',i2.2,'00')") this % iTimeZone
		else
			write(13, "('UTC',i3.3,'00')") this % iTimeZone
		end if
		write(13,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
			iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
			iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + this % iDeltaTime
		write(13,"('00000 ',i4,' 00000 ',i4)") iYearFrom, iYearTo
		
		do i = 1, SIZE(this % ivTimeStamp)
		
			! Prepare date and time, and change hour to reflect CALPUFF/ISC convention
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			iHour = iHour + 1
			
			! Change wind direction from anemometer to flow convention
			rDir = this % rvDir(i) + 180.
			if(rDir > 360.) rDir = rDir - 360.
			
			! Compute Obukhov length
			rHLM = this % rvHLM(i)
			if(ABS(rHLM) > 1.e-3) then
				rL = this % rZr / this % rvHLM(i)
			else
				rL = this % rZr / 1.e-3
			end if
			if(rL < -100000.0) rL = -100000.0
			if(rL >  100000.0) rL =  100000.0
			
			! Adjust Zi
			rZi = MAX(this % rvZi(i), 100.)
			
			! Convert temperature to K
			rTa = this % rvTemp(i) + 273.15
			
			! Generate precipitation code, assuming all precipitation is liquid,
			! with code 1
			if(this % rvRain(i) > 0.0) then
				iPrecCode = 1
			else
				iPrecCode = 0
			end if
      	
			! Print data the CALPUFF/ISC way
			write(13,"(2(4i2,i4),2f9.4,f6.1,i2,2f7.1,f9.4,f10.1,f8.4,i4,f7.2,a10,a5,1x,f8.1,i3)") &
				MOD(iYear, 100), iMonth, iDay, iHour, iMinute * 60 + iSecond, &
				MOD(iYear, 100), iMonth, iDay, iHour, iMinute * 60 + iSecond + this % iDeltaTime, &
				rDir, this % rvVel(i), rTa, this % ivIstab(i), rZi, rZi, &
				this % rvUstar(i), rL, this % rZ0, iPrecCode, this % rvRain(i), ' ', '     ', &
				this % rvRg(i), FLOOR(MIN(this % rvUrel(i),100.0))
            
		end do
		close(13)

	end subroutine WriteDataToCalpuffIsc
	

	subroutine WriteDataToCalpuffProfiled(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer				:: i, j, n
		character(len=256)  :: sLineRaw
		character(len=256)  :: sLineOut
		integer				:: iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
		integer				:: iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer				:: iJday
		integer				:: iPrecCode
		real				:: rNewVel, rNewDir, rNewTemp
		real				:: rHLM, rL, rZi
		
      	! Write surface data in CTDM sub-hourly form
      	open(15, file=tFiles % sCalpuffSrf, status='unknown', action='write')
      	
      	! -1- Header
		call UnpackTime(this % ivTimeStamp(1), iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom)
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo)
		write(15,"('SURFACE.DAT     2.1             Processed data')")
		write(15,"('   1')")
		write(15,"('Prepared by ST-Me')")
		write(15,"('NONE')")
		write(15,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
			iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
			iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + this % iDeltaTime
			
		! -1- Data
      	do i = 1, SIZE(this % ivTimeStamp)
      	
			! Current date and time
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			iJday = DayInYear(iYear, iMonth, iDay)

			! Compute Obukhov length
			rHLM = this % rvHLM(i)
			if(ABS(rHLM) > 1.e-3) then
				rL = this % rZr / this % rvHLM(i)
			else
				rL = this % rZr / 1.e-3
			end if
			if(isnan(rL)) rL = 100000.0
			if(rL < -100000.0) rL = -100000.0
			if(rL >  100000.0) rL =  100000.0
			
			! Adjust Zi
			rZi = MIN(MAX(this % rvZi(i), 100.), 9999.0)
			
			if(this % rvRain(i) > 0.) then
				iPrecCode = 1
			else
				iPrecCode = 0
			end if
			
			! Write data
			write(15,"(2(i4,1x,2(i2,1x),i3,1x,i2,1x,i4,1x),2(i4,1x),1x,f7.3,1x,e15.7,1x,e15.7,1x,i2,1x,f6.2,1x,f8.2,1x,i3)") &
				iYear, iMonth, iDay, iJday, iHour+1, 60*iMinute+iSecond, &
				iYear, iMonth, iDay, iJday, iHour+1, 60*iMinute+iSecond + this % iDeltaTime, &
				INT(rZi), INT(rZi), &
				MAX(this % rvUstar(i), 0.001), rL, &
				this % rZ0, iPrecCode, this % rvRain(i), this % rvRg(i), FLOOR(this % rvUrel(i))
				
		end do
		close(15)
      	
      	! Write profile data in CTDM sub-hourly form
      	open(15, file=tFiles % sCalpuffPrf, status='unknown', action='write')
      	
      	! -1- Header
		write(15,"('PROFILE.DAT     2.1             Processed data')")
		write(15,"('   1')")
		write(15,"('Prepared by ST-Me')")
		write(15,"('NONE')")
		write(15,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
			iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
			iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + this % iDeltaTime
			
		! -1- Data
      	do i = 1, size(this % ivTimeStamp)
      	
			! First (close to ground) line
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(15, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,4(1x,f6.1))") &
				iYear, iMonth, iDay, iHour+1, &
				this % rZr, 0, this % rvDir(i), this % rvVel(i), &
				this % rvTemp(i) + 273.15, &
				-999.9, -999.9, -999.9, -999.9
			
			! Upper lines
			n = this % ivNumHeights(i)
			do j = 1, n - 1
				rNewVel = SQRT(this % rmOutU(i,j)**2 + this % rmOutV(i,j)**2)
				if(rNewVel > 0.) then
					rNewDir = DIR_WIND(this % rmOutU(i,j),this % rmOutV(i,j))
				else
					rNewDir = 0.
				end if
				rNewTemp = this % rmOutTemp(i,j)
				if(rNewTemp /= rNewTemp) then
					rNewTemp = -9999.9
				else
					if(rNewTemp < -263.15) then
						rNewTemp = -9999.9
					end if
				end if
				if(rNewTemp >= -263.15) then
					write(15, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
						iYear, iMonth, iDay, iHour+1, &
						this % rvZ(j), 0, rNewDir, rNewVel, &
						rNewTemp + 273.15, &
						-999.9, -999.9, -999.9
				else
					write(15, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
						iYear, iMonth, iDay, iHour+1, &
						this % rvZ(j), 0, rNewDir, rNewVel, &
						-9999.9, &
						-999.9, -999.9, -999.9
				end if
			end do
			rNewVel = SQRT(this % rmOutU(i,n)**2 + this % rmOutV(i,n)**2)
			if(rNewVel > 0.) then
				rNewDir = DIR_WIND(this % rmOutU(i,n),this % rmOutV(i,n))
			else
				rNewDir = 0.
			end if
			rNewTemp = this % rmOutTemp(i,n)
			if(rNewTemp /= rNewTemp) then
				rNewTemp = -9999.9
			else
				if(rNewTemp < -263.15) then
					rNewTemp = -9999.9
				end if
			end if
			if(rNewTemp >= -263.15) then
				write(15, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
					iYear, iMonth, iDay, iHour+1, &
					this % rvZ(n), 1, rNewDir, rNewVel, &
					rNewTemp + 273.15, &
					-999.9, -999.9, -999.9
			else
				write(15, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
					iYear, iMonth, iDay, iHour+1, &
					this % rvZ(n), 1, rNewDir, rNewVel, &
					-9999.9, &
					-999.9, -999.9, -999.9
			end if
		end do
		close(15)

	end subroutine WriteDataToCalpuffProfiled
	

	subroutine WriteDataToCalpuffProf7(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer				:: i, j, n
		character(len=256)  :: sLineRaw
		character(len=256)  :: sLineOut
		integer				:: iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
		integer				:: iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer				:: iJday
		integer				:: iPrecCode
		real				:: rNewVel, rNewDir, rNewTemp
		real				:: rHLM, rL, rZi
		
      	! Write surface data in CTDM sub-hourly form
      	open(15, file=tFiles % sCalpuffSrf, status='unknown', action='write')
      	
      	! -1- Header
		call UnpackTime(this % ivTimeStamp(1), iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom)
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo)
		write(15,"('SURFACE.DAT     2.1             Processed data')")
		write(15,"('   1')")
		write(15,"('Prepared by ST-Me')")
		write(15,"('NONE')")
		write(15,"('UTC+0100')")
		write(15,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
			iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
			iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + this % iDeltaTime
			
		! -1- Data
      	do i = 1, SIZE(this % ivTimeStamp)
      	
			! Current date and time
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			iJday = DayInYear(iYear, iMonth, iDay)

			! Compute Obukhov length
			rHLM = this % rvHLM(i)
			if(ABS(rHLM) > 1.e-3) then
				rL = this % rZr / this % rvHLM(i)
			else
				rL = this % rZr / 1.e-3
			end if
			if(rL < -100000.0) rL = -100000.0
			if(rL >  100000.0) rL =  100000.0
			
			! Adjust Zi
			rZi = MIN(MAX(this % rvZi(i), 100.), 9999.0)
			
			if(this % rvRain(i) > 0.) then
				iPrecCode = 1
			else
				iPrecCode = 0
			end if
			
			! Write data
			write(15,"(2(i4,1x,2(i2,1x),i3,1x,i2,1x,i4,1x),2(i4,1x),1x,f7.3,1x,e15.7,1x,e15.7,1x,i2,1x,f6.2,1x,f8.2,1x,i3)") &
				iYear, iMonth, iDay, iJday, iHour+1, 60*iMinute+iSecond, &
				iYear, iMonth, iDay, iJday, iHour+1, 60*iMinute+iSecond + this % iDeltaTime, &
				INT(rZi), INT(rZi), &
				MAX(this % rvUstar(i), 0.001), rL, &
				this % rZ0, iPrecCode, this % rvRain(i), this % rvRg(i), FLOOR(this % rvUrel(i))
				
		end do
		close(15)
      	
      	! Write profile data in CTDM sub-hourly form
      	open(15, file=tFiles % sCalpuffPrf, status='unknown', action='write')
      	
      	! -1- Header
		write(15,"('PROFILE.DAT     2.1             Processed data')")
		write(15,"('   1')")
		write(15,"('Prepared by ST-Me')")
		write(15,"('NONE')")
		write(15,"('UTC+0100')")
		write(15,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
			iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
			iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + this % iDeltaTime
			
		! -1- Data
      	do i = 1, size(this % ivTimeStamp)
      	
			! First (close to ground) line
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(15, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,4(1x,f6.1))") &
				iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
				iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + this % iDeltaTime, &
				this % rZr, 0, this % rvDir(i), this % rvVel(i), &
				this % rvTemp(i) + 273.15, &
				-999.9, -999.9, -999.9, -999.9
			
			! Upper lines
			n = this % ivNumHeights(i)
			do j = 1, n - 1
				rNewVel = SQRT(this % rmOutU(i,j)**2 + this % rmOutV(i,j)**2)
				if(rNewVel > 0.) then
					rNewDir = DIR_WIND(this % rmOutU(i,j),this % rmOutV(i,j))
				else
					rNewDir = 0.
				end if
				rNewTemp = this % rmOutTemp(i,j)
				if(rNewTemp /= rNewTemp) then
					rNewTemp = -9999.9
				else
					if(rNewTemp < -263.15) then
						rNewTemp = -9999.9
					end if
				end if
				if(rNewTemp > -263.15) then
					write(15, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
					iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
					iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + this % iDeltaTime, &
						this % rvZ(j), 0, rNewDir, rNewVel, &
						rNewTemp + 273.15, &
						-999.9, -999.9, -999.9
				else
					write(15, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
					iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
					iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + this % iDeltaTime, &
						this % rvZ(j), 0, rNewDir, rNewVel, &
						-9999.9, &
						-999.9, -999.9, -999.9
				end if
			end do
			rNewVel = SQRT(this % rmOutU(i,n)**2 + this % rmOutV(i,n)**2)
			if(rNewVel > 0.) then
				rNewDir = DIR_WIND(this % rmOutU(i,n),this % rmOutV(i,n))
			else
				rNewDir = 0.
			end if
			rNewTemp = this % rmOutTemp(i,n)
				rNewTemp = this % rmOutTemp(i,j)
				if(rNewTemp /= rNewTemp) then
					rNewTemp = -9999.9
				else
					if(rNewTemp < -263.15) then
						rNewTemp = -9999.9
					end if
				end if
				if(rNewTemp > -263.15) then
					write(15, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
					iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
					iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + this % iDeltaTime, &
					this % rvZ(n), 1, rNewDir, rNewVel, &
					rNewTemp + 273.15, &
					-999.9, -999.9, -999.9
				else
					write(15, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
					iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
					iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + this % iDeltaTime, &
					this % rvZ(n), 1, rNewDir, rNewVel, &
					-9999.9, &
					-999.9, -999.9, -999.9
				end if
		end do
		close(15)

	end subroutine WriteDataToCalpuffProf7
	

	subroutine WriteDataToCalmet(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer				:: i, j, k, l, m, n
		character(len=256)  :: sLineRaw
		character(len=256)  :: sLineOut
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer				:: iYearFrom, iJDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
		integer				:: iYearTo, iJDayTo, iHourTo, iMinuteTo, iSecondTo
		real				:: rPlimit, rPmin
		integer				:: iJday
		integer				:: iPrecCode
		integer				:: iCeiling
		
		! Constants
		real, dimension(4), parameter	:: P_ALLOWED = [850., 700., 500., 100.]
		
		! Check time step to be 3600 for CALMET data: given the very large
		! size of data produced, we do not really love going sub-hourly
		! in this case
		if(this % iDeltaTime /= 3600) then
			print *, "st_me:: warning: sub-hourly run selected may give large meteo set"
			print *, "                 (current set seems having a time step of ", &
				this % iDeltaTime, "seconds"
		end if
		
		! Compute the limiting pressure (it will be written, as part of the upper air data file header)
		n = this % ivNumHeights(1)
		rPmin = Pressure(this % rvZ(n), this % rmOutTemp(1,n) + 273.15)
		rPlimit = 0.
		do i = 1, SIZE(P_ALLOWED)
			if(P_ALLOWED(i) < rPmin) then
				rPlimit = P_ALLOWED(i)
				exit
			end if
		end do
		if(rPlimit <= 0.) then
			print *,'st_me: error: Negative pressure limit - Value=',rPlimit
			return	! Without printing - But, this should never happen
		end if
		
      	! Write surface data in Calmet form
      	open(15, file=tFiles % sCalmetSrf, status='unknown', action='write')
      	write(15,"('SURF.DAT        2.1             Hour Start and End Times with Seconds')")
      	write(15,"('   1')")
      	write(15,"('Produced by ST-Me')")
		write(15,"('NONE')")
      	if(this % iTimeZone >= 0) then
			write(15,"('UTC+',i2.2,'00')") this % iTimeZone
		else
			write(15,"('UTC-',i2.2,'00')") abs(this % iTimeZone)
		end if
		call UnpackTime(this % ivTimeStamp(1), iYearFrom, iMonth, iDay, iHourFrom, iMinuteFrom, iSecondFrom)
		iJDayFrom = DayInYear(iYearFrom, iMonth, iDay)
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), iYearTo, iMonth, iDay, iHourTo, iMinuteTo, iSecondTo)
		iJDayTo = DayInYear(iYearTo, iMonth, iDay)
		write(15, "(9i5)") &
			iYearFrom, iJDayFrom, iHourFrom, iMinuteFrom*60+iSecondFrom, &
			iYearTo, iJDayTo, iHourTo, iMinuteTo*60+iSecondTo + this % iDeltaTime, &
			1		! Last value is number of stations
		write(15,"(i8)") 0	! This is station ID, assumed 0 on single run
      	do i = 1, SIZE(this % ivTimeStamp)
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			iJday = DayInYear(iYear, iMonth, iDay)
			if(this % rvRain(i) > 0.) then
				iPrecCode = 1
			else
				iPrecCode = 0
			end if
			write(15, "(8i5)") &
				iYear, iJDay, iHour, 60*iMinute + iSecond, iYear, iJDay, iHour, 60*iMinute + iSecond + this % iDeltaTime
			iCeiling = FLOOR(this % rvZi(i)/(0.3048*100.))
			write(15, "(2(1x,f8.3),2(1x,i4),1x,f8.3,1x,i4,1x,f8.3,1x,i3)") &
				this % rvVel(i), this % rvDir(i), iCeiling, &
				FLOOR(10.*this % rvCover(i)), this % rvTemp(i) + 273.15, &
				NINT(this % rvUrel(i)), Pressure(this % rvZ(1), this % rvTemp(i) + 273.15), iPrecCode
		end do
		close(15)
      	
      	! Write profile data in Calmet form.
      	!
      	! Note: The line "UTC+0000" in profile file is mandatory: profile data must be expressed in UTC,
      	!       assuming they are radio-sounding (of course they are not in this case).
      	!
      	!		In addition, their time span must be the same as
      	!
      	open(15, file=tFiles % sCalmetPrf, status='unknown', action='write')
      	write(15, "('UP.DAT          2.1             Hour Start and End Times with Seconds')")
      	write(15,"('   1')")
      	write(15,"('Produced by ST-Me')")
		write(15,"('NONE')")
		write(15,"('UTC+0000')")
      	!if(this % iTimeZone >= 0) then
		!	write(15,"('UTC+',i2.2,'00')") this % iTimeZone
		!else
		!	write(15,"('UTC-',i2.2,'00')") abs(this % iTimeZone)
		!end if
		!call UnpackTime(this % ivTimeStamp(1) - this % iTimeZone*3600, &
		!	iYearFrom, iMonth, iDay, iHourFrom, iMinuteFrom, iSecondFrom)
		call UnpackTime(this % ivTimeStamp(1), &
			iYearFrom, iMonth, iDay, iHourFrom, iMinuteFrom, iSecondFrom)
		iJDayFrom = DayInYear(iYearFrom, iMonth, iDay)
		!call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)) - this % iTimeZone*3600, &
		!	iYearTo, iMonth, iDay, iHourTo, iMinuteTo, iSecondTo)
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), &
			iYearTo, iMonth, iDay, iHourTo, iMinuteTo, iSecondTo)
		iJDayTo = DayInYear(iYearTo, iMonth, iDay)
		write(15,"(1x,8i5,f5.0,'    1    1')") &
			iYearFrom, iJDayFrom, iHourFrom, 60*iMinuteFrom + iSecondFrom, &
			iYearTo, iJDayTo, iHourTo, 60*iMinuteTo + iSecondTo + this % iDeltaTime, &
			rPlimit
		write(15,"('     F    F    F    F')")
		if(this % iTimeZone > 0) then
			do l = 1, this % iTimeZone * (3600 / this % iDeltaTime)
				n = this % ivNumHeights(1)
				call UnpackTime( &
					this % ivTimeStamp(1) + (l-1)*this % iDeltaTime, &
					iYear, iMonth, iDay, iHour, iMinute, iSecond &
				)
				write(15, "(3x,i4,2x,i8,2(4x,i4,i4,i3,i3,i5),2x,i5,1x,i5)") &
					6201, &																		! Data format type
					0, &																		! 5 letter station ID
					iYear, iMonth, iDay, iHour, 60*iMinute + iSecond, &							! Beginning time stamp
					iYear, iMonth, iDay, iHour, 60*iMinute + iSecond + this % iDeltaTime, &		! Ending time stamp
					n, n																		! Number of data in profiles
				do j = 1, n, 4
					write(15, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
						( &
							Pressure(this % rvZ(k), this % rmOutTemp(1,k) + 273.15), &
							this % rvZ(k), &
							this % rmOutTemp(1,k) + 273.15, &
							NINT(DIR_WIND(this % rmOutU(1,k),this % rmOutV(1,k))), &
							NINT(SQRT(this % rmOutU(1,k)**2 + this % rmOutV(1,k)**2)), &
							k=j,MIN(j+3,n) &
						)
				end do
			end do
		end if
		m = SIZE(this % ivTimeStamp)
      	do i = 1, m
			n = this % ivNumHeights(i)
			call UnpackTime(this % ivTimeStamp(i) + this % iTimeZone*3600, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			!call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(15, "(3x,i4,2x,i8,2(4x,i4,i4,i3,i3,i5),2x,i5,1x,i5)") &
				6201, &																		! Data format type
				0, &																		! 5 letter station ID
				iYear, iMonth, iDay, iHour, 60*iMinute + iSecond, &							! Beginning time stamp
				iYear, iMonth, iDay, iHour, 60*iMinute + iSecond + this % iDeltaTime, &		! Ending time stamp
				n, n																		! Number of data in profiles
			do j = 1, n, 4
				write(15, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
					( &
						Pressure(this % rvZ(k), this % rmOutTemp(i,k) + 273.15), &
						this % rvZ(k), &
						this % rmOutTemp(i,k) + 273.15, &
						NINT(DIR_WIND(this % rmOutU(i,k),this % rmOutV(i,k))), &
						NINT(SQRT(this % rmOutU(i,k)**2 + this % rmOutV(i,k)**2)), &
						k=j,MIN(j+3,n) &
					)
			end do
		end do
		if(this % iTimeZone < 0) then
			do l = 1, abs(this % iTimeZone) * (3600 / this % iDeltaTime)
				n = this % ivNumHeights(m)
				call UnpackTime( &
					this % ivTimeStamp(m) + l*this % iDeltaTime, &
					iYear, iMonth, iDay, iHour, iMinute, iSecond &
				)
				write(15, "(3x,i4,2x,i8,2(4x,i4,i4,i3,i3,i5),2x,i5,1x,i5)") &
					6201, &																		! Data format type
					0, &																		! 5 letter station ID
					iYear, iMonth, iDay, iHour, 60*iMinute + iSecond, &							! Beginning time stamp
					iYear, iMonth, iDay, iHour, 60*iMinute + iSecond + this % iDeltaTime, &		! Ending time stamp
					n, n																		! Number of data in profiles
				do j = 1, n, 4
					write(15, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
						( &
							Pressure(this % rvZ(k), this % rmOutTemp(m,k) + 273.15), &
							this % rvZ(k), &
							this % rmOutTemp(m,k) + 273.15, &
							NINT(DIR_WIND(this % rmOutU(m,k),this % rmOutV(m,k))), &
							NINT(SQRT(this % rmOutU(m,k)**2 + this % rmOutV(m,k)**2)), &
							k=j,MIN(j+3,n) &
						)
				end do
			end do
		end if
		close(15)

	end subroutine WriteDataToCalmet
	

	subroutine WriteDataToCalmet00(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer				:: i, j, k, l, m, n
		character(len=256)  :: sLineRaw
		character(len=256)  :: sLineOut
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer				:: iYearFrom, iJDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
		integer				:: iYearTo, iJDayTo, iHourTo, iMinuteTo, iSecondTo
		real				:: rPlimit, rPmin
		integer				:: iJday
		integer				:: iPrecCode
		integer				:: iCeiling
		
		! Constants
		real, dimension(4), parameter	:: P_ALLOWED = [850., 700., 500., 100.]
		
		! Check time step to be 3600 for CALMET data: given the very large
		! size of data produced, we do not really love going sub-hourly
		! in this case
		if(this % iDeltaTime /= 3600) then
			print *, "st_me:: warning: sub-hourly run selected may give large meteo set"
			print *, "                 (current set seems having a time step of ", &
				this % iDeltaTime, "seconds"
		end if
		
		! Compute the limiting pressure (it will be written, as part of the upper air data file header)
		n = this % ivNumHeights(1)
		rPmin = Pressure(this % rvZ(n), this % rmOutTemp(1,n) + 273.15)
		rPlimit = 0.
		do i = 1, SIZE(P_ALLOWED)
			if(P_ALLOWED(i) < rPmin) then
				rPlimit = P_ALLOWED(i)
				exit
			end if
		end do
		if(rPlimit <= 0.) then
			print *,'st_me: error: Negative pressure limit - Value=',rPlimit
			return	! Without printing - But, this should never happen
		end if
		
      	! Write surface data in Calmet form, with reference to UTC+0000,
      	! whatever the actual time zone is
      	open(15, file=tFiles % sCalmetSrf00, status='unknown', action='write')
      	write(15,"('SURF.DAT        2.1             Hour Start and End Times with Seconds')")
      	write(15,"('   1')")
      	write(15,"('Produced by ST-Me')")
		write(15,"('NONE')")
		write(15,"('UTC+0000')")
		call UnpackTime(this % ivTimeStamp(1), iYearFrom, iMonth, iDay, iHourFrom, iMinuteFrom, iSecondFrom)
		iJDayFrom = DayInYear(iYearFrom, iMonth, iDay)
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), iYearTo, iMonth, iDay, iHourTo, iMinuteTo, iSecondTo)
		iJDayTo = DayInYear(iYearTo, iMonth, iDay)
		write(15, "(9i5)") &
			iYearFrom, iJDayFrom, iHourFrom, iMinuteFrom*60+iSecondFrom, &
			iYearTo, iJDayTo, iHourTo, iMinuteTo*60+iSecondTo + this % iDeltaTime, &
			1		! Last value is number of stations
		write(15,"(i8)") 0	! This is station ID, assumed 0 on single run
      	do i = 1, SIZE(this % ivTimeStamp)
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			iJday = DayInYear(iYear, iMonth, iDay)
			if(this % rvRain(i) > 0.) then
				iPrecCode = 1
			else
				iPrecCode = 0
			end if
			write(15, "(8i5)") &
				iYear, iJDay, iHour, 60*iMinute + iSecond, iYear, iJDay, iHour, 60*iMinute + iSecond + this % iDeltaTime
			iCeiling = FLOOR(this % rvZi(i)/(0.3048*100.))
			write(15, "(2(1x,f8.3),2(1x,i4),1x,f8.3,1x,i4,1x,f8.3,1x,i3)") &
				this % rvVel(i), this % rvDir(i), iCeiling, &
				FLOOR(10.*this % rvCover(i)), this % rvTemp(i) + 273.15, &
				NINT(this % rvUrel(i)), Pressure(this % rvZ(1), this % rvTemp(i) + 273.15), iPrecCode
		end do
		close(15)
      	
      	! Write profile data in Calmet form.
      	!
      	! Note: The line "UTC+0000" in profile file is mandatory: profile data must be expressed in UTC,
      	!       assuming they are radio-sounding (of course they are not in this case).
      	!
      	!		In addition, their time span must be the same as
      	!
      	open(15, file=tFiles % sCalmetPrf00, status='unknown', action='write')
      	write(15, "('UP.DAT          2.1             Hour Start and End Times with Seconds')")
      	write(15,"('   1')")
      	write(15,"('Produced by ST-Me')")
		write(15,"('NONE')")
		write(15,"('UTC+0000')")
		call UnpackTime(this % ivTimeStamp(1), &
			iYearFrom, iMonth, iDay, iHourFrom, iMinuteFrom, iSecondFrom)
		iJDayFrom = DayInYear(iYearFrom, iMonth, iDay)
		call UnpackTime(this % ivTimeStamp(SIZE(this % ivTimeStamp)), &
			iYearTo, iMonth, iDay, iHourTo, iMinuteTo, iSecondTo)
		iJDayTo = DayInYear(iYearTo, iMonth, iDay)
		write(15,"(1x,8i5,f5.0,'    1    1')") &
			iYearFrom, iJDayFrom, iHourFrom, 60*iMinuteFrom + iSecondFrom, &
			iYearTo, iJDayTo, iHourTo, 60*iMinuteTo + iSecondTo + this % iDeltaTime, &
			rPlimit
		write(15,"('     F    F    F    F')")
		m = SIZE(this % ivTimeStamp)
      	do i = 1, m
			n = this % ivNumHeights(i)
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(15, "(3x,i4,2x,i8,2(4x,i4,i4,i3,i3,i5),2x,i5,1x,i5)") &
				6201, &																		! Data format type
				0, &																		! 5 letter station ID
				iYear, iMonth, iDay, iHour, 60*iMinute + iSecond, &							! Beginning time stamp
				iYear, iMonth, iDay, iHour, 60*iMinute + iSecond + this % iDeltaTime, &		! Ending time stamp
				n, n																		! Number of data in profiles
			do j = 1, n, 4
				write(15, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
					( &
						Pressure(this % rvZ(k), this % rmOutTemp(i,k) + 273.15), &
						this % rvZ(k), &
						this % rmOutTemp(i,k) + 273.15, &
						NINT(DIR_WIND(this % rmOutU(i,k),this % rmOutV(i,k))), &
						NINT(SQRT(this % rmOutU(i,k)**2 + this % rmOutV(i,k)**2)), &
						k=j,MIN(j+3,n) &
					)
			end do
		end do
		
		! Add (by replication of last) a profile block to allow CALMET upper air data bracketing
		n = this % ivNumHeights(m)
		call UnpackTime(this % ivTimeStamp(m) + this % iDeltaTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
		write(15, "(3x,i4,2x,i8,2(4x,i4,i4,i3,i3,i5),2x,i5,1x,i5)") &
			6201, &																		! Data format type
			0, &																		! 5 letter station ID
			iYear, iMonth, iDay, iHour, 60*iMinute + iSecond, &							! Beginning time stamp
			iYear, iMonth, iDay, iHour, 60*iMinute + iSecond + this % iDeltaTime, &		! Ending time stamp
			n, n																		! Number of data in profiles
		do j = 1, n, 4
			write(15, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
				( &
					Pressure(this % rvZ(k), this % rmOutTemp(m,k) + 273.15), &
					this % rvZ(k), &
					this % rmOutTemp(m,k) + 273.15, &
					NINT(DIR_WIND(this % rmOutU(m,k),this % rmOutV(m,k))), &
					NINT(SQRT(this % rmOutU(m,k)**2 + this % rmOutV(m,k)**2)), &
					k=j,MIN(j+3,n) &
				)
		end do
		
		close(15)

	end subroutine WriteDataToCalmet00
	

	subroutine WriteDataToAermet(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer	:: i
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		real	:: rDewPoint
		
      	! Write surface data in Aermet form
      	open(16, file=tFiles % sAermet, status='unknown', action='write')
      	do i = 1, SIZE(this % ivTimeStamp)
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(16, "(4(i2,1x),f7.1,1x,f4.1,1x,f6.1)") &
				MOD(iYear, 100), iMonth, iDay, iHour, &
				Pressure(this % rvZ(1), &
				this % rvTemp(i) + 273.15), &
				10.0*this % rvCover(i), &
				this % rvZi(i)
			write(16, "(f5.1,4(1x,f5.1))") &
				this % rvTemp(i), this % rvTdew(i), this % rvUrel(i), this % rvDir(i), this % rvVel(i)
		end do
		close(16)
      	
	end subroutine WriteDataToAermet
	

	subroutine WriteDataToGral(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer	:: i
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		
      	! Write surface data in Aermet form
      	open(16, file=tFiles % sGral, status='unknown', action='write')
      	do i = 1, SIZE(this % ivTimeStamp)
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(16, "(2(i2.2,'.'),i4.4,',',i2.2,':',i2.2,',',f7.1,',',i3,',',i1)") &
				iDay, iMonth, iYear, iHour, iMinute, &
				this % rvVel(i), this % rvDir(i), this % ivLstab(i)
		end do
		close(16)
      	
	end subroutine WriteDataToGral
	

	subroutine WriteDataToAustal2000(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer	:: i
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		
      	! Write surface data in Aermet form
      	open(16, file=tFiles % sAustal2000, status='unknown', action='write')
      	do i = 1, SIZE(this % ivTimeStamp)
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(16, "(2(i2.2,'.'),i4.4,',',i2.2,':',i2.2,',',f7.1,',',i3,',',i1)") &
				iDay, iMonth, iYear, iHour, iMinute, &
				this % rvVel(i), this % rvDir(i), this % ivLstab(i)
		end do
		close(16)
      	
	end subroutine WriteDataToAustal2000
	

	subroutine WriteDataToTinyDisp(this, tFiles)
	
		! Routine arguments
		class(DataSetType), intent(in)	:: this
		type(FileNamesType), intent(in)	:: tFiles
		
		! Locals
		integer	:: i
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		
      	! Write surface data in Aermet form
      	open(16, file=tFiles % sTinyDispSrf, status='unknown', action='write')
      	do i = 1, SIZE(this % ivTimeStamp)
			call UnpackTime(this % ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(16, "(2(i2.2,'.'),i4.4,',',i2.2,':',i2.2,',',f7.1,',',i3,',',i1)") &
				iDay, iMonth, iYear, iHour, iMinute, &
				this % rvVel(i), this % rvDir(i), this % ivLstab(i)
		end do
		close(16)
		
      	open(16, file=tFiles % sTinyDispPrf, status='unknown', action='write')
		close(16)
      	
	end subroutine WriteDataToTinyDisp
	

	! *********************
	! * Internal routines *
	! *********************
	
	function cleanFileExtension(sFileName) result(sNameWithoutExtension)
	
		! Routine arguments
		character(len=256), intent(in)	:: sFileName
		character(len=256)				:: sNameWithoutExtension
		
		! Locals
		integer ::	iPos
		
		! Extension is defined as any character past last '.', so let's find its position
		! and clean anything from then on
		iPos = INDEX(sFileName, ".", back=.true.)
		if(iPos > 0) then
			sNameWithoutExtension = sFileName(1:iPos)
		else
			sNameWithoutExtension = sFileName
		end if
		
	end function cleanFileExtension
	
	
	subroutine sanitizeString(sString)
	
		! Routine arguments
		character(len=*), intent(inout)	:: sString
		
		! Locals
		integer	:: i
		
		! Scan string, and change every occurrence of separator to blank
		do i = 1, len_trim(sString)
			if(verify(sString(i:i), "-/,:") <= 0) sString(i:i) = " "
		end do
		
	end subroutine sanitizeString


    function validateValue(rValue, rMin, rMax) result(rValidatedValue)
    
		! Routine arguments
		real, intent(in)		:: rValue
		real, intent(in)		:: rMin
		real, intent(in)		:: rMax
		real					:: rValidatedValue
        
        ! Locals
        ! -none-
        
        ! Invalidate off-range values
        if(rMin <= rValue .and. rValue <= rMax) then
			rValidatedValue = rValue
        else
			rValidatedValue = NaN
        end if
        
	end function validateValue
    
    
    subroutine BasicStat(rvVal,  rMin,  rMax,  rAvg,  rStd)
    
        ! Routine arguments
        real, dimension(:), intent(in)  :: rvVal
        real, intent(out)               :: rMin
        real, intent(out)               :: rMax
        real, intent(out)               :: rAvg
        real, intent(out)               :: rStd
        
        ! Locals
        integer :: i
        integer :: iNumData
        real    :: rSumVal2
        
        ! Compute the information desired
        iNumData = 0
        rMin =  HUGE(rMin)
        rMax = -HUGE(rMax)
        rAvg = 0.
        rSumVal2 = 0.
        do i = 1, SIZE(rvVal)
            if(.NOT.ISNAN(rvVal(i))) then
                iNumData = iNumData + 1
                rMin = MIN(rMin, rvVal(i))
                rMax = MAX(rMax, rvVal(i))
                rAvg = rAvg + rvVal(i)
                rSumVal2 = rSumVal2 + rvVal(i)**2
            end if
        end do
        if(iNumData > 0) then
            rAvg = rAvg / iNumData
            rStd = SQRT((rSumVal2/iNumData) - rAvg**2)
        else
            rMin = NaN
            rMax = NaN
            rAvg = NaN
            rStd = NaN
        end if
        
    end subroutine BasicStat
    
    
    subroutine DumpBasicStat(this)
    
		! Routine arguments
		type(DataSetType), intent(in)	:: this
		
		! Locals
		integer					:: iIndexMax
		integer                 :: iInvalidVel
		integer                 :: iInvalidDir
		integer                 :: iInvalidTemp
		integer                 :: iInvalidUrel
		integer                 :: iInvalidCover
		integer                 :: iInvalidRg
		integer                 :: iInvalidRain
		integer                 :: iInvalidRn
		integer                 :: iInvalidUstar
		integer                 :: iInvalidH0
		integer                 :: iInvalidHLM
		real                    :: rVelMin
		real                    :: rVelMax
		real                    :: rVelAvg
		real                    :: rVelStd
		real                    :: rTempMin
		real                    :: rTempMax
		real                    :: rTempAvg
		real                    :: rTempStd
		real                    :: rUrelMin
		real                    :: rUrelMax
		real                    :: rUrelAvg
		real                    :: rUrelStd
		real                    :: rCoverMin
		real                    :: rCoverMax
		real                    :: rCoverAvg
		real                    :: rCoverStd
		real                    :: rRgMin
		real                    :: rRgMax
		real                    :: rRgAvg
		real                    :: rRgStd
		real                    :: rRainMin
		real                    :: rRainMax
		real                    :: rRainAvg
		real                    :: rRainStd
		real                    :: rRnMin
		real                    :: rRnMax
		real                    :: rRnAvg
		real                    :: rRnStd
		real                    :: rUstarMin
		real                    :: rUstarMax
		real                    :: rUstarAvg
		real                    :: rUstarStd
		real                    :: rH0Min
		real                    :: rH0Max
		real                    :: rH0Avg
		real                    :: rH0Std
		real                    :: rHLMMin
		real                    :: rHLMMax
		real                    :: rHLMAvg
		real                    :: rHLMStd
		
		! Count invalid data for each column
		iIndexMax = SIZE(this % ivTimeStamp)
		iInvalidVel   = COUNT(ISNAN(this % rvVel))
		iInvalidDir   = COUNT(ISNAN(this % rvDir))
		iInvalidTemp  = COUNT(ISNAN(this % rvTemp))
		iInvalidUrel  = COUNT(ISNAN(this % rvUrel))
		iInvalidCover = COUNT(ISNAN(this % rvCover))
		iInvalidRg    = COUNT(ISNAN(this % rvRg))
		iInvalidRain  = COUNT(ISNAN(this % rvRain))
		iInvalidRn    = COUNT(ISNAN(this % rvRn))
		iInvalidUstar = COUNT(ISNAN(this % rvUstar))
		iInvalidH0    = COUNT(ISNAN(this % rvH0))
		iInvalidHLM   = COUNT(ISNAN(this % rvHLM))
		
		! Build and save statistical indicators for the variables as read
		call BasicStat(this % rvVel,   rVelMin,   rVelMax,   rVelAvg,   rVelStd)
		call BasicStat(this % rvTemp,  rTempMin,  rTempMax,  rTempAvg,  rTempStd)
		call BasicStat(this % rvUrel,  rUrelMin,  rUrelMax,  rUrelAvg,  rUrelStd)
		call BasicStat(this % rvCover, rCoverMin, rCoverMax, rCoverAvg, rCoverStd)
		call BasicStat(this % rvRg,    rRgMin,    rRgMax,    rRgAvg,    rRgStd)
		call BasicStat(this % rvRain,  rRainMin,  rRainMax,  rRainAvg,  rRainStd)
		call BasicStat(this % rvRn,    rRnMin,    rRnMax,    rRnAvg,    rRnStd)
		call BasicStat(this % rvUstar, rUstarMin, rUstarMax, rUstarAvg, rUstarStd)
		call BasicStat(this % rvH0,    rH0Min,    rH0Max,    rH0Avg,    rH0Std)
		call BasicStat(this % rvHLM,   rHLMMin,   rHLMMax,   rHLMAvg,   rHLMStd)
		write(11,"(1x,'Total number of lines: ',i7/)") iIndexMax
		write(11,"(1x,'              %invalid Min     Max     Avg     StdDev')")
		write(11,"(1x,'Wind speed:      ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidVel)/FLOAT(iIndexMax), rVelMin,  rVelMax,  rVelAvg,  rVelStd
		write(11,"(1x,'Temperature:     ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidTemp)/FLOAT(iIndexMax), rTempMin,  rTempMax,  rTempAvg,  rTempStd
		write(11,"(1x,'Rel.humidity:    ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidUrel)/FLOAT(iIndexMax), rUrelMin,  rUrelMax,  rUrelAvg,  rUrelStd
		write(11,"(1x,'Cloud cover:     ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidCover)/FLOAT(iIndexMax), rCoverMin, rCoverMax, rCoverAvg, rCoverStd
		write(11,"(1x,'Global rad.:     ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidRg)/FLOAT(iIndexMax), rRgMin,  rRgMax,  rRgAvg,  rRgStd
		write(11,"(1x,'Net rad.:        ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidRn)/FLOAT(iIndexMax), rRnMin,  rRnMax,  rRnAvg,  rRnStd
		write(11,"(1x,'Precipitation:   ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidRain)/FLOAT(iIndexMax), rRainMin,  rRainMax,  rRainAvg,  rRainStd
		write(11,"(1x,'Friction vel.:   ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidUstar)/FLOAT(iIndexMax), rUstarMin,  rUstarMax,  rUstarAvg,  rUstarStd
		write(11,"(1x,'Sens. heat flux: ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidH0)/FLOAT(iIndexMax), rH0Min,  rH0Max,  rH0Avg,  rH0Std
		write(11,"(1x,'z/L:             ',f5.1,4(1x,f7.2))") &
			100.*FLOAT(iInvalidHLM)/FLOAT(iIndexMax), rHLMMin,  rHLMMax,  rHLMAvg,  rHLMStd
			
    end subroutine DumpBasicStat

    
    function DumpBasicStatByYear(this) result(iRetCode)
    
		! Routine arguments
		type(DataSetType), intent(in)	:: this
		integer							:: iRetCode
		
		! Locals
		integer					:: iErrCode
		integer					:: i
		integer					:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer					:: iYearMin, iYearMax, iYearStart, iYearNext
		integer					:: iTimeFrom, iTimeTo
		integer, dimension(1)	:: ivPos
		integer					:: iNumData
		integer					:: iIndexMax
		integer                 :: iInvalidVel
		integer                 :: iInvalidDir
		integer                 :: iInvalidTemp
		integer                 :: iInvalidUrel
		integer                 :: iInvalidCover
		integer                 :: iInvalidRg
		integer                 :: iInvalidRain
		integer                 :: iInvalidRn
		integer                 :: iInvalidUstar
		integer                 :: iInvalidH0
		integer                 :: iInvalidHLM
		real                    :: rVelMin
		real                    :: rVelMax
		real                    :: rVelAvg
		real                    :: rVelStd
		real                    :: rTempMin
		real                    :: rTempMax
		real                    :: rTempAvg
		real                    :: rTempStd
		real                    :: rUrelMin
		real                    :: rUrelMax
		real                    :: rUrelAvg
		real                    :: rUrelStd
		real                    :: rCoverMin
		real                    :: rCoverMax
		real                    :: rCoverAvg
		real                    :: rCoverStd
		real                    :: rRgMin
		real                    :: rRgMax
		real                    :: rRgAvg
		real                    :: rRgStd
		real                    :: rRainMin
		real                    :: rRainMax
		real                    :: rRainAvg
		real                    :: rRainStd
		real                    :: rRainSum
		real                    :: rRnMin
		real                    :: rRnMax
		real                    :: rRnAvg
		real                    :: rRnStd
		real                    :: rUstarMin
		real                    :: rUstarMax
		real                    :: rUstarAvg
		real                    :: rUstarStd
		real                    :: rH0Min
		real                    :: rH0Max
		real                    :: rH0Avg
		real                    :: rH0Std
		real                    :: rHLMMin
		real                    :: rHLMMax
		real                    :: rHLMAvg
		real                    :: rHLMStd
		integer					:: iNumCriticalInvalid
		integer					:: iNumTotalInvalid
		integer					:: iNumDataInYear
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Count years present in data set
		iNumData = SIZE(this % ivTimeStamp)
		call UnpackTime(this % ivTimeStamp(1), iYearMin, iMonth, iDay, iHour, iMinute, iSecond)
		call UnpackTime(this % ivTimeStamp(iNumData), iYearMax, iMonth, iDay, iHour, iMinute, iSecond)
		
		! Iterate over available years
		write(11, &
			"(" // &
				"' Year, Tot.Avail, Crit.Avail, Vel.Avail, Dir.Avail, Temp.Avail, RelH.Avail, " // &
				"Prec.Avail, Cover.Avail, Rg.Avail, Rn.Avail, Ustar.Avail, H0.Avail, HLM.Avail, " // &
				"Vel.Min, Vel.Mean, Vel.Max, Vel.StdDev, " // &
				"Temp.Min, Temp.Mean, Temp.Max, Temp.StdDev, " // &
				"RelH.Min, RelH.Mean, RelH.Max, RelH.StdDev, " // &
				"Prec.Min, Prec.Total, Prec.Max, Prec.StdDev, " // &
				"Cover.Min, Cover.Mean, Cover.Max, Cover.StdDev, " // &
				"Rg.Min, Rg.Mean, Rg.Max, Rg.StdDev, " // &
				"Rn.Min, Rn.Mean, Rn.Max, Rn.StdDev, " // &
				"U.star.Min, U.star.Mean, U.star.Max, U.star.StdDev, " // &
				"H0.Min, H0.Mean, H0.Max, H0.StdDev, " // &
				"zL.Min, zL.Mean, zL.Max, zL.StdDev'" // &
			")" &
		)
		do iYear = iYearMin, iYearMax
		
			! Search all data rows in current year
			call PackTime(iYearStart, iYear,    1, 1, 0, 0, 0)
			call PackTime(iYearNext, iYear + 1, 1, 1, 0, 0, 0)
			ivPos = MINLOC(ABS(this % ivTimeStamp - iYearStart))
			iTimeFrom = ivPos(1)
			ivPos = MINLOC(ABS(this % ivTimeStamp - iYearNext))
			iTimeTo = ivPos(1) - 1
			if(iTimeTo < iTimeFrom) cycle
			iNumDataInYear = (this % ivTimeStamp(iTimeTo) - this % ivTimeStamp(iTimeFrom)) / this % iDeltaTime + 1
		
			! Count invalid data for each column separately
			iIndexMax = SIZE(this % ivTimeStamp(iTimeFrom:iTimeTo))
			iInvalidVel   = COUNT(ISNAN(this % rvVel(iTimeFrom:iTimeTo)))
			iInvalidDir   = COUNT(ISNAN(this % rvDir(iTimeFrom:iTimeTo)))
			iInvalidTemp  = COUNT(ISNAN(this % rvTemp(iTimeFrom:iTimeTo)))
			iInvalidUrel  = COUNT(ISNAN(this % rvUrel(iTimeFrom:iTimeTo)))
			iInvalidCover = COUNT(ISNAN(this % rvCover(iTimeFrom:iTimeTo)))
			iInvalidRg    = COUNT(ISNAN(this % rvRg(iTimeFrom:iTimeTo)))
			iInvalidRain  = COUNT(ISNAN(this % rvRain(iTimeFrom:iTimeTo)))
			iInvalidRn    = COUNT(ISNAN(this % rvRn(iTimeFrom:iTimeTo)))
			iInvalidUstar = COUNT(ISNAN(this % rvUstar(iTimeFrom:iTimeTo)))
			iInvalidH0    = COUNT(ISNAN(this % rvH0(iTimeFrom:iTimeTo)))
			iInvalidHLM   = COUNT(ISNAN(this % rvHLM(iTimeFrom:iTimeTo)))
			
			! Count number of lines containing at least an invalid value
			iNumCriticalInvalid = 0
			iNumTotalInvalid    = 0
			do i = iTimeFrom, iTimeTo
				if( &
					ISNAN(this % rvVel(i)) .or. &
					ISNAN(this % rvDir(i)) .or. &
					ISNAN(this % rvTemp(i)) .or. &
					ISNAN(this % rvUrel(i)) &
				) iNumCriticalInvalid = iNumCriticalInvalid + 1
				if( &
					ISNAN(this % rvVel(i)) .or. &
					ISNAN(this % rvDir(i)) .or. &
					ISNAN(this % rvTemp(i)) .or. &
					ISNAN(this % rvUrel(i)) .or. &
					(iInvalidCover < iNumDataInYear .and. ISNAN(this % rvCover(i))) .or. &
					(iInvalidRain  < iNumDataInYear .and. ISNAN(this % rvRain(i))) .or. &
					(iInvalidRg    < iNumDataInYear .and. ISNAN(this % rvRg(i))) .or. &
					(iInvalidRn    < iNumDataInYear .and. ISNAN(this % rvRn(i))) .or. &
					(iInvalidUstar < iNumDataInYear .and. ISNAN(this % rvUstar(i))) .or. &
					(iInvalidH0    < iNumDataInYear .and. ISNAN(this % rvH0(i))) .or. &
					(iInvalidHLM   < iNumDataInYear .and. ISNAN(this % rvHLM(i))) &
				) iNumTotalInvalid = iNumTotalInvalid + 1
			end do
		
			! Build and save statistical indicators for the variables as read
			call BasicStat(this % rvVel(iTimeFrom:iTimeTo),   rVelMin,   rVelMax,   rVelAvg,   rVelStd)
			call BasicStat(this % rvTemp(iTimeFrom:iTimeTo),  rTempMin,  rTempMax,  rTempAvg,  rTempStd)
			call BasicStat(this % rvUrel(iTimeFrom:iTimeTo),  rUrelMin,  rUrelMax,  rUrelAvg,  rUrelStd)
			call BasicStat(this % rvCover(iTimeFrom:iTimeTo), rCoverMin, rCoverMax, rCoverAvg, rCoverStd)
			call BasicStat(this % rvRg(iTimeFrom:iTimeTo),    rRgMin,    rRgMax,    rRgAvg,    rRgStd)
			call BasicStat(this % rvRain(iTimeFrom:iTimeTo),  rRainMin,  rRainMax,  rRainAvg,  rRainStd)
			rRainSum = SUM(this % rvRain(iTimeFrom:iTimeTo), MASK=.not.ISNAN(this % rvRain(iTimeFrom:iTimeTo)))
			call BasicStat(this % rvRn(iTimeFrom:iTimeTo),    rRnMin,    rRnMax,    rRnAvg,    rRnStd)
			call BasicStat(this % rvUstar(iTimeFrom:iTimeTo), rUstarMin, rUstarMax, rUstarAvg, rUstarStd)
			call BasicStat(this % rvH0(iTimeFrom:iTimeTo),    rH0Min,    rH0Max,    rH0Avg,    rH0Std)
			call BasicStat(this % rvHLM(iTimeFrom:iTimeTo),   rHLMMin,   rHLMMax,   rHLMAvg,   rHLMStd)
			
			write(11, "(1x,i4,13(',',f6.2),40(',',f9.3))") &
				iYear, &
				100.0 * FLOAT(iNumDataInYear - iNumTotalInvalid) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iNumCriticalInvalid) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidVel) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidDir) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidTemp) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidUrel) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidRain) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidCover) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidRg) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidRn) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidUstar) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidH0) / FLOAT(iNumDataInYear), &
				100.0 * FLOAT(iNumDataInYear - iInvalidHLM) / FLOAT(iNumDataInYear), &
				rVelMin, rVelAvg, rVelMax, rVelStd, &
				rTempMin, rTempAvg, rTempMax, rTempStd, &
				rUrelMin, rUrelAvg, rUrelMax, rUrelStd, &
				rRainMin, rRainSum, rRainMax, rRainStd, &
				rCoverMin, rCoverAvg, rCoverMax, rCoverStd, &
				rRgMin, rRgAvg, rRgMax, rRgStd, &
				rRnMin, rRnAvg, rRnMax, rRnStd, &
				rUstarMin, rUstarAvg, rUstarMax, rUstarStd, &
				rH0Min, rH0Avg, rH0Max, rH0Std, &
				rHLMMin, rHLMAvg, rHLMMax, rHLMStd
		
		end do
			
    end function DumpBasicStatByYear

end module st_files
