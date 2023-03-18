! ST_Me - Main program of Servizi Territorio srl's met processor
!
! Copyright 2017 by Servizi Territorio srl
!                   All rights reserved
!
! Written by: Mauri Favaron
!
program ST_Me

    use abl_physics
    use calendar
    use st_files
    use routines
    use st_sodar_data
    use profiles
    use statistics
    use logging
    use strings
    
    implicit none
    
    ! Locals
    integer				:: iRetCode
    type(FileNamesType)	:: tFiles
    type(DataSetType)	:: tDataSet
	character(len=256)	:: sInputFile
	character(len=256)	:: sSodarFile
	character(len=32)	:: sCurDateTime
	character(len=16)	:: sProcessing
	character(len=2)	:: sBuffer
    integer             :: iDay
    integer             :: iMonth
    integer             :: iYear
    integer             :: iHour
    integer             :: iMinute
    integer             :: iSecond
    integer             :: iCurDay
    integer             :: iCurMonth
    integer             :: iCurYear
    integer             :: iCurHour
    integer             :: iCurMinute
    integer             :: iCurSecond
    real                :: rHour
    real                :: rVel
    real                :: rDir
    real                :: rTemp, rTa
    real                :: rUrel
    real                :: rPa
    real                :: rCover
    real                :: rRg
    real                :: rRain
    real                :: rRn
    real                :: rUstar
    real                :: rH0
    real                :: rHLM
    logical             :: lFirstDateAssigned
    integer             :: iBaseTime
    integer				:: iSodarDateStart
    integer             :: iTime
    integer             :: iIndex
    integer             :: i
    integer             :: iDayInYear
    real				:: rTimeZone, rSunRise, rSunSet, rSinPsi, rAlbedo
    real, dimension(2)	:: rvSunRiseSet
    logical				:: lEstimatedCover
    real				:: rTempUs, rTempTs, rTempH0
    real				:: rRc, rZiMec, rZiConv
    real, dimension(:), allocatable	:: rvUstar, rvH0, rvHLM
    character(len=32)	:: sDateTime
    character(len=512)	:: sData
	real, dimension(60)	:: rvU, rvV, rvTemp2
	integer				:: iHemisphere
	real				:: rTscale, rL
	real				:: rSunElevThreshold
	real				:: rRg3, rRg4, rKt3, rKt4, rKt
	integer				:: iNumIter
	real				:: rErr
	integer				:: m
	logical				:: lScientific
	logical				:: lCalmet
	logical				:: lCalmet_00
	logical				:: lCalpuffIsc
	logical				:: lCalpuffCtdm6
	logical				:: lCalpuffCtdm7
	logical				:: lAermet
	logical				:: lGral
	logical				:: lAustal2000
	logical				:: lTinyDisp
	logical				:: lAnyProcessing

    ! Service constants (change as appropriate)
    real, parameter		:: DEFAULT_CLOUD_COVER_CLEAR    = 0.1	! Used when rain information is present but zero (no rain)
    real, parameter		:: DEFAULT_CLOUD_COVER_NO_RAIN  = 0.0	! Used when rain information is missing (that is, not interesting)
    real, parameter		:: DEFAULT_CLOUD_COVER_RAIN     = 0.99	! Used when rain information is present and positive (raining)
	real, parameter 	:: THRESHOLD_USTAR              = 0.1	! m/s
	real, parameter 	:: TURBIDITY_MINIMUM            = 0.1	! Corresponds to very dusty sky, so to slightly underestimate incoming rad
	real, parameter 	:: TURBIDITY_MAXIMUM            = 1.0	! Corresponds to very clean sky, so to slightly overestimate incoming rad
    real, parameter		:: TURBIDITY_ACCURACY           = 1.e-3
    integer, parameter	:: MAXIMUM_TURBIDITY_ITERATIONS = 40
    integer				:: DAYS_RADIUS                  = 15	! One month typical days used in preliminary gap filling
    integer, parameter	:: PROCESSING_SCIENTIFIC		=  1
    integer, parameter	:: PROCESSING_CALMET			=  2
    integer, parameter	:: PROCESSING_CALPUFF_ISC		=  4
    integer, parameter	:: PROCESSING_CALPUFF_CTDM		=  8
    integer, parameter	:: PROCESSING_AERMET			= 16
    
    ! Get input parameters
    if(COMMAND_ARGUMENT_COUNT() /= 2 .and. COMMAND_ARGUMENT_COUNT() /= 4) then
        print *,'st_me - ST meteorological processor'
        print *
        print *,'Copyright 2018 by Servizi Territorio srl'
        print *,'                  All rights reserved'
        print *
        print *,'Usage:'
        print *
        print *,'  ./st_me <PreMet_V2.0_File> <Processing> [<SODAR_File> <SODAR_Date_Start>]'
        print *
        print *,'where <Processing> is a blank-less string containing the following'
        print *,'names (case sensitive):'
        print *
        print *,'  "scientific"    : Generate QA scientific output and stop there regardless of any other option'
        print *,'  "calmet"        : Generate meteorogical fields'
        print *,'  "cal_00"        : Generate meteorogical fields converting any itime stap to UTC'
        print *,'  "calpuff_isc"   : Generate ISC extended meteo input for Calpuff'
        print *,'  "calpuff_ctdm6" : Generate CTDM extended input for Calpuff 6'
        print *,'  "calpuff_ctdm7" : Generate CTDM extended input for Calpuff 7'
        print *,'  "aermet"        : Generate input for AERMET'
        print *,'  "gral"          : Generate input for GRAL, simple form'
        print *,'  "austal_2000"   : Generate input for AUSTAL-2000'
        print *,'  "tinydisp"      : Generate input for TinyDisp'
        print *,' '
        print *,'and'
        print *,' '
        print *,'  <SODAR_Date_Start> = 0 (time stamp on avg period beginning) or 1 (end period)'
        print *,' '
        print *,'Notice only Calpuff 7 form for CTDM files allows sub-hourly data files.'
        stop
    end if
    call GET_COMMAND_ARGUMENT(1, sInputFile)
    call GET_COMMAND_ARGUMENT(2, sProcessing)
    call ToLower(sProcessing)
    lScientific   = index(sProcessing, 'scientific') > 0
    lCalmet       = index(sProcessing, 'calmet') > 0
    lCalmet_00    = index(sProcessing, 'cal_00') > 0
    lCalpuffIsc   = index(sProcessing, 'calpuff_isc') > 0
    lCalpuffCtdm6 = index(sProcessing, 'calpuff_ctdm6') > 0
    lCalpuffCtdm7 = index(sProcessing, 'calpuff_ctdm7') > 0
    lAermet       = index(sProcessing, 'aermet') > 0
    lGral         = index(sProcessing, 'gral') > 0
    lAustal2000   = index(sProcessing, 'austal_2000') > 0
    lTinyDisp     = index(sProcessing, 'tinydisp') > 0
    lAnyProcessing = any([ &
		lScientific, &
		lCalmet, &
		lCalmet_00, &
		lCalpuffIsc, &
		lCalpuffCtdm6, &
		lCalpuffCtdm7, &
		lAermet, &
		lGral, &
		lAustal2000, &
		lTinyDisp &
    ])
    if(COMMAND_ARGUMENT_COUNT() == 4) then
		call GET_COMMAND_ARGUMENT(3, sSodarFile)
		call GET_COMMAND_ARGUMENT(4, sBuffer)
		read(sBuffer, *, iostat=iRetCode) iSodarDateStart
    else
		sSodarFile = " "
    end if
    
    ! ***************************************************************************
    ! * Gather data, perform basic QC and fill gaps in non-estimable quantities *
    ! ***************************************************************************
    
    ! Obtain filename without extension
    call tFiles % generate(sInputFile)
    if(.not. tFiles % validate()) then
		print *,"st_me:: error: Input file is likely missing, or other file name conflict"
		stop
    end if
    print *, "st_me:: info: Standard file names generated"
    call tFiles % setSodar(sSodarFile)
    if(.not. tFiles % validate()) then
		print *,"st_me:: error: Input file is likely missing, or other file name conflict"
		stop
    end if
    print *, "st_me:: info: SODAR file added to standard file names"
    
    ! Start logging
    iRetCode = logStart(tFiles % sLogFile)
    if(iRetCode /= 0) then
		print *,"st_me:: error: Logging function not started; return code = ", iRetCode
		stop
    end if
    print *, "st_me:: info: Logging started"
    
    ! Read data in standard form (in case of error, give the user
    ! some indication); as standard-form data are only hourly, there
    ! is no need to check explicitly for time stamps to be multiples
    ! of one hour.
    iRetCode = tDataSet % populate(tFiles, iSodarDateStart)
    if(iRetCode /= 0) then
		print *,"st_me:: error: Invalid value(s) in data set; return code = ", iRetCode
		call logError("Invalid data set")
    end if
    print *, "st_me:: info: Data read with no reported error"
    call logInfo("Data read with no reported error")
    
    ! Check the minimum information requirement
    iRetCode = tDataSet % checkUseability()
    if(iRetCode /= 0) then
		print *,"st_me:: error: Less than 50% of valid data for VEL, DIR, TEMP or RELH"
		call logError("Data set is formally valid, but contains not enough valid data to go on")
    end if
    print *, "st_me:: info: Number of data enough to proceed"
	call logInfo("Number of valid data is enough to go on")
    
    ! Start report, adding a summary of data set read to it
    open(11, file = tFiles % sDiaReport, status='unknown', action='write')
    iRetCode = tDataSet % report(tFiles)
    if(iRetCode /= 0) then
		print *,"st_me:: error: Invalid value(s) in reporting"
		call logError("Problems starting processing report")
    end if
    print *, "st_me:: info: Processing report started"
	call logInfo("Processing report started")
    
    ! Write yearly statistics
    iRetCode = tDataSet % reportYear()
    if(iRetCode /= 0) then
		print *,"st_me:: error: Invalid value(s) in yearly reporting"
		call logError("Problems starting yearly report")
    end if
    print *, "st_me:: info: Yearly processing report started"
	call logInfo("Yearly report started")

    ! Write "before processing" wind rose
    call tDataSet % printRose(tFiles % sPreWindRoseFile)
    print *, "st_me:: info: Pre-processing wind rose generated"
	call logInfo("Pre-processing wind rose generated")
    
    ! Write "before processing" data availability report
    call tDataSet % writeDailyAvail(tFiles)
    print *, "st_me:: info: Daily availability report generated"
	call logInfo("Daily availability report generated")
    
    ! Stop here if data check only has been selected
    if(.not.lAnyProcessing) then
    	close(11)
    	print *, &
    		"st_me:: info: Diagnostic-only run completed, terminating execution - Data found: ", &
    		SIZE(tDataSet % ivTimeStamp)
		call logInfo("Diagnostic-only run completed")
		iRetCode = logStop()
    	stop
    end if
    
    ! Fill gaps in wind speed and direction based on typical day
    iRetCode = WindGaps( &
    	tDataSet % ivTimeStamp, &
    	tDataSet % rvVel, &
    	tDataSet % rvDir, &
    	tDataSet % iDeltaTime, DAYS_RADIUS, &
    	tDataSet % lvOriginalWind &
    )
    if(iRetCode /= 0) then
        print *,'st_me: error: Impossible to statistically fill gaps in wind - Error code = ',iRetCode
		call logError("Invalid parameters passed to typical-day-based wind gap filler")
    end if
    print *, "st_me:: info: Statistical (preliminary) wind gap filling performed"
	call logInfo("Typical day based gap filling of wind data completed")
    
    ! Fill gaps in temperature based on typical day
    iRetCode = FillGaps( &
    	tDataSet % ivTimeStamp, &
    	tDataSet % rvTemp, &
    	tDataSet % iDeltaTime, DAYS_RADIUS, &
    	tDataSet % lvOriginalTemp &
    )
    if(iRetCode /= 0) then
        print *,'st_me: error: Impossible to statistically fill gaps in temperature - Error code = ',iRetCode
		call logError("Invalid parameters passed to typical-day-based temperature gap filler")
    end if
    print *, "st_me:: info: Statistical (preliminary) temperature gap filling performed"
	call logInfo("Typical day based gap filling of temperature data completed")
    
    ! Fill gaps in relative humidity based on typical day
    iRetCode = FillGaps( &
    	tDataSet % ivTimeStamp, &
    	tDataSet % rvUrel, &
    	tDataSet % iDeltaTime, DAYS_RADIUS, &
    	tDataSet % lvOriginalRelH &
    )
    if(iRetCode /= 0) then
        print *,'st_me: error: Impossible to statistically fill gaps in relative humidity - Error code = ',iRetCode
		call logError("Invalid parameters passed to typical-day-based humidity gap filler")
    end if
    print *, "st_me:: info: Statistical (preliminary) relative humidity gap filling performed"
	call logInfo("Typical day based gap filling of relative humidity data completed")
    
    ! Write "after-statistical-gap-filling" data availability report
    call tDataSet % writeDailyPostAvail(tFiles)
    print *, "st_me:: info: Daily post-stat-filling availability report generated"
	call logInfo("Daily post-typical-day-filling availability report generated")
    
    ! Fill gaps in wind speed and direction
    if(any(ISNAN(tDataSet % rvVel)) .or. any(ISNAN(tDataSet % rvDir))) then
	    iRetCode = FillWindGaps( &
	    	FLOAT(tDataSet % ivTimeStamp), tDataSet % rvVel, tDataSet % rvDir, tDataSet % lvOriginalWind &
	    )
	    if(iRetCode /= 0) then
	        print *,'st_me: error: Impossible to fill gaps in wind - Error code = ',iRetCode
			call logError("Impossible to fill systematic gaps in wind")
	    end if
	    print *, "st_me:: info: Wind gap filling performed - Residual gaps: ", &
	    	count(ISNAN(tDataSet % rvVel) .or. ISNAN(tDataSet % rvDir))
	else
	    print *, "st_me:: info: Wind gap filling skipped - Nothing to do"
		call logInfo("No systematic gaps to fill in wind: all valid data")
	end if
    
    ! Coerce wind speed to original range
    where(tDataSet % rvVel > 60.0)
        tDataSet % rvVel = 60.0
    end where
    print *, "st_me:: info: After-filling wind speed clipping to max allowed value done"
	call logInfo("Post-fill wind speed clipping to [0,60] m/s interval")
    
    ! Write "after processing" wind rose
    call tDataSet % printRose(tFiles % sPostWindRoseFile)
    print *, "st_me:: info: Post-processing wind rose generated"
	call logInfo("Post-processing wind rose generated")
    
    ! Fill gaps in temperature
    if(any(ISNAN(tDataSet % rvTemp))) then
	    iRetCode = FillScalarGaps(FLOAT(tDataSet % ivTimeStamp), tDataSet % rvTemp, tDataSet % lvOriginalTemp)
	    if(iRetCode /= 0) then
	        print *,'st_me: error: Impossible to fill gaps in temperature - Error code = ',iRetCode
			call logError("Impossible to fill systematic gaps in temperature")
	    end if
	    print *, "st_me:: info: Temperature gap filling performed - Residual gaps: ", &
	    	count(ISNAN(tDataSet % rvTemp))
	else
	    print *, "st_me:: info: Temperature gap filling skipped - Nothing to do"
		call logInfo("No systematic gaps to fill in temperature: all valid data")
	end if
    
    ! Coerce temperature to sensible range
    where(tDataSet % rvTemp < -40.0 .or. tDataSet % rvTemp > 60.0)
    	tDataSet % lvOriginalTemp = .false.
    end where
    tDataSet % rvTemp = MIN(MAX(tDataSet % rvTemp,-40.),+60.)
    print *, "st_me:: info: After-filling temperature clipping to allowed range done"
	call logInfo("Post-fill temperature clipping to [-40,60] °C interval")
    
    ! Fill gaps in relative humidity
    if(any(ISNAN(tDataSet % rvUrel))) then
	    iRetCode = FillScalarGaps(FLOAT(tDataSet % ivTimeStamp), tDataSet % rvUrel, tDataSet % lvOriginalRelH)
	    if(iRetCode /= 0) then
	        print *,'st_me: error: Impossible to fill gaps in relative humidity - Error code = ',iRetCode
			call logError("Impossible to fill systematic gaps in relative humidity")
	    end if
	    print *, "st_me:: info: Relative humidity gap filling performed - Residual gaps: ", &
	    	count(ISNAN(tDataSet % rvUrel))
	else
	    print *, "st_me:: info: Relative humidity gap filling skipped - Nothing to do"
		call logInfo("No systematic gaps to fill in relative humidity: all valid data")
	end if
    
    ! Coerce relative humidity in sensible range
    where(tDataSet % rvUrel < 0.0 .or. tDataSet % rvUrel > 100.0)
    	tDataSet % lvOriginalRelH = .false.
    end where
    tDataSet % rvUrel = MIN(MAX(tDataSet % rvUrel,10.),100.)
    print *, "st_me:: info: After-filling relative humidity clipping to allowed range done"
	call logInfo("Post-fill relative humidity clipping to [0,100] % interval")
    
    ! Write report about data after processing is made
    iRetCode = tDataSet % reportTwo()
    if(iRetCode /= 0) then
		print *,"st_me:: error: Invalid value(s) in reporting"
		call logError("Problems during after-processing report generation")
    end if
    print *, "st_me:: info: Processing report continued with post-filling statistics"
	call logInfo("After-processing report appended")
    
    !***********************
    !* Physical processing *
    !***********************
    
    print *, "st_me:: info: Start of physical processing"
	call logInfo("*** Start of physical processing ***")
    allocate( &
		rvUstar(SIZE(tDataSet % ivTimeStamp)), &
		rvH0(SIZE(tDataSet % ivTimeStamp)), &
		rvHLM(SIZE(tDataSet % ivTimeStamp)), &
		stat=iRetCode &
	)
    if(iRetCode /= 0) then
		print *,"st_me:: error: Failed allocating temporary workspace"
		call logError("Failed allocating temporary storage")
    end if
    rZiConv = 0.
    iRetCode = updateState(LOG_S_METPRO)
    if(iRetCode /= 0) then
		call logError("Invalid state transition")
    end if
    do i = 1, SIZE(tDataSet % ivTimeStamp)
		
		! Get time stamp at official value (to help tracking events in data files;
		! true time stamp at mid of averagin interval will be computed immediately after)
		call UnpackTime( &
			tDataSet % ivTimeStamp(i), &
			iYear, iMonth, iDay, iHour, iMinute, iSecond &
		)
		iRetCode = updateTime(iYear, iMonth, iDay, iHour, iMinute, iSecond)
		
		! Get time stamp at *mid* of averaging interval
		call UnpackTime( &
			tDataSet % ivTimeStamp(i) + tDataSet % iDeltaTime/2, &
			iYear, iMonth, iDay, iHour, iMinute, iSecond &
		)
		rHour = iHour + iMinute / 60.0 + iSecond / 3600.0
		
		! Assume missing rain values are zero (in dispersion modeling this assumption
		! is conservative with respect to ground concentrations: no depletion by
		! wet deposition occurs in this case
		if(ISNAN(tDataSet % rvRain(i))) tDataSet % rvRain(i) = 0.
		
		! Clip speed to the minimum possible resolution (actually defined
		! as the minimum speed detectable by a Metek uSonic-3 anemometer)
		! and check whether estimation is possible.
		!
		! In old "Yellow W" chain this test was considered "just defensive
		! programming". Now, given the more liberal time management in
		! version 2, the test is fully necessary - on head and tail of data set.
		if(.not.ISNAN(tDataSet % rvVel(i)) .and. .not.ISNAN(tDataSet % rvDir(i))) then
			tDataSet % rvVel(i) = MAX(tDataSet % rvVel(i),0.01)
			
			! Get astronomical indicators
			iDayInYear = J_DAY(iYear,iMonth,iDay)
			rTimeZone = tDataSet % iTimeZone
			rvSunRiseSet = SunRiseSunSet(iYear, iMonth, iDay, tDataSet % rLat, tDataSet % rLon, tDataSet % iTimeZone)
			rSunRise = rvSunRiseSet(1)
			rSunSet  = rvSunRiseSet(2)
			rSinPsi = SinSolarElevation( &
				iYear,iMonth,iDay,iHour,iMinute,iSecond, &
				tDataSet % rLat, tDataSet % rLon, tDataSet % iTimeZone, &
				tDataSet % iDeltaTime &
			)
    	    rAlbedo = tDataSet % rAlbedo
    	    
    	    ! If precipitation is missing, assign it a 0 (conservative)
    	    if(ISNAN(tDataSet % rvRain(i))) tDataSet % rvRain(i) = 0
		
			! Estimate cloud cover, if missing
            lEstimatedCover = .false.
			if(ISNAN(tDataSet % rvCover(i))) then
				lEstimatedCover = .true.
				if(.not.ISNAN(tDataSet % rvRain(i))) then
					if(tDataSet % rvRain(i) > 0.) then
						tDataSet % rvCover(i) = DEFAULT_CLOUD_COVER_RAIN
					else
						tDataSet % rvCover(i) = DEFAULT_CLOUD_COVER_CLEAR
					end if
				else
					tDataSet % rvCover(i) = DEFAULT_CLOUD_COVER_NO_RAIN
				end if
			end if
			
            ! Build a first approximation of global radiation, if necessary,
            ! using MPDA method
		    if(rHour < rSunRise .or. rSunSet < rHour) then
				tDataSet % rvRg(i) = 0.0
			elseif(ISNAN(tDataSet % rvRg(i))) then
				tDataSet % rvRg(i) = SUN_RAD2(tDataSet % rvCover(i), rSinPsi)
			end if
			
			! In case of estimated cloud cover, refine it
			! (will be used to estimate net radiation)
			if(lEstimatedCover) then
				if(tDataSet % rvRg(i) > 0.) then
					tDataSet % rvCover(i) = CLOUD_Rg(tDataSet % rvRg(i), rSinPsi)	
				end if
			end if
			
            ! Get temperature and relative humidity, and estimate pressure
			rTa   = tDataSet % rvTemp(i) + 273.15
			rUrel = tDataSet % rvUrel(i)
			rPa   = 1013. * EXP(-0.0342 * tDataSet % rHeight/rTa)
			tDataSet % rvP(i) = rPa
			
			! Estimate dew point
			tDataSet % rvTdew(i) = DewPoint(tDataSet % rvTemp(i), tDataSet % rvUrel(i))
			
			! Estimate Brunt-Vaisala frequency at anemometer level
			tDataSet % rvN(i) = BruntVaisala(tDataSet % rvTemp(i), tDataSet % rZr)

            ! Estimate net radiation by MPDA method
            if(ISNAN(tDataSet % rvRn(i))) then
				rRn = R_NET_D( &
					tDataSet % iLandUse, tDataSet % rLat, tDataSet % rLon, FLOAT(tDataSet % iTimeZone), &
					rHour, iDayInYear, rAlbedo, rTa, tDataSet % rvRg(i), tDataSet % rvCover(i))
				if(rRn < 0.) &
					rRn = R_NET_N(tDataSet % rvCover(i), tDataSet % rZ0, tDataSet % rZr, tDataSet % rvVel(i), rTa)
				tDataSet % rvRn(i) = rRn
			end if

            ! Estimate stability category by ENEL method
			tDataSet % ivIstab(i) = ISTAB7( &
				rHour, rSunRise, rSunSet, &
				tDataSet % rvVel(i), tDataSet % rvRg(i), tDataSet % rvRn(i) &
			)

            ! Estimate soil heat flux
			tDataSet % rvG0(i) = SOIL_HEAT_FLUX(rHour, MAX(tDataSet % rvRn(i), -400.))

            ! Reconstruct Surface Layer parameters
            call PBL_33( &
				tDataSet % iLandUse, tDataSet % rZ0, tDataSet % rD, tDataSet % rZr, &
				tDataSet % rvVel(i), rTa, tDataSet % rvRn(i), tDataSet % rvCover(i), &
				rTempUs, rTempTs, rTempH0, rvHLM(i) &
			)
            tDataSet % rvHLM(i) = tDataSet % rvHLM(i) / tDataSet % rZr
			call SURFACE_PT( &
				tDataSet % rZ0, rTa, rUrel, rPa, &
				tDataSet % rvVel(i), tDataSet % rvRn(i), tDataSet % rvG0(i), &
				rvH0(i), tDataSet % rvHE(i), rvUstar(i), &
				tDataSet % rvTstar(i), rvHLM(i) &
			)
			rvHlm(i) = tDataSet % rZr * rvHlm(i)	! Now it really is zr/L
			if(ISNAN(tDataSet % rvUstar(i))) tDataSet % rvUstar(i) = rvUstar(i)
			if(ISNAN(tDataSet % rvH0(i)))    tDataSet % rvH0(i)    = rvH0(i)
			if(ISNAN(tDataSet % rvHLM(i)))   tDataSet % rvHLM(i)   = rvHLM(i)
			
			! Convert stability parameter to Obukhov length
			if(ABS(tDataSet % rvHLM(i)) > 1.e-3) then
				tDataSet % rvL(i) = tDataSet % rZr / tDataSet % rvHLM(i)
			else
				if(tDataSet % rvL(i) > 0.) then
					tDataSet % rvL(i) =  9000.0
				else
					tDataSet % rvL(i) = -9000.0
				end if
			end if
			
			! Estimate stability category using surface roughness and Obukhov length
			tDataSet % ivLstab(i) = LSTAB(tDataSet % rvL(i), tDataSet % rZ0)
			
			! Estimate backwards Obukhov length from the "turbulent" stability category just computed
			call STAB2L(tDataSet % ivLstab(i), tDataSet % rZ0, tDataSet % rvLalt1(i))
			tDataSet % rvLalt1(i) = tDataSet % rvLalt1(i) * tDataSet % rZr
			if(ABS(tDataSet % rvLalt1(i)) < 1.e-4) then
				if(tDataSet % rvLalt1(i) > 0.) then
					tDataSet % rvLalt1(i) =  1.e-4
				else
					tDataSet % rvLalt1(i) = -1.e-4
				end if
			end if
			
			! Estimate backwards Obukhov length from the "ENEL" stability category
			call STAB2L(tDataSet % ivIstab(i), tDataSet % rZ0, tDataSet % rvLalt2(i))
			tDataSet % rvLalt2(i) = tDataSet % rvLalt2(i) * tDataSet % rZr
			if(ABS(tDataSet % rvLalt2(i)) < 1.e-4) then
				if(tDataSet % rvLalt2(i) > 0.) then
					tDataSet % rvLalt2(i) =  1.e-4
				else
					tDataSet % rvLalt2(i) = -1.e-4
				end if
			end if
			
			! Estimate mixing height
			rRc    = RHOCP(rTa)
			rZiMec = 1330.*rvUstar(i)
			if(rHour > rSunRise .and. rHour < rSunSet) then
				rZiConv = MAX(rZiConv, 0.)
				rZiConv = HMIX_NEW(FLOAT(tDataSet % iDeltaTime),rvH0(i),rvUstar(i),rTa,rRc,rZiConv)
				tDataSet % rvZi(i) = MAX(rZiMec, rZiConv)
			else
				rZiConv = 0.
				tDataSet % rvZi(i) = StableZi( &
					tDataSet % rLat, &
					tDataSet % rvTemp(i), &
					tDataSet % rvH0(i), &
					tDataSet % rvUstar(i), &
					tDataSet % rvL(i), &
					tDataSet % rvN(i) &
				)
			end if
			
			! Compute the convective scale velocity
			if(rvH0(i) > 0. .and. ABS(tDataSet % rvZi(i)) <= 6000.) then
				tDataSet % rvWs(i) = (9.81/rTa * rvH0(i)/rRc * tDataSet % rvZi(i))**0.33333
			else
				tDataSet % rvWs(i) = 0.
			end if
			
			! Update daytime/nighttime indicator
			tDataSet % lvDiurnal(i) = (rHour > rSunRise) .and. (rHour < rSunSet)
			
			! Compute ordinary and saturation water pressure, and then precipitable water
			tDataSet % rvEa(i)   = E_SAT_1(tDataSet % rvTdew(i))
			tDataSet % rvEsat(i) = E_SAT_1(tDataSet % rvTemp(i))
			tDataSet % rvPrecWater(i) = PrecipitableWater(tDataSet % rvEsat(i), tDataSet % rvP(i))
			
			! Store solar position
			tDataSet % rvSinPsi(i) = rSinPsi
			tDataSet % rvPsi(i)    = ASIN(rSinPsi)
			
			! Compute rough and accurate approximation to solar radiation using ASCE method;
			! also, compute extraterrestrial radiation. These three values may be used
			! to validate global radiation measurements and MPDA estimates
			call UnpackTime( &
				tDataSet % ivTimeStamp(i), &
				iCurYear, iCurMonth, iCurDay, iCurHour, iCurMinute, iCurSecond &
			)
			write(sCurDateTime, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") &
				iCurYear, iCurMonth, iCurDay, iCurHour, iCurMinute, iCurSecond
			tDataSet % rvRa(i) = ExtraterrestrialRadiation( &
				sCurDateTime, &
				FLOAT(tDataSet % iDeltaTime), &
				tDataSet % rLat, &
				tDataSet % rLon, &
				FLOAT(tDataSet % iTimeZone) &
			)
			tDataSet % rvRg2(i) = ClearSkyRg_Simple(tDataSet % rvRa(i), tDataSet % rHeight)
			tDataSet % rvRg3(i) = ClearSkyRg_Accurate( &
				sCurDateTime, &
				FLOAT(tDataSet % iDeltaTime), &
				tDataSet % rLat, &
				tDataSet % rLon, &
				FLOAT(tDataSet % iTimeZone), &
				tDataSet % rvP(i), &
				tDataSet % rvTemp(i), &
				tDataSet % rvUrel(i), &
				TURBIDITY_MINIMUM &
			)
			tDataSet % rvRg4(i) = ClearSkyRg_Accurate( &
				sCurDateTime, &
				FLOAT(tDataSet % iDeltaTime), &
				tDataSet % rLat, &
				tDataSet % rLon, &
				FLOAT(tDataSet % iTimeZone), &
				tDataSet % rvP(i), &
				tDataSet % rvTemp(i), &
				tDataSet % rvUrel(i), &
				TURBIDITY_MAXIMUM &
			)
			
			! Roughly estimate the atmospheric turbidity coefficient needed to make MPDA / measured
			! radiation identical to ASCE's.
			if(tDataSet % lvDiurnal(i)) then
				if(tDataSet % rvRg(i) <= tDataSet % rvRg3(i)) then
					tDataSet % rvKt(i) = TURBIDITY_MAXIMUM
					tDataSet % rvKtErr(i) = 0.0
				elseif(tDataSet % rvRg(i) >= tDataSet % rvRg4(i)) then
					tDataSet % rvKt(i) = TURBIDITY_MINIMUM
					tDataSet % rvKtErr(i) = 0.0
				else
					! Neither of the two saturation cases occurs: we're in position to
					! see an intermediate turbidity coefficient. This, incidentally,
					! corresponds to the "ideal" case.
					rRg3 = tDataSet % rvRg3(i) - tDataSet % rvRg(i)
					rRg4 = tDataSet % rvRg4(i) - tDataSet % rvRg(i)
					rKt3 = TURBIDITY_MINIMUM
					rKt4 = TURBIDITY_MAXIMUM
					rErr = HUGE(rErr)
					iNumIter = 0
					do while(rErr > TURBIDITY_ACCURACY .and. iNumIter <= MAXIMUM_TURBIDITY_ITERATIONS)
						rKt = rKt3 - rRg3 * (rKt4 - rKt3) / (rRg4 - rRg3)
						rRg = ClearSkyRg_Accurate( &
							sCurDateTime, &
							FLOAT(tDataSet % iDeltaTime), &
							tDataSet % rLat, &
							tDataSet % rLon, &
							FLOAT(tDataSet % iTimeZone), &
							tDataSet % rvP(i), &
							tDataSet % rvTemp(i), &
							tDataSet % rvUrel(i), &
							rKt &
						) - tDataSet % rvRg(i)
						if(rRg > 0.0) then
							rRg4 = rRg
							rKt4 = rKt
							rErr = ABS(rKt4 - rKt3)
						elseif(rRg < 0.0) then
							rRg3 = rRg
							rKt3 = rKt
							rErr = ABS(rKt4 - rKt3)
						else
							! This will never happen in practice, but in theory luck comes
							! from time to time (in this case, we found the root exactly)
							rErr = 0.
							rKt4 = rKt
							rKt3 = rKt
							exit
						end if
						iNumIter = iNumIter + 1
					end do
					tDataSet % rvKt(i)    = (rKt3 + rKt4) / 2.0
					tDataSet % rvKtErr(i) = rErr
				end if
			else
				! On night-time, the turbidity coefficient is not defined
				tDataSet % rvKt(i)    = -9999.9
				tDataSet % rvKtErr(i) = -9999.9
			end if
			
		end if
		
	end do
	iRetCode = updateState(LOG_S_INIT)
	if(iRetCode /= 0) then
		call logError("Invalid state transition")
    end if
    print *, "st_me:: info: Basic physical processing completed"
	call logInfo("Basic physical processing completed")
		
    do i = 1, SIZE(tDataSet % ivTimeStamp)
    
		! Estimate the short (grass) and tall (alfalfa) evapotranspirations
		! using ASCE reference equation
		tDataSet % rvETs(i) = ASCE_Evapotranspiration_Ref( &
			tDataSet % rvP(i), tDataSet % rvTemp(i), tDataSet % rvVel(i), tDataSet % rvRn(i), tDataSet % rvG0(i), &
			tDataSet % rvEsat(i), tDataSet % rvEa(i), tDataSet % rZr, ASCE_GRASS &
		)
		tDataSet % rvETt(i) = ASCE_Evapotranspiration_Ref( &
			tDataSet % rvP(i), tDataSet % rvTemp(i), tDataSet % rvVel(i), tDataSet % rvRn(i), tDataSet % rvG0(i), &
			tDataSet % rvEsat(i), tDataSet % rvEa(i), tDataSet % rZr, ASCE_ALFALFA &
		)

    end do
	deallocate(rvUstar, rvH0, rvHLM)
    print *, "st_me:: info: Evapotranspiration estimated"
	call logInfo("Evapotranspiration estimated")
    
    ! Report on data statistics after physical processing
    iRetCode = tDataSet % reportThree()
    if(iRetCode /= 0) then
		print *,"st_me:: error: Invalid value(s) in reporting"
		call logError("After-physical processing reporting failed")
    end if
    print *, "st_me:: info: Report after physical processing generated"
	call logInfo("Report after physical processing appended")
    
    ! Report on H0-derived stability
	iRetCode = tDataSet % reportH0Stability()
    print *, "st_me:: info: H0-derived stability reporting done; return code = ", iRetCode
	call logInfo("H0-derived stability reporting done")
    
    ! Report on stability categories, and produce stable/convective roses
	iRetCode = tDataSet % reportStability()
    print *, "st_me:: info: Stability reporting done; return code = ", iRetCode
	call logInfo("Stability reporting done")
    
	call tDataSet % printStbRoses(tFiles)
    print *, "st_me:: info: Categorical wind roses written"
	call logInfo("Categorical wind roses written")
    
    ! Report on typical days
    iRetCode = tDataSet % reportTypicalDays()
    print *, "st_me:: info: Monthly typical days computed and logged; return code = ", iRetCode
	call logInfo("Monthly typical days computed and logged")

    ! Report on directional means
    iRetCode = tDataSet % reportDirMean()
    print *, "st_me:: info: Monthly directional means computed and logged; return code = ", iRetCode
	call logInfo("Monthly directional means computed and logged")

    ! Another diagnostic data item is the ASCE net radiation,
	! for which we need to know the "cloudiness function" value.
	! This is computed using as "measured Rg" the MPDA estimate
	! (or the measurement, where it exists valid)
	if(ABS(tDataSet % rLat) < 27.0) then
		rSunElevThreshold = 0.5
	else
		! For very northern or southern sites, the default value of
		! solar elevation threshold is too large: it is then decreased
		! linearly; you may find a rationale for this on my lab notes
		rSunElevThreshold = 0.5 - 0.3*(ABS(tDataSet % rLat) - 27.0)/(90.0 - 27.0)
	end if
	iRetCode = Cloudiness( &
		tDataSet % rvPsi, tDataSet % rvRg, tDataSet % rvRg3, rSunElevThreshold, tDataSet % rvFcd &
	)
	if(iRetCode /= 0) then
		print *,"st_me:: error: Problem estimating cloudiness"
		call logError("Problem estimating cloudiness")
	end if
    print *, "st_me:: info: Cloudiness function estimated with solar elevation threshold", rSunElevThreshold
	call logInfo("Cloudiness function estimated")
	do i = 1, SIZE(tDataSet % ivTimeStamp)
		tDataSet % rvRn2(i) = NetRadiation( &
			tDataSet % rvRg3(i), &
			tDataSet % rAlbedo, &
			tDataSet % rvFcd(i), &
			tDataSet % rvEsat(i), &
			tDataSet % rvTemp(i) + 273.15 &
		)
	end do
    print *, "st_me:: info: ASCE net radiation estimated"
	call logInfo("ASCE net radiation estimated")
    
    call tDataSet % writeDailyReport(tFiles)
    print *, "st_me:: info: Daily Diagnostic Report generated"
	call logInfo("Daily diagnostic report generated")

	tDataSet % rvApparentLAI = SetApparentLAI( &
		tDataSet % ivTimeStamp, &
		tDataSet % iDeltaTime, &
		tDataSet % rvRn, &
		tDataSet % rvG0 &
	)
    print *, "st_me:: info: Apparent LAI estimate generated"
	call logInfo("Apparent leaf area index estimated")
    
    if(lCalmet .or. lCalmet_00 .or. lCalpuffCtdm6 .or. lCalpuffCtdm7) then
    
		! Build vertical profiles, incorporating SODAR data, if present
		! (they have been already read)
		call logInfo("Entering profile generation phase")
		iRetCode = updateState(LOG_S_METPROF)
		if(iRetCode /= 0) then
			call logError("Invalid state transition")
		end if
		do i = 1, SIZE(tDataSet % ivTimeStamp)
		
			! Get time stamp at official value (to help tracking events in data files;
			! true time stamp at mid of averagin interval will be computed immediately after)
			call UnpackTime( &
				tDataSet % ivTimeStamp(i), &
				iYear, iMonth, iDay, iHour, iMinute, iSecond &
			)
			iRetCode = updateTime(iYear, iMonth, iDay, iHour, iMinute, iSecond)
			
			! Check SODAR data have been read, and a SODAR profile exists for this hour:
			! if so, assume it as the standard profile
			if(tFiles % sSodarFile /= ' ' .and. tDataSet % ivSodarNumHeights(i) > 0) then
			
				! Get all SODAR data
				tDataSet % ivNumHeights(i) = tDataSet % ivSodarNumHeights(i)
				m = tDataSet % ivNumHeights(i)
				tDataSet % rvZ(1:tDataSet % ivSodarNumHeights(i)) = &
					tDataSet % rmSodarHeight(i,1:tDataSet % ivSodarNumHeights(i))
				tDataSet % rmOutU(i,1:m) = tDataSet % rmSodarVel(i,1:m) * sin(tDataSet % rmSodarDir(i,1:m)*3.1415927/180.)
				tDataSet % rmOutV(i,1:m) = tDataSet % rmSodarVel(i,1:m) * cos(tDataSet % rmSodarDir(i,1:m)*3.1415927/180.)
				
				! And now, RASS data: if they present they're used as such; otherwise, they're
				! estimated
				if(count(tDataSet % rmSodarTemp(i,1:m) > -9999.0) > 0) then
				
					! First of all we do an optimistic move, by transferring the profile we have
					! as if it is complete
					tDataSet % rmOutTemp(i,1:m) = tDataSet % rmSodarTemp(i,1:m)
					
					! Make all gaps evident, if any (gaps may exist, and in general are not rare on RASS profiles)
					where(isnan(tDataSet % rmOutTemp(i,1:m)))
						tDataSet % rmOutTemp(i,1:m) = -9999.9
					endwhere
					where(tDataSet % rmOutTemp(i,1:m) < -263.15)
						tDataSet % rmOutTemp(i,1:m) = -9999.9
					endwhere
					
					! If one or more gaps exist, then completion is necessary (Calpuff
					! does not tolerate gaps in temperature profiles). This is done
					! by estimating the temperature profile at SODAR measurement heights
					if(count(tDataSet % rmOutTemp(i,1:m) < -9990.0) > 0) then
					
						! Log event in case a SODAR exists, but no data have been found in this time
						if(tFiles % sSodarFile /= ' ') then
							call logWarning("Gaps found in RASS profile, using a modeled temp profile")
						end if
						
						! Ensure the reciprocal Monin-Obukhov length is not zero,
						! and then use it to compute the actual Monin-Obukhov length.
						rHLM = tDataSet % rvHLM(i)
						if(rHLM < 0. .and. rHLM >= -1.e-4) then
							rHLM = -1.e-4
						elseif(rHLM >= 0. .and. rHLM <= +1.e-4) then
							rHLM = +1.e-4
						end if
						rL = tDataSet % rZr / rHLM
						
						! Estimate profile
						rTscale = TSTAR_0(RHOCP(tDataSet % rvTemp(i) + 273.15),tDataSet % rvUstar(i), tDataSet % rvH0(i))
						iRetCode = TempProfile( &
							tDataSet % rmSodarHeight(i,1:m), &
							tDataSet % rZ0, &
							tDataSet % rZr, &
							tDataSet % rvTemp(i), &
							0.0098, &
							tDataSet % rvZi(i), &
							rTscale, &
							tDataSet % rvUstar(i), &
							rHLM, &
							tDataSet % rvH0(i), &
							rvTemp2(1:m) &
						)
						if(iRetCode /= 0) then
							print *,"st_me:: error: Failed building temperature profile - Ret.code = ", iRetCode
							stop
						end if
						tDataSet % rmOutTemp(i,1:m) = rvTemp2(1:m)
						
					end if
					
				else
				
					! Ensure the reciprocal Monin-Obukhov length is not zero,
					! and then use it to compute the actual Monin-Obukhov length.
					rHLM = tDataSet % rvHLM(i)
					if(rHLM < 0. .and. rHLM >= -1.e-4) then
						rHLM = -1.e-4
					elseif(rHLM >= 0. .and. rHLM <= +1.e-4) then
						rHLM = +1.e-4
					end if
					rL = tDataSet % rZr / rHLM
					
					! Estimate profile
					rTscale = TSTAR_0(RHOCP(tDataSet % rvTemp(i) + 273.15),tDataSet % rvUstar(i), tDataSet % rvH0(i))
					iRetCode = TempProfile( &
						tDataSet % rvZ(1:m), &
						tDataSet % rZ0, &
						tDataSet % rZr, &
						tDataSet % rvTemp(i), &
						0.0098, &
						tDataSet % rvZi(i), &
						rTscale, &
						tDataSet % rvUstar(i), &
						rHLM, &
						tDataSet % rvH0(i), &
						rvTemp2(1:m) &
					)
					if(iRetCode /= 0) then
						print *,"st_me:: error: Failed building temperature profile - Ret.code = ", iRetCode
						stop
					end if
					tDataSet % rmOutTemp(i,1:m) = rvTemp2(1:m)
				
				end if
				
			else
			
				! Log event in case a SODAR exists, but no data have been found in this time
				if(tFiles % sSodarFile /= ' ') then
					call logWarning("Modeled wind profile used, missing a measured one")
				end if
				
				! No SODAR profile available: assign standard heights, used later to estimate
				! a profile using modeling approach
				tDataSet % ivNumHeights(i) = 60
				tDataSet % rvZ(1:60) = [ (50.*i,i=1,60) ]
		
				! Ensure the reciprocal Monin-Obukhov length is not zero,
				! and then use it to compute the actual Monin-Obukhov length.
				rHLM = tDataSet % rvHLM(i)
				if(rHLM < 0. .and. rHLM >= -1.e-4) then
					rHLM = -1.e-4
				elseif(rHLM >= 0. .and. rHLM <= +1.e-4) then
					rHLM = +1.e-4
				end if
				rL = tDataSet % rZr / rHLM
			
				! Check wind speed to be non-zero
				rVel = MAX(tDataSet % rvVel(i), 0.01)
		
				! Build synthetic profiles
				if(tDataSet % rLat >= 0.) then
					iHemisphere = 1
				else
					iHemisphere = 0
				end if
				iRetCode = WindProfile( &
					iHemisphere, &
					tDataSet % rvZ(1:60), &
					tDataSet % rZr, &
					rVel, &
					tDataSet % rvDir(i), &
					tDataSet % rZ0, &
					tDataSet % rvZi(i), &
					tDataSet % rvUstar(i), &
					rHLM, &
					rvU(1:60), rvV(1:60) &
				)
				if(iRetCode /= 0) then
					print *,"st_me:: error: Failed building wind profile - Ret.code = ", iRetCode
					stop
				end if
				
				rTscale = TSTAR_0(RHOCP(tDataSet % rvTemp(i) + 273.15),tDataSet % rvUstar(i), tDataSet % rvH0(i))
				iRetCode = TempProfile( &
					tDataSet % rvZ(1:60), &
					tDataSet % rZ0, &
					tDataSet % rZr, &
					tDataSet % rvTemp(i), &
					0.0098, &
					tDataSet % rvZi(i), &
					rTscale, &
					tDataSet % rvUstar(i), &
					rHLM, &
					tDataSet % rvH0(i), &
					rvTemp2(1:60) &
				)
				if(iRetCode /= 0) then
					print *,"st_me:: error: Failed building temperature profile - Ret.code = ", iRetCode
					stop
				end if
			
				! Save the purely modeled profiles
				tDataSet % rmOutU(i,1:60)    = rvU(1:60)
				tDataSet % rmOutV(i,1:60)    = rvV(1:60)
				tDataSet % rmOutTemp(i,1:60) = rvTemp2(1:60)
				
			end if
			
		end do
	    print *, "st_me:: info: Profile generation completed"
	    iRetCode = updateState(LOG_S_INIT)
		if(iRetCode /= 0) then
			call logError("Invalid state transition")
		end if
		call logInfo("Profile generation completed")
	    
	else
	
	    print *, "st_me:: info: Profile generation skipped"
		call logInfo("Profile generation skipped")
	    
	end if
    
    ! Predispose for output
    call tDataSet % prepareToPrint()
    print *, "st_me:: info: Data predisposed for output"
	call logInfo("Data predisposed to print")
    
	! Scientific output (written always)
    call tDataSet % writeDataToFile(tFiles)
    print *, "st_me:: info: Scientific output written"
	call logInfo("Scientific output written")
    
    ! If scientific processing only is required, stop here
    if(lScientific) then
    	close(11)
    	print *, "st_me:: info: Scientific-only run completed, terminating execution"
    	iRetCode = logStop()
    	stop
    end if

	iRetCode = updateState(LOG_S_MODEL)
	if(iRetCode /= 0) then
		call logError("Invalid state transition")
	end if
	call logInfo("Begin production of model output")
	
    ! ***************************
    ! * CALMET srf/upair output *
    ! ***************************
    
    print *, "st_me:: info: Generating model-specific data"
    
    if(lCalmet) then
		call tDataSet % writeDataToCmet(tFiles)
		print *, "st_me:: info: Standard CALMET output written"
		call logInfo("CALMET meteo input files generated; surface data assembly may be required after ST_Me completion")
    end if
    
    ! **********************************
    ! * CALMET UTC+00 srf/upair output *
    ! **********************************
    
    if(lCalmet_00) then
		call tDataSet % writeDataToCmet00(tFiles)
		print *, "st_me:: info: Standard CALMET output written"
		call logInfo("CALMET meteo input files generated; surface data assembly may be required after ST_Me completion")
    end if
    
    ! ***************************
    ! * Standard CALPUFF output *
    ! ***************************
    
    if(lCalpuffIsc) then
		call tDataSet % writeDataToCIsc(tFiles)
		print *, "st_me:: info: Standard CALPUFF 'ISCMET' output written"
		call logInfo("CALPUFF 'ISCMET' type file generated")
	end if
    
    ! ***************************
    ! * Profiled CALPUFF output *
    ! ***************************
    
    if(lCalpuffCtdm6) then
		call tDataSet % writeDataToPIsc(tFiles)
		print *, "st_me:: info: CALPUFF profiled output written for Calpuff 6"
		call logInfo("CALPUFF 'CTDM' type files generated, for Calpuff 6")
	end if
    if(lCalpuffCtdm7) then
		call tDataSet % writeDataToPIsc7(tFiles)
		print *, "st_me:: info: CALPUFF profiled output written for Calpuff 7"
		call logInfo("CALPUFF 'CTDM' type files generated, for Calpuff 7")
	end if
    
    ! *****************
    ! * AERMET output *
    ! *****************
    
    if(lAermet) then
		call tDataSet % writeDataToAmet(tFiles)
		print *, "st_me:: info: Standard AERMET output written"
		call logInfo("AERMET type file generated")
	end if
    
    ! ***************
    ! * GRAL output *
    ! ***************
    
    if(lGral) then
		call tDataSet % writeDataToGral(tFiles)
		print *, "st_me:: info: Standard GRAL output written"
		call logInfo("GRAL type file generated")
	end if
    
    ! **********************
    ! * AUSTAL-2000 output *
    ! **********************
    
    if(lAustal2000) then
		call tDataSet % writeDataToAustal2000(tFiles)
		print *, "st_me:: info: Standard AUSTAL-2000 output written"
		call logInfo("AUSTAL-2000 type file generated")
	end if
    
    ! Leave
    close(11)
    print *, "st_me:: info: *** End Job ***"
    iRetCode = logStop()
    
end program ST_Me
