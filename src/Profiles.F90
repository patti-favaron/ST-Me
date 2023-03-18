! Profiles - Module supporting the generation, smoothing and
!            blending of wind and temperature profiles.
!
! Copyright 2006 by Servizi Territorio srl
!                   All rights reserved
!
MODULE Profiles

	USE Routines

    IMPLICIT NONE
    
    PRIVATE
    
    ! State variables: Blending weight vector
    INTEGER, PARAMETER              :: MAX_HEIGHTS          = 60
    LOGICAL                         :: lBlendingAssigned    = .FALSE.
    REAL, DIMENSION(MAX_HEIGHTS)    :: rvBlending           = 0.        ! All information from modeled data
    
    ! Public interface
    PUBLIC  :: AssignBlendingWeight
    PUBLIC  :: WindProfile
    PUBLIC  :: TempProfile
    PUBLIC  :: BlendWindProfile
    PUBLIC  :: BlendTempProfile

CONTAINS

    ! This routine fills the blending weight array. The input
    ! is a "nominal" weight, with values between 0 and +infinity.
    ! These data are normalized using the maximum as scaling factor.
    FUNCTION AssignBlendingWeight(rvWeightingFactors) RESULT(iRetCode)
    
        ! Routine arguments
        REAL, DIMENSION(MAX_HEIGHTS), INTENT(IN)    :: rvWeightingFactors
        INTEGER                                     :: iRetCode
        
        ! Locals
        ! -none-
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Assign data, as required
        rvBlending = ABS(rvWeightingFactors)/MAXVAL(ABS(rvWeightingFactors))
        lBlendingAssigned = .TRUE.
        
    END FUNCTION AssignBlendingWeight


    ! Evaluation of wind profile in the ABL
    !   Sozzi Roberto (MI) Italy  2000
    FUNCTION WindProfile(iglo,z,zr,vr,dir,z0,hmix,us,hlm,u,v) RESULT(iRetCode)
    
        ! Routine arguments
        INTEGER, INTENT(IN)             :: iglo ! 1:Northern, 0:Southern hemisphere
        REAL, DIMENSION(:), INTENT(IN)  :: z    ! Heights (m)
        REAL, INTENT(IN)                :: zr   ! Anemometer height (m)
        REAL, INTENT(IN)                :: vr   ! Wind speed at anemometer height (m/s)
        REAL, INTENT(IN)                :: dir  ! Wind direction at anemometer height (° from N)
        REAL, INTENT(IN)                :: z0   ! Roughness length (m)
        REAL, INTENT(IN)                :: hmix ! Mixing height (m)
        REAL, INTENT(IN)                :: us   ! Friction velocity (m/s)
        REAL, INTENT(IN)                :: hlm  ! Scaled reciprocal of Monin-Obukhov Length (---)
        REAL, DIMENSION(:), INTENT(OUT) :: u    ! U wind component (m/s)
        REAL, DIMENSION(:), INTENT(OUT) :: v    ! V wind component (m/s)
        INTEGER                         :: iRetCode

        ! Locals
        INTEGER :: nz
        REAL    :: Vel0
        REAL    :: Coef
        REAL    :: fc
        REAL    :: akm
        REAL    :: avel
        REAL    :: gamma
        INTEGER :: iHemisph
        INTEGER :: i
        REAL    :: aDir
        REAL    :: ax
        REAL    :: ay
        REAL    :: DirSpiral
        REAL    :: DirActual
        
        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Check parameters
        nz = SIZE(Z)
        IF(SIZE(u) /= nz .OR. SIZE(v) /= nz) THEN
            iRetCode = 1
            RETURN
        END IF
        IF(MINVAL(z) <= 0.) THEN
            iRetCode = 2
            RETURN
        END IF
        DO i = 1, nz-1
            IF(z(i) >= z(i+1)) THEN
                iRetCode = 3
                RETURN
            END IF
        END DO
        
        Vel0 = Vel(zr,z0,us,hmix,hLm)
        coef = Vr/Vel0
        fc = 1.e-4
        akm = 0.4/6.*us*hmix
        gamma = SQRT(fc/(2.*akm))
        IF(iglo==1) THEN
            iHemisph = 1
        ELSE
            iHemisph = -1
        END IF

        DO i=1,nz
            ! Computes the absolute wind speed (with no rotation)
            aVel = Vel(z(i),z0,us,hmix,hLm)*coef
            ! Spiral direction at the reference height
            ax = 1. - EXP(-gamma*z(i))*COS(gamma*z(i))
            ay = EXP(-gamma*z(i))*SIN(gamma*z(i))
            DirSpiral = ATAN2(ay,ax)*180./3.1415927
            !DirActual = (Dir+180.)+iHemisph*(45.-DirSpiral)
            DirActual = Dir+iHemisph*(45.-DirSpiral)
            if(DirActual>360.0) DirActual = DirActual-360.
            if(DirActual<0.0) DirActual = DirActual+360.
            DirActual = DirActual*3.1415927/180.
            ! Wind speed components
            u(i) = aVel * SIN(DirActual)
            v(i) = aVel * COS(DirActual)
        END DO

    END FUNCTION WindProfile

    
    ! Estimate the temperature profile from micro-meteorological data
    ! (R. Sozzi - Italy - 1999)
    FUNCTION TempProfile(z,z0,zr,Tr,rGamma,zi,Ts,us,hm,h0,T) RESULT(iRetCode)
    
        ! Routine arguments
        REAL, DIMENSION(:), INTENT(IN)  :: z        ! Heights (m; minimum height > 0)
        REAL, INTENT(IN)                :: z0       ! Roughness length (m)
        REAL, INTENT(IN)                :: zr       ! Temperature measurement height (m)
        REAL, INTENT(IN)                :: Tr       ! Temperature at 'zr' (°K)
        REAL, INTENT(IN)                :: rGamma   ! Temp. lapse rate above the ABL (°K/m)
        REAL, INTENT(IN)                :: zi       ! Mixing height (m)
        REAL, INTENT(IN)                :: Ts       ! Scale temperature (°K)
        REAL, INTENT(IN)                :: us       ! Friction velocity (m/s)
        REAL, INTENT(IN)                :: hm       ! Scaled reciprocal of Monin-Obukhov Length (---)
        REAL, INTENT(IN)                :: h0       ! Turbulent flux of sensible heat (W/m2)
        REAL, DIMENSION(:), INTENT(OUT) :: T        ! Temperature profile (°K)
        INTEGER                         :: iRetCode
        
        ! Locals
        REAL, PARAMETER :: VALIDITY_THRESHOLD = -9000.0
        REAL, PARAMETER :: hk = 0.4
        REAL    :: gamma
        INTEGER :: n
        INTEGER :: i
        REAL    :: smu
        REAL    :: Psi
        REAL    :: Cfun
        REAL    :: AA
        REAL    :: Trif
        REAL    :: hLL
        REAL    :: DelT
        REAL    :: we
        REAL    :: Rie
        REAL    :: Coe
        REAL    :: DelZ
        REAL    :: Gz
        INTEGER :: itop
        INTEGER :: nzhm
        REAL    :: ztop
        REAL    :: zbase
        REAL    :: zbot
        REAL    :: Tbase

        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check input parameters
        n = SIZE(z)
        IF(SIZE(T) /= n) THEN
            iRetCode = 1
            RETURN
        END IF
        IF(MINVAL(z) <= 0.) THEN
            iRetCode = 2
            RETURN
        END IF
        DO i = 1, n-1
            IF(z(i) >= z(i+1)) THEN
                iRetCode = 3
                RETURN
            END IF
        END DO
        
        ! Set the lapse rate to a "sensible" default,
        ! if an "invalid" value was passed
        IF(rGamma < VALIDITY_THRESHOLD) THEN
            gamma = 0.0098 ! °K/m
        ELSE
            gamma = rGamma
        END IF
        
        ! A) => Identification of the PBL top
        IF(z(1) > zi) THEN
            ! The first level is above the PBL top
            NZHM = 1
        ELSE
            NZHM = n
            DO i=2,n
                IF(z(i) > zi) THEN
                    NZHM = i-1
                    EXIT
                END IF
            END DO
        END IF

        ! B) => Profile computation.
        IF(NZHM == 1) THEN
            t(1) = tr
            DO i=2,n
                t(i) = tr + gamma*z(i)
            END DO
            RETURN
        END IF

        smu  = zi*hm
        psi  = psih_prf(zr,hm,h0)
        Cfun = C_fun(smu)
        AA   =-Ts/hk * (ALOG(zi/zr) + psi - Cfun)
        Trif = Tr - AA

        ! Up to PBL top
        DO i=1,NZHM
            psi  =  psih_prf(z(i),hm,h0)
            AA   = -Ts/hk * (ALOG(zi/z(i)) + psi - Cfun)
            T(i) =  trif  + AA
        END DO

        ! Above the PBL top
        IF(smu >= 0.) THEN
            !	-> Stable conditions	
            IF(n > NZHM) THEN
                zbase = z(NZHM)
                DO i=nzhm,N
                    T(i) = T(NZHM) + gamma*(z(i) - zbase)
                END DO
                RETURN
            ELSE
                RETURN
            END IF
        ELSE
            !	> Convective conditions
            hLL  =  MAX(-1000.,1./hm)
            DelT = (0.2*zi-hLL)/(1.4*zi-2.*hLL)*gamma*zi
            we   =  zi**2/(1.4*zi-2.*hLL)
            we   =  (-Ts*us)/gamma/we
            RiE  = 9.81/Tr * DelT*zi/we**2
            Coe  = 3.3/abs(RiE)**0.3333 + 0.2
            DelZ = Coe*zi
            gz   = DelT/DelZ
            DelZ = DelZ/2.
            
            !	Entrainment Layer Depth
            ztop = zi+Delz
            itop = n
            DO i=NZHM,N
                IF(z(i) > ztop) THEN
                    itop = i
                    EXIT
                END IF
            END DO

            ! Entrainment Layer Profile
            Tbase = T(NZHM)
            zbot  = z(NZHM)
            DO i=NZHM+1,itop
                T(i) = Tbase + 2.*gz*(z(i)-zbot)
            END DO

            ! Free Atmosphere Profile
            Tbase = T(itop)
            DO i=itop+1,N
                T(i) = Tbase + gz*(z(i)-ztop)
            END DO
            
        END IF
        
    END FUNCTION TempProfile
    
    
    ! Perform blending of wind profile, returning it in vector form
    FUNCTION BlendWindProfile(rvZ, rvU, rvV, iNumSodarHeights, rvSodarZ, rvSodarVel, rvSodarDir, rvOutU, rvOutV) RESULT(iRetCode)
    
        ! Routine arguments
        REAL, DIMENSION(:), INTENT(IN)  :: rvZ
        REAL, DIMENSION(:), INTENT(IN)  :: rvU
        REAL, DIMENSION(:), INTENT(IN)  :: rvV
        INTEGER, INTENT(IN)             :: iNumSodarHeights
        REAL, DIMENSION(:), INTENT(IN)  :: rvSodarZ
        REAL, DIMENSION(:), INTENT(IN)  :: rvSodarVel
        REAL, DIMENSION(:), INTENT(IN)  :: rvSodarDir
        REAL, DIMENSION(:), INTENT(OUT) :: rvOutU
        REAL, DIMENSION(:), INTENT(OUT) :: rvOutV
        INTEGER                         :: iRetCode
        
        ! Locals
        REAL, PARAMETER                         :: VALIDITY_THRESHOLD = -9000.0
        REAL, DIMENSION(:), ALLOCATABLE         :: rvTempZ
        REAL, DIMENSION(:), ALLOCATABLE         :: rvTempU
        REAL, DIMENSION(:), ALLOCATABLE         :: rvTempV
        REAL, DIMENSION(:), ALLOCATABLE         :: rvNewU
        REAL, DIMENSION(:), ALLOCATABLE         :: rvNewV
        INTEGER                                 :: iNumValid
        INTEGER                                 :: iNumData
        INTEGER                                 :: iNumSodarData
        INTEGER                                 :: i
        INTEGER                                 :: iPos
        INTEGER                                 :: iValid
        INTEGER                                 :: iError
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvH
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvT
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvStdErr
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvY
        REAL(8), DIMENSION(:,:), ALLOCATABLE    :: rmC
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvErrEst
        REAL(8), DIMENSION(:,:), ALLOCATABLE    :: rmWk
        REAL(8)                                 :: rVar
        REAL(8)                                 :: rDelta
        REAL                                    :: rMaxBld
        REAL                                    :: rMinBld
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check parameters
        iNumData = SIZE(rvZ)
        IF( &
			iNumData <= 0 .OR. iNumData /= SIZE(rvU) .OR. &
			iNumData /= SIZE(rvV) .OR. iNumData /= SIZE(rvOutU) .OR. &
			iNumData /= SIZE(rvOutV) &
		) THEN
            iRetCode = 1
            RETURN
        END IF
        iNumSodarData = SIZE(rvSodarZ)
        IF(iNumSodarData /= SIZE(rvSodarVel) .OR. iNumSodarData /= SIZE(rvSodarDir)) THEN
            iRetCode = 2
            RETURN
        END IF
        
        ! If the blending function has not yet been defined, perform
        ! a trivial copy of the input to output
        IF(.NOT.lBlendingAssigned) THEN
            rvOutU = rvU
            rvOutV = rvV
            RETURN
        END IF
        
        ! Count valid SODAR data; if less than 4 are found, the
        ! "trivial" blending is made, that is, output wind
        ! equates the modelled input. The "magic" limit 4 has been
        ! selected because it is the minimum number of data
        ! allowing a cubic spline to be computed: this may
        ! look a bit arbitrary, but on the other side less than 4
        ! SODAR valid levels indicate quite dramatic conditions
        ! in the data acquisition process, so that ignoring them
        ! is nothing else than prudence.
        iNumValid = 0
        DO i = 1, iNumSodarHeights
            IF( &
				rvSodarZ(i) >= VALIDITY_THRESHOLD .AND. &
				rvSodarVel(i) >= VALIDITY_THRESHOLD .AND. &
				rvSodarDir(i) >= VALIDITY_THRESHOLD &
			) THEN
                iNumValid = iNumValid + 1
            END IF
        END DO
        IF(iNumValid < 4) THEN
            rvOutU = rvU
            rvOutV = rvV
            RETURN
        END IF
        ! Post-condition: at least 4 SODAR valid data found
        
        ! Transfer the valid SODAR data
        ALLOCATE(rvTempZ(iNumValid), rvTempU(iNumValid), rvTempV(iNumValid))
        iValid = 0
        DO i = 1, iNumSodarHeights
            IF( &
				rvSodarZ(i) >= VALIDITY_THRESHOLD .AND. &
				rvSodarVel(i) >= VALIDITY_THRESHOLD .AND. &
				rvSodarDir(i) >= VALIDITY_THRESHOLD &
			) THEN
                iValid = iValid + 1
                rvTempZ(iValid) = rvSodarZ(i)
                rvTempU(iValid) = rvSodarVel(i)*SIND(rvSodarDir(i))
                rvTempV(iValid) = rvSodarVel(i)*COSD(rvSodarDir(i))
            END IF
        END DO
        
        ! Build the spline approximant corresponding to the SODAR
        ! profile (it will be used later)
        ! -1- Reserve workspace
        ALLOCATE(rvH(iNumValid),rvT(iNumValid),rvStdErr(iNumValid))
        ALLOCATE(rvY(iNumValid),rmC(iNumValid-1,3),rvErrEst(iNumValid))
        ALLOCATE(rmWk(0:iNumValid+1,7))
        ALLOCATE(rvNewU(iNumData), rvNewV(iNumData))
        ! -1- Estimate smoothing spline for U
        rvH      = rvTempZ
        rvT      = rvTempU
        rvStdErr = 0.01d0
        rVar     = 1.d0
        CALL CUBGCV( &
            rvH, rvT, rvStdErr, iNumValid, &
            rvY, rmC, iNumValid-1, &
            rVar, 1, rvErrEst, &
            rmWk, iError &
        )
        IF(iError > 0) THEN
            iRetCode = 3
            RETURN
        END IF
        ! -1- Evaluate the smoothed U
        DO i = 1, iNumData
            IF(rvZ(i) < rvH(1)) THEN
                rvNewU(i) = rvY(1)
            ELSEIF(rvZ(i) >= rvH(iNumValid)) THEN
                rvNewU(i) = rvY(iNumValid)
            ELSE
                DO iPos = 1, iNumValid-1
                    IF(rvH(iPos) <= rvZ(i) .AND. rvZ(i) < rvH(iPos+1)) THEN
                        rDelta = rvZ(i) - rvH(iPos)
                        rvNewU(i) = ((rmC(iPos,3)*rDelta+rmC(iPos,2))*rDelta+rmC(iPos,1))*rDelta+rvY(iPos)
                        EXIT
                    END IF
                END DO
            END IF
        END DO
        ! -1- Estimate smoothing spline for V
        rvH      = rvTempZ
        rvT      = rvTempV
        rvStdErr = 0.01d0
        rVar     = 1.d0
        CALL CUBGCV( &
            rvH, rvT, rvStdErr, iNumValid, &
            rvY, rmC, iNumValid-1, &
            rVar, 1, rvErrEst, &
            rmWk, iError &
        )
        IF(iError > 0) THEN
            iRetCode = 4
            RETURN
        END IF
        ! -1- Evaluate the smoothed V
        DO i = 1, iNumData
            IF(rvZ(i) < rvH(1)) THEN
                rvNewV(i) = rvY(1)
            ELSEIF(rvZ(i) >= rvH(iNumValid)) THEN
                rvNewV(i) = rvY(iNumValid)
            ELSE
                DO iPos = 1, iNumValid-1
                    IF(rvH(iPos) <= rvZ(i) .AND. rvZ(i) < rvH(iPos+1)) THEN
                        rDelta = rvZ(i) - rvH(iPos)
                        rvNewV(i) = ((rmC(iPos,3)*rDelta+rmC(iPos,2))*rDelta+rmC(iPos,1))*rDelta+rvY(iPos)
                        EXIT
                    END IF
                END DO
            END IF
        END DO
        
        ! Perform the actual blending, using the weight function
        rMinBld = MINVAL(rvBlending(1:iNumData))
        rMaxBld = MAXVAL(rvBlending(1:iNumData))
        DO i = 1, iNumData
            rvOutU(i) = rvU(i)*(1.-rvBlending(i)) + rvNewU(i)*rvBlending(i)
            rvOutV(i) = rvV(i)*(1.-rvBlending(i)) + rvNewV(i)*rvBlending(i)
        END DO
        
        ! Leave
        DEALLOCATE(rvNewU, rvNewV)
        DEALLOCATE(rvTempZ, rvTempU, rvTempV)
        DEALLOCATE(rmWk)
        DEALLOCATE(rvY, rmC, rvErrEst)
        DEALLOCATE(rvH, rvT, rvStdErr)
        
    END FUNCTION BlendWindProfile
    
    
    ! Perform blending of temperature profile
    FUNCTION BlendTempProfile(rvZ, rvTemp, iNumSodarHeights, rvSodarZ, rvSodarTemp, rvOutTemp) RESULT(iRetCode)
    
        ! Routine arguments
        REAL, DIMENSION(:), INTENT(IN)  :: rvZ
        REAL, DIMENSION(:), INTENT(IN)  :: rvTemp
        INTEGER, INTENT(IN)             :: iNumSodarHeights
        REAL, DIMENSION(:), INTENT(IN)  :: rvSodarZ
        REAL, DIMENSION(:), INTENT(IN)  :: rvSodarTemp
        REAL, DIMENSION(:), INTENT(OUT) :: rvOutTemp
        INTEGER                         :: iRetCode
        
        ! Locals
        REAL, PARAMETER                         :: VALIDITY_THRESHOLD = -9000.0
        REAL, DIMENSION(:), ALLOCATABLE         :: rvTempZ
        REAL, DIMENSION(:), ALLOCATABLE         :: rvTempTemp
        REAL, DIMENSION(:), ALLOCATABLE         :: rvNewTemp
        INTEGER                                 :: iNumValid
        INTEGER                                 :: iNumData
        INTEGER                                 :: iNumSodarData
        INTEGER                                 :: i
        INTEGER                                 :: iValid
        INTEGER                                 :: iError
        INTEGER                                 :: iPos
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvH
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvT
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvStdErr
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvY
        REAL(8), DIMENSION(:,:), ALLOCATABLE    :: rmC
        REAL(8), DIMENSION(:), ALLOCATABLE      :: rvErrEst
        REAL(8), DIMENSION(:,:), ALLOCATABLE    :: rmWk
        REAL(8)                                 :: rVar
        REAL(8)                                 :: rDelta
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check parameters
        iNumData = SIZE(rvZ)
        IF(iNumData <= 0 .OR. iNumData /= SIZE(rvTemp) .OR. iNumData /= SIZE(rvOutTemp)) THEN
            iRetCode = 1
            RETURN
        END IF
        iNumSodarData = SIZE(rvSodarZ)
        IF(iNumSodarData /= SIZE(rvSodarTemp)) THEN
            iRetCode = 2
            RETURN
        END IF
        
        ! If the blending function has not yet been defined, perform
        ! a trivial copy of the input to output
        IF(lBlendingAssigned) THEN
            rvOutTemp = rvTemp
            RETURN
        END IF
        
        ! Count valid SODAR data; if less than 4 are found, the
        ! "trivial" blending is made, that is, output wind
        ! equates the modelled input. The "magic" limit 4 has been
        ! selected because it is the minimum number of data
        ! allowing a cubic spline to be computed: this may
        ! look a bit arbitrary, but on the other side less than 4
        ! SODAR valid levels indicate quite dramatic conditions
        ! in the data acquisition process, so that ignoring them
        ! is nothing else than prudence.
        iNumValid = 0
        DO i = 1, iNumSodarHeights
            IF(rvSodarZ(i) >= VALIDITY_THRESHOLD .AND. rvSodarTemp(i) >= VALIDITY_THRESHOLD) THEN
                iNumValid = iNumValid + 1
            END IF
        END DO
        IF(iNumValid < 4) THEN
            rvOutTemp = rvTemp
            RETURN
        END IF
        ! Post-condition: at least 4 SODAR valid data found
        
        ! Transfer the valid SODAR data
        ALLOCATE(rvTempZ(iNumValid), rvTempTemp(iNumValid))
        iValid = 0
        DO i = 1, iNumSodarHeights
            IF(rvSodarZ(i) >= VALIDITY_THRESHOLD .And. rvSodarTemp(i) >= VALIDITY_THRESHOLD) THEN
                iValid = iValid + 1
                rvTempZ(iValid) = rvSodarZ(i)
                rvTempTemp(iValid) = rvSodarTemp(i)
            END IF
        END DO
        
        ! Build the spline approximant corresponding to the SODAR
        ! profile (it will be used later)
        ! -1- Reserve workspace
        ALLOCATE(rvH(iNumValid),rvT(iNumValid),rvStdErr(iNumValid))
        ALLOCATE(rvY(iNumValid),rmC(iNumValid-1,3),rvErrEst(iNumValid))
        ALLOCATE(rmWk(0:iNumValid+1,7))
        ALLOCATE(rvNewTemp(iNumData))
        ! -1- Estimate smoothing spline for temperature
        rvH      = rvTempZ
        rvT      = rvTempTemp
        rvStdErr = 0.01d0
        rVar     = 1.d0
        CALL CUBGCV( &
            rvH, rvT, rvStdErr, iNumValid, &
            rvY, rmC, iNumValid-1, &
            rVar, 1, rvErrEst, &
            rmWk, iError &
        )
        IF(iError > 0) THEN
            iRetCode = 3
            RETURN
        END IF
        ! -1- Evaluate the smoothed temperature
        DO i = 1, iNumData
            IF(rvZ(i) < rvH(1)) THEN
                rvNewTemp(i) = rvY(1)
            ELSEIF(rvZ(i) >= rvH(iNumValid)) THEN
                rvNewTemp(i) = rvY(iNumValid)
            ELSE
                DO iPos = 1, iNumValid-1
                    IF(rvH(iPos) <= rvZ(i) .AND. rvZ(i) < rvH(iPos+1)) THEN
                        rDelta = rvZ(i) - rvH(iPos)
                        rvNewTemp(i) = ((rmC(iPos,3)*rDelta+rmC(iPos,2))*rDelta+rmC(iPos,1))*rDelta+rvY(iPos)
                        EXIT
                    END IF
                END DO
            END IF
        END DO
        
        ! Perform the actual blending, using the weight function
        DO i = 1, iNumData
            rvOutTemp(i) = rvTemp(i)*(1.-rvBlending(i)) + rvNewTemp(i)*rvBlending(i)
        END DO
        
        ! Leave
        DEALLOCATE(rvNewTemp)
        DEALLOCATE(rvTempZ, rvTempTemp)
        DEALLOCATE(rmWk)
        DEALLOCATE(rvY, rmC, rvErrEst)
        DEALLOCATE(rvH, rvT, rvStdErr)
        
    END FUNCTION BlendTempProfile
    
    
    ! *********************
    ! * Internal routines *
    ! *********************
    
    !   Sozzi Roberto (MI) Italy  2000
    FUNCTION C_fun(smu) RESULT(cfun)
    
        ! Routine arguments
        REAL, INTENT(IN)    :: smu
        REAL                :: cfun
        
        ! Locals
        ! -none-
        
        ! ==================================================================
        ! From Yamada's (1976) correlations.
        ! Input:
        !     smu = zi/L
        ! ------------------------------------------------------------------
        IF(smu < 0.) THEN
            cfun = 12.-8.335/(1.-0.03106*smu)**0.3333
        ELSE
            IF(smu > 18.) THEN
                cfun = -4.32*SQRT(smu-11.21)
            ELSE
                cfun = 3.665-0.829*smu
            END IF
        END IF

    END FUNCTION C_fun
    
    
    FUNCTION PsiH_prf(z,hl,h0) RESULT(rPsiH)
    
        ! Routine arguments
        REAL, INTENT(IN)    :: z
        REAL, INTENT(IN)    :: hl
        REAL, INTENT(IN)    :: h0
        REAL                :: rPsiH
        
        ! Locals
        ! -none-
    
        ! Compute the universal similarity function, using the appropriate case
        IF(h0 <= 0.) THEN
            rPsiH = PSIH_S(z,hl)
        ELSE
            rPsiH = PSIH_C(z,hl)
        END IF
        
    END FUNCTION PsiH_prf
    
    
    !   Sozzi Roberto (MI) Italy  2000
    FUNCTION Vel(z,z0,us,hmix,hLm) RESULT(rVel)

        REAL, INTENT(IN)    :: z
        REAL, INTENT(IN)    :: z0
        REAL, INTENT(IN)    :: us
        REAL, INTENT(IN)    :: hmix
        REAL, INTENT(IN)    :: hLm
        REAL                :: rVel
        REAL, PARAMETER     :: k=0.4
        REAL                :: zz,alg,t1,t2,t3,q,hm0,hmu
        zz = MAX(z,z0)
        zz = MIN(zz,hmix)
        alg = ALOG(zz/z0)
        IF(hLm>=0.) THEN
            ! Stable or neutral
            t1  = -(1.-6.9*hmix*hLm)*(zz-z0)/hmix
            t2  = -(6.9/2.0)*hmix*hLm*(zz**2-z0**2)/hmix**2
            rVel = (us/k)*(alg+t1+t2)
        ELSE
            ! Convective
            hmu = (1.-22.*zz*hlm)**0.25
            hm0 = (1.-22.*z0*hlm)**0.25
            q   = ((1.+hm0)**2*(1.+hm0**2))/((1.+hmu)**2*(1.+hmu**2))
            t1  = ALOG(q)
            t2  = 2.*(ATAN(hmu)-ATAN(hm0))
            t3  = 2.*(hmu**3-hm0**3)/(33.*hmix*hlm)
            rVel = (us/k)*(alg+t1+t2+t3)
        END IF
        
    END FUNCTION Vel
    

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
    
END MODULE Profiles
