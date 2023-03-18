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
MODULE routines

	USE Calendar
	USE NaN_support

	INTEGER, PARAMETER	:: LAI_GRASS   = 0
	INTEGER, PARAMETER	:: LAI_ALFALFA = 1

	INTEGER, PARAMETER	:: ASCE_STANDARDATMOSPHERE = 0
	INTEGER, PARAMETER	:: ASCE_STANDARDEQ         = 1
	INTEGER, PARAMETER	:: ASCE_MEANTEMPERATURE    = 2
	INTEGER, PARAMETER	:: ASCE_GRASS              = 1
	INTEGER, PARAMETER	:: ASCE_ALFALFA            = 2

CONTAINS
!
!	=================================================================
      Function J_DAY(ia,im,ig)
!	=================================================================
      INTEGER ngm(13,2)

      DATA ngm/0,31,60,91,121,152,182,213,244,274,305,335,366, &
               0,31,59,90,120,151,181,212,243,273,304,334,365/

      IF(ia.LT.0 .OR. im.LE.0 .OR. im.GT.12 .OR. ig.LE.0 .OR. ig.GT.31) Then
                   j_day = -99
                   RETURN
      Endif

      ianno = ia/4*4
      IF(ianno.eq.ia) Then
		j_day = ig+ngm(im,1)
      Else
		j_day = ig+ngm(im,2)
      Endif

      RETURN
      END FUNCTION J_Day

!	=================================================================
      Function SUN_DECL(jd)
!	=================================================================
      DATA greco/3.14159265/

      sun_decl = 0.409 * COS(2.*greco*(jd-173)/365.25)

      RETURN
      END FUNCTION SUN_DECL

!	=================================================================
      Function S_EL_ANG(dlat,dlong,fuso,hour,declin)
!	=================================================================
      DATA rad/0.0174533/, greco/3.141593/
!	-----------------------------------------------------------------
      IF(ABS(dlat).GT.90.) dlat  =  45.
	IF(ABS(dlong).GT.180.) dlong = -10.
!
      rlat  = dlat*rad
      rlong = dlong*rad
!
      s_el_ang = SIN(rlat)*SIN(declin) - COS(rlat)*COS(declin)* &
     							COS((greco*(hour-fuso)/12.)-rlong)
      s_el_ang = MAX(s_el_ang,0.)
!
      RETURN
      END FUNCTION S_EL_ANG
!
!	=================================================================
      Subroutine SUN_SET(dlat,dlong,fuso,jd,s_rise,s_set)
!	=================================================================
      DATA rad/0.0174533/, greco/3.141593/
!	-----------------------------------------------------------------
      IF(ABS(dlat).GT.90. .OR. ABS(dlong).GT.180. .OR. jd.LT.0. &
      				.OR.jd.GT.366..OR.ABS(fuso).GT.12.) Then
		s_rise = -9999.
		s_set  = -9999.
		RETURN
      Endif
!
      IF(ABS(dlat) .GE. 65.) Then
!
		IF(dlat.gt.0.) Then !N
			IF(jd.ge.80.and.jd.le.266) Then ! Spring, Summer
				s_rise = 0.
				s_set  = 23.99
			Else                            ! Autumn, Winter
				s_rise = 0.
				s_set  = 0.01
			Endif
		Else                !S
			IF(jd.ge.80.and.jd.le.266) Then ! Spring, Summer
				s_rise = 0.
				s_set  = 0.01
			Else                            ! Autumn, Winter
				s_rise = 0.
				s_set  = 23.99
			Endif
		Endif
!
      Else
!
		rlat  = dlat*rad
		rlong = dlong*rad
!
		a    = 0.0175*jd
		b    = 0.0330*SIN(a)
		sl   = 4.871 + a + b
		decl = ASIN(0.398 * SIN(sl))
		h    = ACOS(-TAN(rlat)*TAN(decl))
		tau1 =( h+rlong-0.043*SIN(2.*sl)+0.033*sin(0.0175*jd)+greco)/.262
		tau2 =(-h+rlong-0.043*SIN(2.*sl)+0.033*sin(0.0175*jd)+greco)/.262
		t1   = fuso + tau1
		t2   = fuso + tau2
		IF(t1.LT.0.) Then
			t1 = 24.+t1
		Endif
		IF(t2.LT.0.) Then
			t2 = 24.+t2
		Endif
		s_rise = MIN(t1,t2)
		s_set  = MAX(t1,t2)
!
      Endif
!
      RETURN
      END SUBROUTINE SUN_SET
!
!	=================================================================
      Function SUN_RAD2 (cloud,sin_psi)
!	=================================================================
      DATA a1/990./, a2/-30./, b1/-0.75/, b2/3.4/
!	-----------------------------------------------------------------
      IF(cloud.LT.0.) cloud = 0.5
	IF(cloud.GT.1.) cloud = 1.
!
      sinmin = -a2/a1
!
      IF(sin_psi .GE. sinmin) Then
		sun_rad2 = (a1*sin_psi*exp(-0.057/sin_psi))*(1.+b1*cloud**b2)
      Else
          sun_rad2 = 0.
      Endif
!
      RETURN
      END FUNCTION SUN_RAD2
!
!	==================================================================
      Function CLOUD_Rg (Rg,sin_psi)
!	==================================================================
      DATA a1,a2,b1,b2/990., -30., -0.75, 3.4/
!	------------------------------------------------------------------
      sinpsimin= -a2/a1

      IF( sin_psi .GT. sinpsimin) Then
          Rgmax =  a1*sin_psi * EXP(-0.057/sin_psi)
		IF(Rg .GE. Rgmax) Then
			cloud = 0.
		Else
			cloud = (1./b1*(Rg/rgmax-1.))**(1./b2)
		Endif
      Else
		cloud = 0.5
      Endif
!
	  CLOUD_Rg = cloud
!
      RETURN
      END FUNCTION CLOUD_Rg
!
!     ==================================================================
      Function R_NET_D(iland,dlat,dlong,fuso,hour,jd,albedo,T,Rg,cloud)
!     ==================================================================
      Real alpha(6)
!	------------------------------------------------------------------
      DATA alpha/0.1,0.3,0.5,0.8,1.,1.4/
      DATA c1/5.31E-13/, c2/60./, sig/5.67E-08/
!	------------------------------------------------------------------
      alp     = alpha(iland)
      decl    = SUN_DECL(jd)
      sin_psi = S_EL_ANG(dlat,dlong,fuso,hour,decl)
!
	tt      = T-273.15
	s       = 1.05*EXP((6.42-tt)/17.78)
	c3      = 0.38*((1.-alp)+1.)/(1.+s)
	c3      = 0.12
	R_NET_D = ((1.-albedo)*Rg+c1*T**6-sig*T**4+c2*cloud)/(1.+c3)
!
	Return
	END FUNCTION R_NET_D
!
!     ==================================================================
      Function R_NET_N(cloud,z0,zr,vel,T)
!     ==================================================================
      REAL a0(9),a1(4),a2(4),a3(4)
!	------------------------------------------------------------------
      DATA a0/-96.1,-101.5,-76.1,-80.1,-53.5,-45.3,-35.5,-23.0,-9.9/
      DATA a1/-16.4,-12.6,-13.0,-9.8/
      DATA a2/1.35,0.99,1.16,0.9/
      DATA a3/100.e-15,104.e-15,66.e-15,72.e-15/
!	------------------------------------------------------------------
      k  = ANINT(cloud)+1
      u2 = alog(2./z0)/alog(zr/z0) * vel
      IF(k.GT.4) Then
		R_NET_N = a0(k)
      Else
		R_NET_N = a0(k)+a1(k)*u2+a2(k)*u2*u2+a3(k)*T**6
      Endif
!
      RETURN
      END Function R_NET_N
!
!	==================================================================
	Function SOIL_HEAT_FLUX(hour,Rn)
!	==================================================================
!	Semplificazione del metodo proposto da Santanello e Friedl in
!	Journal of Applied Meteorology, 2003, 42, 851-862
!	==================================================================
	DATA	A/0.33/, B/85000./, C/10800./
	DATA	pg2/6.2831853/
!	------------------------------------------------------------------
	time = 3600.*(hour-12.)
!
	Rap  = A * COS(pg2*(time+C)/B)
!
	SOIL_HEAT_FLUX = Rap * Rn
!
	RETURN
	END FUNCTION SOIL_HEAT_FLUX

	! Based on Choudhury et al. (1987) and Choudhury (1989), and reported
	! in ASCE Standard Reference Evapotranspiration Equation, formula B.13;
	! starting from original formula I've rearranged terms and obtained a
	! rough estimate of LAI, assuming net radiation and ground heat flux are
	! correct. (Mauri Favaron, 2017).
	! See also Santanello, Friedl, 2003 in references.
	!
	! Note: This estimate yields values which increasingly change over year
	!       as latitude's absolute value grows. Also, variability in Rn measurements
	!       affect results changes importantly. So, uding an Rn *estimate*
	!       close to the Equator is likely to yield a value constant over year.
	!
	function SetApparentLAI(ivTimeStamp, iDeltaTime, rvRn, rvG0) result(rvLAI)

		implicit none

		! Routine arguments
		integer, dimension(:), intent(in)	:: ivTimeStamp
		integer, intent(in)					:: iDeltaTime
		real, dimension(:), intent(in)		:: rvRn
		real, dimension(:), intent(in)		:: rvG0
		real, dimension(SIZE(rvRn))			:: rvLAI

		! Locals
		integer	:: iDataPerDay
		real	:: KG
		integer	:: i, j
		integer	:: iNumValid
		real	:: ratio
		real	:: rLAI

		! Compute a rough estimate of LAI by applying Choudhury relationship (rearranged)
		do i = 1, SIZE(ivTimeStamp)
			if(rvRn(i) > 0.0) then
				KG = 0.4
			else
				KG = 2.0
			end if
			if(ABS(rvRn(i)) > 0.001) then
				ratio = rvG0(i)/(KG*rvRn(i))
				if(ratio > 0.001) then
					rvLAI(i) = -2.0*LOG(ratio)
				else
					rvLAI(i) = NaN
				end if
			else
				rvLAI(i) = NaN
			end if
		end do

		! Smooth initial LAI estimation by applying daily averages
		iDataPerDay = (24*3600) / iDeltaTime
		do i = 1, SIZE(ivTimeStamp), iDataPerDay

			! Accumulate LAI over current day, considering only valid data
			iNumValid = 0
			rLAI      = 0.
			do j = i, i+iDataPerDay-1
				if(.not.ISNAN(rvLAI(j))) then
					iNumValid = iNumValid + 1
					rLAI      = rLAI + rvLAI(j)
				end if
			end do
			if(iNumValid > 0) then
				rLAI = rLAI / iNumValid
			else
				rLAI = NaN
			end if

			! Distribute result over current day
			do j = i, i+iDataPerDay-1
				rvLAI(j) = rLAI
			end do

		end do

	end function SetApparentLAI


	! Estimation of leaf area index (LAI) based on vegetation height and type,
	! for coltures, as from the ASCE Evapotranspiration Standard Equation.
	function ASCE_ColtureLAI(rVegetationHeight, iColtureType) result(rLAI)

		implicit none

		! Routine arguments
		real, intent(in)	:: rVegetationHeight	! (m)
		integer, intent(in)	:: iColtureType			! LAI_GRASS, LAI_ALFALFA
		real				:: rLAI

		! Locals
		! -none-

		! Compute the information desired
		select case(iColtureType)
		case(LAI_GRASS)
			rLAI = 24.0 * rVegetationHeight
		case(LAI_ALFALFA)
			rLAI = 5.5 + 1.5 * LOG(rVegetationHeight)
		case default
			rLAI = NaN
		end select

	end function ASCE_ColtureLAI


	function ASCE_LatentVaporizationHeat(rTemp, iCalculationType) result(rLambda)

		implicit none

		! Routine arguments
		real, intent(in)	:: rTemp			! (¡C)
		integer, intent(in)	:: iCalculationType	! ASCE_STANDARDEQ, ASCE_MEANTEMPERATURE
		real				:: rLambda			! (MJ/kg)

		! Locals
		! -none-

		! Compute the information desired
		select case(iCalculationType)
		case(ASCE_STANDARDEQ)
			rLambda = 2.45
		case(ASCE_MEANTEMPERATURE)
			rLambda = 2.501 - 2.361e-3 * rTemp
		case default
			rLambda = NaN
		end select


	end function ASCE_LatentVaporizationHeat


	function ASCE_AirPressure(rZ, rvTemp, rZr, iCalculationType) result(rPk)

		implicit none

		! Routine arguments
		real, intent(in)				:: rZ		! Reference height at which pressure is desired (m)
		real, dimension(:), intent(in)	:: rvTemp	! Air temperature (¡C)
		real, intent(in)				:: rZr		! Height at which temperature measurements are taken (m)
		integer, intent(in)				:: iCalculationType	! ASCE_STANDARDATMOSPHERE, ASCE_STANDARDEQ, ASCE_MEANTEMPERATURE
		real							:: rPk		! Estimated pressure (kPa, as used in ASCE equations; multiply by 10 for normal use and reporting)

		! Locals
		real	:: rTK0	! Reference temperature (K)
		integer	:: i
		integer	:: iNumValid

		! Constants
		real, parameter	:: P0 = 101.3		! Pressure at reference height (kPa)
		real, parameter	:: g  = 9.807		! Gravitation acceleration (m/s2)
		real, parameter	:: z0 = 0.			! Reference altitude for expressing pressure (m above msl)
		real, parameter	:: R  = 287.0		! Specific gas constant (J/kg/K)
		real, parameter	:: Alpha1 = 0.0065	! Constant lapse rate of moist air (K/m)

		! Reference temperature
		select case(iCalculationType)
		case(ASCE_STANDARDATMOSPHERE)
			rTK0 = 288.0
		case(ASCE_STANDARDEQ)
			rTK0 = 293.0
		case(ASCE_MEANTEMPERATURE)
			rTK0 = 0.
			iNumValid = 0
			do i = 1, SIZE(rvTemp)
				if(.not.ISNAN(rvTemp(i))) then
					iNumValid = iNumValid + 1
					rTK0      = rTK0 + rvTemp(i)
				end if
			end do
			if(iNumValid > 0) then
				rTK0 = rTK0 / iNumValid + 273.15
			else
				rPk = NaN
				return
			end if
		case default
			rPk = NaN
			return
		end select

		! Compute pressure
		rPk = P0*((rTK0 - Alpha1*(rZ - z0))/rTK0)**(g/(Alpha1*R))

	end function ASCE_AirPressure


	function ASCE_VirtualTemperature(Temp, ea, P) result(Tv)

		implicit none

		! Routine arguments
		real, intent(in)	:: Temp		! (¡C)
		real, intent(in)	:: ea		! (kPa)
		real, intent(in)	:: P		! (kPa)
		real				:: Tv		! (K)

		! Locals
		! -none-

		! Compute the information desired
		Tv = (Temp + 273.16)/(1.0 - 0.378*ea/P)

	end function ASCE_VirtualTemperature


	function ASCE_MJm2h(Wm2) result(MJm2h)

		implicit none

		! Routine arguments
		real, intent(in)	:: Wm2			! Radiation (W/m2)
		real				:: MJm2h		! Radiation (MJ/m2 h)

		! Locals
		! -none-

		! Convert unit
		MJm2h = 3600.0 * Wm2 / 1.e6

	end function ASCE_MJm2h


	function ASCE_AerodynamicResistance(zw, zh, u, h) result(ra)

		implicit none

		! Routine arguments
		real, intent(in)	:: zw	! Height above ground at which wind is measured (m)
		real, intent(in)	:: zh	! Height above ground at which temperature/humidity are measured (m)
		real, intent(in)	:: u	! Wind speed (m/s)
		real, intent(in)	:: h	! Vegetation height (h)
		real				:: ra	! Aerodynamic resistance

		! Locals
		real	:: d	! Displacement height
		real	:: z0m	! Roughness length governing momentum transfer (m)
		real	:: z0h	! Roughness length governing heat transfer (m)

		! Constant
		real, parameter	:: k = 0.41	! von Karman constant

		! Compute the information desired
		if(u > 0.) then
			d = 0.67 * h
			z0m = 0.123 * h
			z0h = 0.0123 * h
			ra = LOG((zw-d)/z0m) * LOG((zh-d)/z0h) / (k**2 * u)
		else
			ra = NaN
		end if

	end function ASCE_AerodynamicResistance


	function ASCE_Evapotranspiration_Ref(Pres, Temp, Vel, Rn, G, es, ea, Zr, vegType) result(ET)

		implicit none

		! Routine arguments
		real, intent(in)	:: Pres		! Air pressure (hPa)
		real, intent(in)	:: Temp		! Air temperature (¡C)
		real, intent(in)	:: Vel		! Wind speed (m / s)
		real, intent(in)	:: Rn		! Net radiation (W / m2)
		real, intent(in)	:: G		! Ground heat flux (W / m2)
		real, intent(in)	:: es		! Saturation vapor pressure (hPa)
		real, intent(in)	:: ea		! Water vapor pressure (hPa)
		real, intent(in)	:: Zr		! Anemometer measurement height (m above ground)
		integer, intent(in)	:: vegType	! Vegetation type (ASCE_GRASS, ASCE_ALFALFA)
		real				:: ET		! Evapotranspiration (mm/h)

		! Locals
		real	:: Delta	! Slope (first derivative) of saturation vapor pressure relation
		real	:: gamma	! Psychrometric constant (kPa / ¡C)
		real	:: Vel2		! Wind speed at 2 m above ground
		real	:: h		! Vegetation height (m)
		real	:: d		! Displacement height (m)
		real	:: z0		! Aerodynamic roughness length (m)
		real	:: cd
		real	:: cn

		! Estimate coefficients based on reference vegetation type
		select case(vegType)
		case(ASCE_GRASS)
			h  =  0.12
			cn = 37.0
			if(Rn > 0.) then
				cd = 0.24
			else
				cd = 0.96
			end if
		case(ASCE_ALFALFA)
			h  =  0.50
			cn = 66.0
			if(Rn > 0.) then
				cd = 0.25
			else
				cd = 1.70
			end if
		case default
			ET = NaN
			return
		end select

		! Compute evapotranspiration
		Delta = 2503.0 * EXP(17.27*Temp/(Temp + 237.3)) / (Temp + 237.3)**2
		gamma = 0.0000665*Pres
		d     = 0.67 * h
		z0    = 0.123 * h
		Vel2  = Vel * LOG((2.0 - d)/z0) / LOG((Zr - d) / z0)
		ET = (&
			(0.408*Delta*ASCE_MJm2h(Rn-G) + gamma*cn/(Temp + 273.0)*Vel2*0.1*(es-ea)) / &
			(Delta + gamma*(1.0 - cd*Vel2)) &
		)

	end function ASCE_Evapotranspiration_Ref

!
!	==================================================================
      Function PSIM(zL)
!	==================================================================
!	Funzione Universale di Similarita' di Monin-Obukhov
!	per la velocit' del vento nel SL.
!	------------------------------------------------------------------
	IF(zL.LT.0.) Then
!		Situazione convettiva
		x    = (1.-16.*zL)**0.25
		y    = (1.+x)/2.
		PSIM = ALOG((1.+x*x)/2.*y*y) - 2.*ATAN(x) + 1.570796
	Else
!		Situazione stabile
		PSIM = -17.*(1.-EXP(-0.29*zL))
	Endif

	RETURN
      END FUNCTION PSIM

!	==================================================================
	Function PSIH(zL)
!	==================================================================
!	Funzione Universale di Similarita' di Monin-Obukhov
!	per la temperatura dell'aria nel SL.
!	------------------------------------------------------------------
	IF(zL.LT.0.) Then
!		Situazione convettiva
		y    = SQRT(1.-16.*zl)
		PSIH = 2.*ALOG((1.+y)/2.)
	Else
!		Situazione stabile
		PSIH = -17.*(1.-EXP(-0.29*zL))
	Endif

	RETURN
	END FUNCTION PSIH


	! Zero-lapse estimation of pressure with respect to height above mean sea level
	FUNCTION Pressure(z, Tk) RESULT(P)

		! Routine arguments
		REAL, INTENT(IN)	:: z	! Altitude above mean sea level (m)
		REAL, INTENT(IN)	:: Tk	! Temperature (K)
		REAL				:: P	! Pressure (hPa)

		! Locals
		REAL, PARAMETER	:: g = 9.80665
		REAL, PARAMETER	:: R = 8.31432
		REAL, PARAMETER	:: M = 0.0289644
		REAL, PARAMETER	:: P0 = 1013.

		! Calculate the desired quantity
		P = P0 * EXP(-g*M*z/(R*Tk))

	END FUNCTION Pressure


!
!	==================================================================
	Function E_SAT(T)
!	==================================================================
!	Tensione di vapore alla saturazione (la temperatura è in °C)
!	------------------------------------------------------------------
	E_SAT = 6.1 + T*(0.475 + T*(0.0095 + 0.0005*T))
!
	RETURN
	END FUNCTION E_SAT


	! Saturation water vapor pressure given air temperature, using
	! ASCE formula, a variant (up to constants decimals) of
	! Clausius-Clapeyron formula. This routine is the recommended
	! replacement of E_SAT.
	!
	!     Input: T = air temperature (°C)
	!
	!     Output: ESAT = saturation vapor pression (hPa)
	!
	function E_SAT_1(T) result(rEsat)

		! Routine arguments
		real, intent(in)	:: T
		real				:: rEsat

		! Locals
		! -none-

		! Compute the data item required
		rEsat = 6.108*EXP(17.27*T/(T+237.3))

	end function E_SAT_1


	! Precipitable water given water vapor pressure
	!
	!	Input:
	!
	!		Ea		Actual water vapor pressure (hPa)
	!
	!		Pa		Actual pressure at measurement altitude (i.e. not reduced to mean sea level) (hPa)
	!
	!	Output:
	!
	!		W		Precipitable water (mm)
	!
	function PrecipitableWater(Ea, Pa) result(W)

		! Routine arguments
		real, intent(in)	:: Ea, Pa
		real				:: W

		! Locals
		! -none-

		! Compute the data item required
		W = 0.0014*Ea*Pa + 2.1

	end function PrecipitableWater


    ! Compute the derivative of the saturation vapor pressure multiplied
    ! by P/0.622; the input temperature is in °K.
	FUNCTION D_E_SAT(T) RESULT(DEsat)

	    ! Routine arguments
	    REAL, INTENT(IN)    :: T
	    REAL                :: DEsat

	    ! Locals
	    REAL, PARAMETER :: E0 =   0.6112
	    REAL, PARAMETER :: a  =  17.67
	    REAL, PARAMETER :: T0 = 273.15
	    REAL, PARAMETER :: Tb =  29.66

	    ! Compute the saturation vapor tension
	    DEsat = E0*a*(1./(T-Tb) + (T-T0)/(T-Tb)**2)*EXP(a*(T-T0)/(T-Tb))
!
	END FUNCTION D_E_SAT


	FUNCTION DewPoint(rTemp, rRelH) RESULT(rTdew)

		! Routine arguments
		REAL, INTENT(IN)	:: rTemp	! Temperature (°C)
		REAL, INTENT(IN)	:: rRelH	! Relative humidity (%)
		REAL				:: rTdew

		! Locals
		REAL	:: Gm

		! Constants
		REAL, PARAMETER	:: a =   6.1121	! hPa
		REAL, PARAMETER	:: b =  18.678	! ---
		REAL, PARAMETER	:: c = 257.14	! °C
		REAL, PARAMETER	:: d = 234.5	! °C

		! Check parameters
		IF(rTemp < -40.0 .OR. rTemp > 60.0 .OR. rRelH < 0.0 .OR. rRelH > 100.0) THEN
			rTdew = -9999.9
			RETURN
		END IF

		! Compute dew point by Arden-Buck equation
		Gm = LOG(rRelH/100.0 * EXP((b-rTemp/d)*(rTemp/(c+rTemp))))
		rTdew = c*Gm / (b - Gm)

	END FUNCTION DewPoint

!
!	==================================================================
      Function ISTAB7 (hour,sun_r,sun_s,vel,rg,rn)
!	==================================================================
      REAL  vuppern(4),vupperd(5),riupper(5)
      INTEGER nan1(5),nan2(5),nan3(5),nad(6,6)
!	------------------------------------------------------------------
      DATA nad/1,1,2,2,3,3,1,2,2,2,3,3,2,2,2,3,3,4,2,2,3,3,3,4,3,3,3,4,4,4,4,4,4,4,4,4/
      DATA riupper/140.,270.,400.,540.,700./
      DATA nan1/4,4,4,4,4/, nan2/4,4,4,5,6/, nan3/4,4,5,6,6/
      DATA vuppern/6.,5.,3.,2./, vupperd/6.,5.,4.,3.,2./
!	------------------------------------------------------------------
      IF(vel.LT.0. .OR. rg.LT.0.) Then
		ISTAB7 = 4
		RETURN
      Endif
      IF((hour .LE. (sun_r+1.)) .OR. (hour .GE. (sun_s-1.))) Then
		ik = 5
		DO i=1,4
			IF(vel.GT.vuppern(i)) Then
				ik = i
				EXIT
			Endif
		EndDO
		IF(rn.EQ.0. )  Then
				ISTAB7 = nan2(ik)
				RETURN
		Endif
		IF(rn.GE.-20.) Then
			ISTAB7 = nan1(ik)
		Elseif(rn.LT.-40.) Then
			ISTAB7 = nan3(ik)
		Else
			ISTAB7 = nan2(ik)
		Endif
		RETURN
      Else
		jmat = 1
		DO j=1,5
			IF(rg.LT.riupper(j)) Then
				jmat = 7-j
				EXIT
			Endif
		EndDO
		imat = 1
		DO j=1,5
			IF(vel.GE.vupperd(j)) Then
				imat = 7-j
				EXIT
			Endif
		EndDO
      Endif
      ISTAB7 = nad(imat,jmat)
!
      RETURN
      END FUNCTION ISTAB7


      integer function lstab(el,zr0)
!----------------------------------------------------------------------
!
! --- CALPUFF    Version: 6.42     Level: 960521                  LSTAB
!
! --- PURPOSE:  Calculate a PG class given the Monin-Obukhov length
!               and the surface roughness from Golder's 1972 curves
!
! --- Taken from CTDMplus subroutine of the same name
!
! ASSUMPTIONS: THE DIVIDING LINES BETWEEN CATEGORIES ARE ASSUMED TO BE
!               LINEAR.
!
! LIMITATIONS: THIS FUNCTION IS ONLY VALID FOR 0.01 <= Z0 <= 0.5(M).
!              HOWEVER, RESULTS ARE EXTENDED TO OTHER VALUES OF Z0 BY
!              USING Z0 = 0.01 IF Z0 < 0.01 M, AND BY USING Z0 = 0.5
!              IF Z0 > 0.5 M.
!
! --- INPUTS:
!       EL      REAL    MONIN-OBUKHOV LENGHT (M)
!       ZR0     REAL    SURFACE ROUGHNESS LENGTH (M)
!
! --- OUTPUT:
!       LSTAB   INT     P-G STABILITY CATEGORY 1=A, 2=B, ETC.
!
! CALLING ROUTINES: SEQMOD
!
! EXTERNAL ROUTINES: NONE
!
! INTERNAL FUNCTIONS:
!       XL - EQUATION OF DIVIDING LINE BETWEEN P-G STABILITY CLASSES
!
! INTRINSIC FUNCTIONS: ALOG
!
! REFERENCES:
!       GOLDER, D. (1972): RELATIONS AMONG STABILITY PARAMETERS IN THE
!                       SURFACE LAYER, BOUNDARY-LAYER METEOROLOGY, 3:56.
!
!-----------------------------------------------------------------------
!
        REAL            EL, XEL, XL, Z0, ZR0

        XL(Y,XM,B)=XM/(ALOG(Y)-B)

        Z0 = ZR0
        IF(Z0 .GT. 0.5) Z0 = 0.5
        IF(Z0 .LT. 0.01) Z0 = 0.01
        IF(EL .LT. 0.0) THEN
            XEL = -EL
            IF(XEL .LE. XL(Z0,-70.0,4.35)) THEN
!               STABILITY A
                LSTAB=1
              ELSE IF(XEL .LE. XL(Z0,-85.2,0.502)) THEN
!               STABILITY B
                LSTAB=2
              ELSE IF(XEL .LE. XL(Z0,-245.,0.050)) THEN
!               STABILITY C
                LSTAB=3
              ELSE
!               STABILITY D
                LSTAB=4
            ENDIF
          ELSE
            IF(EL .GE. XL(Z0,-327.,0.627)) THEN
!               STABILITY D
                LSTAB=4
              ELSE IF(EL .GE. XL(Z0,-70.0,0.295)) THEN
!               STABILITY E
                LSTAB=5
              ELSE
!               STABILITY F
                LSTAB=6
            ENDIF
        ENDIF

        RETURN
        END FUNCTION lstab

!----------------------------------------------------------------------
      subroutine stab2l(istab,zr0,eli)
!----------------------------------------------------------------------
!
! --- CALPUFF    Version: 6.42     Level: 960521                 STAB2L
! ---            D. Strimaitis
!
! --- PURPOSE:  Calculate a Monin-Obukhov length given the PG class
!               and the surface roughness from Golder's 1972 curves
!               as presented in Seinfeld, 1986 (Atm. Chem. & Phys. of
!               Air Poll.)
!
! LIMITATIONS: THIS FUNCTION IS VALID FOR 0.01 <= Z0 <= 0.5(M).
!              HOWEVER, RESULTS ARE EXTENDED TO OTHER VALUES OF Z0 BY
!              USING Z0 = 0.01 IF Z0 < 0.01 M, AND BY USING Z0 = 0.5
!              IF Z0 > 0.5 M.
!
! --- INPUTS:
!       ISTAB - integer    - P-G stability category 1=A, 2=B, ETC.
!         ZR0 - real       - Surface roughness length (m)
!
! --- OUTPUT:
!         ELI - real       - 1/Monin-Obukhov length (1/m)
!
! --- STAB2L called by:  RDPLM
! --- STAB2L calls:      none
!----------------------------------------------------------------------
      real a(6),b(6)

      data a/-.096,-.037,-.002,0.0,.004,.035/
      data b/.029,.029,.018,0.0,-.018,-.036/

      z0 = zr0
      if(z0.GT.0.5) z0 = 0.5
      if(z0.LT.0.01) z0 = 0.01
      eli=a(istab)+b(istab)*ALOG10(z0)

      return
      end subroutine stab2l



!     ================================================================
      Subroutine PBL_33(iland,z0,d,zr,vel,T,Rn,cloud,us,Ts,H0,hlm)
!     ================================================================
      REAL alpha(6)

      DATA alpha/0.1,0.3,0.5,0.7,1.,1.4/
      DATA beta/20./, hk/0.4/, gg/9.81/
!     -----------------------------------------------------------------
	IF(z0 .LT.0.) z0 = 0.1
      if(iland.LT.1  .OR. iland.GT.6) iland = 4

      hlmin    = 1./5.
      ustarmin = 0.05
      rc       = 1305.*273.16/T
      rground  = 0.8

      alu    = ALOG((zr-d)/z0)
      tt     = T-273.15
      S      = 1.05*EXP( (6.42-tt)/17.78 )
      alp    = alpha(iland)

      H0 = ((1-alp)+S)/(1+S) * rground*Rn - beta

	IF(H0.GT.0.) Then
		usn = hk*vel/alu
		zz0 = z0/zr
		aln = ALOG(z0/zr)
		IF(zz0 .LE.0.01) Then
					  d1 = 0.128+0.005*aln
		Else
					  d1 = 0.107
		Endif
		d2 = 1.95+32.6*(zz0)**0.45
		IF(h0 .LE. 0.) Then
				  d3 = 0.
		Else
				  d3 = H0/rc * (hk*gg*zr)/(T*usn**3)
		Endif
		us  = usn * (1. + d1*ALOG(1. + d2*d3))
		IF(us .LT. ustarmin) us = ustarmin
		Ts  = -H0/rc/us
		hlm = hk*gg/T * Ts/us**2
      Else
		Ts1    = 0.09*(1. - 0.5*cloud**2)
		Ts2    = hk*T*vel**2/(18.8*zr*gg*alu)
		Ts     = AMIN1(ts1,ts2)
		us_min = hk/alu*vel
		uss    = 0.5*hk*vel/alu
		uuu    = 1.- 4.* 4.7*gg*zr*Ts*alu/(hk*T*vel**2)
		IF(uuu.LE.0.) Then
			hlm = hlmin
			us  = hk*vel/(alu + 4.7*zr*hlm)
			IF(us .LT.us_min) us = us_min
			H0  = -rc*us*ts
		Else
			uuu = SQRT(uuu)
			us  = uss * (1+uuu)
			IF(us .LT. us_min) us = us_min
			h0  = -rc*us*ts
			hlm = hk*gg/T * Ts/us**2
		Endif
      Endif

      RETURN
      END
!
!     =================================================================
      Function Hmix_new (dtime,H0,us,Tm,rc,hold)
!     =================================================================
      DATA n_step/60/
!     -----------------------------------------------------------------
      Hmix_new = -9999.
      dt       = dtime/n_step

      if(rc.eq.-9999.) rc = 1200.
      hmm  = hold
      DO i=1,n_step
		 ggmm = gg(hmm)
           hk1  = dt * F(rc,Tm,ggmm,us,H0,hmm)
		 ggmm = gg(hmm+hk1/2.)
           hk2  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk1/2.)
		 ggmm = gg(hmm+hk2/2.)
           hk3  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk2/2.)
		 ggmm = gg(hmm+hk3/2.)
           hk4  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk3/2.)
           hmm  = hmm + (hk1+2.*(hk2+hk3)+hk4)/6.
      EndDO
!
      Hmix_new = hmm
!
      RETURN
      END FUNCTION Hmix_New


	function F(rc,Ta,Gm,Ustar,h0,zi)

		implicit none

		! Routine arguments
		real, intent(in)	:: rc		! RhoCp
		real, intent(in)	:: Ta		! Air temperature (K)
		real, intent(in)	:: Gm		! Value of "Gamma" (temperature lapse rate at old mixing height)
		real, intent(in)	:: Ustar	! Friction velocity (m/s)
		real, intent(in)	:: H0		! Turbulent sensible heat flux (W/m2)
		real, intent(in)	:: zi		! Old convective mixing height
		real				:: F

		! Locals
		real(8)	:: L	! Obukhov length (m)
		real(8)	:: H0c	! Revised turbulent sensible heat flux

		! Constants
		real(8), parameter	:: K = 0.4d0	! von Karman constant
		real(8), parameter	:: G = 9.807d0	! Gravity acceleration constant
		real(8), parameter	:: A = 0.2d0
		real(8), parameter	:: B = 2.5d0
		real(8), parameter	:: C = 8.0d0

		! Compute Obukhov length
		H0c = SIGN(MAX(0.01, ABS(H0)), H0)
		L = -rc*Ta*Ustar**3/(K*G*H0c)

		! Compute Gryning-Batchvarova function
		F = H0/(rc*Gm) * &
			1.d0/( &
				DBLE(zi)**2/((1.d0+2.d0*A)*DBLE(zi)-2.d0*L) + &
				C*DBLE(Ustar)**2.d0*DBLE(Ta)/(DBLE(Gm)*G*((1.+A)*DBLE(zi)-DBLE(L))) &
			)

	end function F
!
!     =================================================================
	FUNCTION gg(z)
!     =================================================================
	gg = 3./(z+1.) - 1.98E-3 + 2.27E-6*z
!
	RETURN
	END FUNCTION gg



	function StableZi(Lat, Temp, H0, Ustar, L, N) result(Zi)

		! Routine arguments
		real, intent(in)	:: Lat		! Latitude (degrees)
		real, intent(in)	:: Temp		! Air temperature (¡C)
		real, intent(in)	:: H0		! Turbulent sensible heat flux (W/m2)
		real, intent(in)	:: Ustar	! Friction velocity (m/s)
		real, intent(in)	:: L		! Obukhov length (m)
		real, intent(in)	:: N		! Brunt-Vaisala frequency (Hz)
		real				:: Zi

		! Locals
		real(8)				:: rLat
		real(8)				:: f
		real(8)				:: Ta
		real(8)				:: a
		real(8)				:: b1
		real(8)				:: b2
		real(8)				:: b3
		real(8)				:: b
		real(8)				:: wt
		real(8)				:: rc

		! Constants
		real(8), parameter	:: g = 9.807d0

		! Check something is to be done
		if(L < 1.e-5 .or. Ustar < 1.e-5 .or. Temp < -40.0) then
			Zi = 1330.0 * Ustar		! Degrade to purely mechanical rough estimate
			return
		end if
		! From now on, stability is guaranteed

		! Compute Coriolis parameter
		rLat = Lat * 3.14159265358979d0 / 180.d0
		f    = 2.d0*7.29d-5*SIN(rLat)

		! Compute temperature in K
		Ta = Temp + 273.15d0

		! Compute w't'
		rc = 1305.d0 * 273.16d0/Ta
		wt = H0 / rc

		! Compute Zilitinkevich non-advective function parts
		a  = (f/(0.5d0*Ustar))**2
		b1 = 0.1d0 / L
		b2 = N / (26.d0*Ustar)
		b3 = SQRT(ABS(g*f*wt/Ta)) / (1.7d0*Ustar)
		b  = b1 + b2 + b3

		! Compute stable estimate of mixing height
		Zi = (SQRT(4.d0*a + b**2) - b)/(2.d0*a)

		! Accept only if >= than purely mechanical approx
		if(Zi > 2.0*1330.0*Ustar .or. Zi < 0.5*1330.0*Ustar) then
			Zi = 1330.0*Ustar
		else
			Zi = MAX(Zi, 1330.0*Ustar)
		end if

	end function StableZi
!
!     =================================================================
      Function RHOCP(T)
!     =================================================================
      IF(T.LT.0.) Then
                  RHOCP = -9999.
                  RETURN
      Endif
      RHOCP = 1305. * 273.16/T
!
      RETURN
      END FUNCTION RHOCP

      Function TSTAR_0(rc,us,h0)
      if(rc.LT.0. .OR. us.LT.0.) then
		 TSTAR_0 = -9999.
		 return
      endif

      TSTAR_0 = -h0 / (rc*us)

      return
      end function TSTAR_0


	Function DIR_WIND(vx,vy)
		if(abs(vx)<0.001 .and. abs(vy)<0.001) then
			dir_wind = 0.
			return
		endif
		dir_wind = ATAN2(vx,vy)*57.29578
		dir_wind = mod(dir_wind, 360.)
		if(dir_wind < 0.  ) dir_wind = dir_wind + 360.
		return
	end function DIR_WIND


!	==================================================================
	Subroutine SURFACE_PT(z0,Tk,Rh,Pres,Vel,Rn,G0,H0,HE,us,Ts,hL)
!	==================================================================
!	Stima del flusso turbolento di calore sensibile e latente, della
!	velocity supposto noto il flusso di calore nel suolo.
!
!	Metodo di Priestley-Taylor modificato
!	------------------------------------------------------------------
	DATA	hk/0.4/, g/9.81/, z0h/1.E-3/,  ga/4.08E-04/
	DATA	zru/10./,zrT/2./,alfa/0.8/,beta/20./
!	------------------------------------------------------------------
!  => Calcolo di alcuni coefficienti
	Rho_Cp = 350.14 * Pres/Tk
	Scc    = 0.622/Pres * D_E_SAT(Tk)
!
	zLu  = zru*hL
	zLT  = zrT*hL
	zLu0 = z0*hL
	zLT0 = z0h*hL
!
	alm = ALOG(zru/z0)
	alh = ALOG(zrT/z0h)
	psm  = PSIM(zLu)
	psh  = PSIH(zLT)
	psm0 = PSIM(zLu0)
	psh0 = PSIH(zLT0)
!
	AA = ga/Scc
	H0 = ((1.-alfa)+(AA))/(1.+AA)*(Rn-G0)-beta
	HE = (alfa)/(1.+AA)*(Rn-G0)+beta
!
	us   = MAX(hk*Vel/(alm - psm + psm0),0.05)
	Ts   = -H0/Rho_Cp/us
	hL   = hk*g/Tk * Ts/us**2
!
	END SUBROUTINE SURFACE_PT


      function TPOT_2(T,z)
      if(z.LT.0. .OR. T.LT.0.) then
                               tpot_2 = -9999.
                               return
      endif

      TPOT_2 = T + 0.0098 * z
      return
      end function tpot_2


	function BruntVaisala(T,z)

		real tpot

		if(z.LT.0. .OR. T.LT.0.) then
			BruntVaisala = -9999.
			return
		endif

		tpot = T + 273.15 + 0.0098 * z
		BruntVaisala = SQRT(ABS(9.807/tpot * 0.0098))

	end function BruntVaisala


	Function PSIH_S(zr,hl)

		DATA a/1./, b/0.667/, c/5./, d/0.35/

		PSIH_S = -9999.
		if(zr.LE.0. .OR. hl.LT.0.) return

		zl = zr*hl

		! van Ulden - Holtslag
		PSIH_S = -17. * (1.-exp(-0.29*zl))
		return

	end function PSIH_S


	Function PSIH_C(zr,hl)

		PSIH_C = -9999.
		if(zr.LE.0. .OR. hl.GT.0.) return

		zl = zr*hl

		! Businger
		y      = SQRT(1.-16.*zl)
		PSIH_C = 2.*alog((1.+y)/2.)
		return

	end function PSIH_C

	! ********************************************
	! * New "Net Radiation" part                 *
	! * (M. Favaron, after ASCE report           *
	! * "Reference evapotranspiration equation") *
	! ********************************************
	!
	! All former relationships dealing with net and global radiation
	! are a legacy from the original PBL_MET. The ones following here are
	! based on a different approach, coming from:
	!
	!	R.G. Allen et al, "The ASCE Standardized Reference Evapotranspiration Equation",
	!	American Society of Civil Engineers, 2005
	!
	! This new approach does basically replace cloud cover by water vapor pressure (which
	! can easily obtained from a thermo-hygrometer) and the "cloudiness function", a
	! dimensionless value obtainable from measurement of global radiation or, these missing,
	! by statistical means.


	! Julian day, according to NOAA conventions
	function calcJD(year, month, day) result(jd)

		! Routine arguments
		integer, intent(in)	:: year, month, day
		real				:: jd

		! Locals
		integer	:: A
		integer	:: B
		integer	:: yy
		integer	:: mm

		! Compute the Julian day corresponding to passed date
		yy = year
		mm = month
		if(mm <= 2) then
			yy = yy - 1
			mm = mm + 12
		end if
		A = yy/100
		B = 2 - A + A/4
		jd = FLOOR(365.25*(yy + 4716)) + FLOOR(30.6001*(mm+1)) + day + B - 1524.5

	end function calcJD


	! Convert between Julian day and Julian century (unit
	! of common use in astronomy)
	function calcTimeJulianCent(jd) result(T)

		! Routine arguments
		real, intent(in)	:: jd
		real				:: T

		! Locals
		! -none-

		! Compute the Julian century
		T = (jd - 2451545.0)/36525.0

	end function calcTimeJulianCent


	! Estimation of clear sky radiation by the simplified method
	!
	! Input:
	!
	!	Ra		Extraterrestrial radiation (W/m2)
	!
	!	z		Site elevation above mean sea level (m)
	!
	! Output:
	!
	!	Rso		Clear sky radiation (W/m2)
	!
	function ClearSkyRg_Simple(Ra, z) result(Rso)

		implicit none

		! Routine arguments
		real, intent(in)	:: Ra
		real, intent(in)	:: z
		real				:: Rso

		! Locals
		! -none-

		! Compute the information item desired
		Rso = Ra * (0.75 + 2.0e-5*z)

	end function ClearSkyRg_Simple


	! Estimation of clear sky radiation by the extended, more accurate method
	!
	! Input:
	!
	!	timeStamp			String, in form "YYYY-MM-DD HH:MM:SS" indicating time on *beginning* of averaging period
	!						(beware: many Italian weather station use a time stamp on *end* of averaging period:
	!						if so, subtract one hour)
	!
	!	averagingPeriod		Length of averaging period (s)
	!
	!	lat					Local latitude (degrees, positive northwards)
	!
	!	lon					Local longitude (degrees, positive eastwards)
	!
	!	zone				Time zone number (hours, positive Eastwards, in range -12 to 12)
	!
	!	Pa					Local pressure, that is, pressure not reduced to mean sea level (hPa)
	!
	!	Temp				Local temperature (Celsius degrees)
	!
	!	Hrel				Relative humidity (%)
	!
	!	Kt					Turbidity coefficient (dimensionless, 0 excluded to 1 included;
	!						value 1 corresponds to perfectly clean air; for extremelyturbid,
	!						dusty or polluted air 0.5 may be assumed; recommended value lacking
	!						better data: 1, the default)
	!
	! Output:
	!
	!	Rso					Clear sky radiation (W/m2)
	!
	function ClearSkyRg_Accurate(timeStamp, averagingPeriod, lat, lon, zone, Pa, Temp, Hrel, Kt_In) result(Rso)

		implicit none

		! Routine arguments
		character(len=*), intent(in)	:: timeStamp
		real, intent(in)				:: averagingPeriod, lat, lon, zone, Pa, Temp, Hrel
		real, intent(in), optional		:: Kt_In
		real							:: Rso

		! Locals
		real	:: Kt

		real	:: Kb, Kd, Ra
		real	:: beta, sinBeta, W
		real	:: e, es, Ta
		integer	:: ss, mm, hh, yy, mo, dy, doy
		real	:: dr
		real	:: omega, omega1, omega2, omegaS
		real	:: timenow, JD, t, Sc, b, t1
		real	:: solarDeclination, centralMeridianLongitude, localLongitude
		integer	:: iErrCode

		! Constants
		real, parameter	:: SOLAR_CONSTANT = 1.e5*49.2/3600.0		! W/m2
		real, parameter	:: PI             = 3.1415927

		! Get optional parameter (assign default if missing)
		if(present(Kt_In)) then
			Kt = Kt_In
		else
			Kt = 1.0
		end if

		! get date and time
		read(timeStamp, "(i4,5(1x,i2))", iostat=iErrCode) yy, mo, dy, hh, mm, ss
		if(iErrCode /= 0) then
			Rso = NaN
			return
		end if
		doy = J_DAY(yy,mo,dy)

		! Compute solar declination
		solarDeclination = 0.409*SIN(2*PI/365*doy - 1.39)

		! Compute Julian day
		timenow = hh + mm/60.0 + ss/3600.0 - zone
		JD = calcJD(yy, mo, dy)

		! Inverse squared relative distance factor for Sun-Earth
		dr = 1.0 + 0.033*COS(2*PI*doy/365.0)

		! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
		centralMeridianLongitude = -zone*15.0
		if(centralMeridianLongitude < 0.0) then
			centralMeridianLongitude = centralMeridianLongitude + 360.0
		end if
		localLongitude = -lon
		if(localLongitude < 0.0) then
			localLongitude = localLongitude + 360.0
		end if

		! Compute hour at mid of averaging time
		t1 = averagingPeriod / 3600.0
		t = timenow + zone + 0.5*t1

		! Calculate seasonal correction for solar time
		b  = 2.*PI*(doy-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Solar time angle at midpoint of averaging time
		omega = (PI/12.0) * ((t + 0.06667*(centralMeridianLongitude - localLongitude) + Sc) - 12.0)

		! Solar time angle at beginning and end of averaging period
		omega1 = omega - PI*t1/24.0
		omega2 = omega + PI*t1/24.0

		! Adjust angular end points to exclude nighttime hours
		omegaS = ACOS(-TAN(lat*PI/180.0)*TAN(solarDeclination))	! Sunset angle
		if(omega1 < -omegaS) then
			omega1 = -omegaS
		end if
		if(omega2 < -omegaS) then
			omega2 = -omegaS
		end if
		if(omega1 > omegaS) then
			omega1 = omegaS
		end if
		if(omega2 > omegaS) then
			omega2 = omegaS
		end if
		if(omega1 > omega2) then
			omega1 = omega2
		end if

		! Compute extraterrestrial radiation
		Ra = 12/PI * SOLAR_CONSTANT * dr * ( &
				(omega2-omega1)*SIN(lat*PI/180.0)*SIN(solarDeclination) + &
				COS(lat*PI/180.0)*COS(solarDeclination)*(SIN(omega2) - SIN(omega1)) &
		)

		! Estimate the amount of precipitable water
		Ta = Temp + 273.15
		es = E_SAT_1(Temp)
		e  = Hrel*es/100.0
		W  = PrecipitableWater(e, Pa)

		! Compute solar elevation (refractive correction is not applied, in compliance with ASCE standard evapotranspiration equation)
		sinBeta = SIN(lat*PI/180.0)*SIN(solarDeclination) + COS(lat*PI/180.0)*COS(solarDeclination)*COS(omega)
		if(sinBeta > 0.0) then

			! Estimate the clearness index for direct beam radiation
			Kb = 0.98*EXP(-0.000149*Pa/(Kt*sinBeta) - 0.075*(W/sinBeta)**0.4)

			! Estimate the transmissivity index for diffuse radiation
			if(Kb >= 0.15) then
				Kd = 0.35 - 0.36*Kb
			else
				Kd = 0.18 + 0.82*Kb
			end if

		else

			! Assume null clearness and transmissivity on night-time
			Kb = 0.0
			Kd = 0.18

		end if

		! Last, estimate clear-sky radiation
		Rso = Ra * (Kb + Kd)

	end function ClearSkyRg_Accurate


	! Accurate estimate of extraterrestrial solar radiation
	!
	! Input:
	!
	!	timeStamp			String, in form "YYYY-MM-DD HH:MM:SS" indicating time on *beginning* of averaging period
	!						(beware: many Italian weather station use a time stamp on *end* of averaging period:
	!						if so, subtract one hour)
	!
	!	averagingPeriod		Length of averaging period (s)
	!
	!	lat					Local latitude (degrees, positive northwards)
	!
	!	lon					Local longitude (degrees, positive eastwards)
	!
	!	zone				Time zone number (hours, positive Eastwards, in range -12 to 12)
	!
	! Output:
	!
	!	ra					Extraterrestrial radiation (W/m2)
	!
	function ExtraterrestrialRadiation(timeStamp, averagingPeriod, lat, lon, zone) result(ra)

		implicit none

		! Routine arguments
		character(len=*), intent(in)	:: timeStamp
		real, intent(in)				:: averagingPeriod, lat, lon, zone
		real							:: ra

		! Locals
		integer	:: iErrCode
		integer	:: ss, mm, hh, yy, mo, dy, doy
		real	:: dr
		real	:: omega, omega1, omega2, omegaS
		real	:: timenow, JD, t, Sc, b, t1
		real	:: solarDeclination, centralMeridianLongitude, localLongitude

		! Constants
		real, parameter	:: SOLAR_CONSTANT = 1.e5*49.2/3600.0		! W/m2
		real, parameter	:: PI             = 3.1415927

		! Get date and time
		read(timeStamp, "(i4,5(1x,i2))", iostat=iErrCode) yy, mo, dy, hh, mm, ss
		if(iErrCode /= 0) then
			Ra = NaN
			return
		end if
		doy = J_DAY(yy,mo,dy)

		! Compute solar declination
		solarDeclination = 0.409*SIN(2*PI/365*doy - 1.39)

		! Compute Julian day
		timenow = hh + mm/60.0 + ss/3600.0 - zone
		JD = calcJD(yy, mo, dy)

		! Inverse squared relative distance factor for Sun-Earth
		dr = 1.0 + 0.033*COS(2*PI*doy/365.0)

		! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
		centralMeridianLongitude = -zone*15.0
		if(centralMeridianLongitude < 0.0) then
			centralMeridianLongitude = centralMeridianLongitude + 360.0
		end if
		localLongitude = -lon
		if(localLongitude < 0.0) then
			localLongitude = localLongitude + 360.0
		end if

		! Compute hour at mid of averaging time
		t1 = averagingPeriod / 3600.0
		t = timenow + zone + 0.5*t1

		! Calculate seasonal correction for solar time
		b  = 2.*PI*(doy-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Solar time angle at midpoint of averaging time
		omega = (PI/12.0) * ((t + 0.06667*(centralMeridianLongitude - localLongitude) + Sc) - 12.0)

		! Solar time angle at beginning and end of averaging period
		omega1 = omega - PI*t1/24.0
		omega2 = omega + PI*t1/24.0

		! Adjust angular end points to exclude nighttime hours
		omegaS = ACOS(-TAN(lat*PI/180.0)*TAN(solarDeclination))	! Sunset angle
		if(omega1 < -omegaS) then
			omega1 = -omegaS
		end if
		if(omega2 < -omegaS) then
			omega2 = -omegaS
		end if
		if(omega1 > omegaS) then
			omega1 = omegaS
		end if
		if(omega2 > omegaS) then
			omega2 = omegaS
		end if
		if(omega1 > omega2) then
			omega1 = omega2
		end if

		! Compute extraterrestrial radiation
		ra = 12/PI * SOLAR_CONSTANT * dr * ( &
				(omega2-omega1)*SIN(lat*PI/180.0)*SIN(solarDeclination) + &
				COS(lat*PI/180.0)*COS(solarDeclination)*(SIN(omega2) - SIN(omega1)) &
			)

	end function ExtraterrestrialRadiation


	! Estimation of net radiation not using cloud cover, as from ASCE standardized reference evapotranspiration equation.
	!
	! Input:
	!
	!	Rg		Measured or estimated global radiation (W/m2)
	!
	!	albedo	Albedo at site (dimensionless)
	!
	!	fcd		Cloudiness function (dimensionless, 0 to 1)
	!
	!	Ea		Water vapor pressure (hPa)
	!
	!	Ta		Air temperature (K)
	!
	! Output:
	!
	!	Rn		Net radiation (W/m2)
	!
	! Note 1 (fcd):
	!
	! An accurate evaluation of the cloudiness function is critical for Rn estimate to yield
	! sensible results. fcd is defined as
	!
	!	fcd = 1.35*(Rg/Rgc) - 0.35
	!
	! where Rg is global radiation, and Rgc the clear-sky radiation computed when solar elevation
	! exceeds a given safety threshold (typically assumed to 0.3 radians computed on mid-averaging
	! period). Defined this way, fcd value is valid only on center-daytime, and undefined elsewhere.
	! But, it may be prolonged by computing an appropriate value on the preceding day's.
	!
	! Alternatively, fcd may be assumed to be fixed to some reference value, derived e.g. by the statistical
	! study of data from a nearby met station equipped with a reliable Rg measurement, and then used to
	! estimate Rg from Rgc:
	!
	!	Rg = Rgc * (fcd + 0.35) / 1.35
	!
	! Although dangerous, the last way may be the only resort when no global radiation measurement
	! is available at met station site.
	!
	! Note 2 (why not cloud cover?):
	!
	! Old PBL_MET estimates made extensive use of cloud cover, a notoriously difficult quantity to get.
	! In this formulation, the information coming from the cloud cover is jointly proxied by fcd, the
	! relatively slowly changing cloudiness function, and Ea, the water vapor pressure (which in case of
	! strong cloud cover will tend to approach saturation pressure, and whose value is intuitively
	! related to cloud cover to some extent).
	!
	function NetRadiation(Rg, albedo, fcd, Ea, Ta) result(Rn)

		implicit none

		! Routine arguments
		real, intent(in)	:: Rg
		real, intent(in)	:: albedo
		real, intent(in)	:: fcd
		real, intent(in)	:: Ea
		real, intent(in)	:: Ta
		real				:: Rn

		! Locals
		real	:: Rns, Rnl		! Short- and long-wave components of net radiation

		! Short-wave component of net radiation is the part which is not reflected
		Rns = Rg*(1.0 - albedo)

		! Long-wave component depends on various things
		Rnl = 5.6722e-8 * fcd * (0.34 - 0.14*SQRT(Ea/10.0)) * Ta**4		! 5.6722e-8 = sigma[MJ / m2 h] * = 2.042e-10 * 1000000 / 3600

		! Finally, the Net Radiation:
		Rn = Rns - Rnl

	end function NetRadiation


	function Cloudiness(rvElAng, rvRg, rvRg3, rSunElevThreshold, rvFcd) result(iRetCode)

		implicit none

		! Routine arguments
		real, dimension(:), intent(in)	:: rvElAng
		real, dimension(:), intent(in)	:: rvRg
		real, dimension(:), intent(in)	:: rvRg3
		real, intent(in)				:: rSunElevThreshold
		real, dimension(:), intent(out)	:: rvFcd
		integer							:: iRetCode

		! Locals
		integer	:: i
		integer	:: iErrCode
		real	:: rFcdOld
		real	:: rFcdFirst
		real	:: rPhi
		real	:: rRatio
		logical:: lIsFirst = .true.

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Iterate over all radiation readings, assumed valid
		rFcdOld   = NaN
		rFcdFirst = NaN
		do i = 1, SIZE(rvRg)
			rPhi = rvElAng(i)
			if(rPhi > rSunElevThreshold) then
				rRatio = MAX(MIN(rvRg(i) / rvRg3(i), 1.0), 0.0)
				rvFcd(i) = 1.35 * rRatio - 0.35
				rFcdOld  = rvFcd(i)
			else
				rvFcd(i) = rFcdOld
			end if
			if(lIsFirst) then
				if(.not.ISNAN(rvFcd(i))) then
					rFcdFirst = rvFcd(i)
					lIsFirst  = .false.
				end if
			end if
		end do
		! Typically, first data items cloudiness remains unassigned
		if(ISNAN(rFcdOld)) then
			iRetCode = 1
			return
		end if

		! Locate first NaNs, and replace them with first over-threshold Cloudiness
		do i = 1, SIZE(rvRg)
			if(ISNAN(rvFcd(i))) then
				rvFcd(i) = rFcdFirst
			end if
		end do

	end function Cloudiness


	function SunRiseSunSet(yy, mo, dy, lat, lon, zone) result(sunRiseSet)

		implicit none

		! Routine arguments
		integer, intent(in)	:: yy, mo, dy
		real, intent(in)	:: lat, lon
		integer, intent(in)	:: zone
		real, dimension(2)	:: sunRiseSet

		! Locals
		integer	:: doy
		real	:: solarDeclination
		real	:: t, b, Sc
		real	:: centralMeridianLongitude
		real	:: localLongitude
		real	:: omegaZeroElev, tZeroElev1, tZeroElev2

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		doy = J_DAY(yy,mo,dy)
		solarDeclination = 0.409*SIN(2*PI/365*doy - 1.39)

		! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
		centralMeridianLongitude = -zone*15.0
		if(centralMeridianLongitude < 0.0) then
			centralMeridianLongitude = centralMeridianLongitude + 360.0
		end if
		localLongitude = -lon
		if(localLongitude < 0.0) then
			localLongitude = localLongitude + 360.0
		end if

		! Calculate seasonal correction for solar time
		b  = 2.*PI*(doy-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Sunrise and sunset angles
		omegaZeroElev = ACOS(-TAN(lat*PI/180.0)*TAN(solarDeclination))
		tZeroElev1 =  omegaZeroElev * 12 / PI + 12.0 - Sc - 0.06667*(centralMeridianLongitude - localLongitude)
		if(tZeroElev1 < 0.) tZeroElev1 = tZeroElev1 + 12.0
		tZeroElev2 = -omegaZeroElev * 12 / PI + 12.0 - Sc - 0.06667*(centralMeridianLongitude - localLongitude)
		if(tZeroElev2 < 0.) tZeroElev2 = tZeroElev2 + 12.0
		sunRiseSet(1) = MIN(tZeroElev1, tZeroElev2)
		sunRiseSet(2) = MAX(tZeroElev1, tZeroElev2)

	end function SunRiseSunSet


	function SinSolarElevation(yy, mo, dy, hh, mm, ss, lat, lon, zone, averagingPeriod) result(sinBeta)

		implicit none

		! Routine arguments
		integer, intent(in)	:: yy, mo, dy, hh, mm, ss
		real, intent(in)	:: lat, lon
		integer, intent(in)	:: zone
		integer, intent(in)	:: averagingPeriod
		real				:: sinBeta

		! Locals
		integer	:: doy
		real	:: solarDeclination
		real	:: t, b, Sc
		real	:: centralMeridianLongitude
		real	:: localLongitude
		real	:: omega

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		doy = J_DAY(yy,mo,dy)
		solarDeclination = 0.409*SIN(2*PI/365*doy - 1.39)

		! Compute current hour at mid of averaging period
		t = hh + mm/60.0 + ss/3600.0 + 0.5 * averagingPeriod / 3600.0

		! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
		centralMeridianLongitude = -zone*15.0
		if(centralMeridianLongitude < 0.0) then
			centralMeridianLongitude = centralMeridianLongitude + 360.0
		end if
		localLongitude = -lon
		if(localLongitude < 0.0) then
			localLongitude = localLongitude + 360.0
		end if

		! Calculate seasonal correction for solar time
		b  = 2.*PI*(doy-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Solar time angle at midpoint of averaging time
		omega = (PI/12.0) * ((t + 0.06667*(centralMeridianLongitude - localLongitude) + Sc) - 12.0)

		! Sine of solar elevation angle
		sinBeta = SIN(lat*PI/180.0)*SIN(solarDeclination) + COS(lat*PI/180.0)*COS(solarDeclination)*COS(omega)

	end function SinSolarElevation


	function SolarDeclination(yy, mo, dy) result(sunDecl)

		implicit none

		! Routine arguments
		integer, intent(in)	:: yy, mo, dy
		real				:: sunDecl

		! Locals
		integer	:: doy

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		doy = J_DAY(yy,mo,dy)
		sunDecl = 0.409*SIN(2.*PI/365.*doy - 1.39)

	end function SolarDeclination


END MODULE routines
