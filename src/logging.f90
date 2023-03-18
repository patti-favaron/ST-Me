! logging - A module implementing basic status logging functionalities,
!           especially matailored to ST-Me needs.
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
module logging

	implicit none
	
	private
	
	! Public interface
	public	:: logStart
	public	:: logStop
	public	:: updateState
	public	:: updateTime
	public	:: logInfo
	public	:: logWarning
	public	:: logError
	public	:: LOG_S_INIT
	public	:: LOG_S_METPRO
	public	:: LOG_S_METPROF
	public	:: LOG_S_MODEL
	
	! Constants
	integer, parameter	:: LOG_S_UNKNOWN = 0	! Status has not been defined yet
	integer, parameter	:: LOG_S_INIT    = 1	! ST-Me is performing pre-main-loop initializations
	integer, parameter	:: LOG_S_METPRO  = 2	! Within main loop, performing surface met processor and related tasks
	integer, parameter	:: LOG_S_METPROF = 3	! Within main loop, performing profiles generation
	integer, parameter	:: LOG_S_MODEL   = 4	! Performing model(s) output generation
	integer, parameter	:: LOG_S_TERM    = 5	! Terminating, under success
	integer, parameter	:: LOG_L_INFO    = 0
	integer, parameter	:: LOG_L_WARNING = 1
	integer, parameter	:: LOG_L_ERROR   = 2
	
	! Status
	character(len=256)	:: logFileName
	integer				:: state = LOG_S_UNKNOWN
	integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
	
contains

	function logStart(logName) result(iRetCode)
	
		! Routine arguments
		character(len=256)	:: logName
		integer				:: iRetCode
		
		! Locals
		! -none-
		
		! Set initial state
		state = LOG_S_INIT
		
		! Connect log file for output
		logFileName = logName
		open(1001, file=logFileName, status='unknown', action='write', iostat=iRetCode)
		call logInfo("*** ST-Me run started ***")
		
	end function logStart
	
	
	function logStop() result(iRetCode)
	
		! Routine arguments
		integer				:: iRetCode
		
		! Locals
		! -none-
		
		! Set final status, and log the state transition
		state = LOG_S_TERM
		call logInfo("*** Execution terminated successfully ***")
		
		! Unconnect log file - this actually terminates logging
		close(1001, iostat=iRetCode)
		
	end function logStop
	
	
	function updateState(newState) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)	:: newState
		integer				:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check the couple (state,newState) corresponds to a
		! permitted state transition within ST-Me (so this routine
		! acts as a state-driven watchdog (a cute device I've
		! discovered in my years of PES safety engineering)
		select case(state)
		case(LOG_S_UNKNOWN)	! Status has not been defined yet
			! Most-never may any transition from here by 'updateState'! No, no!
			iRetCode = 1
		case(LOG_S_INIT)	! ST-Me is performing pre-main-loop initializations
			if( &
				newState /= LOG_S_METPRO .and. &
				newState /= LOG_S_METPROF .and. &
				newState /= LOG_S_MODEL .and. &
				newState /= LOG_S_TERM &
			) then
				iRetCode = 2
			end if
		case(LOG_S_METPRO)	! Within main loop, performing surface met processor and related tasks
			if( &
				newState /= LOG_S_MODEL .and. &
				newState /= LOG_S_TERM .and. &
				newState /= LOG_S_INIT &
			) then
				iRetCode = 3
			end if
		case(LOG_S_METPROF)	! Within main loop, performing profiles generation
			if(newState /= LOG_S_MODEL .and. newState /= LOG_S_INIT) then
				iRetCode = 4
			end if
		case(LOG_S_MODEL)	! Performing model(s) output generation
			if(newState /= LOG_S_TERM) then
				iRetCode = 5
				        
			end if
		case(LOG_S_TERM)	! Terminating, under success
			! Uh, oh... It is not possible to get out of here by 'updateState'!
			iRetCode = 6
		end select
		if(iRetCode /= 0) return
		
		! Actuate transition, updating state
		state = newState
		
	end function updateState
	
	
	function updateTime(iNewYear, iNewMonth, iNewDay, iNewHour, iNewMinute, iNewSecond) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)	:: iNewYear, iNewMonth, iNewDay, iNewHour, iNewMinute, iNewSecond
		integer				:: iRetCode
		
		! Locals
		! -none-
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Update time "acritically"
		iYear   = iNewYear
		iMonth  = iNewMonth
		iDay    = iNewDay
		iHour   = iNewHour
		iMinute = iNewMinute
		iSecond = iNewSecond
		
	end function updateTime
	
	
	subroutine logInfo(message, rvVector)
	
		! Routine arguments
		character(len=*), intent(in)				:: message
		real, dimension(:), intent(in), optional	:: rvVector
		
		! Locals
		character(len=7)	:: sFirst
		character(len=28)	:: sSecond
		integer				:: i
		
		! Invoke the actual logger with the proper parameters
		if(present(rvVector)) then
			call logger(LOG_L_INFO, message, rvVector)
		else
			call logger(LOG_L_INFO, message)
		end if
		
	end subroutine logInfo
	
	
	subroutine logWarning(message, rvVector)
	
		! Routine arguments
		character(len=*), intent(in)				:: message
		real, dimension(:), intent(in), optional	:: rvVector
		
		! Locals
		character(len=7)	:: sFirst
		character(len=28)	:: sSecond
		integer				:: i
		
		! Invoke the actual logger with the proper parameters
		if(present(rvVector)) then
			call logger(LOG_L_WARNING, message, rvVector)
		else
			call logger(LOG_L_WARNING, message)
		end if
		
	end subroutine logWarning
	
	
	subroutine logError(message, rvVector)
	
		! Routine arguments
		character(len=*), intent(in)				:: message
		real, dimension(:), intent(in), optional	:: rvVector
		
		! Locals
		character(len=7)	:: sFirst
		character(len=28)	:: sSecond
		integer				:: i
		
		! Invoke the actual logger with the proper parameters
		if(present(rvVector)) then
			call logger(LOG_L_ERROR, message, rvVector)
		else
			call logger(LOG_L_ERROR, message)
		end if
		
	end subroutine logError
	
	! *********************
	! * Internal routines *
	! *********************
	
	subroutine logger(level, message, rvVector)
	
		! Routine arguments
		integer, intent(in)							:: level
		character(len=*), intent(in)				:: message
		real, dimension(:), intent(in), optional	:: rvVector
		
		! Locals
		character(len=7)	:: sFirst
		character(len=30)	:: sSecond
		integer				:: i
		
		! Form preamble from level and state
		select case(level)
		case(LOG_L_INFO)
			sFirst = "Info   "
		case(LOG_L_WARNING)
			sFirst = "WARNING"
		case(LOG_L_ERROR)
			sFirst = "*ERROR*"
		end select
		select case(state)
		case(LOG_S_UNKNOWN)
			sSecond = "????"
		case(LOG_S_INIT)
			sSecond = "Init"
		case(LOG_S_METPRO)
			write(sSecond, "('Met proc - ',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") &
				iYear, iMonth, iDay, iHour, iMinute, iSecond
		case(LOG_S_METPROF)
			write(sSecond, "('Profiles - ',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") &
				iYear, iMonth, iDay, iHour, iMinute, iSecond
		case(LOG_S_MODEL)
			sSecond = "Model"
		case(LOG_S_TERM)
			sSecond = "End"
		end select
		
		! Form first line of message
		write(1001, "(a,2(' - ',a))") &
			trim(sFirst), trim(sSecond), trim(message)
			
		! In case the input vector is present, write all the corresponding lines
		if(present(rvVector)) then
			do i = 1, size(rvVector)
				write(1001, "(a,' - ',a,'   ',f15.4)") &
					trim(sFirst), trim(sSecond), rvVector(i)
			end do
		end if
		
		! Make sure the contents is written to log file also in case of some catastrophic failure...
		flush(1001)
		
		! Stop on error
		if(level >= LOG_L_ERROR) stop
		
	end subroutine logger

end module logging
