! strings.f90 - Fortran 90 module, supporting (very simple) string processing.
!              This is not a general purpose package: rather, it has been
! written for use within the ST-Me system. Might devise some extensions, in future.
module strings
	
	implicit none
	
	private
	
	! Public interface
	public	:: ToUpper
	public	:: ToLower
	
contains

	subroutine ToUpper(sString)
	
		! Routine arguments
		character(len=*), intent(inout)	:: sString
		
		! Locals
		integer		:: i
		character	:: c
		
		! Change all alphabetic letters to uppercase
		do i = 1, len_trim(sString)
			c = sString(i:i)
			if(c >= 'a' .and. c <= 'z') then
				c = char(ichar(c) - ichar('a') + ichar('A'))
			end if
		end do
		
	end subroutine ToUpper
	

	subroutine ToLower(sString)
	
		! Routine arguments
		character(len=*), intent(inout)	:: sString
		
		! Locals
		integer		:: i
		character	:: c
		
		! Change all alphabetic letters to lowercase
		do i = 1, len_trim(sString)
			c = sString(i:i)
			if(c >= 'A' .and. c <= 'Z') then
				c = char(ichar(c) - ichar('A') + ichar('a'))
			end if
		end do
		
	end subroutine ToLower
	

end module strings
