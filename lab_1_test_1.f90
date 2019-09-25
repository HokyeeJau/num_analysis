program first
	implicit none
	!---------------time and declaration block-------------------!
	! Declare variables
	character(len=40) :: str
	integer :: strlen
	logical :: ifPalindromic
	character(len=20) :: dateinfo 
	character(len=25) :: timeinfo

	!current date and time
	call date_and_time(dateinfo, timeinfo)
	dateinfo = dateinfo(1:4)//'.'//dateinfo(5:6)//'.'//dateinfo(7:8)//'.'
	timeinfo = timeinfo(1:2)//':'//timeinfo(3:4)//':'//timeinfo(5:10)
	print *, trim(dateinfo)
	print *, trim(timeinfo)
	!---------------time and declaration block-------------------!

	! Compute the length of the string
	print *, 'input your number less than 40 char'
	read *, str
	strlen = len_trim(str)

	! Judge if string is palindromic
	call ifStrPalin(str, strlen, ifPalindromic)

	if(ifPalindromic .eqv. .true.) then
		print *, 'It is palindromic'
	else
		print *, 'It is not palindromic'
	end if 

end program first

!-------------------------------
subroutine ifStrPalin(str, strlen, ifPalindromic)
	
	implicit none
	character(len=40) :: str
	integer :: i, j, strlen 
	logical :: ifPalindromic

	j = strlen 
	print *, 'In subroutine'
	print *, strlen

	ifPalindromic = .true.
	do i = 1, strlen/2, +1
		j = strlen - i + 1
		if(str(i:i) /= str(j:j)) then
			ifPalindromic = .false.
			exit
		end if
	end do 
end subroutine ifStrPalin
