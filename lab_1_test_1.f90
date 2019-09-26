program first
	implicit none
	!---------------time and declaration block-------------------!
	! Declare variables
	character(len=40) :: str
	integer :: strlen
	logical :: ifPalindromic
	character(len=20) :: dateinfo 
	character(len=25) :: timeinfo
	character :: ifOut

	!current date and time
	call date_and_time(dateinfo, timeinfo)
	dateinfo = dateinfo(1:4)//'.'//dateinfo(5:6)//'.'//dateinfo(7:8)//'.'
	timeinfo = timeinfo(1:2)//':'//timeinfo(3:4)//':'//timeinfo(5:10)
	print *, trim(dateinfo)
	print *, trim(timeinfo)
	!---------------time and declaration block-------------------!

	! Compute the length of the string
	open(1, file='lab_1_test_1.txt')
	do 
		print *, 'input your number less than 40 char'
		read *, str
		strlen = len_trim(str)

		! Judge if string is palindromic
		! And record them in a txt
		call ifStrPalin(str, strlen, ifPalindromic)
	
		write(1, *) str, strlen
		if(ifPalindromic .eqv. .true.) then
			print *, 'It is palindromic'
			write(1, *) 'It is palindromic'
		else
			print *, 'It is not palindromic'
			write(1, *) 'It is not palindromic'
		end if 

		print *, 'If you continue to test, kick Y'
		read *, ifOut
		if(ifOut == 'Y' .or. ifOut =='y') exit

	end do

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
