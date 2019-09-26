program findprime
	implicit none 
	! Declaration
	!---------------time and declaration block-------------------!
	! Declare variables
	
	character(len=20) :: dateinfo 
	character(len=25) :: timeinfo
	real :: num, stadd
	integer :: index, stad, temp, i
	integer, dimension(10000) :: warehouse
	logical :: ifodd

	!current date and time
	call date_and_time(dateinfo, timeinfo)
	dateinfo = dateinfo(1:4)//'.'//dateinfo(5:6)//'.'//dateinfo(7:8)//'.'
	timeinfo = timeinfo(1:2)//':'//timeinfo(3:4)//':'//timeinfo(5:10)
	print *, trim(dateinfo)
	print *, trim(timeinfo)
	!---------------time and declaration block-------------------!
	
	! Mark down the index of the array stored the prime number
	! Define the neccessary varibales
	index = 1
	ifodd = .true.
	! Judge if the number can be divided without reminder
	! The 2nd method
	do i = 1, 10000, +1 
		! Judgement
		stadd = sqrt(float(i))+1
		stad = ifix(stadd)
		do temp = 2, stad, +1
			if(mod(i, temp) == 0) then
				ifodd = .false.
				exit
			end if
		end do

		! Processing
		if(ifodd .eqv. .true.) then
			warehouse(index) = i
			index = index + 1
		else
			ifodd = .true.
		end if
	end do

	!record in txt
	open(1, file='lab_1_test_2.txt')
	print *, 'There are ', index-1, ' prime numbers'
	write(1, *) 'There are ', index-1, ' prime numbers'
	i = 1
	do 
		if(warehouse(i) /= 0) then
			print *, warehouse(i)
			write(1, *) warehouse(i)
			i = i + 1
		else
			exit
		end if
	end do
end program findprime
