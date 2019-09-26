program matsum
	implicit none
	! Process
	! Declare 100*100 matrix to store the values
	! Calculate the sum of sub-diagonal values and store in an array of size 200
	! Store the matrix and the sum into txt

	! Declaration
	integer, dimension(4, 4) :: matrix
	integer, dimension(8) :: subsum
	integer :: i, j, row, vol, temp

	! Input

	open(1, file='lab_1_test_3.txt')
	write(1, *) 'Because of the amounts of the dataset, this result will only operate 4*4 matrix'
	print *, 'please enter the values of matrix'
	
	do i=1, 4, +1
		do j=1, 4, +1
			print *, i, j
			read *, matrix(i, j)
		end do
	end do 

	i = 1
	do temp=0, 3, +1
		subsum(i) = 0
		subsum(8-temp) = 0
		do row=1, 4, +1
			vol = row + temp
			if(vol <= 4)then
				print *, row, vol
				subsum(i) = subsum(i) + matrix(row, vol)
				subsum(8-temp) = subsum(8-temp) + matrix(vol, row)
			end if
		end do
		print *, i, subsum(i)
		print *, 8-temp, subsum(8-temp)
		i = 1 + i
	end do


	print *, subsum
	i = 1
	do 
		if(i /= 8) then
			write(1, *) subsum(i)
			i = i + 1
		else
			exit
		end if
	end do
end program matsum