program problem_02
	implicit none
	integer:: n,i
	integer,allocatable::a(:)
	open(1,file='output.txt')
	read(*,*)n
	allocate(a(n))
	do i=0,n
		if(i==0 .or. i==1)then
			a(i)=i
		else if(mod(i,2)==0)then
			a(i)=a(i/2)
		else
			a(i)=a((i-1)/2)+a((i+1)/2)
		endif
	end do
	write(1,'(T15,A,T35,A)')'Value of n','n-th term'
	do i=0,n
		write(1,'(T20,I0,T40,I0)')i,a(i)
	end do
end program
