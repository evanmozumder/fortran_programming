program problem_06
  implicit none
  integer::l,i,n,decToBin,bin,j
  logical:: check
  write(*,*)"Enter the length of the bit strings: "
  read(*,*)l
  do i=0,(2**l)-1
    bin=0
    j=0
    n=i
    check=.true.
    do while(check)
      bin=bin+(mod(n,2)*(10**j))
      n=n/2
      j=j+1
      if(n==0)check=.false.
    end do
    write(*,1)bin
    1 format(I0)
  end do
!  print*,decToBin(10)
!  n=10
!  i=l

!  do while(check)
!    ar(i)=mod(n,2)
!!    print*,mod(n,2)
!    n=n/2
!    i=i-1
!    if(n==0)check=.false.
!  end do
!  do i=1,l
!    write(*,'(I0)',advance='no')ar(i)
!  end do
end program

!integer function decToBin(n)
!  implicit none
!  integer::n,j,bin
!  logical:: check=.true.
!  bin=0
!  j=0
!  do while(check)
!    bin=bin+(mod(n,2)*(10**j))
!    n=n/2
!    j=j+1
!    if(n==0)check=.false.
!  end do
!  decToBin=bin
!end function decToBin
