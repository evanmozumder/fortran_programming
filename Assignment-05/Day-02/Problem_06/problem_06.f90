program problem_06
  implicit none
  integer::l,i,n,decToBin
!  integer,allocatable:: ar(:)
!  logical:: check=.true.

  write(*,*)"Enter the length of the bit strings: "
!  read(*,*)l
!  l=4
!  do i=0,(2**l)-1
!    print*,decToBin(i)
!  end do
  print*,decToBin(10)
!  n=10
!  allocate(ar(l))
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
!  bin=0
!  i=0
!  n=20
!  do while(check)
!!    ar(i)=mod(n,2)
!    bin=bin+(mod(n,2)*(10**i))
!!    print*,mod(n,2)
!    n=n/2
!    i=i+1
!    if(n==0)check=.false.
!  end do
!  print*,bin
end program

integer function decToBin(n)
  implicit none
  integer::n,j,bin
!  integer,intent(inout)::bin
  logical:: check=.true.
!  integer,allocatable:: ar(:)
!  allocate(ar(l))
  bin=0
  j=0
  do while(check)
!    ar(i)=mod(n,2)
    bin=bin+(mod(n,2)*(10**j))
!    print*,mod(n,2)
    n=n/2
    j=j+1
    if(n==0)check=.false.
  end do
!  print*,bin
  decToBin=bin
!  do i=1,l
!    write(*,'(I0)',advance='no')ar(i)
!  end do
end function decToBin
