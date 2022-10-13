program ass_02_05
  implicit none
  integer::n,i,k
  logical::ifPrime
  integer,allocatable::arr(:)
  read(*,*)n
  allocate(arr(n))
  call prime_divs(arr,n,k)
!  print*,k
  do i=1,k-1
    write(*,*)arr(i)
  end do
!  write(*,*)ifPrime(n)
end program

subroutine prime_divs(arr,n,k)
  implicit none
  logical::ifPrime
  integer::arr(n),n,i,j,k
  k=1
  do i=2,n
    if(mod(n,i)==0)then
      if(ifPrime(i))then
        arr(k)=i
        k=k+1
      end if
    end if
  end do
end subroutine

logical function ifPrime(n)
  implicit none
  integer::n,i
  do i=2,int(sqrt(real(n)))
    if(mod(n,i)==0)then
      ifPrime=.false.
      return
    end if
  end do
  ifPrime=.true.
  return
end function
