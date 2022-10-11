program problem_05
  implicit none
  integer::n,i
  logical:: check
  read(*,*)n
  do i=2,int(n/2)+1
    do while(mod(n,i)==0)
      call primeD(i,check)
      if(check)then
        write(*,'(I0,2X)',advance="no")i
      end if
      n=n/i
      if(n<i)exit
    end do
  end do
end program

subroutine primeD(n,decesion)
  implicit none
  integer::n,i
  logical:: decesion
  decesion=.true.
  do i=2,int(sqrt(real(n)))
    if(mod(n,i)==0)then
      decesion=.false.
      exit
    end if
  end do
end subroutine
