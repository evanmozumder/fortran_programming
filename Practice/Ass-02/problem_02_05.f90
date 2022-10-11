program problem_02_05
  implicit none
  integer::n,i
  logical::check
  read(*,*)n
  call primeD(n,check)
  if(check)then
    write(*,*)"Number is Prime Number"
  else
    do i=2,int(n/2)+1
      do while(mod(n,i)==0)
        call primeD(i,check)
        if(check)then
          write(*,'(I0,2X)',advance="no")i
          n=n/i
          if(n<i)exit
        end if
      end do
    end do
  end if
end program

subroutine primeD(n,check)
  implicit none
  integer::n,i
  logical,intent(out)::check
  check=.true.
  do i=2,int(sqrt(real(n)))
    if(mod(n,i)==0)then
      check=.false.
      return
    end if
  end do
end subroutine
