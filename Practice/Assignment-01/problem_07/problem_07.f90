program problem_07
  implicit none
  integer:: n,n_digit,i,d,sq_sum,j
  integer,allocatable,dimension(:)::a_digit
  logical::check=.false.
  read(*,*)n
  do j=1,100
    sq_sum=0
    n_digit=floor(log10(real(n)))+1
    allocate(a_digit(n_digit))
    do i=1,n_digit
      d=mod(n,10)
      n=n/10
      sq_sum=sq_sum+d**2
      a_digit(i)=d
    end do
    do i=n_digit,1,-1
      write(*,'(I0,A)',advance="no")a_digit(i),"^2"
      if(i/=1)then
        write(*,'(A)',advance="no")" + "
      end if
    end do
    n=sq_sum
    write(*,'(" = ",I0)')n
    if(n==1)then
      check=.true.
      exit
    end if
    deallocate(a_digit)
  end do
  if(check)then
    write(*,*)"Happy Number"
  else
    write(*,*)"Maximum iteration reached test inconclusive."
  end if
end program
