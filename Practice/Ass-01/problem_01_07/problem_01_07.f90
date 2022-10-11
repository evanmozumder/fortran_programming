program happy_num
  implicit none
  integer::n,i,sq_sum,j,d,digit
  integer,allocatable,dimension(:)::adigit
  read(*,*)n
  do j=1,100
    digit=floor(log10(real(n)))+1
    allocate(adigit(digit))
    sq_sum=0
    do i=1,digit
      d=mod(n,10)
      n=n/10
      adigit(i)=d
      sq_sum=sq_sum+d**2
    end do
    do i=digit,1,-1
      write(*,'(I0,A)',advance="no")adigit(i),'^2'
      if(i/=1)then
        write(*,'(A)',advance="no")' + '
      else
        write(*,'(A,I0)')' = ',sq_sum
      end if
    end do
    if(sq_sum==1)then
      write(*,*)"Happy Number"
      stop
    end if
    n=sq_sum
    deallocate(adigit)
  end do
  write(*,*)"Test Inconclusive"
end program
