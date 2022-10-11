program problem_07
  integer::n,i,j,num_digit,summ
  integer,allocatable::arr(:)
  read(*,*)n
  do i=1,100
    summ=0
    num_digit=floor(log10(real(n)))+1
    allocate(arr(num_digit))
    do j=num_digit,1,-1
      arr(j)=mod(n,10)
      summ=summ+arr(j)**2
      n=n/10
    end do
    do j=1,num_digit
      write(*,10,advance='no')arr(j)
      if(j/=num_digit)then
        write(*,11,advance='no')
      else
        write(*,12)summ
      end if
    end do
    10 format(I0,'^2')
    11 format(' + ')
    12 format(' = ',I0)
    n=summ
    deallocate(arr)
    if(summ==1)then
      print*,'Happy Number'
      stop
    end if
  end do
  print*,'Max Iteration Reached, Result Inconclusive'
end program
