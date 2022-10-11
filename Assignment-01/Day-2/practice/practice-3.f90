program practice_03
  integer,allocatable::all_digits(:)
  integer:: n,summ
  read(*,*)n
  open(1,file='steps.txt')
  do i=1,100
    summ=0
    num_digit=floor(log10(real(n)))+1
    allocate(all_digits(num_digit))
    do j=num_digit,1,-1
      all_digits(j)=mod(n,10)
      summ=summ+all_digits(j)**2
      n=n/10
    end do
    do j=1,num_digit
      write(1,10,advance='no')all_digits(j)
      if(j/=num_digit)then
        write(1,11,advance='no')
      else
        write(1,12)summ
      end if
    end do
    10 format(I0,'^2')
    11 format(' + ')
    12 format(' = ',I0)
    deallocate(all_digits)
    n=summ
    if(summ==1)then
      print*,'Happy Number'
      stop
    end if
  end do
  print*,'max iteration 100 reached'
end program
