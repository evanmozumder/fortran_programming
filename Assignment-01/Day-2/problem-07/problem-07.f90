program problem_07
  integer::n_digit,summ,check,n,i,j,k
  integer,allocatable,dimension(:):: a_digit
  read(*,*)n
  do i=1,100
    summ=0
    n_digit=floor(log10(real(n)))+1
    allocate(a_digit(n_digit))
    do j=1,n_digit
      a_digit(j)=mod(n,10)
      summ=summ+a_digit(j)**2
      n=n/10
    end do
    n=summ
    do k=n_digit,1,-1
      write(*,100,advance='no')a_digit(k)
      if(k /= 1)then
        write(*,'(A)',advance='no')' + '
      end if
    end do  
    write(*,101)summ
    100 format(I0,'^2')
    101 format(' = ',I0)
    if(summ==1)then
      write(*,*)'Happy Number'
      stop
    end if
    deallocate(a_digit)
  end do
  print*,'test inconclusive'
end program
