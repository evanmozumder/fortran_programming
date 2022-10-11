program test
  implicit none
  integer,allocatable,dimension(:):: a_digit
  integer::i,j,summ,n,n_digit
  read(*,*)n
  summ=0
  n_digit=floor(log10(real(n)))+1
  do j=1,n_digit
    a_digit(j)=mod(n,10)
    summ=summ+a_digit(j)**2
    n=n/10
  end do
  write(*,*)(a_digit(i),i=1,n_digit)
end program
