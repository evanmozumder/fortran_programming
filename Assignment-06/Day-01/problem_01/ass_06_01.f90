program ass_06_01
  implicit none
  real,allocatable,dimension(:)::x_arr,y_arr
  integer::n,i,j
  real::x,su=0,nominator=1,denominator=1,resultt
  open(1,file="input_06_01.txt")
  print*,"Enter the number of given points: "
  read(*,*)n
  allocate(x_arr(n),y_arr(n))
  do i=1,n
    read(1,"(f4.2,f7.5)")x_arr(i),y_arr(i)
  end do
  print*,"Enter the the value"
  read(*,*)x
  call lagrange(x,x_arr,y_arr,n,resultt)
  print*,resultt
end program

subroutine lagrange(x,x_arr,y_arr,n,resultt)
  implicit none
  real::nominator,denominator,x,resultt
  real,dimension(n)::x_arr,y_arr
  integer::n,i,j
  resultt=0
  do i=1,n
    nominator=1
    denominator=1
    do j=1,n
      if(i/=j)then
        nominator=nominator*(x-x_arr(j))
        denominator=denominator*(x_arr(i)-x_arr(j))
      end if
    end do
    resultt=resultt+((nominator/denominator)*y_arr(i))
  end do
end subroutine
