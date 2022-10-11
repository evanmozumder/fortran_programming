program problem_04
  implicit none
  integer::i,j,n
  real,dimension(3,3)::a,ainv
  logical :: ifInv
  open(101,file='input.txt')
  open(102,file='output.txt')
  do i=1,3
    do j=1,3
        read(101,*)a(i,j)
    end do
  end do
  write(102,*)"a="
  do i=1,3
    do j=1,3
      write(102,'(f10.2,2x)',advance='no')a(i,j)

    end do
      write(102,'(/)')
  end do

  call matInv(a,ainv,ifInv)
  if(ifInv)then
    write(102,*)"a inverse = "
    do i=1,3
      do j=1,3
        write(102,'(f10.2,2x)',advance='no')ainv(i,j)
      end do
    write(102,'(/)')
    end do
  else
    write(102,*)" a is singular "
  end if
end program

subroutine matInv (a, ainv, ifInv)
  implicit none
  real, dimension(3,3), intent(in)  :: a
  real, dimension(3,3), intent(out) :: ainv
  logical, intent(out) :: ifInv
  !double precision, parameter :: eps = 1.0d-10
  real :: det
  real, dimension(3,3) :: cofactor
  det =   a(1,1)*a(2,2)*a(3,3)  &
        - a(1,1)*a(2,3)*a(3,2)  &
        - a(1,2)*a(2,1)*a(3,3)  &
        + a(1,2)*a(2,3)*a(3,1)  &
        + a(1,3)*a(2,1)*a(3,2)  &
        - a(1,3)*a(2,2)*a(3,1)
  if (det==0) then
     ifInv = .false.
     return
  end if
  cofactor(1,1) = +(a(2,2)*a(3,3)-a(2,3)*a(3,2))
  cofactor(1,2) = -(a(2,1)*a(3,3)-a(2,3)*a(3,1))
  cofactor(1,3) = +(a(2,1)*a(3,2)-a(2,2)*a(3,1))
  cofactor(2,1) = -(a(1,2)*a(3,3)-a(1,3)*a(3,2))
  cofactor(2,2) = +(a(1,1)*a(3,3)-a(1,3)*a(3,1))
  cofactor(2,3) = -(a(1,1)*a(3,2)-a(1,2)*a(3,1))
  cofactor(3,1) = +(a(1,2)*a(2,3)-a(1,3)*a(2,2))
  cofactor(3,2) = -(a(1,1)*a(2,3)-a(1,3)*a(2,1))
  cofactor(3,3) = +(a(1,1)*a(2,2)-a(1,2)*a(2,1))

  ainv = transpose(cofactor) / det
  ifInv = .true.
  return
end subroutine


