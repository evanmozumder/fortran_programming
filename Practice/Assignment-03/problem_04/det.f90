program det_of_mat
  implicit none
  real::a(3,3),i,j,num,aInv(3,3),m,n,mSize(2),FindDet
  logical::ifInv
  open(1,file="input.txt")
  do i=1,3
    do j=1,3
      if(j==3)then
        read(1,'(F4.1,X)')a(i,j)
      else
        read(1,'(F4.1,X)',advance="no")a(i,j)
      end if
    end do
  end do
  do i=1,3
    do j=1,3
      write(*,'(F4.1,X)',advance="no")a(i,j)
    end do
    write(*,*)
  end do
!  mSize=shape(a)
!  m=mSize(1)
!  n=mSize(2)
!  call matInv(a,aInv,ifInv)
!  print*,FindDet(a,3)
  do i=1,3
      do j=1,3
        write(*,'(F4.1,2X)',advance="no")a(i,j)
      end do
      write(*,*)
    end do
end program

real FUNCTION FindDet(m, n)
  IMPLICIT NONE
  real, DIMENSION(n,n) :: m
  real::mult
  INTEGER, INTENT(IN) :: n,i,j
  do i=1,n
    do j=1,n
      mult=m(i,j)*
    end do
  end do
END FUNCTION FindDet
