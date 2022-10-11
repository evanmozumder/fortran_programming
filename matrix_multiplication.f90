program mat_multipli
  implicit none
  integer::s1,s2,s3,s4,v,i,j
  integer, allocatable:: m1(:,:),m2(:,:),m(:,:)
  read(*,*)s1,s2,s3,s4
  allocate(m1(s1,s2))
  allocate(m2(s3,s4))
  allocate(m(s1,s4))
  do i=1,s1
    do j=1,s2
      read(*,*)m1(i,j)
    end do
  end do

  do i=1,s3
    do j=1,s4
      read(*,*)m2(i,j)
    end do
  end do
  call mat_multi(s1,s4,m1,m2,m)

  do i=1,s3
    do j=1,s4
      write(*,'(I0,2X)',advance='no')m(i,j)
    end do
    write(*,*)
  end do
end program

subroutine mat_multi(s1,s4,matA,matB,ab)
  implicit none
  integer, intent(in)::s1,s4
  integer, intent(in):: matA(:,:), matB(:,:)
  integer,intent(out):: ab(s1,s4)
  integer:: i,j,k,l(2),m(2)
  l = shape(matA)
  m = shape(matB)
!  allocate(matA(l(1),l(2)))
!  allocate(matB(m(1),m(2)))
!  allocate(ab(l(1),m(2)))
  do i=1,l(1)
    do k=1,m(2)
      ab(i,k)=0
      do j=1,l(2)
        ab(i,k)=ab(i,k)+(matA(i,j)*matB(j,k))
      end do
    end do
  end do
end subroutine
