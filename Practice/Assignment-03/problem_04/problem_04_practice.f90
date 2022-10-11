program problem_04
  implicit none
  integer::a(3,3),i,j,num,aInv(3,3),m,n,mSize(2),FindDet
  logical::ifInv
  open(1,file="input.txt")
  do i=1,3
    do j=1,3
      if(j==3)then
        read(1,'(I2,X)')a(i,j)
      else
        read(1,'(I2,X)',advance="no")a(i,j)
      end if
    end do
  end do
  do i=1,3
    do j=1,3
      write(*,'(I0,X)',advance="no")a(i,j)
    end do
    write(*,*)
  end do
!  mSize=shape(a)
!  m=mSize(1)
!  n=mSize(2)
  call matInv(a,aInv,ifInv)
  print*,FindDet(a,3)
  do i=1,3
      do j=1,3
        write(*,'(I0,2X)',advance="no")a(i,j)
      end do
      write(*,*)
    end do
end program

subroutine matInv(a,aInv,ifInv)
  implicit none
  integer,dimension(3,3)::a,aInv
  integer::det
  logical::ifInv

end subroutine

integer FUNCTION FindDet(matrix, n)
    IMPLICIT NONE
    integer, DIMENSION(n,n) :: matrix
    INTEGER, INTENT(IN) :: n
    integer :: m, temp
    INTEGER :: i, j, k, l
    LOGICAL :: DetExists = .TRUE.
    l = 1
    !Convert to upper triangular form
    DO k = 1, n-1
        IF (matrix(k,k) == 0) THEN
            DetExists = .FALSE.
            DO i = k+1, n
                IF (matrix(i,k) /= 0) THEN
                    DO j = 1, n
                        temp = matrix(i,j)
                        matrix(i,j)= matrix(k,j)
                        matrix(k,j) = temp
                    END DO
                    DetExists = .TRUE.
                    l=-l
                    EXIT
                ENDIF
            END DO
            IF (DetExists .EQV. .FALSE.) THEN
                FindDet = 0
                return
            END IF
        ENDIF
        DO j = k+1, n
            m = matrix(j,k)/matrix(k,k)
            DO i = k+1, n
                matrix(j,i) = matrix(j,i) - m*matrix(k,i)
            END DO
        END DO
    END DO

    !Calculate determinant by finding product of diagonal elements
    FindDet = l
    DO i = 1, n
        FindDet = FindDet * matrix(i,i)
    END DO

END FUNCTION FindDet
