
PROGRAM BISECTION
  IMPLICIT NONE

  INTEGER :: I,J,K
  REAL,DIMENSION(0:100) :: P
  REAL :: F,A,B,C
  REAL :: ROOT
  A = -2
  B = 0
  WRITE(*,'(5A10)') "A","B","C","P(N)","P(N)-P(N)/P(N)"
  P(0) = 0
  DO I =1,100
    C = (A+B)/2
    P(I) = C
    WRITE(*,'(5F10.5)') A,B,C,P(I),ABS(P(I)-P(I-1))/ABS(P(I))
    IF(F(P(I)) == 0) THEN
      ROOT = P(I)
      PRINT *, "ROOT IS : ",ROOT
      EXIT
    END IF
  END DO

  IF(F(A)*F(C) > 0) THEN
    A = C
  ELSE
    B = C
  END IF

END PROGRAM

REAL FUNCTION F(X)
  REAL :: X
  F = 1.5*X**3-7*X-1-EXP(X)
END FUNCTION
