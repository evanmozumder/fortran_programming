
PROGRAM quadratic_roots
    IMPLICIT NONE
    REAL:: a, b, c, desc, value1, value2
    COMPLEX:: root1, root2

    OPEN(UNIT = 7, FILE = "out1q2.txt", ACTION = "WRITE")
    WRITE(*, 100)"Enter the value of the  coefficients a, b, c  of  ax^2 + bx + c : "
    100 FORMAT(A)

    READ(*,*) a, b, c

    desc = (b**2) - 4.0*a*c

    root1 = ( -b + SQRT(COMPLEX(desc, 0.0))) / (2.0 * a)
    root2 = ( -b - SQRT(COMPLEX(desc, 0.0))) / (2.0 * a)
    print*,real(root1)
    print*,aimag(root1)

   IF(desc >= 0 )THEN
        WRITE(7, 200)
        200 FORMAT("Roots are Real")
        WRITE(7,201)"Real Roots Are : "
        201 FORMAT(A)
        WRITE(7, 202) REAL(root1), REAL(root2)
        202 FORMAT("X1 = ", F12.5,/, "X2 = ", F12.5)

   ELSE
    WRITE(7, 203)
    203 FORMAT("Roots are Complex")
    WRITE(7, 204)
    204 FORMAT("Complex Roots Are: ")

    WRITE(7, 205)REAL(root1), AIMAG(root1), REAL(root2), AIMAG(root2)
    205 FORMAT("X1 = ", F12.5, " + i", F12.5, / , "X2 = ", F12.5, " + i ", F12.5, / )
    END IF
    CLOSE(7)
END PROGRAM
