PROGRAM fixed_point_iteration
    IMPLICIT none
    INTEGER :: n
    REAL(KIND=8) :: fn, x, a, b, c, tol, a1, b1, g
    fn=-(2**(-x))+(x**3)-(0.5*(x**2))+x
    g=(2**(-x))-(x**3)+(0.5*(x**2))
    WRITE(*,'(A)') "Enter the endpoints of the interval: "
    READ(*,*) a, b
    WRITE(*,'(A)') "Enter tolerance: "
    READ(*,*) tol
    a1=-(2**(-a))+(a**3)-(0.5*(a**2))+a
    b1=-(2**(-b))+(b**3)-(0.5*(b**2))+b
    IF(a1*b1>0) THEN
        WRITE(*,'(A)') "There is no solution between this interval. Change the endpoints of interval and run the code again."
        STOP
    ELSE IF(a1*b1==0) THEN
        IF(a1==0) THEN
            WRITE(*,'(A, 1F10.6)') "The root is: ", a
        ELSE IF(b1==0) THEN
            WRITE(*,'(A, 1F10.6)') "The root is: ", b
        END IF
    ELSE IF(a1*b1<0) THEN
        WRITE(*,'(A)') "There is a root between this interval."
    END IF
    c=(a+b)/2
    WRITE(*,'(10X,A,10X,A,25X,A,25X,A,19X,A)') "No.", "Pn-1", "Pn", "f(Pn)"
    DO n=1,1000
        fn=-(2**(-c))+(c**3)-(0.5*(c**2))+c
        g=(2**(-c))-(c**3)+(0.5*(c**2))
        WRITE(*,*) n, c, g, fn
        IF(ABS(fn)<tol.OR.fn==0) THEN
            WRITE(*,*) c
            EXIT
        ELSE
            c=g
        END IF
    END DO
END PROGRAM

