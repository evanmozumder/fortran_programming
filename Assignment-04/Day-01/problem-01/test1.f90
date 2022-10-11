PROGRAM bisection
    IMPLICIT NONE
    INTEGER :: n
    REAL(KIND=8) :: fn, fn1, x, a, b, c, tol, a1, b1
    fn=(1.5*(x**3)) - (7*x) - 1 - (2.71828**x)
    WRITE(*,'(A)') "Enter the endpoints of the interval: "
    READ(*,*) a, b
    WRITE(*,'(A)') "Enter tolerance: "
    READ(*,*) tol
    a1=(1.5*(a**3)) - (7*a) - 1 - (2.71828**a)
    b1=(1.5*(b**3)) - (7*b) - 1 - (2.71828**b)
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
    fn1=(1.5*(c**3)) - (7*c) - 1 - (2.71828**c)
    IF(fn1*a1<0) THEN
        b=c
    ELSE IF(fn1*a1>0) THEN
        a=c
    END IF
    WRITE(*,'(10X,A,10X,A,25X,A,25X,A,19X,A)') "No.", "a", "b", "Pn", "|Pn-Pn1|/|Pn|"
    DO n=1,1000
        c=(a+b)/2
        fn=(1.5*(c**3)) - (7*c) - 1 - (2.71828**c)
        a1=(1.5*(a**3)) - (7*a) - 1 - (2.71828**a)
        b1=(1.5*(b**3)) - (7*b) - 1 - (2.71828**b)
        WRITE(*,*) n, a, b, fn, ABS(fn-fn1)/ABS(fn)
        IF(fn*a1<0) THEN
            b=c
        ELSE IF(fn*a1>0) THEN
            a=c
        END IF
        IF(ABS(fn-fn1)/ABS(fn)<tol.OR.fn==0) THEN
            WRITE(*,*) c
            EXIT
        END IF
        fn1=fn
    END DO
END PROGRAM
