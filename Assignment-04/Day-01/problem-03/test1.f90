

function f(x)

    f=16*(x**4)+88*(x**3)+159*(x**2)+76*x-240

end function

function df(x)

    df=64*(x**3)+264*(x**2)+318*x+76

end function

program Newton_Raphson

    implicit none

    integer :: i,n
    real :: a,c,b,TOL,f,df,toltest,p

    write(*,*) 'Enter the initial guess, x0:'
    read(*,*) a

    TOL=0.00001
    n=200

    write(*,7)'No of Iterations','P(n-1)',"f'(P(n-1))",'P(n)','f(P(n))','Tolerance'
    7 format (a,10x,a,10x,a,12x,a,15x,a,8x,a)

    i=1
    do while (i<=n)
        if (df(a)==0) then
            write(*,*)'Method fails.'
            exit
        end if

        p=a-(f(a))/(df(a))
        toltest=abs(p-a)

        write(*,3) i,a,df(a),p,f(p),toltest
        3 format (1i2,15x,f17.10,2x,f17.8,2x,f17.10,2x,f17.10,2x,f17.10)

        if (toltest<TOL) then
            write(*,*) 'The root is:',p
            exit
        end if

        a=p
        i=i+1

    end do
end program
