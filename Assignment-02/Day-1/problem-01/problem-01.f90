program problem_01
    implicit none
    real:: a,b,c,det
    complex:: root1,root2
    open(1,file='outputFile.txt')
    write(*,*)'Enter three real numbers: '
    read(*,*)a,b,c
    det = b**2 - 4.0*a*c
    root1=(-b+sqrt(complex(det,0.0)))/(2.0*a)
    root2=(-b-sqrt(complex(det,0.0)))/(2.0*a)

    if(det .GE. 0)then
        write(1,'(A,/,A)')'Roots are Real','Roots are: '
        write(1,10)real(root1),real(root2)
    else
        write(1,'(A,/,A)')'Roots are Imaginary','Roots are: '
        write(1,11)real(root1),aimag(root1),real(root2),aimag(root2)
    end if
    10 format('X1 = ',F10.5,/,'X2 = ',F10.5)
    11 format('X1 = ',F10.5,' + ','(',F10.5,')','i',/,'X2 = ',F10.5,' + ','(',F10.5,')','i')
end program
