program problem_03
    implicit none
    integer:: a,b,gcd
    write(*,*)"Enter the value of a and b : "
    read(*,*)a,b
    write(*,'(A,I0)')'gcd is: ',gcd(a,b)
end program

recursive function gcd(a,b)result(ans)
    implicit none
    integer:: a,b
    integer::ans
    if(mod(a,b)==0)then
        ans = b
    else
        ans = gcd(b,mod(a,b))
    end if
end function
