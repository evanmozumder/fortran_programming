program primeDivisor
    integer:: n
    write(*,*)'Enter the value of n: '
    read(*,*)n
    write(*,*)'Prime Divisors are: '
    call primeD(n)
end program

subroutine primeD(n)
    implicit none
    integer:: n,i,j,det
    do i=2,n
        if(mod(n,i)==0)then
            det=0
            do j=2,i
                if(mod(i,j)==0 .and. i/=j)then
                    det=1
                    exit
                end if
            end do
            if(det/=1)print*,i
        end if
    end do
end subroutine
