program prime_divisor
    implicit none
    integer::n,i
    logical::yesno
    write(*,*)"Enter the number : "
    read(*,*)n

    do i = 2, int(n/2)+1
        do while(mod(n,i)==0)
            call primeD(i,yesno)
            if(yesno)then
                write(*,45,advance="NO")i
                45 format(I0,2X)
                n = n/i
                if(n<i)exit
            end if
        end do
    end do
    write(*,*)
end program

subroutine primeD(n,decision)
    implicit none
    integer,intent(in)::n
    logical,intent(out)::decision
    integer::i
    decision = .true.
    do i = 2, int(sqrt(real(n)))
        if(mod(n,i)==0)then
            decision = .false.
            return
        end if
    end do
end subroutine
