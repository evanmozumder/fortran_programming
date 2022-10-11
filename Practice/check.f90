
program all_bit_string_of_length_n
    integer :: i,j,n,l
    character,allocatable :: bit(:)

    print*,"Enter n:"
    read*,n
    allocate(bit(n))
    bit='0'
    total=2**n
    do i=1,l
        write(*,*)bit
        do j=n,1,-1
            if (bit(j)=='0') then
                bit(j)='1'
                exit
                else
                    bit(j)='0'
            end if
        end do
    end do
end program
