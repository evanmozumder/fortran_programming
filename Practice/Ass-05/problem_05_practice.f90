program binary
  implicit none
  integer::i,l,j,bin,n
  read(*,*)l
  do i=0,(2**l)-1
    bin=0
    j=0
    n=i
    do while(.true.)
      bin=bin+mod(n,2)*10**j
      n=n/2
      j=j+1
      if(n==0)exit
    end do
    write(*,*)bin
  end do
end program
