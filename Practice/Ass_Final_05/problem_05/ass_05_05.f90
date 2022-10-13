program ass_05_05
  implicit none
  integer::n,i,j,l,bin
  read(*,*)l
  do i=0,(2**l)-1
    j=0
    n=i
    bin=0
    do while(.true.)
      bin=bin+mod(n,2)*10**j
      j=j+1
      n=n/2
      if(n==0)exit
    end do
    write(*,*)bin
  end do
end program
