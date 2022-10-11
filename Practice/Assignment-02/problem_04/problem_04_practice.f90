program problem_04
  implicit none
  integer::n,i,gcd,c=0
  read(*,*)n
  do i=1,n
    if(gcd(n,i)==1)then
      write(*,*)i
      c=c+1
    end if
  end do
  write(*,'(A,I0)')"The number of integer is: ",c
end program

recursive function gcd(a,b) result(res)
  implicit none
  integer:: a,b,res
  if(mod(a,b)==0)then
    res=b
  else
    res=gcd(b,(mod(a,b)))
  end if
end function
