program problem_04
  implicit none
  integer:: n,i,recursion,c=0
  write(*,*)'Enter the value of n: '
  read(*,*)n
  do i=1,n
    if(recursion(i,n)==1)then
      c=c+1
    end if
  end do
  write(*,*)"The number of integer is: ",c
end program

recursive function recursion(a,b)result(gcd)
    implicit none
    integer:: a,b
    integer::gcd
    if(mod(a,b)==0)then
        gcd = b
    else
        gcd = recursion(b,mod(a,b))
    end if
end function
