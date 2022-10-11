program problem_03
  implicit none
  integer::a,b,gcd
  read(*,*)a,b
  write(*,*)gcd(a,b)
end program

recursive function gcd(a,b) result(res)
  implicit none
  integer::a,b,res
  if(mod(a,b)==0)then
    res=b
  else
    res=gcd(b,(mod(a,b)))
  end if
end function
