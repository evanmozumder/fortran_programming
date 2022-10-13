program ass_02_03
  implicit none
  integer::a,b,gcd
  read(*,*)a,b
  print*,gcd(a,b)
end program

recursive integer function gcd(a,b) result(res)
  implicit none
  integer::a,b
  if(mod(a,b)==0)then
    res=b
  else
    res=gcd(b,mod(a,b))
  end if
end function
