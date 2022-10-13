program ass_02_04
  implicit none
  integer::i,n,euler
  read(*,*)n
  do i=1,n
    if(euler(i,n)==1)write(*,*)i
  end do
end program

recursive integer function euler(a,b) result(res)
  implicit none
  integer::n,a,b
  if(mod(a,b)==0)then
    res=b
  else
    res=euler(b,mod(a,b))
  end if
end function
