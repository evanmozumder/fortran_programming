program test
  implicit none
  if(0.0001>0.000001)then
    print*,"Yes"
  else
    print*,"N0"
  end if
end program
