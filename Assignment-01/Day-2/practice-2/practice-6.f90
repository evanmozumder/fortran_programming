program practice_06
  real::a,b,c
  read(*,*)a,b,c
  if(a+b>c .or. b+c>a .or. a+c>b)then
    print*,'triangle'
    if(a==b .and. b==c .and. c==a)then
      print*,'equilateral triangle'
    else
      print*,'not an equilateral triangle'
    end if
    if(a==b .or. b==c .or. c==a)then
      print*,'isosceles triangle'
    else
      print*,'not an isosceles triangle'
    end if
    if(a**2+b**2==c**2 .or. a**2+c**2==b**2 .or. b**2+c**2==a**2)then
      print*,'right-triangle'
    else
      print*,'not right-triangle'
    end if
  end if
end program
