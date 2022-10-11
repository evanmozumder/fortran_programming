program problem_06
  implicit none
  real::a,b,c
  read(*,*)a,b,c
  if(a+b>c .and. a+c>b .and. b+c>a)then
    write(*,*)"form a triangle"
    if(a==b .or. b==c .or. a==c)then
      write(*,*)"isosceles triangle"
    else
      write(*,*)"not an isosceles triangle"
    end if
    if(a==b .and. b==c .and. a==c)then
      write(*,*)"equilateral triangle"
    else
      write(*,*)"not an equilateral triangle"
    end if
    if(a**2+b**2==c**2 .or. a**2+c**2==b**2 .or. b**2+c**2==a**2)then
      write(*,*)"right-angled triangle"
    else
      write(*,*)"not a right-angled triangle"
    end if
  else
    write(*,*)"don't form a triangle"
  end if
end program
