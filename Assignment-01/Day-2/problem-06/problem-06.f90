program problem_06
  real:: a,b,c
  open(unit=1,file='mat.txt',status='replace')
  write(*,*)'Enter the length of the sides of the triangle:'
  read(*,*)a,b,c
  write(1,10)a,b,c
  10 format("Sides are: ",/,'A: ',F10.2,/,'B: ',F10.2,/,'C: ',F10.2)
  if(a+b>c .and. b+c>a .and. a+c>b)then
    write(1,'(A)')'It is a triangle'
    if(a==b .and. b==c .and. c==a)then
      write(1,'(A)')'This triangle is Equilateral'
    else
      write(1,'(A)')'This triangle is not Equilateral'
    end if
    if(a==b .or. b==c .or. c==a)then
      write(1,'(A)')'This triangle is Isosceles'
    else
      write(1,'(A)')'This triangle is not Isosceles'
    end if
    if(a**2+b**2==c**2 .or. a**2+c**2==b**2 .or. b**2+c**2==a**2)then
      write(1,'(A)')'This is a right angled triangle'
    else
      write(1,'(A)')'This is not a right angled triangle'
    end if
  else
    write(1,'(A)')'Not a triangle'
  end if
end program
