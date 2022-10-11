program practice_02
  implicit none
  real::a,b,c
  read(*,*),a,b,c
  open(1,file='output.txt')
  if(a+b>c .and. a+c>b .and. b+c>a)then
    print*,'triangle'
    write(1,'(A)')'triangle'
    if(a==b .and. b==c .and. c==a)then
      print*,'equilateral'
      write(1,'(A)')'equilateral'
    else
      print*,'not equilateral'
      write(1,'(A)')'not equilateral'
  end if
  if(a==b .or. b==c .or. c==a)then
    print*,'isosceles'
    write(1,'(A)')'isosceles'
  else
    print*,'not isosceles'
    write(1,'(A)')'not isosceles'
  end if
  if(a*a+b*b==c*c .or. a*a+c*c==b*b .or. b*b+c*c==a*a)then
    print*,'right-triangle'
    write(1,'(A)')'right-triangle'
  else
    print*,'not right-triangle'
    write(1,'(A)')'not right-triangle'
  end if
  else
    print*,'not triangle'
    write(1,'(A)')'not triangle'
  end if
end program
