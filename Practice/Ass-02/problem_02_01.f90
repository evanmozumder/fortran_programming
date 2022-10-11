program problem_02_01
  implicit none
  real::a,b,c,det
  complex::r1,r2
  read(*,*)a,b,c
  det=b**2-(4*a*c)
  r1=(-b+sqrt(complex(det,0)))/(2*a)
  r2=(-b-sqrt(complex(det,0)))/(2*a)
  if(det<0)then
    write(*,*)"roots are imaginary, roots are: "
    write(*,'(2(F10.3,"+ i(",F10.3,")",/))',advance="no")real(r1),aimag(r1),real(r2),aimag(r2)
  else
    write(*,*)"roots are real, roots are: "
    write(*,'(2(F10.3,/))',advance="no")real(r1),real(r2)
  end if
end program
