program ass_02_01
  implicit none
  real::a,b,c,det
  complex::root1,root2
  read(*,*)a,b,c
  det=b**2-4*a*c
  root1=(-b+sqrt(complex(det,0)))/(2*a)
  root2=(-b-sqrt(complex(det,0)))/(2*a)
  if(det<0)then
    write(*,*)"roots are imaginary: "
    write(*,"(2(F10.3,'+i(',F10.3,')'))")real(root1),aimag(root1),real(root2),aimag(root2)
  else
    write(*,*)"Roots are real: "
    write(*,*)real(root1),real(root2)
  end if
end program
