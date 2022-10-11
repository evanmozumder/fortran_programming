program problem_01
  implicit none
  real:: a,b,c,det
  complex:: root1,root2
  read(*,*)a,b,c
  det=b**2-4*a*c
  root1=(-b+sqrt(complex(det,0.0)))/(2.0*a)
  root2=(-b-sqrt(complex(det,0.0)))/(2.0*a)
  if(det<0)then
    write(*,*)"Roots are imaginary, roots are: "
    write(*,'(2(F6.2," + i(",F6.2,")",/))',advance="no")real(root1),aimag(root1),real(root2),imag(root2)
  else
    write(*,*)"Roots are real, roots are: "
    write(*,'(2(F6.2,/))',advance="no")real(root1),real(root2)
  end if
end program
