program test
  implicit none
  character(len=50)::name
  open(1,file="input.txt")
  read(1,*)name
  write(*,"(A20)")name
end program
