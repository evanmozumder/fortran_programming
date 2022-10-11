program problem_02
  implicit none
  character(len=50)::name
  integer::i,act_len=0,asc
  character::dt
  open(1,file="name.txt")
  read(1,'(A)')name
  ! actual length
  do i=1,len(name)
    if(name(i:i)==' ')then
      if(name(i+1:i+1)==' ')then
        exit
      end if
    end if
    act_len=act_len+1
  end do
  ! (1)
  do i=1,act_len
    if(name(i:i)==' ')cycle
    write(*,'(A)',advance="no")name(i:i)
  end do
  write(*,*)
  ! (2)
  do i=1,act_len
    if(name(i:i)=='A'.or.name(i:i)=='E'.or.name(i:i)=='I'.or.name(i:i)=='O'.or.name(i:i)=='U')cycle
    if(name(i:i)=='a'.or.name(i:i)=='e'.or.name(i:i)=='i'.or.name(i:i)=='o'.or.name(i:i)=='u')cycle
    write(*,'(A)',advance="no")name(i:i)
  end do
  write(*,*)
  ! (3)
  do i=1,act_len
    asc=iachar(name(i:i))
    dt=name(i:i)
    if(asc>=97)then
      dt=achar(asc-32)
    end if
    write(*,'(A)',advance="no")dt
  end do
  write(*,*)
  ! (4)
  do i=act_len,1,-1
    write(*,'(A)',advance="no")name(i:i)
  end do
end program
