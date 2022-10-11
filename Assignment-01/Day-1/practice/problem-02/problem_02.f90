program problem_02
  character(len=50):: name
  integer:: act_len,an
  open(unit=1,file='name.txt',status='old')
  read(1,'(A)')name
  write(*,'(A)')name

  act_len=0
  do i=1,len(name)
    if(name(i:i)==' ')then
      if(name(i+1:i+1)==' ')exit
    end if
    act_len = act_len+1
  end do
  ! (i)
  do i=1,act_len
    if(name(i:i)==' ')cycle
    write(*,'(A)',advance='no')name(i:i)
  end do
  write(*,*)
  ! (ii)
  do i=1,act_len
    if(name(i:i)=='A' .or. name(i:i)=='E' .or. name(i:i)=='I' .or. name(i:i)=='O' .or. name(i:i)=='U')cycle
    if(name(i:i)=='a' .or. name(i:i)=='e' .or. name(i:i)=='i' .or. name(i:i)=='o' .or. name(i:i)=='u')cycle
    write(*,'(A)',advance='no')name(i:i)
  end do
  write(*,*)

  ! (iii)
  do i=1,act_len
    an = iachar(name(i:i))
    if(an>=97)then
      an = an-32
      write(*,'(A)',advance='no')achar(an)
    else
      write(*,'(A)',advance='no')name(i:i)
    end if
  end do
  write(*,*)

  ! (iv)
  do i=act_len,1,-1
    write(*,'(A)',advance='no')name(i:i)
  end do
end program
