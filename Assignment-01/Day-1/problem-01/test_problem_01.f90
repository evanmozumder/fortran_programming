
program test
    implicit none
    character, allocatable :: first_array(:)
    integer, allocatable:: second_array(:)
    real, allocatable:: third_array(:),fourth_array(:)
    integer:: st,i,j,len1,len2,len3,len4,te,iData,fm
    character:: cData
    character(len=100)err_msg
    open(unit=1,file="InputFile_1.txt",status="old")
    open(unit=2,file="InputFile_2.txt",status="old")
    open(unit=3,file="InputFile_3.txt",status="old")
    open(unit=4,file="InputFile_4.txt",status="old")
    open(unit=5,file="OutputFile.txt",status="replace")

    ! reading first file
    read(1,'(I1,X)'),len1
    close(1)
    open(unit=1,file="InputFile_1.txt",status="old")
    allocate(first_array(len1))
    do i=1,len1
      if(i==1)then
        read(1,6,advance="no")first_array(i)
      else
        read(1,7,ADVANCE="NO")first_array(i)
      end if
    end do
    6 format(2X,A)
    7 format(X,A)
    do i=1,len1
!      write(*,'(A,1X)',advance="no")first_array(i)
      write(5,'(A,1X)',advance="no")first_array(i)
    end do

    ! reading second file
    read(2,'(I1)'),len2
    allocate(second_array(len2))
    close(2)
    open(unit=2,file="InputFile_2.txt",status="old")
    do i=1,len2
      if(i==1)then
        read(2,'(/,I2)',iostat=te,advance='no')second_array(i)
        write(5,'(/,I0,1X)',advance='no')second_array(i)
      else if(i==4 .or. i==6)then
        read(2,9,iostat=te,advance='no')second_array(i)
        write(5,'(I0,1X)',advance='no')second_array(i)
      else
        read(2,8,advance='no',iostat=te,iomsg=err_msg)iData
        second_array(i)=iData
        write(5,'(I0,1X)',advance='no')second_array(i)
!        if(te>0)then
!          backspace(2)
!          read(2,9,advance='no',iostat=te)iData
!        end if
      end if
    end do
    8 format(1X,I1)
    9 format(1X,I2)


    ! reading third file
    read(3,'(I1)')len3
    close(3)
    open(unit=3,file="InputFile_3.txt",status="old")
    allocate(third_array(len3))
    do i=1,len3
      if(i==1)then
        read(3,'(/,F4.1)')third_array(i)
        write(5,'(/,F4.1)')third_array(i)
      else if(i==4)then
        read(3,'(F4.2)')third_array(i)
        write(5,'(F4.2)')third_array(i)
      else
        read(3,'(F3.1)')third_array(i)
        write(5,'(F3.1)')third_array(i)
      end if
    end do

    ! reading fourth file
    read(4,'(I1)')len4
    allocate(fourth_array(len4))
    close(4)
    open(unit=4,file="InputFile_4.txt",status="old")
    do i=1,len4
      if(i==1)then
        read(4,'(/,F4.1)',advance='no')fourth_array(i)
        write(5,'(F4.1,X)',advance='no')fourth_array(i)
      else if(i==4)then
        read(4,'(F4.2)',advance='no')fourth_array(i)
        write(5,'(F4.2,X)',advance='no')fourth_array(i)
      else if(i==3 .or. i==5)then
        read(4,'(X,F3.1)')fourth_array(i)
        write(5,'(F3.1,X)')fourth_array(i)
      else if(i==6)then
        read(4,'(F3.1)',advance='no')fourth_array(i)
        write(5,'(F3.1)',advance='no')fourth_array(i)
      else
        read(4,'(X,F3.1)',advance='no')fourth_array(i)
        write(5,'(F3.1,X)',advance='no')fourth_array(i)
      end if
    end do
    write(*,*)(fourth_array(i),i=1,6)
end program

