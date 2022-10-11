program problem_05
  implicit none
  real,allocatable::year(:),pop(:),table(:,:)
  integer::n,i,j,p
  n=9
  allocate(year(n),pop(n),table(n,n))
  year=(/1860,1880,1900,1920,1940,1960,1980,2000,2020/)
  pop=(/249,277,316,350,431,539,689,833,1014/)
  do i=1,n
    table(i,1)=year(i)
    table(i,2)=pop(i)
  end do
  do j=3,n+1
    do i=1,n-j+2
      table(i,j)=table(i+1,j-1)-table(i,j-1)
    end do
  end do

  do i=1,n
    do j=1,2
      write(*,'(F15.4)',advance="no")table(i,j)
    end do
    do j=3,n-i+2
      write(*,'(F15.4)',advance="no")table(i,j)
    end do
    write(*,*)
  end do
end program
