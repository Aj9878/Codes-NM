program hornersmethod
    implicit none
    real x,y,z,x0
    integer, parameter:: n=3
    integer i
    real, dimension(0:n)::a
    a=(/7,6,5,4/)
    x0=1

    y=a(n)
    z=a(n)
    do i=n-1,1,-1
        y=x0*y+a(i)
        z=x0*z+y

    end do
    y=x0*y+a(0)
    write(*,*) y,z
end program



