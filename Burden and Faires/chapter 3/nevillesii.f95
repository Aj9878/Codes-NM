program nev
    implicit none
    integer, parameter:: n=
    integer i,j,k
    real xo,f
    real, dimension(0:n) x
    real, dimension(0:n,0:n) Q

    x=(//)
    xo=
    do i=0,n,1
        Q(i,0)=f(x(i))
    enddo
    do j=1,n,1
        do k=1,j,1
            Q(i,j)=((xo-x(i-j))*Q(i,j-1)-(xo-x(i))*Q(i-1,j-1))/(x(i)-x(i-j))
            write(*,*) Q(i,j)
        enddo
        write(*,*) '____'
    enddo
    end program
    real function f(x)
    real, intent(in):: x
    f=
    end function

