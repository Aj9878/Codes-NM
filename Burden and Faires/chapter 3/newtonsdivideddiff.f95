program nddf
    implicit none
    integer, parameter:: n=
    integer i,j,k
    real g
    real, dimension(0:n) x
    real, dimension(0:n,0:n) F

    x=(//)

    do i=0,n,1
        F(i,0)=g(x(i))
    enddo
    do j=1,n,1
        do k=1,j,1
            F(i,j)=(F(i,j-1)-F(i-1,j-1))/(x(i)-x(i-j))
            if(i=j) then
            write(*,*) F(i,j)
            endif

        enddo
        write(*,*) '____'
    enddo
    end program
    real function g(x)
    real, intent(in):: x
    g=
    end function


