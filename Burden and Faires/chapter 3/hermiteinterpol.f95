program hermiteinterpol
    implicit none
    integer, parameter:: n=
    integer i,j,k
    real g
    real, dimension(0:n) x
    real, dimension(0:n) F
    real, dimension(0:n) F`
    real, dimension(0:2*n+1) z
    real, dimension(0:2*n+1,0:2*n+1)


    x=(//)

    do i=0,n,1
        F(i)=g(x(i))
        F`(i)=g`(x(i))
    enddo
    do j=0,n,1
        z(2*i)=x(i)
        z(2*i+1)=x(i)
        Q(2*i,0)=F(i)
        Q(2*i+1,0)=F(i)
        Q(2*i+1,1)=F`(i)
        if (i/=0) then
            Q(2*i,1)=(Q(2*i,0)-Q(2*i-1,0))/(z(2*i)-z(2*i-1))
        endif
    enddo
    do i=2,2*n+1,1
        do j=2,i,1
            Q(i,j)=(Q(i,j-1)-Q(i-1,j-1))/(z(i)-z(i-j))
        enddo
    enddo
    do i=0,2*n+1,1
        do j=0,2*n+1,1
            if(i=j) then
            write(*,*) Q(i,j)
            endif
        enddo
    enddo
    end program
    real function g(x)
    real, intent(in):: x
    g=
    end function

    real function g`(x)
    real, intent(in):: x
    g`  =
    end function



