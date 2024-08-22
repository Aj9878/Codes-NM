program ccs
    implicit none
    real FPO, FPN
    integer, parameter:: n=
    integer i,j
    real, dimension(0:n) x
    real, dimension(0:n) l
    real, dimension(0:n) z
    real, dimension(0:n) c
    real, dimension(0:n-1) u
    real, dimension(0:n) a
    real, dimension(1:n-1) alpha
    real, dimension(0:n-1) b
    real, dimension(0:n-1) d
    real, dimension(0:n-1) h


    x=(//)

    do i=0,n,1
        a(i)=g(x(i))

    enddo
    FPO=g`(x(0))
    FPN=g`(x(n))
    do j=0,n-1,1
        h(i)=x(i+1)-x(i)
    enddo
    alpha(0)=3(a(1)-a(0))/h(0)-3*FPO
    alpha(n)=3*FPN-3(a(n)-a(n-1))/h(n-1)
    do i=1,n-1,1
        alpha(i)=3(a(i+1)-a(i))/h(i)-3(a(i)-(a(i-1)))/h(i-1)
    enddo
    l(0)=2*h(0)
    u(0)=0.5
    z(0)=alpha(0)/l(0)
    l(n)=h(n-1)*(2-u(n-1))
    z(n)=(alpha(n)-h(n-1)*z(n-1))/l(n)
    c(n)=z(n)
    do i=1,n-1,1
        l(i)=2(x(i+1)-x(i-1))-h(i-1)*u(i-1)
        u(i)=h(i)/l(i)
        z(i)=(alpha(i)-h(i-1)*z(i-1))l(i)
        do j=n-1,0,-1
            c(j)=z(j)-u(j)*c(j+1)
            b(j)=(a(j+1)-a(j))/h(j)-h(j)(c(j+1)+2(c(j)))/3
            d(j)=(c(j+1)-c(j))/3(h(j))
        enddo
    enddo

    do i=0,n-1,1
        write(*,*) a(j), b(j), c(j), d(j)
    enddo


    end program

    real function g(x)
    real, intent(in):: x
    g=
    end function
    real function g`(x)
    real, intent(in):: x
    g`=
    end function




