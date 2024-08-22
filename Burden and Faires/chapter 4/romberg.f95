program compsimp

    implicit none
    real a,b,h,f,dum
    integer i,j,k
    integer, parameter:: n=5
    real, dimension(1:2,1:n):: R
    a=0
    b=1

    h=b-a

    R(1,1)=h*(f(a)+f(b))/2
    write(*,*) R(1,1)


    do i=2,n,1
        do k=1,2**(i-2),1
            dum=f(a+(k-0.5)*h)
        enddo
            R(2,1)=0.5*(R(1,1)+h*dum)
        do j=2,i,1
            R(2,j)=R(2,j-1)+(R(2,j-1)-R(1,j-1))/(4**(j-1)-1)
        enddo
        do j=1,i,1
            write(*,*) R(2,j)
        enddo
        h=h/2
        do j=1,i,1
            R(1,j)=R(2,j)
        enddo
    end do

end program

real function f(x)
real,intent(in)::x
f=sin(x)
end function



