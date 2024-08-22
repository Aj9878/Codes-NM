program rkmsde
    implicit none
    integer, parameter::m=2
    integer n,i,j
    real a,b,alpha(1:m),h,t,f,k(4,m),w(m)
    a=0
    b=0.5
    n=5

    alpha(1)=0
    alpha(2)=0

    h=(b-a)/n
    t=a
    do j=1,m,1
        w(j)=alpha(j)
    enddo
    write(*,*) t,w
    do i=1,n,1
        do j=1,m,1
            k(1,j)=h*f(t,w(1),w(2),j)
        enddo
        do j=1,m,1
            k(2,j)=h*f(t+h/2,w(1)+k(1,1)/2,w(2)+k(1,2)/2,j)
        enddo
        do j=1,m,1
            k(3,j)=h*f(t+h/2,w(1)+k(2,1)/2,w(2)+k(2,2)/2,j)
        enddo
        do j=1,m,1
            k(4,j)=h*f(t+h,w(1)+k(3,1),w(2)+k(3,2),j)
        enddo
        do j=1,m,1
            w(j)=w(j)+(k(1,j)+2*k(2,j)+2*k(3,j)+k(4,j))/6
        enddo



    t=a+i*h

    write(*,*) t,w

    enddo

endprogram

real function f(t,w1,w2,j)
real t,w
integer j

if(j==1) then
f=-4*w1+3*w2+6
elseif(j==2) then
f=0.6*(-4*w1+3*w2+6)-0.2*w2
elseif(j==3) then
f=0
endif
end function

