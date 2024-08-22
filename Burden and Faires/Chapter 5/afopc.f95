program rk4
    implicit none
    real a,b,alpha,ww,f,h,k1,k2,k3,k4,tt
    real, dimension(0:3):: t
    real, dimension(0:3):: w
    integer n,i,j
    a=0
    b=2
    n=10
    alpha=0.5



    h=(b-a)/n
    t(0)=a
    w(0)=alpha
    write(*,*) t(0),w(0)
    do i = 1,3,1
    k1=h*f(t(i-1),w(i-1))
    k2=h*f(t(i-1)+h/2,w(i-1)+k1/2)
    k3=h*f(t(i-1)+h/2,w(i-1)+k2/2)
    k4=h*f(t(i-1)+h,w(i-1)+k3)

    w(i)=w(i-1)+(k1+2*k2+2*k3+k4)/6

    t(i)=a+i*h
    write (*,*) t(i),w(i)
    enddo
    do i=4,n,1
        tt=a+i*h
        ww=w(3)+h*(55*f(t(3),w(3))-59*f(t(2),w(2))+37*f((t(1)),w(1))-9*f(t(0),w(0)))/24
        write(*,*) tt,ww
        do j=0,2,1
            t(j)=t(j+1)
            w(j)=w(j+1)
            t(3)=tt
            w(3)=ww
        enddo
    enddo

end program

real function f(t,w)
real, intent(in):: t, w
f=w-t**2+1
end function


