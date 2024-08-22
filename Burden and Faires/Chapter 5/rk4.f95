program rk4
    implicit none
    real a,b,alpha,w,t,f,h,k1,k2,k3,k4
    integer n,i
    a=0
    b=2
    n=10
    alpha=0.5


    h=(b-a)/n
    t=a
    w=alpha
    write(*,*) t,   w
    do i = 1,n,1
    k1=h*f(t,w)
    k2=h*f(t+h/2,w+k1/2)
    k3=h*f(t+h/2,w+k2/2)
    k4=h*f(t+h,w+k3)

    w=w+(k1+2*k2+2*k3+k4)/6

    t=a+i*h
    write (*,*) t,  w
    enddo
end program

real function f(t,w)
real, intent(in):: t, w
f=w-t**2+1
end function

