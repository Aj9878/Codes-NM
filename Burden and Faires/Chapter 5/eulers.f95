program eulers
    implicit none
    real a,b,alpha,w,t,f,h
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
    w=w+h*f(t,w)
    t=a+i*h
    write (*,*) t,  w
    enddo
end program

real function f(t,w)
real, intent(in):: t, w
f=w-t**2+1
end function
