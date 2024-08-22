program rk4
    implicit none
    real h,a,b,alpha,w,t,f,hmax,hmin,Tol,k1,k2,k3,k4,k5,k6,FLAG,del,R

    a=0
    b=2
    hmax=0.25
    hmin=0.01
    alpha=0.5
    Tol=1e-4

    t=a
    w=alpha
    h=hmax
    FLAG=1
    write(*,*) t,   w

    do while(FLAG==1)

    k1=h*f(t,w)
    k2=h*f(t+h/4,w+k1/4)
    k3=h*f(t+3*h/8,w+3*k1/32+9*k2/32)
    k4=h*f(t+12*h/13,w+1932*k1/2197-7200*k2/2197+7296*k3/2197)
    k5=h*f(t+h,w+439*k1/216-8*k2+3680*k3/513-845*k4/4104)
    k6=h*f(t+h/2,w-8*k1/27+2*k2-3544*k3/2565+1859*k4/4104-  11*k5/40)

    R=(1/h)*(k1/360-128*k3/4275-2197*k4/75240+k5/50+2*k6/55)

    if(R<=Tol) then
        t=t+h
        w=w+k1*25/216+k3*1408/2565+k4*2197/4104-k5/5
        write (*,*) t,   w,   h
    endif
    del=0.84*(Tol/R)**(1/4)
    if(del<=0.1) then
        h=0.1*h
    elseif(del>=4) then
        h=4*h
    else
        h=del*h
    endif

    if(h>hmax) then
        h=hmax
    endif

    if(t>=b) then
        FLAG=0
    elseif((t+h)>b) then
        h=b-t
    elseif(h<hmin) then
        FLAG=0
        write(*,*) ' minimum h exceeded'
    endif

    enddo
end program

real function f(t,w)
real, intent(in):: t, w
f=w-t**2+1
end function


