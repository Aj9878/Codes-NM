program trapwithnewtoniteration
    implicit none
    real a,b,tol,alpha,t,w,w0,k1,h,f,fy
    integer n,m,i,j,flag

    a=0
    b=1
    alpha=exp(1.0)
    tol=10e-5
    m=100
    n=10

    h=(b-a)/n
    t=a
    w=alpha
    write(*,*) t,w

    do i=1,n,1
        k1=w+(h/2)*f(t,w)
        w0=k1
        j=1
        flag=0

        do while(flag==0)
            w=w0-(w0-(h/2)*f(t+h,w0)-k1)/(1-(h/2)*fy(t+h,w0))
        if(abs(w-w0)<tol) then
            flag=1
        else
            j=j+1
            w0=w
            if(j>m) then
                write(*,*) 'max no. of iterations exceeded'
                stop
            endif
        endif
        enddo
    t=a+i*h
    write(*,*) t,w
    enddo
endprogram


real function f(t,w)
real t,w

f=-9*w

end function

real function fy(t,w)
real t,w

fy=exp(1-9*t)

end function

