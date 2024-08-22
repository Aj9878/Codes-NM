!to approximate the solution to the nonlinear boundary value problem
program nfd
    implicit none
    integer,parameter::n=19
    real ain,bin,tol,alpha,beta,w(0:n+1),h,t,f,fy,fyy,x,l(n),c(n),a(n),b(n),z(n),u(n),d(n),v(n)
    integer i,j,k,m
    a=1.0
    b=3.0
    al=17.0 
    be=43/3.0
    tol=10e-8
    h=0.25
    nn=4
    mm=10

    h=(b-a)/(nn+1)
    w0=al 
    w(nn+1)=be 
    do i=1,nn 
        w(i)=al+i*((be-al)/(b-a))
    enddo
    k=1 
    do while(k<=mm)
        x=a+h
        t=(w(2)-al)/(2*h)
        a(1)=2+(h**2)*fy(x,w(1),t)
        b(1)=-1+(h/2)*fyy(x,w(1),t)
        d(1)=-(2*w(1)-w(2)-al+h**2*f(x,w(1),t))
    enddo
    do i=2,n-1,1
        x=ain+i*h
        t=(w(i+1)-w(i-1))/(2*h)
        a(i)=2+(h**2)*fy(x,w(i),t)
        b(i)=-1+(h/2)*fyy(x,w(i),t)
        c(i)=-1-(h/2)*fyy(x,w(i),t)
        d(i)=-(2*w(i)-w(i+1)-w(i-1)+(h**2)*f(x,w(i),t))
    enddo
    x=bin-h
    t=(be-w(n-1))/(2*h)
    a(n)=2+(h**2)*fy(x,w(n),t)
    c(n)=-1-(h/2)*fyy(x,w(n),t)
    d(n)=-(2*w(n)-w(n-1)-be+(h**2)*f(x,w(n),t))

    l(1)=a(1)
        u(1)=b(1)/a(1)
        z(1)=d(1)/l(1)
    
        do i=2,n-1
            l(i)=a(i)-c(i)*u(i-1)
            u(i)=b(i)/l(i)
            z(i)=(d(i)-c(i)*z(i-1))/l(i)
        enddo
        l(n)=a(n)-c(n)*u(n-1)
        z(n)=(d(n)-c(n)*z(n-1))/l(n)

        v(n)=z(n)
        w(n)=w(n)+v(n)
        do i=n-1,1,-1
            v(i)=z(i)-u(i)*v(i+1)
            w(i)=w(i)+v(i)
        enddo
        if(norm2(v)<=tol) then
            do i=0,n+1
                x=ain+i*h
                print*,'x=',x,'w=',w(i)
            enddo
            stop
        endif
        k=k+1
    enddo

    print*, 'max iterations exceeded'
end program nfd
real function f(x,y,yy)
implicit none
real x,y,yy
f=(32+2*(x**3)-y*yy)/8.0
endfunction
real function fy(x,y,yy)
implicit none
real x,y,yy
fy=-yy/8.0
endfunction
real function fyy(x,y,yy)
implicit none
real x,y,yy
fyy=-y/8.0
endfunction
