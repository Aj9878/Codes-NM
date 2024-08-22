!Nonlinear Shooting with Newton’s Method
!To approximate the solution of the nonlinear boundary-value problem
!y′′ = f (x, y, y′),
!for a ≤ x ≤ b, with y(a) = α and y(b) =be
program nlswnm
    implicit none
    !nn-max no of interval nn or N
    integer,parameter::nn=10
    !endpoints a,b,boundary condition al and be
    
    real al,be,a,b,tol,h,tk,rk(4,2),rk2(4,2),w(2,0:nn),u(2),f,fy,fyy,x
    integer mm,i,j,k
    !max no of iteration M or mm
    mm=20
    tol=10e-6
    a=1.0
    b=2.0
    al=1.0
    be=2.0
    !step 1
  h=(b-a)/nn
    k=1
    tk=(be-al)/(b-a)
    !step 2
    do while(k<=mm)
        w(1,0)=al
        w(2,0)=tk
        u(1)=0.0
        u(2)=1.0
    !rk4 implimented
    do i=1,nn
            x=a+(i-1)*h
            rk(1,1)=h*w(2,i-1)
            rk(1,2)=h*f(x,w(1,i-1),w(2,i-1))
            rk(2,1)=h*(w(2,i-1)+0.5*rk(1,2))
            rk(2,2)=h*f(x+h/2.0,w(1,i-1)+0.5*rk(1,1),w(2,i-1)+0.5*rk(1,2))
            rk(3,1)=h*(w(2,i-1)+0.5*rk(2,2))
            rk(3,2)=h*f(x+h/2.0,w(1,i-1)+0.5*rk(2,1),w(2,i-1)+0.5*rk(2,2))
            rk(4,1)=h*(w(2,i-1)+rk(3,2))
            rk(4,2)=h*f(x+h,w(1,i-1)+rk(3,1),w(2,i-1)+rk(3,2))
            w(1,i)=w(1,i-1)+(rk(1,1)+2*rk(2,1)+2*rk(3,1)*rk(4,1))/6.0
            w(2,i)=w(2,i-1)+(rk(1,2)+2*rk(2,2)+2*rk(3,2)*rk(4,2))/6.0
            rk2(1,1)=h*u(2)
            rk2(1,2)=h*(fy(x,w(1,i-1),w(2,i-1))*u(1)+fyy(x,w(1,i-1),w(2,i-1))*u(2))
            rk2(2,1)=h*(u(2)+0.5*rk2(1,2))
            rk2(2,2)=h*(fy(x+h/2.0,w(1,i-1),w(2,i-1))*(u(1)+0.5*rk2(1,1))+fyy(x+h/2.0,w(1,i-1),w(2,i-1))*(u(2)+0.5*rk2(1,2)))
            rk2(3,1)=h*(u(2)+0.5*rk2(2,2))
            rk2(3,2)=h*(fy(x+h/2.0,w(1,i-1),w(2,i-1))*(u(1)+0.5*rk2(2,1))+fyy(x+h/2.0,w(1,i-1),w(2,i-1))*(u(2)+0.5*rk2(2,2)))
            rk2(4,1)=h*(u(2)+rk2(3,2))
            rk2(4,2)=h*(fy(x+h,w(1,i-1),w(2,i-1))*(u(1)+rk2(3,1))+fyy(x+h,w(1,i-1),w(2,i-1))*(u(2)+rk2(3,2)))
            u(1)=u(1)+(rk2(1,1)+2*rk2(2,1)+2*rk2(3,1)*rk2(4,1))/6.0
            u(2)=u(2)+(rk2(1,2)+2*rk2(2,2)+2*rk2(3,2)*rk2(4,2))/6.0
        enddo
    if(abs(w(1,nn)-be)<=tol) then
            do i=0,nn
                x=a+i*h
                print*, x,w(1,i),w(2,i)
            enddo
            stop
        endif
        tk=tk-(w(1,nn)-be)/u(1)
        k=k+1
    enddo
    print*, 'max iterations exceeded'
 endprogram
    !y''=p*y'+q*y+r y(a)=al y(b)=beta
    real function f(x,y,yd)
    implicit none
    real x,y,yd
    f=-2.0*(yd)/x+2.0*(y)/x**2+sin(2.303*log(x))/x**2
    endfunction
    !coeff of y
    real function fy(a,b,c)
    implicit none
    real a,b,c
    fy=2.0/a**2
    endfunction
    !coefficient of y' 
    real function fyy(a,b,c)
    implicit none
    real a,b,c
    fyy=2.0/a
    endfunction
    