!to approx soln for non linear system F(x)=0 given an initial approximation x
!input no of equation and unknowns:initial approximation x=(x1,...,xn)t
!tolerance tol,max num of iteration nn
program nmfs
    implicit none
    integer,parameter::n=3
    real x(n,1),tol,f,ja(n,n),y(n,1),ff(n,1),norm,dum
    integer nn,i,j,k,stopper
    tol=0.01
    nn=10000
    stopper=0
    x(1,1)=0.1
    x(2,1)=0.1
    x(3,1)=-0.1

    k=1
    do while(k<=nn)
    ja(1,1)=3*x(1,1)
    ja(1,2)=x(3,1)*sin(x(2,1)*x(3,1))
    ja(1,3)=x(2,1)*sin(x(3,1)*x(2,1))
    ja(2,1)=2*x(1,1)
    ja(2,2)=-162*(x(2,1)+0.1)
    ja(2,3)=cos(x(3,1))
    ja(3,1)=-x(2,1)*exp(-x(1,1)*x(2,1))
    ja(3,2)=-x(1,1)*exp(-x(1,1)*x(2,1))
    ja(3,3)=20

    ff(1,1)=-(f(1,x(1,1),x(2,1),x(3,1)))
    ff(2,1)=-(f(2,x(1,1),x(2,1),x(3,1)))
    ff(3,1)=-(f(3,x(1,1),x(2,1),x(3,1)))
    call ge(ja,ff,y,n,stopper)
    if(stopper==1) then
        print*,'error solving J(x)y=-F(x)'
        stop
    endif
    do i=1,n,1
    x(i,1)=x(i,1)+y(i,1)
    enddo
    dum=0.0
    do i=1,n
        dum=dum+((y(i,1))**2)
    enddo
    norm=sqrt(dum)
    print*,'solution:',x
    stop
endif
k=k+1
enddo
print*,'max iterations exceeded'
endprogram
subroutine gaussian_elimination_wbs
    
real function f(i,x1,x2,x3)
implicit none
real x1,x2,x3
integer i
if(i==1) then
    f=3*x1-cos(x2*x3)-(1/2.0)
endif
if(i==2) then
    f=x1*x1-(81*((x2+0.1)**2))+sin(x3)+1.06
endif
if(i==3) then
    f=exp(-x1*x2)+20*x3+(31.4-3)/3.0
endif
end function
