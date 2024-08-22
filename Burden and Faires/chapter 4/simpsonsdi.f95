program sdi
    implicit none
    real a,b,h,x,y,f,c,d,HX,L,JJ,Q
    integer m,n,i,o
    real, dimension(1:3):: J
    real, dimension(1:3):: K
    a=0.1
    b=0.5
    m=10
    n=10
    h=(b-a)/n
    J(1)=0
    J(2)=0
    J(3)=0
    do i=0,n,1
        x=a+i*h
        HX=(d(x)-c(x))/m
        K(1)=f(x,c(x))+f(x,d(x))
        K(2)=0
        K(3)=0
        do o=1,m-1,1
            y=c(x)+o*HX
            Q=f(x,y)
            if(mod(o,2)==0) then
                K(2)=K(2)+Q
            else
                K(3)=K(3)+Q
            endif
        enddo
    L=(K(1)+2*K(2)+4*K(3))*HX/3
    if(i==0 .or. i==n) then
        J(1)=J(1)+L
    else
        if(mod(i,2)==0) then
            J(2)=J(2)+L
        else
            J(3)=J(3)+L
        endif
    endif
    enddo
    JJ=h*(J(1)+2*J(2)+4*J(3))/3


    write(*,*) JJ


end program


real function f(x,y)
real,intent(in)::x,y
f=exp(y/x)
end function

real function d(x)
real,intent(in)::x
d=x**2
end function

real function c(x)
real,intent(in)::x
c=x**3
end function

