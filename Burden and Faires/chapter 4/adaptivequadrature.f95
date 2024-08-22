program compsimp

    implicit none
    real a0,b,f,Tol,APP,FD,FE,S1,S2
    integer i
    integer, parameter:: n=100
    real, dimension(1:n)::T
    real, dimension(1:n)::a
    real, dimension(1:n)::h
    real, dimension(1:n)::FA
    real, dimension(1:n)::FC
    real, dimension(1:n)::FB
    real, dimension(1:n)::S
    real, dimension(1:n)::L
    real, dimension(1:8)::v
    a0=0
    b=1
    Tol=1e-3
    APP=0
    i=1
    T(i)=10*Tol
    a(i)=a0
    h(i)=(b-a0)/2
    FA(i)=f(a0)
    FC(i)=f(a0+h(i))
    FB(i)=f(b)
    S(i)=h(i)*(FA(i)+4*FC(i)+FB(i))/3
    L(i)=1

    do while(i>0)
        FD=f(a(i)+h(i)/2)
        FE=f(a(i)+3*h(i)/2)
        S1=h(i)*(FA(i)+4*FD+FC(i))/6
        S2=h(i)*(FC(i)+4*FE+FB(i))/6
        v(1)=a(i)
        v(2)=FA(i)
        v(3)=FC(i)
        v(4)=FB(i)
        v(5)=h(i)
        v(6)=T(i)
        v(7)=S(i)
        v(8)=L(i)
        i=i-1
        if(abs(S1+S2-v(7))<v(6)) then
            APP=APP+(S1+S2)
        else
            if(v(8)>=n) then
                write(*,*) 'level exceeded'
                stop
            else
                i=i+1
                a(i)=v(1)+v(5)
                FA(i)=v(3)
                FC(i)=FE
                FB(i)=v(4)
                h(i)=v(5)/2
                T(i)=v(6)/2
                S(i)=S2
                L(i)=v(8)+1
                i=i+1
                a(i)=v(1)
                FA(i)=v(2)
                FC(i)=FD
                FB(i)=v(3)
                h(i)=h(i-1)
                T(i)=T(i-1)
                S(i)=S1
                L(i)=L(i-1)
            endif
        endif
    end do
    write(*,*) APP
end program

real function f(x)
real,intent(in)::x
f=sin(x)
end function




