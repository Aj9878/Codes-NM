program gdi
    implicit none
    real a,b,c1,d1,h1,h2,x,y,f,c,d,JX,Q,k1,k2,J
    integer n,i,o,m

    real, dimension(1:5,1:5):: cc
    real, dimension(1:5,1:5):: rr

    rr(2,1)=0.5773502692
    rr(2,2)=-0.5773502692
    rr(3,1)=0.7745966692
    rr(3,2)=0.0000000000
    rr(3,3)=-0.7745966692
    rr(4,1)=0.8611363116
    rr(4,2)=0.3399810436
    rr(4,3)=-0.3399810436
    rr(4,4)=-0.8611363116
    rr(5,1)=0.9061798459
    rr(5,2)=0.5384693101
    rr(5,3)=0.0000000000
    rr(5,4)=-0.5384693101
    rr(5,5)=-0.9061798459


    cc(2,1)=1.0000000000
    cc(2,2)=1.0000000000
    cc(3,1)=0.5555555556
    cc(3,2)=0.8888888886
    cc(3,3)=0.5555555556
    cc(4,1)=0.3478548451
    cc(4,2)=0.6521451549
    cc(4,3)=0.6521451549
    cc(4,4)=0.3478548451
    cc(5,1)=0.2369268850
    cc(5,2)=0.4786286705
    cc(5,3)=0.5688888889
    cc(5,4)=0.4786286705
    cc(5,5)=0.2369268850

    a=1.4
    b=2.0
    n=3
    m=3


    if(n>5 .or. m>5) then
        write(*,*) ' the algorithm only works for n,m <= 5'
        stop
    endif


    h1=(b-a)/2
    h2=(b+a)/2
    J=0
    do i=1,m,1
        JX=0
        x=h1*rr(m,i)+h2
        d1=d(x)
        c1=c(x)
        k1=(d1-c1)/2
        k2=(d1+c1)/2
        do o=1,n,1
            y=k1*rr(n,o)+k2
            Q=f(x,y)
            JX=JX+cc(n,o)*Q


        enddo
        J=J+cc(m,i)*k1*JX

    enddo
    J=h1*J


    write(*,*) J


end program


real function f(x,y)
real,intent(in)::x,y
f=log(x+2*y)
end function

real function d(x)
real,intent(in)::x
d=1.5
end function

real function c(x)
real,intent(in)::x
c=1.0
end function


