program linear_shooting
    implicit none
    real,external::p,q,r
    !real w(1,n),y(0:n)
    real,dimension(:,:),allocatable::v,k,kdashed,w,u
    real,dimension(:),allocatable::W1,W2,y
    real::a,b,h,al,be,x
    integer::i,j,n
    n=10

    a=1.0
    b=2.0
    al=1.0
    be=2.0

    h=(b-a)/n
    allocate(u(1:2,0:10))
    allocate(v(1:2,0:10))
    allocate(w(1:2,0))
    allocate(k(1:4,1:2))
    allocate(kdashed(1:4,1:2))
    allocate(y(0:10))
    allocate(w1(1:10))
    allocate(W2(1:10))
    !boundary condition
    y(a)=al
    y(b)=be

    u(1,0)=al
    u(2,0)=0.0
    v(1,0)=0.0
    v(2,0)=1.0

    do i=0,n-1
        x=a+i*h
        k(1,1)=h*u(2,i)
        k(1,2)=h*(p(x)*u(2,i)+q(x)*u(1,i)+r(x))
        k(2,1)=h*(u(2,i)+0.5*k(1,2))
        k(2,2)=h*(p(x+h/2)*(u(2,i)+0.5*k(1,2))+q(x+h/2)*(u(1,i)+0.5*k(1,1))+r(x+h/2))
        k(3,1)=h*(u(2,i)+0.5*k(2,2))
        k(3,2)=h*(p(x+h/2)*(u(2,i)+0.5*k(2,2))+q(x+h/2)*(u(1,i)+0.5*k(2,1))+r(x+h/2))
        k(4,1)=h*(u(2,i)+k(3,2))
        k(4,2)=h*(p(x+h)*(u(2,i)+k(3,2))+q(x+h)*(u(1,i)+k(3,1))+r(x+h))
        u(1,i+1)=u(1,i)+(1.0/6.0)*(k(1,1)+2*k(2,1)+2*k(3,1)+k(4,1))
        u(2,i+1)=u(2,i)+(1.0/6.0)*(k(1,2)+2*k(2,2)+2*k(3,2)+k(4,2))
        kdashed(1,1)=h*v(2,i)
        kdashed(1,2)=h*(p(x)*v(2,i)+q(x)*v(1,i))
        kdashed(2,1)=h*(v(2,i)+(0.5)*kdashed(1,2))
        kdashed(2,2)=h*(p(x+h/2)*(v(2,i)+0.5*kdashed(1,2))+q(x+h/2)*(v(1,i)+(0.5)*kdashed(1,1)))
        kdashed(3,1)=h*(v(2,i)+0.5*kdashed(2,2))
        kdashed(3,2)=h*(p(x+h/2)*(v(2,i)+(0.5)*kdashed(2,2))+q(x+h/2)*(v(1,i)+(0.5)*kdashed(2,1)))
        kdashed(4,1)=h*(v(2,i)+kdashed(3,2))
        kdashed(4,2)=h*(p(x+h)*(v(2,i)+kdashed(3,2))+q(x+h)*(v(1,i)+kdashed(3,1)))
        v(1,i+1)=v(1,i)+(1.0/6.0)*(kdashed(1,1)+2*kdashed(2,1)+2*kdashed(3,1)+kdashed(4,1))
        v(2,i+1)=v(2,i)+(1.0/6.0)*(kdashed(1,2)+2*kdashed(2,2)+2*kdashed(3,2)+kdashed(4,2))
        end do

        w(1,0)=al
        w(2,0)=(be-u(1,n))/(v(1,n))

         print*,'x:',a,'y(x)',w(1,0),"y'(x)",w(2,0)
        do i=1,n
            W1(i)=u(1,i)+w(2,0)*v(1,i)
            W2(i)=u(2,i)+w(2,0)*v(2,i)
            x=a+i*h

            print*,'x:',x,'y(x)',w1(i),"y'(x)",w2(i)
    end do

    stop
end program
!coefficient of y'
real function p(g)
implicit none
real::g
p=-2.0/g
return
end function

!coefficient of y
real function q(g)
implicit none
real::g
q=2.0/(g**2)
return
end function
!just the x/last term of the equation 
real function r(g)
implicit none
real::g
r=(sin(log(g)))/(g**2)
return
end function



