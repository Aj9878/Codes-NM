program h
    implicit none
    integer,parameter::n=4
    real a(n,n),q,alpha,rsq,u(n),v(n),z(n),dum,prod
    integer i,j,k,l
    print*, 'dimension n must be assigned in the program'
    do i=1,n
        do j=1,n
            print*,i,',',j,'element of a'
            read(*,*) a(i,j)
        enddo
    enddo

    do k=1,n-2
        do j=k+1,n

        q=q+(a(j,k))**2.0
        enddo
        if(a(k+1,k)==0) then
            alpha=-(q**(0.5))
        else
            alpha=-(q**(0.5)*a(k+1,k))/(abs(a(k+1,k))*1.0)
        endif
        rsq=(alpha**2.0)-alpha*a(k+1,k)
        v(k)=0.0
        v(k+1)=a(k+1,k)-alpha
        do j=k+2,n
            v(j)=a(j,k)
        enddo
        do j=k,n
            dum=0
            do i=k+1,n
                dum=dum+a(j,i)*v(i)
            enddo
            u(j)=(1.0/rsq)*dum
        enddo
        prod=0
        do i=k+1,n
            prod=prod+v(i)*u(i)
        enddo
        do j=k,n
            z(j)=u(j)-(prod*v(j)/(2.0*rsq))
        enddo
        do l=k+1,n-1
            do j=l+1,n
                a(j,l)=a(j,l)-v(l)*z(j)-v(j)*z(l)
                a(l,j)=a(j,l)
            enddo
            a(l,l)=a(l,l)-2*v(l)*z(l)
        enddo
        a(n,n)=a(n,n)-2*v(n)*z(n)
        do j=k+2,n
            a(j,k)=0
            a(k,j)=0
        enddo
        a(k+1,k)=a(k+1,k)-v(k+1)*z(k)
        a(k,k+1)=a(k+1,k)
        enddo
    print*, a
endprogram
