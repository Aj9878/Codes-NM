program ge
    implicit none
    integer,parameter::n=3
    real dum(n,n+1),dums

    real,dimension(n)::e
    real,dimension(n,n+1)::a
    real,dimension(n)::x
    real,dimension(2:n,n-1)::m
    integer i,j,p

    a(1,1)=4
    a(1,2)=-1
    a(1,3)=1
    a(1,4)=8
    a(2,1)=2
    a(2,2)=5
    a(2,3)=2
    a(2,4)=3
    a(3,1)=1
    a(3,2)=2
    a(3,3)=4
    a(3,4)=11



    p=0
    do i=1,n-1,1
        do j=i,n,1
        if(a(j,i).ne.0) then
            p=j
            exit
        endif

        enddo
        if(p==0) then
            write(*,*)'no unique solution exists'
            stop
        endif



        if(p/=i) then
        dum(p,1)=a(p,1)
        dum(p,2)=a(p,2)
        dum(p,3)=a(p,3)
        dum(p,4)=a(p,4)
        a(p,1)=a(i,1)
        a(p,2)=a(i,2)
        a(p,3)=a(i,3)
        a(p,4)=a(i,4)
        a(i,1)=dum(p,1)
        a(i,2)=dum(p,2)
        a(i,3)=dum(p,3)
        a(i,4)=dum(p,4)
        endif
        do j=i+1,n,1
            m(j,i)=a(j,i)/a(i,i)
            a(j,1)=a(j,1)-m(j,i)*a(i,1)
            a(j,2)=a(j,2)-m(j,i)*a(i,2)
            a(j,3)=a(j,3)-m(j,i)*a(i,3)
            a(j,4)=a(j,4)-m(j,i)*a(i,4)
        enddo
    enddo
    if(a(n,n)==0) then
        write(*,*)'no unique solution exists'
        stop
    endif

    x(n)=a(n,n+1)/a(n,n)
    do i=n-1,1,-1
        dums=0
        do j=i+1,n,1
            dums=dums+a(i,j)*x(j)
        enddo
        x(i)=(a(i,n+1)-dums)/a(i,i)
    enddo
    write(*,*) x(1),x(2),x(3)

endprogram




