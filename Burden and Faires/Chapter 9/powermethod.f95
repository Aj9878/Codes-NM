program pm
    implicit none
    integer,parameter::n=2
    real a(n,n),x(n,1),tol,nn,mu,er,y(n,1),dum(n)
    integer i,j,k,p

    tol=10e-5
    nn=100
    a(1,1)=-2
    a(1,2)=-3
    a(2,1)=6
    a(2,2)=7
    x(1,1)=1
    x(2,1)=1
    k=1
    do i=1,n,1
        if(abs(x(i,1))==maxval(abs(x(1:n,1)))) then
        p=i
        exit
        endif
    enddo
    do i=1,n,1
        x(i,1)=x(i,1)/x(p,1)
    enddo
    do while(k<=nn)
        call mm(a,x,y,n,n,1)
        mu=y(p,1)
        do i=1,n,1
            if(abs(y(i,1))==maxval(abs(y(1:n,1)))) then
                p=i
            exit
            endif
        enddo
        if(y(p,1)==0) then
            write(*,*) 'eigenvector',x
            write(*,*) 'a has the eigenvalue 0, select a new vector x and restart'
            stop
        endif
        do i=1,n,1
            dum(i)=x(i,1)-(y(i,1)/y(p,1))
        enddo
        er=maxval(abs(dum(1:n)))
        do i=1,n,1
            x(i,1)=y(i,1)/y(p,1)
        enddo
        if (er<tol) then
            write(*,*) 'mu=',mu,'x=',x
            stop
        endif
        k=k+1
    enddo
    write(*,*)'max no. of iterations exceeded'

endprogram
subroutine mm(a,b,c,n,m,p)
    implicit none
    integer n,m,p
    real a(n,m),b(m,p),c(n,p),dum
    integer i,j,k

    do i=1,n,1
        do j=1,p,1
            dum=0
            do k=1,m,1
                dum=dum+a(i,k)*b(k,j)
            enddo
            c(i,j)=dum
        enddo
    enddo

endsubroutine



