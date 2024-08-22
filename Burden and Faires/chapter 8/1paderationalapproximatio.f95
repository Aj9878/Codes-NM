program pra
    implicit none
    real 
    !nn here is N
    integerr m,n,nn,i,j
    real q(1:m),p(1:n),b(n,n)
    nn=m+n 
    !example for maclaurin series
    do i=0,nn 
        a(i)=(-1)**i/fact(i)
    enddo
    nn=5
    q(0)=1
    p(0)=a(0)
    do j=1,i-1
        if(j<=n) then 
            b(i,i)=0
        enddo 
        if(i .le. n) then 
            b(i,i)=1
        endif
        do j=i+1,nn 
            b(i,j)=0
        enddo
        do j=1,i 
            if ( j .le. m ) then
                b(i,n+j)=-a(i-j)
            end if
        enddo
        do j=n+i+1,nn
            b(i,j)=0
        enddo
        b(i,nn+1)=a(i)
    enddo
        end program pra