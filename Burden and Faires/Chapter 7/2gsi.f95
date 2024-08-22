 !to solve Ax=b given parameter w and an initial approximation x(0)
program sor
    implicit none
    integer,parameter::n=4
    integer m,i,j,k
    real a(n,n),x(n),tol,sum1,sum2,w,b(n),xo(n)
   
    m=10
    m=10
    w=1.25
    a(1,1)=10
    a(1,2)=-1
    a(1,3)=2
    a(1,4)=0
    a(2,1)=-1
    a(2,2)=11
    a(2,3)=-1
    a(2,4)=3
    a(3,1)=2
    a(3,2)=-1
    a(3,3)=10
    a(3,4)=-1
    a(4,1)=0
    a(4,2)=3
    a(4,3)=-1
    a(4,4)=8

    b(1)=6
    b(2)=25
    b(3)=-11
    b(4)=15

    xo(1)=0
    xo(2)=0
    xo(3)=0
    xo(4)=0

    tol=0.001
     k=1 
    do while(k<=m)
        
        do i=1,n 
        sum1=0
        sum2=0
            do j=1,i-1
            sum1=sum1+a(i,j)*x(j)
            enddo 
            do j=i+1,n 
                sum2=sum2+a(i,j)*xo(j)
             enddo
            x(i)=(1-w)*xo(i)+(1/a(i,i))*(w*(-sum1-sum2+b(i))) 
            enddo
              if(abs(x(1)-xo(1))<tol .and. abs(x(2)-xo(2))<tol .and. abs(x(3)-xo(3))<tol .and. abs(x(4)-xo(4))<tol) then
                 print*,x
                 stop
             end if
             k=k+1 
             do j=1,n 
                xo(i)=x(i)
             enddo
        enddo
         
    
    print *, 'max number of iteraation exceeded'
stop
    
end program  