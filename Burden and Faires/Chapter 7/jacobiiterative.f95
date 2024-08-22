program ji
    implicit none
    integer,parameter::n=4
    real k,x(n),tol,sum,a(n,n),xo(n),xoo,b(n)
    
    integer i,j,m  
    k=1 
     m=10

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
    xoo=xo(1)

    tol=0.001

    do while(k<=m)
        do i=1,n
            sum=0
            do j=1,n 
                if(j .ne. i) then 
                    sum=sum+a(i,j)*xo(j)
                endif
            enddo
                x(i)=(-sum+b(i))/a(i,i)
         enddo
                if(abs(x(1)-xo(1))<tol .and. abs(x(2)-xo(2))<tol .and. abs(x(3)-xo(3))<tol .and. abs(x(4)-xo(4))<tol) then 
                    print*,x 
                    stop
                endif
                k=k+1
                do i=1,n
                   xo(i)=x(i)
                enddo
             
        
    enddo
print*, 'max num of iterations exceeded'
          

 end program  