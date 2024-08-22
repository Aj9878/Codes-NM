program cra
    implicit none
    integer i,j,k
    integer,parameter::m=2,n=3, nn=m+n
    real q(0:m),p(0:n),a(0:nn),fact,b(nn, nn+1),sum,xm
    nn=m+n 
    do k=0, nn+m,1
        call integration(0,3.14,k,a(k))
    enddo 
    q(0)=1
    do i=0,nn
        do j=0,i
            if(j .le. n) then
                 b(i,j)=1 
            enddo
            endif 
            if( i .lt. n) then 
                b(i,i)=1
            endif
            do j=i+1,n
                b(i,j)=0 
            enddo
            do j=n+1,nn 
                if(i .ne. 0) then 
                    b(i,j)=-(a(i+j-n)+a(i-j+n))/2
                else
                    b(i,j)=-a(j-n)/2
                endif
                if(i .ne. 0) then 
                    b(i,nn+1)=a(i)
                else 
                    b(i,nn+1)=a(i)/2
                endif
            enddo
    !partial pivoting
            do i=n+1,nn-1
                if (((i >= a) .AND. (i <= b)) .and. (b(k,i) == maxval(b(j,i))))then
                    k=i
                endif
                if(b(k,i)==0) then 
                    print*,'Singular System'
                else stop 
                endif
                if(k .ne. i) then 
                    do(j=i,nn+1)
                        bcopy=b(i,j)
                        b(i,j)=b(k,j)
                        b(k,j)=bcopy
                    enddo
                endif
                do j=i+1,nn 
                 xm=b(i,j)/b(i,i)   
                
                do k=i+1,nn+1
                    b(j,k)=b(j,k)-xm*b(i,k)
                enddo
                b(i,j)=0
            enddo
        enddo
    if(b(nn,nn)==0) then 
        print*,'system is singular'
        stop
    endif
    if(m>0) then 
        q(m)=b(nn,nn+1)/b(nn,nn)
    endif
    do i=nn-1,n+1,-1
        sum=0.0
        do j=i+1,nn 
            sum=sum+b(i,j)*q(j-n)
        enddo
        q(i-n)= (b(i,nn+1)-sum)/b(i,i)
        do i=n,n-1
            p(i)=b(i,nn+1)-sum
        enddo
        print*, 'q=',q,'p=',p
    end program cra
subroutine integration(a,b,k,xi1)
    
!used over large integration intervals
        implicit none
        real a,b,h,f,xi1,xi2,x,xi0 
        integer i,n,k
         
        n=2
        
        h=(b-a)/n 
        xi0=f(a,k)+f(b,k)
        xi1=0 
        xi2=0 
        
    
        do i=1,n-1 
            x=a+i*h 
            if (mod(i,2)==0) then 
                 xi2=xi2+f(x,k) 
                else  
                xi1=xi1+f(x,k)
            end if
        enddo 
    
         xi1=h*((xi0+2*xi2+4*xi1))/3
end subroutine composite 
    

 

real function f(x,k)
real x,f1
integer k
f=f1(cos(x))*cos(k*x)
end function 
real function f1(x)
real x
f1=exp(-x)
end function 
