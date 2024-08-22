 !to solve Ax=b given the parameter w and an initial approximation x(0)
program IterativeRefinement
    !num oe eqn and unknown n
    !a(i,j) be entries where 1<=i,j<=n of matrix A
    !entries b(i) of matrix b
    !entries xo(i) of x(i)
    !parameter w
    !tolerance tol  
    implicit none
    integer i,j,k,NN,kk,nnn
    integer, parameter::n=4
    real a(n,n),b(n),xo(n),tol,x(n),sum1,sum2,m(n,n-1),y(n),r(n),cond,yo(n),t,xx(n)
    NN=10
    nnn=1
    t=1/10

    ! Initialize matrix A and vector b
    A = reshape([10, -1, 2, 0, &
    -1, 11, -1, 3, &
    2, -1, 10, -1, &
    0, 3, -1, 8], &
    shape(A))
    
    b = [6, 25, -11, 15]

    

    ! Iterative refinement process
    
    xo(1)=0
    xo(2)=0
    xo(3)=0
    xo(4)=0

    tol=0.001



    k=1
    do while(k<=NN)
        do i=1,n,1
            sum=0
            sum2=0

            do j=1,i-1,1
                sum2=sum2+a(i,j)*x(j)
            enddo
            do j=i+1,n,1
                sum1=sum+a(i,j)*xo(j)
            enddo
            x(i)=(1/a(i,i))*(-sum2-sum+b(i))
        enddo

            if(abs(x(1)-xo(1))<tol .and. abs(x(2)-xo(2))<tol .and. abs(x(3)-xo(3))<tol .and. abs(x(4)-xo(4))<tol) then

                exit
            endif

            k=k+1

            do i=1,n,1
                xo(i)=x(i)
            enddo
    enddo
    k=1
    do while(k<=nnn)
        do i=1,n,1
            sum=0


            do j=1,n,1
                sum=sum+a(i,j)*x(j)
            enddo

            r(i)=b(i)-sum
        enddo

    yo(1)=0
    yo(2)=0
    yo(3)=0
    yo(4)=0

    kk=1
    do while(kk<=NN)
        do i=1,n,1
            sum=0
            sum2=0

            do j=1,i-1,1
                sum2=sum2+a(i,j)*y(j)
            enddo
            do j=i+1,n,1
                sum=sum+a(i,j)*yo(j)
            enddo
            y(i)=(1/a(i,i))*(-sum2-sum+r(i))
        enddo

            if(abs(y(1)-yo(1))<tol .and. abs(y(2)-yo(2))<tol .and. abs(y(3)-yo(3))<tol .and. abs(y(4)-yo(4))<tol) then

                exit
            endif

            kk=k+1

            do i=1,n,1
                yo(i)=y(i)
            enddo
    enddo
    do i=1,n,1
        xx(i)=x(i)+y(i)
    enddo
    if(k==1) then
            cond=(y(1)/xx(1))*10**t
    endif
    if(abs(x(1)-xx(1))<tol) then
        write(*,*) 'xx'
        write(*,*) xx
        write(*,*) 'cond:', cond
        stop
    endif

    k=k+1
    do i=1,n,1
        x(i)=xx(i)
    enddo
    enddo






    write(*,*)'max no. of iterations exceeded'
    write(*,*) cond
    endprogram

