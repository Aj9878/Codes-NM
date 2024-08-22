program qr
    implicit none
    integer,parameter::n=3
    real a(n),b(n),tol,lamda,bb,c,d,mu,mu1,mu2,lamda1,lamda2,sigma
    real dd(n),cc(n),sig(n),z(n),x(n),y(n),s(n),q(n),r(n),shift
    integer i,j,k,m,nn
    nn=n
    !

    print*, 'the dimension n should be assigned in the program'
    print*,'tridiagonal elements of matrix b(1:n),a(1:n),a is the diagonal'
    do i=1,nn,1

    print*, 'a',i
    read(*,*) a(i)
    enddo
    do i=2,nn,1
    print*, 'b',i
    read(*,*) b(i)
    enddo
    print*, 'max iterations'
    read*, m
    print*,'tolerance'
    read*, tol

    k=1
    shift=0
    do while(k<=m)
        if(abs(b(nn))<=tol) then
            lamda=a(nn)+shift
            print*, 'lamda=',lamda
            nn=nn-1
        endif
        if(abs(b(2))<=tol) then
            lamda=a(1)+shift
            print*, 'lamda=',lamda
            nn=nn-1
            a(1)=a(2)
            do j=2,nn
                a(j)=a(j+1)
                b(j)=b(j+1)
            enddo
        endif
        if(nn==0) then
            stop
        endif
        if(nn==1) then
            lamda=a(1)+shift
            write(*,*) 'lamda=',lamda
            stop
        endif
        do j=3,nn-1
            if(abs(b(j))<=tol) then
                print*,'split into'
                print*,'a:'

                do i=1,j-1
                    print*,a(i)
                enddo
                print*,'b:'
                do i=1,j-1
                    print*,b(i)
                enddo
                print*,'and'
                print*,'a:'

                do i=j,nn
                    print*,a(i)
                enddo
                print*,'b:'
                do i=j,nn
                    print*,b(i)
                enddo
                stop
            endif
        enddo
        bb=-(a(nn-1)+a(nn))
        c=a(nn)*a(nn-1)-(b(nn)**2)
        d=((bb**2)-4*c)**(0.5)
        if(bb>0) then
            mu1=(-2*c)/(bb+d)
            mu2=-(bb+d)/2.0
        else
            mu1=(d-bb)/2.0
            mu2=2*c/(d-bb)
        endif
        if(nn==2) then
            lamda1=mu1+shift
            lamda2=mu2+shift
            print*, 'lamda1=',lamda1,'lamda2=',lamda2
            stop
        endif

        if(abs(mu2-a(nn))<abs(mu1-a(nn))) then
            sigma=mu2
        else
            sigma=mu1
        endif
        shift=shift+sigma
        do j=1,nn
            dd(j)=a(j)-sigma
        enddo
        x(1)=dd(1)
        y(1)=b(2)
        do j=2,nn
            z(j-1)=((x(j-1)**2)+(b(j)**2))**0.5
            cc(j)=x(j-1)/z(j-1)
            sig(j)=b(j)/z(j-1)
            q(j-1)=cc(j)*y(j-1)+sig(j)*dd(j)
            x(j)=-sig(j)*y(j-1)+cc(j)*dd(j)
            if(j.ne.nn) then
                r(j-1)=sig(j)*b(j+1)
                y(j)=cc(j)*b(j+1)
                endif
            enddo
            z(nn)=x(nn)
            a(1)=sig(2)*q(1)+cc(2)*z(1)
            b(2)=sig(2)*z(2)
            do j=2,nn-1
                a(j)=sig(j+1)*q(j)+cc(j)*cc(j+1)*z(j)
                b(j+1)=sig(j+1)*z(j+1)
            enddo
            a(nn)=cc(nn)*z(nn)
            k=k+1

        enddo
        print*,'max iterations exceeded'
    end program




                                                                                                                                                        pro
