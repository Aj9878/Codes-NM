program extrapolation
    implicit none
    real f,a,b,alpha,Tol,hmax,hmin,t,w,w1,w2,w3,NK(8),t0,w0,h,q(7,7),HK,y(32),v
    integer FLAG,nflag,i,j,k

    NK=(/2,4,6,8,12,16,24,32/)


    a=0
    b=2
    alpha=0.5
    Tol=1e-5
    hmax=0.2
    hmin=0.01

    t0=a
    w0=alpha
    h=hmax
    Flag=1

    do i=1,7,1
        do j=1,i,1
            q(i,j)=(nk(i+1)/nk(j))**2
        end do
    end do
    do while(flag==1)
        k=1
        nflag=0
        do while(k<=8 .and. nflag==0)
            HK=h/NK(k)
            t=t0
            w2=w0
            w3=w2+hk*f(t,w2)
            t=t0+hk
            do j=1,nk(k)-1,1
                w1=w2
                w2=w3
                w3=w1+2*hk*f(t,w2)
                t=t0+(j+1)*hk
            end do
            y(k)=(w3+w2+hk*f(t,w3))/2

            if(k>=2) then
                j=k
                v=y(1)
                do while(j>=2)
                    y(j-1)=y(j)+(y(j)-y(j-1))/(q(k-1,j-1)-1)
                    j=j-1
                end do
                if(abs(y(1)-v)<=tol) then
                    nflag=1
                end if
            end if
            k=k+1
        end do
        k=k-1
        if(nflag==0) then
            h=h/2
            if(h<hmin) then
                write(*,*) 'hmin exceeded'
                flag=0
            end if
        else
            w0=y(1)
            t0=t0+h
            write(*,*) t0,w0

            if(t0>=b) then
                flag=0
            elseif((t0+h)>b) then
                h=b-t0
            elseif(k<=3 .and. h<0.5*hmax) then
                h=2*h
            end if
        end if

    end do


end program

real function f(t,y)
real,intent(in)::t,y
f=y-t**2+1

end function
