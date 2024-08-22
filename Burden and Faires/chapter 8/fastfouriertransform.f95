program fft
    implicit none
    real, parameter::pi=3.142857
    integer, parameter::m=2,p=1
    integer l,i,j,k,mm,qq
    !jj for indices of k on step 8
    complex y(0:2*m-1),zeta,zai(0:m+m),c(0:2*m-1),n,kk,jj,kk1,kk2
    do j=0,2*m-1,1
        c(j)=y(j)
    enddo
    do j=1,mm
        zai(j)=zeta**j
        zai(j+mm)=-zai(j)
    enddo
    kk=0.0
    kk1=0.0
    kk2=0.0
    zai(0)= 1
    do l=1,p+1
        do while(k<(2*m-1))
            do j=1,mm
                do jj=0,p 
                    kk=k(p-jj)*2**(p-jj)+kk 
                 enddo
                do jj=q,p 
                    kk1=kk1+K(jj)*2**(jj-q)
                    kk2=kk2+k(jj)*2**(p-(jj-q))
                enddo
                n=c(k)+mm*zeta*kk2 
                c(kk+mm)=c(kk)-n 
                c(kk)=c(kk)+n 
                kk=kk+1
            enddo
                kk=kk+mm 
        enddo
                kk=0 
                mm=mm/2 
                q=q-1
    enddo
                do while(kk<(2*m-1))
                    kk=k(p)
                end do
    end program fft