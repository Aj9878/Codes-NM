program mullersmethod
    implicit none
    complex p,f,p0,p1,p2,h1,h2,d1,d2,d,b,bD,E,h
    real T
    integer i,N
    N=1000
    i=3
    T=0.00001
    p0=(0.5,0)
    p1=(-0.5,0)
    p2=(0,0)
    h1=p1-p0
    h2=p2-p1
    d1=(f(p1)-f(p0))/h1
    d2=(f(p2)-f(p1))/h2
    d=(d2-d1)/(h2+h1)

    do i=3,N,1
        b=d2+h2*d
        bD=(b**2-4*f(p2)*d)**(1/2)
        if(abs(b-bD)<abs(b+bD)) then
            E=b+bD
        else
            E=b-bD
        endif

        h=-2*f(p2)/E
        p=p2+h
        if(abs(h)<T) then
            write(*,*) 'the root is at x=', p
            stop
        endif
        p0 = p1
        p1 = p2
        p2 = p
        h1=p1-p0
        h2=p2-p1
        d1=(f(p1)-f(p0))/h1
        d2=(f(p2)-f(p1))/h2
        d=(d2-d1)/(h2+h1)
    end do
    write(*,*) 'failed after',N,'iterations'

end program


complex function f(x)
    complex, intent(in) :: x
    f = x**4+(-3,0)*x**3+x**2+x+(1,0)
end function f



