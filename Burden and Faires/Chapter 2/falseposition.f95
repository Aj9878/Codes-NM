program fp
implicit none
real q,f,pone,pnot,p,qnot,qone,T
integer i,N
N=1000
pnot=3
pone=4
T=1e-9
qnot=f(pnot)
qone=f(pone)
do i=2,N,1
    p=pone-qone*(pone-pnot)/(qone-qnot)
    if (abs(p-pone)<T) then
        write(*,*) 'the root is at x =',p
        stop
    endif
    q=f(p)
    if (q*qone<0) then
        pnot=pone
        qnot=qone
    endif
    pone=p
    qone=q


enddo


end program fp

real function f(x)
    real, intent(in) :: x
    f = cos(x)-x
end function f



