program sm
implicit none
real f,p1,p0,p,T,p2
integer i,N
N=1000
p0=3
T=0.00001

do i=1,N,1
    p1=f(p0)
    p2=f(p1)
    p=p0-((p1-p0)**2)/(p2-2*p1+p0)
    if (abs(p-p0)<T) then
        write(*,*) 'the root is at x =',p
        stop
    endif
    p0=p
enddo


end program sm

real function f(x)
    real, intent(in) :: x
    f = x**3 + 4*x**2 - 10
end function f



