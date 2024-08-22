program fpi
implicit none
real p,T,f,pnot
integer i,N
i=1
N=1000
pnot=1.5
T=1e-9
do i=1,N,1
    p=f(pnot)


    if(abs(p-pnot)<T) then
        write(*,*) 'the root is at x=', p
        stop
    endif
    pnot=p


enddo


end program fpi

real function f(x)
    real, intent(in) :: x
    f = x-(x**3 + 4*x**2- 10)/(3*x**2 + 8*x)
end function f
