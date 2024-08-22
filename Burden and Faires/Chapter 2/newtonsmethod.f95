program NM
    implicit none
    real pnot,p,f,g,T
    integer N,i

i=1
N=1000
T=1e-9
pnot=3
do i=1,N,1
    p=pnot-f(pnot)/g(pnot)
    if (abs(p-pnot)<T) then
        write(*,*) 'the root is at x =', p
        stop
    endif
    pnot=p
    write(*,*) p
enddo

end program NM

real function f(x)
    real, intent(in) :: x
    f = cos(x)-x
end function f
real function g(x)
    real, intent(in) :: x
    g = -sin(x)-1
end function g


