program bisection
    implicit none
    real :: a,b, root

    a=0
    b=1.0

    call bisect(a, b, root)
    print*, "the root of the given functon is ", root
    
end program bisection

real function f(x)
    implicit none
    real :: x
    f = x-cos(x)
end function

subroutine bisect(a, b, root)
    implicit none
    real :: a, b 
    real :: c 
    real :: tol, error, f    
    real :: root
    tol = 0.001
50  c = (a + b ) / 2.0

    if (f(c)*f(b)< 0) then
        a=c
    else
        b=c  
    end if

    end if 
    error =abs((a-b)/c )
    if (error <= tol) then 
    root = c
    else
    goto 50
    end if 
end subroutine bisect