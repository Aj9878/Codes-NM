!Linear Finite-Difference
!To approximate the solution of the boundary-value problem
!y′′ = p(x)y′ + q(x)y + r(x),
!for a ≤ x ≤ b, with y(a) = α and y(b) = β
program linear_finite_difference
    implicit none
    integer,parameter::N=9
    integer :: i
    real :: a0, b0, alpha, beta, h, x,q,r,p
    real, allocatable :: w(:), a_coef(:), b_coef(:), c_coef(:), d_coef(:), l(:), u(:), z(:)

    ! Input initial conditions
    a0 = 1.0
    b0 = 2.0
    alpha = 1.0
    beta = 2.0
   
  
    ! Step 1: Initialization
    h = (b0 - a0) / real(N + 1)
    allocate(w(0:N+1), a_coef(1:N), b_coef(1:N), c_coef(1:N), d_coef(1:N), l(1:N), u(1:N), z(1:N))

    x = a0 + h
    a_coef(1) = 2.0 + h**2 * q(x)
    b_coef(1) = -1.0 + (h/2.0) * p(x)
    d_coef(1) = -h**2 * r(x) + (1.0 + (h/2.0) * p(x)) * alpha

    ! Step 2: Coefficients calculation
    do i = 2, N-1
        x = a0 + i*h
        a_coef(i) = 2.0 + h**2 * q(x)
        b_coef(i) = -1.0 + (h/2.0) * p(x)
        c_coef(i-1) = -1.0 - (h/2.0) * p(x)
        d_coef(i) = -h**2 * r(x)
    end do

    ! Step 3: Last coefficients calculation
    x = b0 - h
    a_coef(N) = 2.0 + h**2 * q(x)
    c_coef(N-1) = -1.0 - (h/2.0) * p(x)
    d_coef(N) = -h**2 * r(x) + (1.0 - (h/2.0) * p(x)) * beta

    ! Step 4: Tridiagonal linear system solution
    l(1) = a_coef(1)
    u(1) = b_coef(1) / a_coef(1)
    z(1) = d_coef(1) / l(1)

    ! Step 5
    do i = 2, N
        l(i) = a_coef(i) - c_coef(i-1) * u(i-1)
        u(i) = b_coef(i) / l(i)
        z(i) = (d_coef(i) - c_coef(i-1) * z(i-1)) / l(i)
    end do

    ! Step 6
    l(N) = a_coef(N) - c_coef(N-1) * u(N-1)
    z(N) = (d_coef(N) - c_coef(N-1) * z(N-1)) / l(N)

    ! Step 7
    w(0) = alpha
    w(N+1) = beta
    w(N) = z(N)

    ! Step 8
    do i = N-1, 1, -1
        w(i) = z(i) - u(i) * w(i+1)
    end do

    ! Step 9: Output
    do i = 0, N+1
        x = a0 + i*h
        print *, 'x',x,'w(',i,')=' ,w(i)
    end do

    deallocate(w, a_coef, b_coef, c_coef, d_coef, l, u, z)
end program linear_finite_difference

    ! Define functions
    real function p(g)
        implicit none
        real :: g
        p = -2.0 / g
    end function p

    real function q(g)
        implicit none
        real :: g
        q = 2.0 / (g**2)
        return
    end function q

    real function r(g)
        implicit none
        real :: g
        r = (sin(log(g))) / (g**2)
    end function r
